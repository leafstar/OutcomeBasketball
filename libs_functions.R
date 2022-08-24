library(INLA)
inla.setOption(inla.mode="experimental")
library(dplyr)
library(magrittr)
library(locfit)
library(extraDistr)
library(Matrix)
library(lme4)
library(bootstrap)
library(VGAM)
K=120
lambda1 = 4
lambda2 = 6
prob2 = 0
for(k in 0:K){
  prob2 = prob2+ (1-sum(exp(-lambda1)*lambda1**(0:k)/factorial(0:k)))*exp(-lambda2)*lambda2**k/factorial(k)
}
prob = 0
for(i in 1:100){
  prob = prob + dskellam(i,4,6,log=FALSE)
}
denominator = 1- exp(-lambda1-lambda2)* sum((lambda1*lambda2)**(0:K)/factorial((0:K))**2)
deno.skellam = 1 - dskellam(0,4,6,log=FALSE)
print(prob - prob2)
print(deno.skellam - denominator)

## team1 wins
winning.prob = function(lambda1,lambda2){
  sum(dskellam(1:100,lambda1,lambda2,log=FALSE))/(1 - dskellam(0,lambda1,lambda2,log=FALSE))
}


## probability that P(Z1>Z2) and Z1~N(mu1,sigma1),  Z2~N(mu2,sigma1) because we assume they are homoscedastic
winning.prob.G = function(mu1,mu2,sigma){
  1 - pnorm(0, mu1-mu2, sigma*2)
}

# ranked probability score
rps<- function(obs,pred, R){
  res = 0
  for (i in 1:(R-1)){
    subres = 0
    for(j in 1:i){
      subres = subres + (pred[j] - obs[j])
    }
    res = res + subres**2
  }
  res* 1/ (R-1)
}
rps(c(1,0,0),c(0.8,0.2,0),3)

rps.var <- function(probs){
  vars = probs*(1-probs)*(probs^3+(1-probs)^3- probs*(1-probs))
  n = length(probs)
  sum(vars)/(n**2)
}

rps.sd <- function(probs){
  sqrt(rps.var(probs))
}

oracle<- function(winning.prob){
  
  #pred = rowMeans(winning.prob)
  pred = winning.prob
  n = length(pred)
  #pred= rep(0.5,n)
  obs = c()
  for(i in 1:n){
    obs = c(obs, rbinom(1,1,pred[i]))
  }
  acc = sum(as.numeric(obs == as.numeric(pred>0.5)))/n
  mean.rps = round(mean((pred - obs)**2),5)
  sd.rps = round(sd((pred - obs)**2),5)
  list(acc = acc, mean.rps = mean.rps, sd.rps = sd.rps)
}
get.exp.val<- function(winning.prob){
  acc = c()
  mean.rps = c()
  sd.rps = c()
  for(i in 1:1000){
    res = oracle(winning.prob)
    acc = c(acc, res$acc)
    mean.rps = c(mean.rps, res$mean.rps)
    sd.rps = c(sd.rps, res$sd.rps)
  }
  list(acc=acc, rps = mean.rps)
}