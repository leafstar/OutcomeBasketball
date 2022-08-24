## Estimating RPS based on true probabilities
data.probs = read.csv("./M6_2.csv")
Mdata= data.probs[data.probs$model == "M7",]
res7l = get.exp.val(Mdata$logiProb)
Mdata= data.probs[data.probs$model == "M6",]
res6l = get.exp.val(Mdata$logiProb)
Mdata= data.probs[data.probs$model == "M5",]
res5l = get.exp.val(Mdata$logiProb)
Mdata= data.probs[data.probs$model == "M4",]
res4l = get.exp.val(Mdata$logiProb)
Mdata= data.probs[data.probs$model == "M3",]
res3l = get.exp.val(Mdata$logiProb)
Mdata= data.probs[data.probs$model == "M2",]
res2l = get.exp.val(Mdata$logiProb)
Mdata= data.probs[data.probs$model == "M1",]
res1l = get.exp.val(Mdata$logiProb)

Mdata= data.probs[data.probs$model == "M7",]
res7p = get.exp.val(Mdata$poisProb )
Mdata= data.probs[data.probs$model == "M6",]
res6p = get.exp.val(Mdata$poisProb )

Mdata= data.probs[data.probs$model == "M5",]
res5p = get.exp.val(Mdata$poisProb )

Mdata= data.probs[data.probs$model == "M4",]
res4p = get.exp.val(Mdata$poisProb )

Mdata= data.probs[data.probs$model == "M3",]
res3p = get.exp.val(Mdata$poisProb )
Mdata= data.probs[data.probs$model == "M2",]
res2p = get.exp.val(Mdata$poisProb )
Mdata= data.probs[data.probs$model == "M1",]
res1p = get.exp.val(Mdata$poisProb )


par(mfrow=c(2,4))

hist(res1p$rps,xlab = "RPS",main="M1 Poisson", xlim = c(0.23,0.25))
abline(v = 0.2343, col = "red")
sum(res1p$rps<0.2343)

hist(res2p$rps,xlab = "RPS",main="M2 Poisson")
abline(v = 0.2209, col = "red")
sum(res2p$rps<0.2209)

hist(res3p$rps,xlab = "RPS",main="M3 Poisson", xlim = c(0.219,0.25))
abline(v = 0.2196, col = "red")
sum(res3p$rps<0.2196)

hist(res4p$rps,xlab = "RPS",main="M4 Poisson")
abline(v = 0.2210, col = "red")
sum(res4p$rps<0.2210)
hist(res5p$rps,xlab = "RPS",main="M5 Poisson", xlim = c(0.218,0.25))
abline(v = 0.2185, col = "red")
sum(res5p$rps<0.2185)
hist(res6p$rps,xlab = "RPS",main="M6 Poisson")
abline(v = 0.2188, col = "red")
sum(res6p$rps<0.2188)
hist(res7p$rps,xlab = "RPS",main="M7 Poisson")
abline(v = 0.2183, col = "red")
sum(res7p$rps<0.2183)
par(mfrow=c(2,4))

hist(res1l$rps,xlab = "RPS",main="M1 Logistic", xlim = c(0.23,0.25))
abline(v = 0.2334, col = "red")
sum(res1l$rps<0.2334)
hist(res2l$rps,xlab = "RPS",main="M2 Logistic")
abline(v = 0.2189, col = "red")
sum(res2l$rps<0.2189)
hist(res3l$rps,xlab = "RPS",main="M3 Logistic")
abline(v = 0.2167, col = "red")
sum(res3l$rps<0.2167)
hist(res4l$rps,xlab = "RPS",main="M4 Logistic")
abline(v = 0.2167, col = "red")
sum(res4l$rps<0.2167)

hist(res5l$rps,xlab = "RPS",main="M5 Logistic")
abline(v = 0.2146, col = "red")
sum(res5l$rps<0.2146)

hist(res6l$rps,xlab = "RPS",main="M6 Logistic")
abline(v = 0.2145, col = "red")
sum(res6l$rps<0.2145)

hist(res7l$rps,xlab = "RPS",main="M7 Logistic")
abline(v = 0.2161, col = "red")
sum(res7l$rps<0.2161)
