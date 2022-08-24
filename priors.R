
for(team in (as.vector(unique(a)))){
  cat(paste0("a",team), "=", "prec.fixed",",")
}
for(team in (as.vector(unique(a)))){
  cat(paste0("d",team), "=", "prec.fixed",",")
}

HT.prior = "expression:
  sigma = exp(-theta/2);
  nu = 3;
  log_dens = 0 - 0.5 * log(nu * pi) - (-0.1207822);
  log_dens = log_dens - 0.5 * (nu + 1) * log(1 + sigma * sigma);
  log_dens = log_dens - log(2) - theta / 2;
  return(log_dens);
"

UN.prior = "expression:
  log_dens = 0 - log(2) - theta / 2;
  return(log_dens);
"
HC.prior  = "expression:
  sigma = exp(-theta/2);
  gamma = 25;
  log_dens = log(2) - log(pi) - log(gamma);
  log_dens = log_dens - log(1 + (sigma / gamma)^2);
  log_dens = log_dens - log(2) - theta / 2;
  return(log_dens);
"
HN.prior = "expression:
  tau0 = 0.001;
  sigma = exp(-theta/2);
  log_dens = log(2) - 0.5 * log(2 * pi) + 0.5 * log(tau0);
  log_dens = log_dens - 0.5 * tau0 * sigma^2;
  log_dens = log_dens - log(2) - theta / 2;
  return(log_dens);  
"
### dof = list(prior = "loggamma",param = c(1,1))
prec.fixed = 1
default = 0.01
prior.beta <- list(mean.intercept = 0, prec.intercept = 0.1,
                   prec = list( aCavaliers = prec.fixed ,aWarriors = prec.fixed ,aRockets = prec.fixed ,aCeltics = prec.fixed ,a76ers = prec.fixed ,aPelicans = prec.fixed ,aJazz = prec.fixed ,aRaptors = prec.fixed ,aPacers = prec.fixed ,aWizards = prec.fixed ,aBucks = prec.fixed ,aThunder = prec.fixed ,aTimberwolves = prec.fixed ,aSpurs = prec.fixed ,aHeat = prec.fixed ,"aTrail Blazers" = prec.fixed ,aKings = prec.fixed ,aClippers = prec.fixed ,aBulls = prec.fixed ,aMagic = prec.fixed ,aHawks = prec.fixed ,aMavericks = prec.fixed ,aLakers = prec.fixed ,aNuggets = prec.fixed ,aPistons = prec.fixed ,aKnicks = prec.fixed ,aNets = prec.fixed ,aSuns = prec.fixed ,aGrizzlies = prec.fixed ,aHornets = prec.fixed ,
                                dCavaliers = prec.fixed ,dWarriors = prec.fixed ,dRockets = prec.fixed ,dCeltics = prec.fixed ,d76ers = prec.fixed ,dPelicans = prec.fixed ,dJazz = prec.fixed ,dRaptors = prec.fixed ,dPacers = prec.fixed ,dWizards = prec.fixed ,dBucks = prec.fixed ,dThunder = prec.fixed ,dTimberwolves = prec.fixed ,dSpurs = prec.fixed ,dHeat = prec.fixed ,"dTrail Blazers" = prec.fixed ,dKings = prec.fixed ,dClippers = prec.fixed ,dBulls = prec.fixed ,dMagic = prec.fixed ,dHawks = prec.fixed ,dMavericks = prec.fixed ,dLakers = prec.fixed ,dNuggets = prec.fixed ,dPistons = prec.fixed ,dKnicks = prec.fixed ,dNets = prec.fixed ,dSuns = prec.fixed ,dGrizzlies = prec.fixed ,dHornets = prec.fixed ,
                                h = prec.fixed, PTS_cur_season =prec.fixed,FG_PCT_cur_season=prec.fixed, FT_PCT_cur_season=prec.fixed, FG3_PCT_cur_season=prec.fixed, AST_cur_season=prec.fixed,REB_cur_season=prec.fixed,WINRATE_cur_season=prec.fixed, PTS_LOST_cur_season =prec.fixed,
                                default = default))


prior.list = list(
  #default = list(prec = list(prior = "loggamma", param = c(1, 0.00005))),
  #default2 = list(prec = list(prior = "loggamma", param = c(0.1, 0.1))),
  #default3 = list(prec = list(prior = "loggamma", param = c(0.1, 0.1)))
  #half.normal = list(prec = list(prior = HN.prior)),
  #half.cauchy = list(prec = list(prior = HC.prior)),
  #h.t = list(prec = list(prior = HT.prior)),
  #uniform = list(prec = list(prior = UN.prior)),
  #pc.prec = list(prec = list(prior = "pc.prec", param = c(5, 0.01)))
  #default4 = list(prec=list(initial = log(0.1), fixed = TRUE))
  #default5 = list(prec=list(initial = log(10), fixed = TRUE)),
  #default6 = list(prec=list(initial = log(100), fixed = TRUE)),
  default6 = list(prec=list(initial = log(1000), fixed = TRUE))
  #default7 = list(prec=list(initial = log(2000), fixed = TRUE))
  #default8 = list(prec=list(initial = log(900), fixed = TRUE))
)

csize.models <- lapply(prior.list, function(tau.prior) {
  # inla(y~1+a+d+h+PTS_cur_season+FG_PCT_cur_season+FT_PCT_cur_season+FG3_PCT_cur_season+AST_cur_season+REB_cur_season+WINRATE_cur_season+ PTS_LOST_cur_season +log(salary)+type:a +type+type:d
  #      
  #      + f(a.season, model = "generic0", Cmatrix = Q.a, rankdef = 1, constr = TRUE,hyper = tau.prior) 
  #      + f(d.season, model = "generic0", Cmatrix = Q.d, rankdef = 1, constr = TRUE,hyper = tau.prior)
  #      #+ f(a.quarter, model = "generic0", Cmatrix = Q.a.q, rankdef = 1, constr = TRUE) 
  #      #+ f(d.quarter, model = "generic0", Cmatrix = Q.d.q, rankdef = 1, constr = TRUE)
  #      #+ f(quarter)
  #      #+ f(depend)
  #      +f(inla.group(travel), model = "rw1", scale.model = TRUE)
  #      ,control.fixed = prior.beta
  #      , data = data, family = "poisson",control.compute=list(config = TRUE,cpo = TRUE,dic = TRUE),control.predictor = list(compute = TRUE),verbose = FALSE)
  inla(y.binary~1+a+d+h+PTS_cur_season+FG_PCT_cur_season+FT_PCT_cur_season+FG3_PCT_cur_season+AST_cur_season+REB_cur_season+WINRATE_cur_season+ PTS_LOST_cur_season +log(salary)+type*a + type*d
                              + f(a.season, model = "generic0", Cmatrix = Q.a, rankdef = 1, constr = TRUE,hyper = tau.prior)
                              + f(d.season, model = "generic0", Cmatrix = Q.d, rankdef = 1, constr = TRUE,hyper = tau.prior)
                              #+ f(quarter)
                              #+ f(a.quarter, model = "generic0", Cmatrix = Q.a.q, rankdef = 1, constr = TRUE) 
                              #+ f(d.quarter, model = "generic0", Cmatrix = Q.d.q, rankdef = 1, constr = TRUE)
                              
                              #+ f(game_day, model = "rw1", scale.model = TRUE)
                              #+ ,control.fixed = prior.beta
                              + f(inla.group(travel), model = "rw1", scale.model = TRUE)
       ,control.fixed = prior.beta
                              , data = data, family = "binomial",Ntrials = 1,control.compute=list(config = TRUE,dic=TRUE,cpo=TRUE),control.predictor = list(compute = TRUE),num.threads = 2)
  
})

for(m in csize.models){
  cat("fixed ", prec.fixed," interaction ", default, prior.list$default3$prec$prior, prior.list$default3$prec$param
      ,",")
  cat(m$dic$dic)
  cat("\n")
}







prec.list = list(a = 0.001, b = 0.01, c = 0.1, d = 1, e = 10, f = 100)
test.models <- lapply(prec.list, function(prec.fixed) {
  prior.beta <- list(mean.intercept = 0, prec.intercept = 0.001,
                     prec = list( aCavaliers = prec.fixed ,aWarriors = prec.fixed ,aRockets = prec.fixed ,aCeltics = prec.fixed ,a76ers = prec.fixed ,aPelicans = prec.fixed ,aJazz = prec.fixed ,aRaptors = prec.fixed ,aPacers = prec.fixed ,aWizards = prec.fixed ,aBucks = prec.fixed ,aThunder = prec.fixed ,aTimberwolves = prec.fixed ,aSpurs = prec.fixed ,aHeat = prec.fixed ,"aTrail Blazers" = prec.fixed ,aKings = prec.fixed ,aClippers = prec.fixed ,aBulls = prec.fixed ,aMagic = prec.fixed ,aHawks = prec.fixed ,aMavericks = prec.fixed ,aLakers = prec.fixed ,aNuggets = prec.fixed ,aPistons = prec.fixed ,aKnicks = prec.fixed ,aNets = prec.fixed ,aSuns = prec.fixed ,aGrizzlies = prec.fixed ,aHornets = prec.fixed ,
                                  dCavaliers = prec.fixed ,dWarriors = prec.fixed ,dRockets = prec.fixed ,dCeltics = prec.fixed ,d76ers = prec.fixed ,dPelicans = prec.fixed ,dJazz = prec.fixed ,dRaptors = prec.fixed ,dPacers = prec.fixed ,dWizards = prec.fixed ,dBucks = prec.fixed ,dThunder = prec.fixed ,dTimberwolves = prec.fixed ,dSpurs = prec.fixed ,dHeat = prec.fixed ,"dTrail Blazers" = prec.fixed ,dKings = prec.fixed ,dClippers = prec.fixed ,dBulls = prec.fixed ,dMagic = prec.fixed ,dHawks = prec.fixed ,dMavericks = prec.fixed ,dLakers = prec.fixed ,dNuggets = prec.fixed ,dPistons = prec.fixed ,dKnicks = prec.fixed ,dNets = prec.fixed ,dSuns = prec.fixed ,dGrizzlies = prec.fixed ,dHornets = prec.fixed ,
                                  h = prec.fixed, PTS_cur_season =prec.fixed,FG_PCT_cur_season=prec.fixed, FT_PCT_cur_season=prec.fixed, FG3_PCT_cur_season=prec.fixed, AST_cur_season=prec.fixed,REB_cur_season=prec.fixed,WINRATE_cur_season=prec.fixed, PTS_LOST_cur_season =prec.fixed,
                                  default = 10))
  
  inla(y~1+a+d+h+PTS_cur_season+FG_PCT_cur_season+FT_PCT_cur_season+FG3_PCT_cur_season+AST_cur_season+REB_cur_season+WINRATE_cur_season+ PTS_LOST_cur_season +log(salary)+type:a +type+type:d
       
       + f(a.season, model = "generic0", Cmatrix = Q.a, rankdef = 1, constr = TRUE,hyper = prior.list$pc.prec) 
       + f(d.season, model = "generic0", Cmatrix = Q.d, rankdef = 1, constr = TRUE,hyper = prior.list$pc.prec) 
       #+ f(a.quarter, model = "generic0", Cmatrix = Q.a.q, rankdef = 1, constr = TRUE) 
       #+ f(d.quarter, model = "generic0", Cmatrix = Q.d.q, rankdef = 1, constr = TRUE)
       #+ f(quarter)
       #+ f(depend)
       +f(inla.group(travel), model = "rw1", scale.model = TRUE)
       ,control.fixed = prior.beta
       , data = data, family = "poisson",control.compute=list(config = TRUE,cpo = TRUE,dic = TRUE),control.predictor = list(compute = TRUE),verbose = FALSE)
  
})
for(m in test.models){
  print(m$dic$dic)
}
