## script for Poisson regression and Logistic Regression model
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
path = 'newdat/games_with_type.csv'
games.total = read.csv(path)

glm.pois.acc=c()
glm.pois.rps=c()
glm.pois.dic = c()
glm.pois.rps.sd = c()

glm.logi.acc=c()
glm.logi.rps=c()
glm.logi.dic=c()
glm.logi.rps.sd=c()

model.prob.pois=c()
model.prob.logi=c()

obs = c()

splits = c("2021-05-22","2020-08-17","2019-04-13","2018-04-14","2017-04-15","2016-04-13",
           "2015-04-15","2014-04-19","2013-04-20","2012-04-28","2011-04-16","2010-04-17",
           "2009-04-18","2008-04-19","2007-04-21","2006-04-22")
split.i = 16
training.season = 17
for(end.year in 2005:2020){
  
  
  ## change type to date
  #games.total %<>%
  #mutate(GAME_DATE_EST= as.Date(GAME_DATE_EST, format= "%m/%d/%Y"))
  
  
  
  #[start ------training-------)--[split-------testing-------end]
  #dates = unique(games.total$GAME_DATE_EST)
  ##  end           season
  #split = "2021-05-22" ##  2021-07-20  2020-2021    2020.csv
  #split = "2020-08-17"  ## 2020-10-11  2019-2020    2019.csv
  #split = "2019-04-13"  ## 2019-06-13  2018-2019    2018.csv
  #split = "2018-04-14"  ## 2018-06-08  2017-2018    2017.csv
  #split = "2017-04-15"  ## 2017-06-12  2016-2017    2016.csv
  #split = "2016-04-13"  ## 2016-06-19  2015-2016    2015.csv
  #split = "2015-04-15"  ## 2015-06-16  2014-2015    2014.csv 
  #split = "2014-04-19"  ## 2014-06-15  2013-2014    2013.csv
  #split = "2013-04-20"  ## 2013-06-20  2012-2013    2012.csv
  #split = "2012-04-28"  ## 2012-06-21  2011-2012    2011.csv
  #split = "2011-04-16"  ## 2011-06-12  2010-2011    2010.csv
  #split = "2010-04-17"  ## 2010-06-17  2009-2010    2009.csv
  #split = "2009-04-18"  ## 2009-06-14  2008-2009    2008.csv
  #split = "2008-04-19"  ## 2008-06-17  2007-2008    2007.csv
  #split = "2007-04-21"  ## 2007-06-14  2006-2007    2006.csv
  #split = "2006-04-22"  ## 2006-06-20  2005-2006    2005.csv
  #split = "2005-04-23"  ## 2005-06-23  2004-2005    2004.csv
  
  split = splits[split.i]
  split.i <- split.i -1
  
  games.sub = games.total[(games.total$SEASON <= end.year)&(games.total$SEASON >= end.year-training.season),]
  #games.sub = games.total[games.total$SEASON<=2020,]
  games.true = games.sub

  games.sub$PTS_home[games.sub$GAME_DATE_EST>=split] = NA  ## for prediction
  games.sub$PTS_away[games.sub$GAME_DATE_EST>=split] = NA
  
  games.sub$HOME_TEAM_WINS[games.sub$GAME_DATE_EST>=split] = NA
  
  

    games.sub$PTS_home_cur_season[games.sub$PTS_home_cur_season == 0] <- NA #1
    games.sub$PTS_LOST_home_cur_season[games.sub$PTS_LOST_home_cur_season == 0] <- NA #1
    games.sub$FG_PCT_home_cur_season[games.sub$FG_PCT_home_cur_season == 0] <- NA #2
    games.sub$FT_PCT_home_cur_season[games.sub$FT_PCT_home_cur_season == 0] <- NA #3
    games.sub$FG3_PCT_home_cur_season[games.sub$FG3_PCT_home_cur_season == 0] <- NA #4
    games.sub$AST_home_cur_season[games.sub$AST_home_cur_season == 0] <- NA #5
    games.sub$REB_home_cur_season[games.sub$REB_home_cur_season == 0] <- NA #6
    
    games.sub$PTS_away_cur_season[games.sub$PTS_away_cur_season == 0] <- NA #7
    games.sub$PTS_LOST_away_cur_season[games.sub$PTS_LOST_away_cur_season == 0] <- NA #7
    games.sub$FG_PCT_away_cur_season[games.sub$FG_PCT_away_cur_season == 0] <- NA #8
    games.sub$FT_PCT_away_cur_season[games.sub$FT_PCT_away_cur_season == 0] <- NA #9
    games.sub$FG3_PCT_away_cur_season[games.sub$FG3_PCT_away_cur_season == 0] <- NA #10
    games.sub$AST_away_cur_season[games.sub$AST_away_cur_season == 0] <- NA #11
    games.sub$REB_away_cur_season[games.sub$REB_away_cur_season == 0] <- NA #12
    print(path)
  
  n.season = length(unique(games.sub$SEASON))
  n.team = length(unique(games.sub$NAME_HOME))
  n.quarter = length(unique(games.sub$QUARTER_I))
  
  max.season = max(games.sub$SEASON)
  min.season = min(games.sub$SEASON)
  min.quarter = min(games.sub$QUARTER_I)
  
  
  ## add attack/defend index feature
  teams = levels(as.factor(games.total$NAME_HOME))
  
  
  a.index = c()
  d.index = c()
  for (i in 1:nrow(games.sub)){
    home.team = games.sub[i,]$NAME_HOME
    away.team = games.sub[i,]$NAME_AWAY
    s = games.sub[i,]$SEASON
    a.index = c(a.index, (which(teams == home.team)-1) * n.season+ s - min.season+1)
    d.index = c(d.index, (which(teams == away.team)-1) * n.season+ s - min.season+1)
  }
  games.sub$a.index = a.index
  games.sub$d.index = d.index
  
  ## add attack/defende index quarter feature
  
  
  a.index.q = c()
  d.index.q = c()
  for (i in 1:nrow(games.sub)){
    home.team = games.sub[i,]$NAME_HOME
    away.team = games.sub[i,]$NAME_AWAY
    q = games.sub[i,]$QUARTER_I
    a.index.q = c(a.index.q, (which(teams == home.team)-1) * n.quarter+ q- min.quarter+1 )
    d.index.q = c(d.index.q, (which(teams == away.team)-1) * n.quarter+ q - min.quarter+1)
  }
  games.sub$a.index.q = a.index.q
  games.sub$d.index.q = d.index.q
  
  
  ## generate features.
  G = nrow(games.sub)
  
  salary = c(games.sub$salary_home, games.sub$salary_away)
  
  travel = c(rep(0,G), games.sub$distance_for_away)
  
  type = c(games.sub$type_home, games.sub$type_away)
  
  type2 = c(games.sub$type_home_2, games.sub$type_away_2)
  
  type3 = c(games.sub$type_home_3, games.sub$type_away_3)
  
  quarter = c(games.sub$QUARTER, games.sub$QUARTER)
  
  game_day = c(games.sub$GAME_DAY, games.sub$GAME_DAY)
  
  day_after_last_game = c(games.sub$day_after_last_game_home, games.sub$day_after_last_game_away)
  
  y1 = games.sub$PTS_home
  y2 = games.sub$PTS_away
  y = c(y1,y2)
  
  y1.binary = games.sub$HOME_TEAM_WINS
  y.binary = c(y1.binary, 1-y1.binary)
  
  a1 = as.character(games.sub$NAME_HOME)
  a2 = as.character(games.sub$NAME_AWAY)
  a = as.factor(c(a1,a2))
  d = as.factor(c(a2,a1))
  
  a.season = c(games.sub$a.index, games.sub$d.index)
  d.season = c(games.sub$d.index, games.sub$a.index)
  a.quarter = c(games.sub$a.index.q, games.sub$d.index.q)
  d.quarter = c(games.sub$d.index.q, games.sub$a.index.q)
  
  h1 = rep(1, G)
  h2 = rep(0, G)
  h = c(h1,h2)
  
  depend = c(1:G,1:G)
  
  PTS_cur_season = c(games.sub$PTS_home_cur_season, games.sub$PTS_away_cur_season)
  PTS_cur_season_oppo = c(games.sub$PTS_away_cur_season,games.sub$PTS_home_cur_season)
  
  PTS_LOST_cur_season = c(games.sub$PTS_LOST_home_cur_season, games.sub$PTS_LOST_away_cur_season)
  PTS_LOST_cur_season_oppo = c(games.sub$PTS_LOST_away_cur_season,games.sub$PTS_LOST_home_cur_season)
  
  FG_PCT_cur_season = c(games.sub$FG_PCT_home_cur_season, games.sub$FG_PCT_away_cur_season)
  FG_PCT_cur_season_oppo = c(games.sub$FG_PCT_away_cur_season, games.sub$FG_PCT_home_cur_season)
  
  FT_PCT_cur_season = c(games.sub$FT_PCT_home_cur_season, games.sub$FT_PCT_away_cur_season)
  FT_PCT_cur_season_oppo = c(games.sub$FT_PCT_away_cur_season,games.sub$FT_PCT_home_cur_season)
  
  FG3_PCT_cur_season = c(games.sub$FG3_PCT_home_cur_season, games.sub$FG3_PCT_away_cur_season)
  FG3_PCT_cur_season_oppo = c(games.sub$FG3_PCT_away_cur_season,games.sub$FG3_PCT_home_cur_season)
  
  AST_cur_season = c(games.sub$AST_home_cur_season, games.sub$AST_away_cur_season)
  AST_cur_season_oppo = c(games.sub$AST_away_cur_season,games.sub$AST_home_cur_season)
  REB_cur_season = c(games.sub$REB_home_cur_season, games.sub$REB_away_cur_season)
  REB_cur_season_oppo = c(games.sub$REB_away_cur_season,games.sub$REB_home_cur_season)
  WINRATE_cur_season = c(games.sub$WINRATE_home_cur_season, games.sub$WINRATE_away_cur_season)
  WINRATE_cur_season_oppo = c(games.sub$WINRATE_away_cur_season,games.sub$WINRATE_home_cur_season)
  
  ROUND = c(games.sub$round_number_home, games.sub$round_number_away)
  
  season = c(games.sub$SEASON, games.sub$SEASON)
  
  data = data.frame(y,y.binary,a,d,h,PTS_cur_season,FG_PCT_cur_season,FT_PCT_cur_season,FG3_PCT_cur_season,AST_cur_season,REB_cur_season,WINRATE_cur_season,
                    PTS_cur_season_oppo,FG_PCT_cur_season_oppo,FT_PCT_cur_season_oppo,FG3_PCT_cur_season_oppo,AST_cur_season_oppo,REB_cur_season_oppo,
                    WINRATE_cur_season_oppo,ROUND,PTS_LOST_cur_season,PTS_LOST_cur_season_oppo, season, a.season, d.season,depend, travel, type,a.quarter,d.quarter,salary,type2, type3
  )
  
  
  Q.single = toeplitz(c(2,-1, rep(0,n.season-2)))
  Q.single[1,1] = Q.single[n.season,n.season] = 1
  matrix.list = replicate(n.team, Q.single , simplify = FALSE)
  Q.a = bdiag(matrix.list)
  Q.d = Q.a
  
  ## for quarter
  Q.single.q = toeplitz(c(2,-1, rep(0,n.quarter-2)))
  Q.single.q[1,1] = Q.single.q[n.quarter,n.quarter] = 1
  matrix.list.q = replicate(n.team, Q.single.q , simplify = FALSE)
  Q.a.q = bdiag(matrix.list.q)
  Q.d.q = Q.a.q
  
  
  #### prior ####
  # prior.list = list(
  #   default = list(prec = list(prior = "loggamma", param = c(1, 0.00005))),
  #   half.normal = list(prec = list(prior = HN.prior)),
  #   half.cauchy = list(prec = list(prior = HC.prior)),
  #   h.t = list(prec = list(prior = HT.prior)),
  #   uniform = list(prec = list(prior = UN.prior)),
  #   pc.prec = list(prec = list(prior = "pc.prec", param = c(5, 0.01)))
  # ) 
  random.prec = list(prec=list(initial = log(700), fixed = TRUE))
  prec.fixed = 0.0001
  default = 100
  # prior.beta <- list(mean.intercept = 0, prec.intercept = 0.1,
  #                    prec = list( aCavaliers = prec.fixed ,aWarriors = prec.fixed ,aRockets = prec.fixed ,aCeltics = prec.fixed ,a76ers = prec.fixed ,aPelicans = prec.fixed ,aJazz = prec.fixed ,aRaptors = prec.fixed ,aPacers = prec.fixed ,aWizards = prec.fixed ,aBucks = prec.fixed ,aThunder = prec.fixed ,aTimberwolves = prec.fixed ,aSpurs = prec.fixed ,aHeat = prec.fixed ,"aTrail Blazers" = prec.fixed ,aKings = prec.fixed ,aClippers = prec.fixed ,aBulls = prec.fixed ,aMagic = prec.fixed ,aHawks = prec.fixed ,aMavericks = prec.fixed ,aLakers = prec.fixed ,aNuggets = prec.fixed ,aPistons = prec.fixed ,aKnicks = prec.fixed ,aNets = prec.fixed ,aSuns = prec.fixed ,aGrizzlies = prec.fixed ,aHornets = prec.fixed ,
  #                                 dCavaliers = prec.fixed ,dWarriors = prec.fixed ,dRockets = prec.fixed ,dCeltics = prec.fixed ,d76ers = prec.fixed ,dPelicans = prec.fixed ,dJazz = prec.fixed ,dRaptors = prec.fixed ,dPacers = prec.fixed ,dWizards = prec.fixed ,dBucks = prec.fixed ,dThunder = prec.fixed ,dTimberwolves = prec.fixed ,dSpurs = prec.fixed ,dHeat = prec.fixed ,"dTrail Blazers" = prec.fixed ,dKings = prec.fixed ,dClippers = prec.fixed ,dBulls = prec.fixed ,dMagic = prec.fixed ,dHawks = prec.fixed ,dMavericks = prec.fixed ,dLakers = prec.fixed ,dNuggets = prec.fixed ,dPistons = prec.fixed ,dKnicks = prec.fixed ,dNets = prec.fixed ,dSuns = prec.fixed ,dGrizzlies = prec.fixed ,dHornets = prec.fixed ,
  #                                 h = prec.fixed, PTS_cur_season =prec.fixed,FG_PCT_cur_season=prec.fixed, FT_PCT_cur_season=prec.fixed, FG3_PCT_cur_season=prec.fixed, AST_cur_season=prec.fixed,REB_cur_season=prec.fixed,WINRATE_cur_season=prec.fixed, PTS_LOST_cur_season =prec.fixed,
  #                                 default = default))
  
  ################   summary(basketball.inla)
  print("Poisson")
  #+scale(PTS_cur_season) #4
  #+scale(FG_PCT_cur_season) #5
  #+scale(FT_PCT_cur_season)#6
  #+scale(FG3_PCT_cur_season)#7
  #+scale(AST_cur_season)#8
  #+scale(REB_cur_season)#9
  #+scale(WINRATE_cur_season)#10
  #+scale(PTS_LOST_cur_season)#11
  basketball.inla = inla(y~1
                         #+a+d #1,2
                         +h # 3
                         #+scale(PTS_cur_season) #4
                         #+scale(FG_PCT_cur_season) #5
                         #+scale(FT_PCT_cur_season)#6
                         #+scale(FG3_PCT_cur_season)#7
                         +scale(AST_cur_season)#8
                         +scale(REB_cur_season)#9
                         #+scale(WINRATE_cur_season)#10
                         +scale(PTS_LOST_cur_season)#11
                         + log(salary) #12
                         + f(a.season, model = "generic0", Cmatrix = Q.a, rankdef = 1, constr = TRUE) 
                         + f(d.season, model = "generic0", Cmatrix = Q.d, rankdef = 1, constr = TRUE) 
                         +f(inla.group(travel), model = "rw1", scale.model = TRUE)
                         +type:a + type:d
                         , data = data, family = "poisson",control.compute=list(config = TRUE,cpo = TRUE,dic = TRUE),control.predictor = list(compute = TRUE),verbose = FALSE)
  
  samp.r <- inla.posterior.sample(1000, basketball.inla)
  
  predictors.r=inla.posterior.sample.eval(function(...) {Predictor}, samp.r)
  
  to.be.predicted = which(is.na(data$y))
  
  number.matches = length(to.be.predicted)/2
  
  predictors.test = predictors.r[to.be.predicted,]
  
  lambda.test = exp(predictors.test)
  
  winning.prob.matrix = matrix(rep(0,number.matches*1000),nrow = number.matches,ncol = 1000)
  
  for(i in 1:number.matches){
    for( j in 1:1000){
      winning.prob.matrix[i,j] = winning.prob(lambda.test[i,j],lambda.test[i+number.matches,j])
    }
  }
  
  obs.win = games.true$HOME_TEAM_WINS[to.be.predicted[1:number.matches]]
  obs = c(obs, obs.win)
  true.score =c( games.true[to.be.predicted[1:number.matches],]$PTS_home,games.true[to.be.predicted[1:number.matches],]$PTS_away)
  
  pred.win.binary = as.numeric(rowMeans(winning.prob.matrix)>0.5)
  pred.win.prob = rowMeans(winning.prob.matrix)
  model.prob.pois = c(model.prob.pois, pred.win.prob)
  cat("Accuracy:", sum(as.numeric(obs.win == pred.win.binary))/number.matches,"\n") ## accuracy
  cat("RPS:",round(mean((pred.win.prob - obs.win)**2),5)) ## mean rps
  #cat(" (",round(sd((pred.win.prob - obs.win)**2),5), "\n") ## sd rps
  #rps(obs.win, pred.win, 2)
  nslcpo = -sum(log(na.omit(basketball.inla$cpo$cpo)))
  cat("nsdlcpo: ", nslcpo, "\n")
  cat("DIC:",basketball.inla$dic$dic,"\n")
  #summary(basketball.inla)
  
  glm.pois.acc = c(glm.pois.acc, sum(as.numeric(obs.win == pred.win.binary))/number.matches)
  glm.pois.rps = c(glm.pois.rps, round(mean((pred.win.prob - obs.win)**2),5))
  glm.pois.dic = c(glm.pois.dic, basketball.inla$dic$dic)
  glm.pois.rps.sd = c(glm.pois.rps.sd, round(rps.sd(pred.win.prob),5))
  
  
  # print("logistic")
  # 
  # basketball.inla.logi = inla(y.binary~1
  #                             #+ a+d #1,2
  #                             + h #3
  #                             +scale(PTS_cur_season) #4
  #                             #+scale(FG_PCT_cur_season) #5
  #                             #+scale(FT_PCT_cur_season)#6
  #                             #+scale(FG3_PCT_cur_season)#7
  #                             #+scale(AST_cur_season)#8
  #                             #+scale(REB_cur_season)#9
  #                             +scale(WINRATE_cur_season)#10
  #                             +scale(PTS_LOST_cur_season)#11
  #                             + log(salary) #12
  #                             + f(a.season, model = "generic0", Cmatrix = Q.a, rankdef = 1, constr = TRUE) 
  #                             + f(d.season, model = "generic0", Cmatrix = Q.d, rankdef = 1, constr = TRUE) 
  #                             + f(inla.group(travel), model = "rw1", scale.model = TRUE)
  #                             + type:a + type:d #14 15
  #                             , data = data, family = "binomial",Ntrials = 1,control.compute=list(config = TRUE,dic=TRUE,cpo=TRUE),control.predictor = list(compute = TRUE),num.threads = 2)
  # #summary(basketball.inla.logi)
  # samp.r.logi <- inla.posterior.sample(1000, basketball.inla.logi)
  # 
  # ## n*1000 table
  # predictors.r.logi=inla.posterior.sample.eval(function(...) {Predictor}, samp.r.logi)
  # ## convert to probabilty n*1000 #pi#
  # prob.logi = expit(predictors.r.logi[to.be.predicted,])
  # winning.prob.logi = matrix(rep(1000*number.matches),nrow = number.matches,ncol = 1000)
  # for(i in 1:number.matches){
  #   winning.prob.logi[i,] = prob.logi[i,]/ (prob.logi[i,] + prob.logi[i+number.matches,])
  # }
  # 
  # pred.win.binary.logi = as.numeric(rowMeans(winning.prob.logi)>0.5)
  # pred.win.prob.logi = rowMeans(winning.prob.logi)
  # model.prob.logi = c(model.prob.logi, pred.win.prob.logi)
  # cat("Accuracy:", sum(as.numeric(obs.win == pred.win.binary.logi))/number.matches, "\n") ## accuracy
  # cat("RPS:",round(mean((pred.win.prob.logi - obs.win)**2),5)) ## mean rps
  # cat(" (",round(rps.sd(pred.win.prob.logi),5),") \n") ## sd rps
  # 
  # cat("total games:", nrow(games.sub) , "test data:", length(obs.win), ", from:",min.season, ",to: ", max.season,  "\n")
  # cat("DIC:",basketball.inla.logi$dic$dic,"\n")
  # glm.logi.acc=c(glm.logi.acc,sum(as.numeric(obs.win == pred.win.binary.logi))/number.matches)
  # glm.logi.rps=c(glm.logi.rps,round(mean((pred.win.prob.logi - obs.win)**2),5))
  # glm.logi.dic=c(glm.logi.dic,basketball.inla.logi$dic$dic)
  # glm.logi.rps.sd=c(glm.logi.rps.sd,round(rps.sd(pred.win.prob.logi),5))
  # 
  
  #val1 = get.exp.val(winning.prob.matrix)
  #val2 = get.exp.val(winning.prob.logi)
  #pois.rps.exp = c(pois.rps.exp, val1$rps)
  #logi.rps.exp = c(logi.rps.exp, val2$rps)
  #pois.acc.exp = c(pois.acc.exp, val1$acc)
  #logi.acc.exp = c(logi.acc.exp, val2$acc)
}

for(i in 1:length(glm.pois.acc)){
  cat(glm.pois.acc[length(glm.pois.acc)-i+1],"\n")
}
for(i in 1:length(glm.pois.acc)){
  cat(glm.pois.rps[length(glm.pois.acc)-i+1],"(", glm.pois.rps.sd[length(glm.pois.acc)-i+1],"\n")
}
for(i in 1:length(glm.pois.acc)){
  cat(glm.pois.dic[length(glm.pois.acc)-i+1],"\n")
}


for(i in 1:length(glm.pois.acc)){
  cat(glm.logi.acc[length(glm.pois.acc)-i+1],"\n")
}
for(i in 1:length(glm.pois.acc)){
  cat(glm.logi.rps[length(glm.pois.acc)-i+1],"(", glm.logi.rps.sd[length(glm.pois.acc)-i+1],"\n")
}
for(i in 1:length(glm.pois.acc)){
  cat(glm.logi.dic[length(glm.pois.acc)-i+1],"\n")
}


#summary(basketball.inla)


for(i in 1:length(glm.pois.acc)){
  cat(pois.acc.exp[length(glm.pois.acc)-i+1],"\n")
}
for(i in 1:length(glm.pois.acc)){
  cat(pois.rps.exp[length(glm.pois.acc)-i+1],"\n")
}

for(i in 1:length(glm.pois.acc)){
  cat(logi.acc.exp[length(glm.pois.acc)-i+1],"\n")
}
for(i in 1:length(glm.pois.acc)){
  cat(logi.rps.exp[length(glm.pois.acc)-i+1],"\n")
}



##### model check 
pred_min = c()
for(i in 1: ncol(lambda.test)){
  pred_min = c(pred_min, min(lambda.test[,i]))
}
pred_max = c()
for(i in 1: ncol(lambda.test)){
  pred_max = c(pred_max, max(lambda.test[,i]))
}
pred_median = c()
for(i in 1: ncol(lambda.test)){
  pred_median = c(pred_median, median(lambda.test[,i]))
}

hist(pred_min,xlim = c(53,120))
abline(v = 54,col = "red")

hist(pred_max, xlim = c(120,170))
abline(v = max(max(games.true$PTS_away),max(games.true$PTS_home)),col = "red")

hist(pred_median, xlim = c(95,120))
abline(v = median(c(games.true$PTS_away,games.true$PTS_home)),col = "red")
mo = basketball.inla.logi
par(mfrow=c(2,4))
plot(mo$marginals.fixed$`scale(PTS_cur_season)`,
     main = "Posterior density of beta4",type = 'l')
plot(mo$marginals.fixed$`scale(FG_PCT_cur_season)` ,
     main = "Posterior density of beta5",type = 'l')
plot(mo$marginals.fixed$`scale(FT_PCT_cur_season)` ,
     main = "Posterior density of beta6",type = 'l')
plot(mo$marginals.fixed$`scale(FG3_PCT_cur_season)` ,
     main = "Posterior density of beta7",type = 'l')
plot(mo$marginals.fixed$`scale(AST_cur_season)` ,
     main = "Posterior density of beta8",type = 'l')
plot(mo$marginals.fixed$`scale(REB_cur_season)` ,
     main = "Posterior density of beta9",type = 'l')
plot(mo$marginals.fixed$`scale(WINRATE_cur_season)` ,
     main = "Posterior density of beta10",type = 'l')
plot(mo$marginals.fixed$`scale(PTS_LOST_cur_season)` ,
     main = "Posterior density of beta11",type = 'l')

par(mfrow=c(2,2))
posterior.mean.attack = c()
for(iii in 477:493){
  posterior.mean.attack = c(posterior.mean.attack, mean(basketball.inla$marginals.random$a.season[[iii]][,1]))
}
plot(1:17,posterior.mean.attack,type='l',xlab="season", main = "Attacking strength of Warriors(Poisson)")

posterior.mean.attack = c()
for(iii in 477:493){
  posterior.mean.attack = c(posterior.mean.attack, mean(basketball.inla.logi$marginals.random$a.season[[iii]][,1]))
}
plot(1:17,posterior.mean.attack,type='l',xlab="season", main = "Attacking strength of Warriors(Logistic)")

posterior.mean.defend = c()
for(iii in 477:493){
  posterior.mean.defend = c(posterior.mean.defend, mean(basketball.inla$marginals.random$d.season[[iii]][,1]))
}
plot(1:17,posterior.mean.defend,type='l',xlab="season", main = "Defending strength of Warriors(Poisson)")

posterior.mean.defend = c()
for(iii in 477:493){
  posterior.mean.defend = c(posterior.mean.defend, mean(basketball.inla.logi$marginals.random$d.season[[iii]][,1]))
}
plot(1:17,posterior.mean.defend,type='l',xlab="season", main = "Defending strength of Warriors(Logistic)")
