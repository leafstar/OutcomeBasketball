## script for Poisson regression and Logistic Regression model
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
path = 'newdat/games_with_type.csv'
games.total = read.csv(path)

#games.sub = games.total[(games.total$SEASON <= end.year)&(games.total$SEASON >= end.year-training.season),]
  games.sub = games.total[games.total$SEASON<=2020,]
  games.true = games.sub
  
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
                         #+a+d #1 2
                         +h # 3
                         #+scale(PTS_cur_season) #4
                         #+scale(FG_PCT_cur_season) #5
                         #+scale(FT_PCT_cur_season)#6
                         #+scale(FG3_PCT_cur_season)#7
                         +scale(AST_cur_season)#8
                         +scale(REB_cur_season)#9
                         #+scale(WINRATE_cur_season)#10
                         +scale(PTS_LOST_cur_season)#11
                         + log(salary) # 12
                         + f(a.season, model = "generic0", Cmatrix = Q.a, rankdef = 1, constr = TRUE) #16
                         + f(d.season, model = "generic0", Cmatrix = Q.d, rankdef = 1, constr = TRUE) #17
                         +f(inla.group(travel), model = "rw1", scale.model = TRUE) # 18
                         +type:a + type:d #14 15
                         , data = data, family = "poisson",control.compute=list(config = TRUE,cpo = TRUE,dic = TRUE),control.predictor = list(compute = TRUE),verbose = FALSE)
  nslcpo = -sum(log(na.omit(basketball.inla$cpo$cpo)))
  cat("nlscpo: ", nslcpo, "\n")
  cat("DIC:",basketball.inla$dic$dic,"\n")
  hist(basketball.inla$cpo$pit, main = "Histogram of PIT for M1 (Poisson)", xlab="PIT")
  print("logistic")
  
  basketball.inla.logi = inla(y.binary~1
                              #+ a+d #1,2
                              + h #3
                              +scale(PTS_cur_season) #4
                              +scale(FG_PCT_cur_season) #5
                              +scale(FT_PCT_cur_season)#6
                              +scale(FG3_PCT_cur_season)#7
                              +scale(AST_cur_season)#8
                              +scale(REB_cur_season)#9
                              +scale(WINRATE_cur_season)#10
                              +scale(PTS_LOST_cur_season)#11
                              + log(salary) #12
                              + f(a.season, model = "generic0", Cmatrix = Q.a, rankdef = 1, constr = TRUE) 
                              + f(d.season, model = "generic0", Cmatrix = Q.d, rankdef = 1, constr = TRUE) 
                              + f(inla.group(travel), model = "rw1", scale.model = TRUE)
                              #+ type:a + type:d #14 15
                              , data = data, family = "binomial",Ntrials = 1,control.compute=list(config = TRUE,dic=TRUE,cpo=TRUE),control.predictor = list(compute = TRUE),num.threads = 2)
  #summary(basketball.inla)
  #summary(basketball.inla.logi)
  cat("DIC:",basketball.inla.logi$dic$dic,"\n")
  nslcpo = -sum(log(na.omit(basketball.inla.logi$cpo$cpo)))
  hist(basketball.inla.logi$cpo$pit, main = "Histogram of PIT for M1 (Logistic)", xlab = "PIT")
  cat("nlscpo: ", nslcpo, "\n")


  
  
    
  pit.pois7 = basketball.inla$cpo$pit
  pit.logi5 = basketball.inla.logi$cpo$pit
  
  
  
  
  
  
  

  par(mfrow=c(2,4))
  
  hist(pit.logi1, main = "Histogram of PIT for M1 (Logistic)", xlab = "PIT")
  hist(pit.logi2, main = "Histogram of PIT for M2 (Logistic)", xlab = "PIT")
  hist(pit.logi3, main = "Histogram of PIT for M3 (Logistic)", xlab = "PIT")
  hist(pit.logi4, main = "Histogram of PIT for M4 (Logistic)", xlab = "PIT")
  hist(pit.logi5, main = "Histogram of PIT for M5 (Logistic)", xlab = "PIT")
  hist(pit.logi6, main = "Histogram of PIT for M6 (Logistic)", xlab = "PIT")
  hist(pit.logi7, main = "Histogram of PIT for M7 (Logistic)", xlab = "PIT")
  
  hist(pit.pois1, main = "Histogram of PIT for M1 (Poisson)", xlab="PIT")
  hist(pit.pois2, main = "Histogram of PIT for M2 (Poisson)", xlab="PIT")
  hist(pit.pois3, main = "Histogram of PIT for M3 (Poisson)", xlab="PIT")
  hist(pit.pois4, main = "Histogram of PIT for M4 (Poisson)", xlab="PIT")
  hist(pit.pois5, main = "Histogram of PIT for M5 (Poisson)", xlab="PIT")
  hist(pit.pois6, main = "Histogram of PIT for M6 (Poisson)", xlab="PIT")
  hist(pit.pois7, main = "Histogram of PIT for M7 (Poisson)", xlab="PIT")
  
  
  