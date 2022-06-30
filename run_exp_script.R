## script for Poisson regression and Logistic Regression model
path = 'newdat/games_extended.csv'
games.total = read.csv(path)



poisson.acc = c()
logi.acc = c()
for(yyear in 2004:2015){



## change type to date
games.total %<>%
  mutate(GAME_DATE_EST= as.Date(GAME_DATE_EST, format= "%m/%d/%Y"))



#[start ------training-------)--[split-------testing-------end]
dates = unique(games.total$GAME_DATE_EST)
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

split = "2016-06-20" # 

start.date = dates[length(dates)]
start.date = "2019-06-13"
end.date = "2020-10-11"
games.sub = games.total[games.total$GAME_DATE_EST<=end.date,] ## the dataset used for training and testing
games.sub = games.sub[games.sub$GAME_DATE_EST>start.date,]
# |(games.total$SEASON == 2016)|(games.total$SEASON == 2015)|(games.total$SEASON == 2014)|(games.total$SEASON == 2013)|(games.total$SEASON == 2012)|(games.total$SEASON == 2011)
games.sub = games.total[(games.total$SEASON <= 2016)&(games.total$SEASON >= yyear),]
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

max.season = max(games.sub$SEASON)
min.season = min(games.sub$SEASON)


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





G = nrow(games.sub)
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
                  PTS_cur_season_oppo,FG_PCT_cur_season_oppo,FT_PCT_cur_season_oppo,FG3_PCT_cur_season_oppo,AST_cur_season_oppo,REB_cur_season_oppo,WINRATE_cur_season_oppo,ROUND,PTS_LOST_cur_season,PTS_LOST_cur_season_oppo, season, a.season, d.season,depend)


Q.single = toeplitz(c(2,-1, rep(0,n.season-2)))
Q.single[1,1] = Q.single[n.season,n.season] = 1
matrix.list = replicate(n.team, Q.single , simplify = FALSE)
Q.a = bdiag(matrix.list)
Q.d = Q.a



##
print(Sys.time())
print("Poisson")
#+PTS_cur_season_oppo+FG_PCT_cur_season_oppo+FT_PCT_cur_season_oppo+FG3_PCT_cur_season_oppo+AST_cur_season_oppo+REB_cur_season_oppo+WINRATE_cur_season_oppo + PTS_LOST_cur_season+PTS_LOST_cur_season_oppo
#+
#+ +f(season, model = "ar1c", args.ar1c = list(Z = Z, Q.beta = Q.beta))
basketball.inla = inla(y~1+a+d+h+PTS_cur_season+FG_PCT_cur_season+FT_PCT_cur_season+FG3_PCT_cur_season+AST_cur_season+REB_cur_season+WINRATE_cur_season+ PTS_LOST_cur_season
                       + f(a.season, model = "generic0", Cmatrix = Q.a, rankdef = 1, constr = TRUE) 
                       + f(d.season, model = "generic0", Cmatrix = Q.d, rankdef = 1, constr = TRUE) 
                       + f(depend)
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

true.score =c( games.true[to.be.predicted[1:number.matches],]$PTS_home,games.true[to.be.predicted[1:number.matches],]$PTS_away)

pred.win.binary = as.numeric(rowMeans(winning.prob.matrix)>0.5)
pred.win.prob = rowMeans(winning.prob.matrix)

cat("Accuracy:", sum(as.numeric(obs.win == pred.win.binary))/number.matches,"\n") ## accuracy
cat("RPS:",round(mean((pred.win.prob - obs.win)**2),5)) ## mean rps
cat(" (",round(sd((pred.win.prob - obs.win)**2),5), "\n") ## sd rps
#rps(obs.win, pred.win, 2)
nslcpo = -sum(log(na.omit(basketball.inla$cpo$cpo)))
cat("nsdlcpo: ", nslcpo, "\n")
cat("DIC:",basketball.inla$dic$dic,"\n")
#summary(basketball.inla)
print(Sys.time())
poisson.acc = c(poisson.acc, sum(as.numeric(obs.win == pred.win.binary))/number.matches)


print("logistic")
print(Sys.time())
#+PTS_cur_season_oppo+FG_PCT_cur_season_oppo+FT_PCT_cur_season_oppo+FG3_PCT_cur_season_oppo+AST_cur_season_oppo+REB_cur_season_oppo+WINRATE_cur_season_oppo
basketball.inla.logi = inla(y.binary~1+a+d+h+PTS_cur_season+FG_PCT_cur_season+FT_PCT_cur_season+FG3_PCT_cur_season+AST_cur_season+REB_cur_season+WINRATE_cur_season+ PTS_LOST_cur_season
                            #+ f(a.season, model = "generic0", Cmatrix = Q.a, rankdef = 1, constr = TRUE) 
                            #+ f(d.season, model = "generic0", Cmatrix = Q.d, rankdef = 1, constr = TRUE)
                            , data = data, family = "binomial",Ntrials = 1,control.compute=list(config = TRUE,dic=TRUE,cpo=TRUE),control.predictor = list(compute = TRUE),num.threads = 2)
#summary(basketball.inla.logi)
samp.r.logi <- inla.posterior.sample(1000, basketball.inla.logi)

## n*1000 table
predictors.r.logi=inla.posterior.sample.eval(function(...) {Predictor}, samp.r.logi)
## convert to probabilty n*1000 #pi#
prob.logi = expit(predictors.r.logi[to.be.predicted,]) 
winning.prob.logi = matrix(rep(1000*number.matches),nrow = number.matches,ncol = 1000)
for(i in 1:number.matches){
  winning.prob.logi[i,] = prob.logi[i,]/ (prob.logi[i,] + prob.logi[i+number.matches,])
}

pred.win.binary.logi = as.numeric(rowMeans(winning.prob.logi)>0.5)
pred.win.prob.logi = rowMeans(winning.prob.logi)

cat("Accuracy:", sum(as.numeric(obs.win == pred.win.binary.logi))/number.matches, "\n") ## accuracy
cat("RPS:",round(mean((pred.win.prob.logi - obs.win)**2),5)) ## mean rps
cat(" (",round(sd((pred.win.prob.logi - obs.win)**2),5),") \n") ## sd rps 
print(Sys.time())
cat("total games:", nrow(games.sub) , "test data:", length(obs.win), ", from:",min.season, ",to: ", max.season,  "\n")
cat("DIC:",basketball.inla.logi$dic$dic,"\n")
logi.acc = c(logi.acc, sum(as.numeric(obs.win == pred.win.binary.logi))/number.matches)

print(summary(poisson.acc))
print(summary(logi.acc))

}
