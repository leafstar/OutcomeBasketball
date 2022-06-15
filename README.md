# OutcomeBasketball

Master Project

Using INLA to predict the outcome of basketball matches.

games2021_v2.csv: adding the average statistics before the current game in this season for both teams. 



experiment record

| Model                | training data             | testing                  | accuracy  | RPS                   |
| -------------------- | ------------------------- | ------------------------ | --------- | --------------------- |
| Poisson team         | 2021-10-03 to 2022-03-11  | 2022-3-11 to 2022-3-12   | 0.4285714 | 0.2705284(0.1307386)  |
| logistic: team+oppo  | 2021-10-03 to 2022-03-11  | 2022-3-11 to 2022-3-12   | 0.5714286 | 0.2431918(0.1531605)  |
| Poisson  team+oppo   | 2020-12-11 to 2021-06-18  | 2021-06-19 to 2021-07-20 | 0.6       | 0.2458671(0.05785481) |
| Poisson team         | 2020-12-11 to 2021-06-18  | 2021-06-19 to 2021-07-20 | 0.6       | 0.2467726(0.06836937) |
| logistic: team       | 2020-12-11 to 2021-06-18  | 2021-06-19 to 2021-07-20 | 0.5       | 0.2555504(0.09213182) |
| logistic: only a+d+h | 2020-12-11 to 2021-06-18  | 2021-06-19 to 2021-07-20 | 0.45      | 0.2544203(0.08816962) |
| Poisson:  team       | 2020 data except playoffs | playoffs                 | 0.6588235 | 0.2288713(0.1002076)  |
| logistic: team       | 2020 data except playoffs | playoffs                 | 0.6470588 | 0.2340443( 0.1182221) |
| Poisson: team        | 2019 data except playoffs | playoffs                 | 0.5783133 | 0.2396454(0.1177967)  |
| logistic: team       | 2019 data except playoffs | playoffs                 | 0.6385542 | 0.235342(0.1408508)   |
| Poisson: team        | 2018 data except playoffs | playoffs                 | 0.6463415 | 0.2258569(0.1126017)  |
| logistic: team       | 2018 data except playoffs | playoffs(82 games)       | 0.6341463 | 0.2234996(0.1465644)  |
| Poisson: team        | 2017 data except playoffs | playoffs                 | 0.6341463 | 0.2247936(0.1173484)  |
| logistic: team       | 2017 data except playoffs | playoffs                 | 0.6585366 | 0.2134947(0.1447523)  |
| Poisson: team        | 2016 data except playoffs | playoffs                 | 0.6455696 | 0.208242(0.1148515)   |
| logistic: team       | 2016 data except playoffs | playoffs                 | 0.6835443 | 0.2031052(0.1372898)  |

```R
basketball.inla = inla(y~1+a+d+h+PTS_cur_season+FG_PCT_cur_season+FT_PCT_cur_season+FG3_PCT_cur_season+AST_cur_season+REB_cur_season+WINRATE_cur_season, data = data, family = "poisson",control.compute=list(config = TRUE),control.predictor = list(compute = TRUE))
```

0.6712892 0.6127765 0.6251958 0.6582197 0.5966509 0.7538268 0.5440745