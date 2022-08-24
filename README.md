

# OutcomeBasketball

Master Project

Using INLA to predict the outcome of basketball matches.

The folder nba_data contains the datasets from the Kaggle website.

The folder newdat contains preprocessed datasets.

[AddDistance.ipynb](https://github.com/leafstar/OutcomeBasketball/blob/main/AddDistance.ipynb), [AddSalary.ipynb](https://github.com/leafstar/OutcomeBasketball/blob/main/AddSalary.ipynb),[AddTypes.ipynb](https://github.com/leafstar/OutcomeBasketball/blob/main/AddTypes.ipynb),[DataPreprocessing.ipynb](https://github.com/leafstar/OutcomeBasketball/blob/main/DataPreprocessing.ipynb) are the codes for data-preprocessing.

[ExploratoryDataAnalysis.ipynb](https://github.com/leafstar/OutcomeBasketball/blob/main/ExploratoryDataAnalysis.ipynb) does a simple analysis of win percentage.

[libs_functions.R](https://github.com/leafstar/OutcomeBasketball/blob/main/libs_functions.R) loads the libraries and helper functions.

[run_all_training_sizes.R](https://github.com/leafstar/OutcomeBasketball/blob/main/run_all_training_sizes.R) runs the experiments with all possible choices of training sizes.

[run_with_specified_training_sizes.R](https://github.com/leafstar/OutcomeBasketball/blob/main/run_with_specified_training_sizes.R) runs the experiments with specified training sizes.

[bayesianModelCheck.R](https://github.com/leafstar/OutcomeBasketball/blob/main/bayesianModelCheck.R) is for model checking, like DIC, PIT, etc.

[rps_true_prob.R](https://github.com/leafstar/OutcomeBasketball/blob/main/rps_true_prob.R) estimates what is the theoretical best RPS. 

[Bins.R](https://github.com/leafstar/OutcomeBasketball/blob/main/Bins.R) plots the actual win percentage of the home team against the expected win percentage of the home team.

[priors.R](https://github.com/leafstar/OutcomeBasketball/blob/main/priors.R) experiments with different priors.

Note that there are duplicate codes, and this is because we extended the experiments progressively and some helper functions were not defined.

