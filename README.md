Development on this package is not active. Please take a look at https://github.com/mlr-org/mlr3keras for an implementation of tabnet on mlr3 via keras instead. 

The original intention for this package was to use fast.ai tabular and a pytorch implementation of TabNet, in R as an mlr3 learner.

However torch crashes Rstudio at time of writing due to https://github.com/rstudio/reticulate/issues/273. 

Consequently we look to develop the requisite functionality in keras which is better supported in R instead.
