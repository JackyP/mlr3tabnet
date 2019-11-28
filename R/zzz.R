# global reference to scipy (will be initialized in .onLoad)
scipy <- NULL

#' @import data.table
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class
#' @importFrom mlr3 mlr_learners LearnerClassif LearnerRegr
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to scipy
  scipy <<- reticulate::import("scipy", delay_load = TRUE)

  # nocov start
  # get mlr_learners dictionary from the mlr3 namespace
  x <- utils::getFromNamespace("mlr_learners", ns = "mlr3")

  # add the learner to the dictionary
  x$add("classif.TabNet", LearnerClassifTabNet)
  x$add("classif.Rossman", LearnerClassifRossman)
  x$add("regr.TabNet", LearnerRegrTabNet)
  x$add("regr.Rossman", LearnerRegrRossman)

} # nocov end
