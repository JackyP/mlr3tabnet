#' @title Rossman Tabular Learner for Regression
#'
#' @aliases mlr_learners_regr.Rossman
#' @format [R6::R6Class] inheriting from [mlr3::LearnerRegr].
#'
#' @description
#' A [mlr3::LearnerRegr] for a Fast.ai Tabular Model.
#'
#' @references
#' An Introduction to Deep Learning for Tabular Data
#' \url{https://www.fast.ai/2018/04/29/categorical-embeddings/}
#'
#' @export
LearnerRegrRossman <- R6Class("LearnerRegrRossman",
  inherit = LearnerRegr, # Adapt the name to your learner. For regression learners inherit = LearnerRegr.
  public = list(
    initialize = function() {
      ps <- ParamSet$new( # parameter set using the paradox package
        params = list(
          # Tabular Model params https://docs.fast.ai/tabular.html
          ParamUty$new(id = "layers", tags = "train"),
          ParamUty$new(id = "embedding_sizes", tags = "train"),
          ParamUty$new(id = "metrics", tags = "train"),
          ParamUty$new(id = "layer_dropout_percentages", tags = "train"), #ps
          ParamDbl$new(id = "embedding_dropout_percentage", default=0.0, lower=0.0, upper=1.0, tags = "train"), #emb_drop
          ParamUty$new(id = "y_range", tags = "train"),
          ParamLgl$new(id = "use_batchnorm", default = TRUE, tags = "train"),
          # lr_find params https://docs.fast.ai/train.html#lr_find
          ParamDbl$new(id = "start_lr", default = 1e-07, lower=0, tags = "train"),
          ParamDbl$new(id = "end_lr", default = 10.0, lower=0, tags = "train"),
          # fit_one_cycle params https://docs.fast.ai/train.html#fit_one_cycle
          ParamInt$new(id = "cycle_length", default=1L, lower = 1L, tags = "train"),
          # other params
          ParamDbl$new(id = "validation_split", lower = 0, upper = 1, default = 1/3, tags = "train"),
          ParamInt$new(id = "batch_size", default = 128L, lower = 1L, tags = c("train", "predict"))
        )
      )

      ps$values <- list(importance = "none") # Change the defaults. We set this here, because the default is FALSE in the randomForest package.

      super$initialize(
        # see the mlr3book for a description: https://mlr3book.mlr-org.com/extending-mlr3.html
        id = "classif.randomForest",
        packages = "randomForest",
        feature_types = c("numeric", "factor", "ordered"),
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("weights", "twoclass", "multiclass", "importance", "oob_error")
      )
    },

    train_internal = function(task) {
      pars <- self$param_set$get_values(tags = "train")

      # Setting the importance value to logical
      if (pars[["importance"]] != "none") {
        pars[["importance"]] <- TRUE
      } else {
        pars[["importance"]] <- FALSE
      }

      # Get formula, data, classwt, cutoff for the randomForest
      f <- task$formula() # the formula is available in the task
      data <- task$data() # the data is avail
      levs <- levels(data[[task$target_names]])
      n <- length(levs)

      if (!"cutoff" %in% names(pars)) {
        cutoff <- rep(1 / n, n)
      }
      if ("classwt" %in% names(pars)) {
        classwt <- pars[["classwt"]]
        if (is.numeric(classwt) && length(classwt) == n && is.null(names(classwt))) {
          names(classwt) <- levs
        }
      } else {
        classwt <- NULL
      }
      if (is.numeric(cutoff) && length(cutoff) == n && is.null(names(cutoff))) {
        names(cutoff) <- levs
      }
      invoke(randomForest::randomForest, formula = f, data = data, classwt = classwt, cutoff = cutoff, .args = pars) # use the mlr3misc::invoke function (it's similar to do.call())
    },

    predict_internal = function(task) {
      pars <- self$param_set$get_values(tags = "predict") # get parameters with tag "predict"
      newdata <- task$data(cols = task$feature_names) # get newdata
      type <- ifelse(self$predict_type == "response", "response", "prob") # this is for the randomForest package

      p <- invoke(predict, self$model,
        newdata = newdata,
        type = type, .args = pars
      )

      # Return a prediction object with PredictionClassif$new() or PredictionRegr$new()
      if (self$predict_type == "response") {
        PredictionRegr$new(task = task, response = p)
      } else {
        PredictionRegr$new(task = task, prob = p)
      }
    },

    # Add method for importance, if learner supports that.
    # It must return a sorted (decreasing) numerical, named vector.
    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      imp <- data.frame(self$model$importance)
      pars <- self$param_set$get_values()
      if (pars[["importance"]] == "accuracy") {
        x <- setNames(imp[["MeanDecreaseAccuracy"]], rownames(imp))
        return(sort(x, decreasing = TRUE))
      }
      if (pars[["importance"]] == "gini") {
        x <- setNames(imp[["MeanDecreaseGini"]], rownames(imp))
        return(sort(x, decreasing = TRUE))
      }
      if (pars[["importance"]] == "none") {
        return(message("importance was set to 'none'. No importance available."))
      }
    },

    # Add method for oob_error, if learner supports that.
    oob_error = function() {
      mean(self$model$err.rate[, 1])
    }
  )
)
