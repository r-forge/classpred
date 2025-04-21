# Copyright (C) Kevin R. Coombes, 2025.

## Assume rows are features and columns are samples.
## If a feature is not bimodal, this procedure returns some measure
## of central tendency.
dichotomize <- function(data) {
  bi <-bimodalIndex(data)
  cuts <- (bi$mu1 + bi$mu2)/2
  bidata <- 1*(sweep(data, 1, cuts, "-") > 0)
  list(data = bidata, cuts = cuts) # return cuts to apply to new data
}

## Logic Regression Modeler

## Remember that the 'Modeler' convention (borrowed from the earliest
## microarry gene expession data sets) is that features are rows and
## samples are columns. That's the opposite of most statistical prediction
## models, because of how data.frames work.
##
## Anneal parameters are magic numbers. But the user interface to Modeler
## makes it complicated for the user to change them.
my.anneal <- logreg.anneal.control(start = 2, end = -2, iter = 10000)
learnLogic <- function(data, status, params, pfun, debug = FALSE) {
  ## Logic regression only works with binary predictors.
  if (!all(data %in% c(0, 1, NA))) {
    if (debug) cat("Dichotomizing\n", file = stderr())
    did <- dichotomize(data)
    data <- did$data
    cuts <- did$cuts
  } else {
    if (debug) cat("Already Binary\n", file = stderr())
    cuts <- NA
  }
  arglist <- c(list(x = t(data), y = status,
                    B = 20, nleaves = 10, rand = 54321, # more magic numbers
                    anneal.control = my.anneal), params)
  model <- do.call(logic.bagging, arglist)
  FittedModel(pfun, data, status,
              details=list(model=model, cuts = cuts))
}

predictLogic <- function(newdata, details, status, debug = FALSE, ...) {
  if (!all(newdata %in% c(0, 1, NA))) {
    if (debug) cat("Dichotomizing new data.\n", file = stderr())
    newdata <- 1*(sweep(newdata, 1, details$cuts, "-") > 0)
  }
  if(debug) cat("Status during prediction:", class(status), "\n", file = stderr())
  pop <- predict(details$model, t(newdata), ...)
  if (all(pop %in% c(0,1)) & inherits(status, "factor")) {
    pop <- factor(levels(status)[1+pop], levels = levels(status))
  }
  pop
}

logicModeler <- Modeler(learnLogic, predictLogic)

