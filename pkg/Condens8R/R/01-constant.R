# Copyright (C) Kevin R. Coombes, 2025.

## The "Modeler" asumption is that rows are features and columns are samples.
learnConstant <- function(data, status, params, predfun) {
  if (is.null(params$value)) {
    if (mode(data) == "numeric") {
      params$value <- mean(data, na.rm = TRUE)
    } else {
      params$value <- "X"
    }
  }
  FittedModel(predictConstant, data, status,
              details=list(value = params$value))
}

## Repeat; samples are columns.
predictConstant <- function(newdata, details, status, ...) {
  rep(details$value, ncol(newdata))
}

## Should really check that the 'value' is a legitmate symbol
## in the status factor.
makeLeaf <- function(value, status = factor(c("L", "R"))) {
  learn(Modeler(learnConstant, predictConstant, value = value),
        matrix(), status, keepAll)
}

