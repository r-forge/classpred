# Copyright (C) Kevin R. Coombes, 2025.

predictors <- new.env()
registerPredictor <- function(tag, description, modeler) {
  if (tag %in% names(predictors)) {
    warning("Replacing an existing predictor, 'tag', described as:",
            predictors[[tag]]$description, ".\n")
  }
  assign(tag, value = list(description = description,
                           modeler = modeler),
         envir = predictors)
  invisible(tag)
}

registerPredictor("logic", "Logic Regression", logicModeler)
registerPredictor("svm", "Support Vector Machine", modelerSVM)
registerPredictor("lr", "Logistic regression", modelerLR)
registerPredictor("rf", "Random Forest", modelerRF)
registerPredictor("tr", "Tail Rank", modelerTailRank)

availablePredictors <- function() {
  tags <- names(predictors)
  scrip <- sapply(tags, function(S) {
    predictors[[S]]$description
  })
  data.frame(tag = tags, description = scrip)
}


setClass("Node",
         slots = c(model = "FittedModel",
                   name = "character",
                   cluster = "character"))

setClass("BinaryNode",
         contains = "Node",
         slots = c(Left = "Node",
                   Right = "Node"))

setClass("LeafNode",
         contains = "Node")

setMethod("predict", "Node", function(object, newdata, ...) {
  cat("Leaf Tag:", object@name, "\n", file = stderr())
  cat("Size:",  dim(newdata), "\n", file = stderr())
  predict(object@model, newdata = newdata, ...)
})

setMethod("predict", "BinaryNode", function(object, newdata, ...) {
  cat("Tag:", object@name, "\n", file = stderr())
  if (missing(newdata)) {
    newdata <- object@model@trainData
  }
  pop <- predict(object@model, newdata = newdata)
  cat("Main model predictions:", table(pop),  "\n", file = stderr())
  cat("Start pop sizes:", table(pop), "\n", file = stderr())
  cat("Length pop:", length(pop), "\n", file = stderr())
  if (is.numeric(pop)) {
    pop <- 1 + pop - min(pop)
  } else {
    pop <- as.numeric(factor(pop))
  }
  cat("New data s1ze:", dim(newdata), "\n", file = stderr())
  cat("Modified pop sizes:", table(pop), "\n", file = stderr())
  cat("Going left\n", file = stderr())
  P1 <-newdata[, pop == 1, drop = FALSE]
  lpred <- predict(object@Left, newdata = P1)
  cat("Going right\n", file = stderr())
  P2 <- newdata[, pop == 2, drop = FALSE]
  rpred <- predict(object@Right, newdata = P2)
  cat("Stepping back\n", file = stderr())
  out <- rep(NA, ncol(newdata))
  out[pop==1] <- lpred
  out[pop == 2] <- rpred
  out
})

## Need to fix this so it learns and remembers cluster assignments
## as it goes.
createTree <- function(data, metric, label = "H", pcut = 0.05, N = 100,
                       splitter = "hc", modeler = "logic") {
  cat("Label:", label, "\n", file = stderr())
  cat("Data size:", dim(data), "\n", file = stderr())
  if (ncol(data) < 5) {
    cat("Small sample. Making Leaf\n", file = stderr())
    val <- new("LeafNode", model = makeLeaf(paste0(label, "X")))
    return(val)
  }
  dmat <- gendist(data, metric)
  mySplit <- findSplit(dmat, splitter = splitter)
  cluster <- as.character(mySplit)
  cat ("Split size:", table(mySplit), "\n", file = stderr())
  sw <- evalSplit(mySplit, data, metric, "sw", N = N)
  cat("Silhouette:", unlist(sw), "\n", file = stderr())
  ssq <- evalSplit(mySplit, data, metric, "ssq", N = N)
  cat("Sum of squares:", unlist(ssq), "\n", file = stderr())
  if (min(table(mySplit)) < 5 | sw$pv > pcut | ssq$pv > pcut) {
    tickle <- paste0(label, "X")
    cat("Making Leaf, '", tickle, "'\n", file = stderr())
    val <- new("LeafNode", model = makeLeaf(tickle), name = tickle,
               cluster = rep(tickle, ncol(data)))
  } else {
    cat("Learning model\n", file = stderr())
    myModeler <- predictors[[modeler]]$modeler
    myModel <- learn(myModeler, data, mySplit, keepAll)
    cat("\nRecursing left.\n", file = stderr())
    leftNode <- createTree(data[, mySplit == "L"], metric,
                           label = paste0(label, "L"),
                           pcut = pcut, splitter = splitter)
    cat("\nRecursing right.\n", file = stderr())
    rightNode <- createTree(data[, mySplit == "R"], metric,
                            label = paste0(label, "R"),
                           pcut = pcut, splitter = splitter)
    cat("\nBacking out.\n\n", file = stderr())
    cluster[mySplit == "L"] <- leftNode@cluster
    cluster[mySplit == "R"] <- rightNode@cluster
    cat("Clusters:", table(cluster), "\n", file = stderr())
    barf <- new ("Node", model = myModel, name = label, cluster = cluster)
    val <- new("BinaryNode", barf,
               Left = leftNode, Right = rightNode)
  }
  val
}

if(FALSE) {
  Modeler(learn, predict, ...)
  learn(model, data, status, prune=keepAll)
  FittedModel(predict, data, status, details, ...)
  predict(object, newdata=object@trainData, ...)
  learnLR(data, status, params, pfun)
  predictLR(newdata, details, status, type="response", ...)
  modelerLR <- Modeler(learnLR, predictLR)
}
