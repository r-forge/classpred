# Copyright (C) Kevin R. Coombes, 2025.

setClass("Node",
         slots = c(model = "FittedModel",
                   name = "character"))

setClass("BinaryNode",
         contains = "Node",
         slots = c(Left = "Node",
                   Right = "Node"))

setClass("LeafNode",
         contains = "Node")

setMethod("predict", "Node", function(object, ...) {
  cat("Leaf Tag:", object@name, "\n", file = stderr())
  predict(object@model, ...)
})

setMethod("predict", "BinaryNode", function(object, newdata, ...) {
  cat("Tag:", object@name, "\n", file = stderr())
  if (missing(newdata)) {
    newdata <- object@model@trainData
  }
  pop <- predict(object@model)
  cat("Main model predictons:", table(pop),  "\n", file = stderr())
  if (is.numeric(pop)) {
    pop <- 1 + pop - min(pop)
  } else {
    pop <- as.numeric(factor(pop))
  }
  cat("Going left\n", file = stderr())
  lpred <- predict(object@Left, newdata[, pop == 1, drop = FALSE])
  cat("Going right\n", file = stderr())
  rpred <- predict(object@Right, newdata[, pop == 2, drop = FALSE])
  cat("Stepping back\n", file = stderr())
  out <- rep(NA, ncol(newdata))
  out[pop==1] <- lpred
  out[pop == 2] <- rpred
  out
})

createTree <- function(data, metric, label, pcut = 0.05, N = 100) {
  cat("Label:", label, "\n", file = stderr())
  cat("Data size:", dim(data), "\n", file = stderr())
  if (ncol(data) < 5) {
    cat("Small sample. Making Leaf\n", file = stderr())
    val <- new("LeafNode", model = makeLeaf(paste0(label, "X")))
    return(val)
  }
  dmat <- gendist(data, metric)
  mySplit <- findSplit(dmat)
  cat ("Split size:", table(mySplit), "\n", file = stderr())
  sw <- evalSplit(mySplit, data, metric, "sw", N = N)
  cat("Silhouette:", unlist(sw), "\n", file = stderr())
  ssq <- evalSplit(mySplit, data, metric, "ssq", N = N)
  cat("Sum of squares:", unlist(ssq), "\n", file = stderr())
  if (min(table(mySplit)) < 5 | sw$pv > pcut | ssq$pv > pcut) {
    cat("Making Leaf\n", file = stderr())
    val <- new("LeafNode", model = makeLeaf(paste0(label, "X")))
  } else {
    cat("Learning model\n", file = stderr())
    myModel <- learn(logicModeler, data, mySplit, keepAll)
    cat("\nRecursing left.\n", file = stderr())
    leftNode <- createTree(data[, mySplit == "L"], metric,
                           label = paste0(label, "L"))
    cat("\nRecursing right.\n", file = stderr())
    rightNode <- createTree(data[, mySplit == "R"], metric,
                            label = paste0(label, "R"))
    cat("\nBacking out.\n\n", file = stderr())
    barf <- new ("Node", model = myModel, name = label)
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
