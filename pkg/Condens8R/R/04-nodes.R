# Copyright (C) Kevin R. Coombes, 2025.

if(FALSE) {
  Modeler(learn, predict, ...)
  learn(model, data, status, prune=keepAll)
  FittedModel(predict, data, status, details, ...)
  predict(object, newdata=object@trainData, ...)
  learnLR(data, status, params, pfun)
  predictLR(newdata, details, status, type="response", ...)
  modelerLR <- Modeler(learnLR, predictLR)
}

if (FALSE) {
setClass("Node",
         slots = c(model = "FittedModel"))

setClass("BinaryNode",
         contains = "Node",
         slots(Left = "Node",
               Right = "Node"))

setClass("LeafNode",
         contains = "Node")


createtree <- function(data, metric) {
  dmat <- distancematrix(data, metric)
  mySplit <- findSplit(dmat)
  if (!validSplit(mySplit)) {
    ## make a leaf node
    val <- new("leafNode", modelerEnd)
  } else {
    myModel <- learn(modelerLogic, data, mySplit, keepAll)
    leftNode <- createTree(data[, mySplit == "L"], metric)
    rightNode <- createTree(data[, mySplit == "R"], metric)
    val <- new("BinaryNode", myModel,
               Left = leftNode, Right = rightNode)
  }
  val
}
}
