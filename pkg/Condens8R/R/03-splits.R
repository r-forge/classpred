# Copyright (C) Kevin R. Coombes, 2025.

findSplit <- function(data,
                      metric = "euclidean",
                      algorithm = c("hc", "km", "dv", "ap"),
                      LR = c("L", "R")) {
  dmat <- distancematrix(data, metric)
  bin <- switch(algorithm,
                hc = cutree(hclust(dmat, method = "ward.D2"), k = 2),
                km = kmeans(dmat, k = 2, nstart = 10)$cluster,
                dv = cutree(as.hclust(diana(jacc@distance)), k = 2),
                ap = cutree(as.hclust(apcluster(-dmat^2)), k = 2))
  factor(LR[bin], levels = LR)
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
