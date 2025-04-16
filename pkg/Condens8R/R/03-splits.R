# Copyright (C) Kevin R. Coombes, 2025.

splitters <- new.env()
registerSplitter <- function(tag, description, FUN) {
  if (tag %in% names(splitters)) {
    warning("Replacing an existing splitter, 'tag', described as:",
            splitters[[tag]]$description)
  }
  assign(tag, value = list(description = description, action = FUN),
         envir = splitters)
  invisible(tag)
}
registerSplitter("hc", "(Agglomerative) Hierarchical Clustering",
                 function(dmat) cutree(hclust(dmat, method = "ward.D2"), k = 2))
registerSplitter("km", "K-Means Clustering",
                 function(dmat) kmeans(dmat, centers = 2, nstart = 10)$cluster)
registerSplitter("dv", "Divisive Hierarchical Clustering",
                 function(dmat) cutree(as.hclust(diana(dmat)), k = 2))
registerSplitter("ap", "Affinity Propagation Clustering",
                 function(dmat) cutree(as.hclust(apcluster(-dmat^2)), k = 2))


findSplit <- function(data,
                      metric = "euclidean",
                      algorithm = names(splitters),
                      LR = c("L", "R")) {
  dmat <- distanceMatrix(data, metric)
  algorithm <- match.arg(algorithm)
  FUN <- get(algorithm, envir = splitters)
  bin <- FUN$action(dmat)
  factor(LR[bin], levels = LR)
}

