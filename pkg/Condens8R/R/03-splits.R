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

gendist <- function(X, metric) {
  dmat <- try(distanceMatrix(X, metric), silent = TRUE)
  if (inherits(dmat, "try-error")) {
    dmat <- try(binaryDistance(X, metric), silent = TRUE)
  }
  dmat
}

findSplit <- function(data,
                      metric = "euclidean",
                      algorithm = names(splitters),
                      LR = c("L", "R")) {
  dmat <- gendist(data, metric)
  algorithm <- match.arg(algorithm)
  FUN <- get(algorithm, envir = splitters)
  bin <- FUN$action(dmat)
  factor(LR[bin], levels = LR)
}

evalSplit <- function(split, data, metric, tool = c("sw", "ssq")) {
  silwidth <- function(split, dmat) {
    cat("In silwidth\n", file = stderr())
    sw <- silhouette(as.numeric(split), dmat)
    msw <- mean(sw[,3])
    swad <- sapply(1:100, function(ignore) {
      sw <- silhouette(sample(as.numeric(split)), dmat)
      mean(sw[,3])
    })
    M <- sum(swad > msw)
    list(stat = msw, pv = (M+1)/102)
  }
  sumsq <- function(split, dmat) {
    cat("In sumsq\n", file = stderr())
    D2 <- as.matrix(dmat)^2
    foo <- data.frame(X = split)
    G <- model.matrix(~ X - 1, data = foo)
    SSWithin <- sum(diag(t(G) %*% D2 %*% G)/(2*apply(G, 2, sum)))
    empire <- sapply(1:100, function(ignore) {
      foo <- data.frame(X = sample(split))
      G <- model.matrix(~ X - 1, data = foo)
      SSWithin <- sum(diag(t(G) %*% D2 %*% G)/(2*apply(G, 2, sum)))
      SSWithin
    })
    M <- sum(empire < SSWithin)
    list(stat = SSWithin, pv = (M+1)/102)
  }
  tool <- match.arg(tool)
  cat("Calling", tool, "\n", file = stderr())
  dmat <- gendist(data, metric)
  val <- switch(tool,
                sw = silwidth(split, dmat),
                ssq = sumsq(split, dmat))
  val
}
