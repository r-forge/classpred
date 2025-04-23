# Copyright (C) Kevin R. Coombes, 2025.

splitters <- new.env()
registerSplitter <- function(tag, description, FUN) {
  if (tag %in% names(splitters)) {
    warning("Replacing an existing splitter, 'tag', described as:",
            splitters[[tag]]$description, ".\n")
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

availableSplitters <- function() {
  tags <- names(splitters)
  scrip <- sapply(tags, function(S) {
    splitters[[S]]$description
  })
  data.frame(tag = tags, description = scrip)
}

## Auxiliary function to pool two different sets of distance metrics
## from other packages.
gendist <- function(X, metric) {
  dmat <- try(distanceMatrix(X, metric), silent = TRUE)   # ClassDiscovery
  if (inherits(dmat, "try-error")) {
    dmat <- try(binaryDistance(X, metric), silent = TRUE) # Mercator
  }
  dmat # tough luck if you passed in a metric not found in either place
}

findSplit <- function(data,
                      metric = "euclidean",
                      splitter = names(splitters),
                      LR = c("L", "R")) {
  dmat <- gendist(data, metric)
  splitter <- match.arg(splitter)
  FUN <- get(splitter, envir = splitters)
  bin <- FUN$action(dmat)
  factor(LR[bin], levels = LR) # forces your own splitter to give only two classes
}

## Add a parameter for user to specify size of empirical test?
evalSplit <- function(split, data, metric, tool = c("sw", "ssq"), N = 100) {
  silwidth <- function(split, dmat, N) {
    cat("In silwidth\n", file = stderr())
    sw <- silhouette(as.numeric(split), dmat)
    msw <- mean(sw[,3])
    swad <- sapply(1:N, function(ignore) {
      sw <- silhouette(sample(as.numeric(split)), dmat)
      mean(sw[,3])
    })
    M <- sum(swad > msw)
    list(stat = msw, pv = (M + 1)/(N + 2)) # pseudo Bayesian
  }
  sumsq <- function(split, dmat, N) {
    cat("In sumsq\n", file = stderr())
    D2 <- as.matrix(dmat)^2
    foo <- data.frame(X = split)
    G <- model.matrix(~ X - 1, data = foo)
    SSWithin <- sum(diag(t(G) %*% D2 %*% G)/(2*apply(G, 2, sum)))
    empire <- sapply(1:N, function(ignore) {
      foo <- data.frame(X = sample(split))
      G <- model.matrix(~ X - 1, data = foo)
      SSWithin <- sum(diag(t(G) %*% D2 %*% G)/(2*apply(G, 2, sum)))
      SSWithin
    })
    M <- sum(empire < SSWithin)
    list(stat = SSWithin, pv = (M + 1)/(N + 2))
  }
  tool <- match.arg(tool)
  cat("Calling", tool, "\n", file = stderr())
  dmat <- gendist(data, metric)
  val <- switch(tool,
                sw = silwidth(split, dmat, N),
                ssq = sumsq(split, dmat, N))
  val
}
