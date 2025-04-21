library(Condens8R)
nr <- 200 # features
nc <- 60  # samples
set.seed(80123)
comat <- matrix(rnorm(nr*nc, 0, 1), nrow = nr)
dimnames(comat) <- list(paste0("F", 1:nr),
                        paste0("S", 1:nc))
splay <- rep(c(2, -2), each = nc/2)
for(J in 1:30) comat[J, ] <- comat[J, ] + splay
bimat <- Condens8R:::dichotomize(comat)$data

ct <- createTree(bimat, "jaccard", "H")
table(pred <- predict(ct))
table(PRE=pred, TRU=ct@cluster)
## At second step down to the left, split is 12-13, but predictions are 11-14.

ct <- createTree(bimat, "jaccard", "H", pcut = 0.01, N = 1000)
table(pred <- predict(ct))
table(PRE=pred, TRU=ct@cluster)

ct <- createTree(bimat, "jaccard", "H", algorithm = "dv")
table(pred <- predict(ct))
table(PRE=pred, TRU=ct@cluster)

ct <- createTree(bimat, "jaccard", "H", algorithm = "km")
table(pred <- predict(ct))
table(PRE=pred, TRU=ct@cluster)

if (FALSE) {
  X1 <- ct@Left                                     # this fails to predict
  X2 <- X1@Left; class(X1); dim(X1@model@trainData) # this fails to predict
  X3 <- X2@Left; class(X2); dim(X2@model@trainData) # this is okay
  newdata = X2@model@trainData
  pop <- predict(X2@model)
  pop <- as.numeric(factor(pop))
  table(pop)
  P1 <-newdata[, pop == 1, drop = FALSE]
  dim(P1)
  predict(X3, newdata = P1)
  M <- matrix(rbinom(200*17, 1, 0.35), nrow = 200)
  predict(X3, newdata = M)

  ct <- createTree(bimat, "jaccard", "H", N=1000)
  library(Mercator)
  bdist <- binaryDistance(bimat, "jaccard")
  merc <- Mercator(bdist, "Jaccard", "hclust", K = 6)
  merc <- addVisualization(merc, "mds")
  merc <- addVisualization(merc, "umap")
  merc <- addVisualization(merc, "tsne", perplexity = 10)
  venus <- setClusters(merc, as.numeric(factor(final)))

  opar <- par(mfrow = c(1,2)) 
  barplot(merc)
  barplot(venus)

  plot(merc, view = "hclust")
  plot(venus, view = "hclust")

  plot(merc, view = "mds")
  plot(venus, view = "mds")

  plot(merc, view = "umap")
  plot(venus, view = "umap")

  plot(merc, view = "tsne")
  plot(venus, view = "tsne")
  par(opar)
}
