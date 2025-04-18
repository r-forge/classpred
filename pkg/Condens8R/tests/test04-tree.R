library(Condens8R)
packageVersion("Condens8R")
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
table(predict(ct@model))
table(final <- predict(ct))

bdist <- binaryDistance(bimat, "jaccard")
merc <- Mercator(bdist, "Jaccard", "hclust", K = 6)
merc <- addVisualization(merc, "mds")
merc <- addVisualization(merc, "umap")
merc <- addVisualization(merc, "tsne", perplexity = 10)
barplot(merc)
plot(merc, view = "hclust")
plot(merc, view = "mds")
plot(merc, view = "umap")
plot(merc, view = "tsne")


venus <- setClusters(merc, as.numeric(factor(final)))
barplot(venus)
plot(venus, view = "hclust")
plot(venus, view = "mds")
plot(venus, view = "umap")
plot(venus, view = "tsne")

