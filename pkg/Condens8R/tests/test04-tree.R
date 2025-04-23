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

ct <- createTree(bimat, "jaccard", "H", splitter = "dv")
table(pred <- predict(ct))
table(PRE=pred, TRU=ct@cluster)

ct <- createTree(bimat, "jaccard", "H", splitter = "km")
table(pred <- predict(ct))
table(PRE=pred, TRU=ct@cluster)

