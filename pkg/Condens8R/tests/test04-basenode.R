library(Condens8R)
new("Node")
cons <- Modeler(learnConstant, predictConstant, value = "XYZ")
try(learn(cons, matrix(), LETTERS, keepAll))
fm <- learn(cons, matrix(), factor(LETTERS), keepAll)
predict(fm)
predict(fm, matrix(NA, nrow=5, ncol = 10))

nr <- 200 # features
nc <- 60  # samples
set.seed(80123)
comat <- matrix(rnorm(nr*nc, 0, 1), nrow = nr)
dimnames(comat) <- list(paste0("F", 1:nr),
                        paste0("S", 1:nc))
splay <- rep(c(2, -2), each = nc/2)
for(J in 1:30) comat[J, ] <- comat[J, ] + splay
bimat <- Condens8R:::dichotomize(comat)$data

dmat <- Mercator::binaryDistance(bimat, "jaccard")
mySplit <- findSplit(dmat, splitter = "hc")

left  <- makeLeaf("XYZ")
right <- makeLeaf("UVW")

cluster <- as.character(mySplit)
cluster[mySplit == "L"] <- predict(left, bimat[, mySplit == "L"])
cluster[mySplit == "R"] <- predict(right, bimat[, mySplit == "R"])




