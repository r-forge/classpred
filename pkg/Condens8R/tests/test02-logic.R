library(Condens8R)
##
nr <- 100 # features
nc <- 40  # samples
set.seed(97531)
bimat <- matrix(rbinom(nr*nc, 1, 0.45), nrow = nr)
dimnames(bimat) <- list(paste0("B", 1:nr),
                        paste0("S", 1:nc))
stat <- rbinom(nc, 1, 0.37)

myMod <- learn(logicModeler, bimat, stat)
table(predict(myMod), stat) # on the diagonal

rantest <- matrix(rbinom(nr*nc, 1, 0.45), nrow = nr)
dimnames(rantest) <- list(paste0("B", 1:nr),
                          paste0("S", 1:nc))
table(predict(myMod, rantest))

kmod <- learn(modeler3NN, bimat, stat)
table(predict(kmod), stat)

comat <- matrix(rnorm(nr*nc, 0, 1), nrow = nr)
dimnames(comat) <- list(paste0("B", 1:nr),
                        paste0("S", 1:nc))
coMod <- learn(logicModeler, comat, stat)
table(predict(coMod), stat) # on the diagonal
table(predict(coMod, rantest))
