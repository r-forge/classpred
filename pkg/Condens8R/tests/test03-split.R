library(Condens8R)
##
nr <- 200 # features
nc <- 60  # samples
set.seed(80123)
comat <- matrix(rnorm(nr*nc, 0, 1), nrow = nr)
dimnames(comat) <- list(paste0("F", 1:nr),
                        paste0("S", 1:nc))
splay <- rep(c(2, -2), each = nc/2)
for(J in 1:30) comat[J, ] <- comat[J, ] + splay

## Test four different splitters
C.hc <- findSplit(comat, splitter = "hc")
C.km <- findSplit(comat, splitter = "km")
C.dv <- findSplit(comat, splitter = "dv")
C.ap <- findSplit(comat, splitter = "km")
table(C.hc, splay)
table(C.hc, C.km)
table(C.hc, C.dv)
table(C.hc, C.ap)

evalSplit(C.hc, comat, "euclid", "sw")
evalSplit(C.hc, comat, "euclid", "ssq")
