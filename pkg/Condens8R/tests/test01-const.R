library(Condens8R)
leftLeaf <- makeLeaf("L")
predict(leftLeaf, matrix(1, 3, 5))
rightLeaf <- makeLeaf("R")
predict(rightLeaf, matrix(1, 5, 3))
xyz <- makeLeaf("XYZ") # generates a warning
predict(xyz, matrix(1, 5, 3))
