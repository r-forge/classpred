library(Condens8R)
## straightforward expected test
leftLeaf <- makeLeaf("L")
predict(leftLeaf, matrix(1, 3, 5)) # columns are samples, so expect five responses
rightLeaf <- makeLeaf("R")
predict(rightLeaf, matrix(1, 5, 3)) # this time, expect three
xyz <- makeLeaf("XYZ") # generates a warning because of levels
predict(xyz, matrix(1, 5, 3)) # but it works anyway
