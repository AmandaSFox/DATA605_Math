# Defining the two 2x2 matrices A and B
A <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
B <- matrix(c(5, 6, 7, 8), nrow = 2, byrow = TRUE)

# Computing the determinants
det_A <- det(A)
det_B <- det(B)
det_A_plus_B <- det(A + B)

# Checking whether det(A + B) = det(A) + det(B)
det_A + det_B == det_A_plus_B 


# check eigenvalues and vectors

library(Matrix)

A <- matrix(c(3,1,2,
              0,5,4,
              0,0,2),
            nrow=3, 
            byrow=TRUE)

A

my_eigen <- eigen(A)
my_eigen$values

my_eigen$vectors
