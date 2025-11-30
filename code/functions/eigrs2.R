eigrs2 <- function(y) {
  # sort eigenvalues and corresponding eigenvectors in ascending order
  # Output: eigenvalues as column vector in ascending order
  eig_result <- eigen(y)
  lamv <- eig_result$values
  I <- order(lamv)
  lamv <- lamv[I]
  evec <- eig_result$vectors[, I]
  return(list(evec = evec, lamv = lamv))
}
