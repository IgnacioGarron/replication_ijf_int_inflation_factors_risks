replace_outliers <- function(x,lambda=10, na.rm = TRUE, ...) {
  y=as.matrix(x)
  for (j in 1:ncol(y)){
    qnt <- quantile(y[,j], probs=c(.25, .75), na.rm = na.rm, ...)
    H <- lambda * IQR(y[,j], na.rm = na.rm)
    y[y[,j]< (qnt[1] - H),j] <- qnt[1] - H
    y[y[,j]> (qnt[2] + H),j] <- qnt[2] + H
  }
  return(y)
}