blockfact0 <- function(y, Nregio,r_glob,r_reg){
  
  g=length(Nregio)
  RegInd = c(0, cumsum(Nregio))
  RegFno=cumsum(r_reg)
  r = r_glob + r_reg
  rsum = cumsum(r);
  
  fi=c()
  i=1
    
  for (i in 1:g){
    evec<-eigrs2(t(y[,(RegInd[i]+1):RegInd[i+1]]) %*% y[,(RegInd[i]+1):RegInd[i+1]])$evec
    fi<-cbind(fi,y[,(RegInd[i]+1):RegInd[i+1]]%*%evec[,(ncol(evec)-r[i]+1):ncol(evec)])
  }
  
  # scale factors by their SD
  fi<-fi / kronecker(matrix(1,nrow = nrow(fi),ncol=1),t(matrix(sqrt(diag(t(fi)%*%fi)))))
  
  fhat=c()

  for (i in 1:(g-1)){
    C = t(fi[,1:rsum[1]]) %*% fi[,(rsum[i]+1):rsum[i+1]]
    evec = eigrs2(t(C) %*% C)$evec
    fhat = cbind(fhat,fi[,(rsum[i]+1):rsum[i+1]] %*% evec[,(ncol(evec)-r_glob+1):ncol(evec)])
  }
  
  C = t(fhat) %*% fhat 
  evec = eigrs2(t(C) %*% C)$evec;
  fhatblock = fhat %*% evec[,(ncol(evec)-r_glob+1):ncol(evec)]*-1; # -1 to get Matlab relust


  return(fhatblock)
}