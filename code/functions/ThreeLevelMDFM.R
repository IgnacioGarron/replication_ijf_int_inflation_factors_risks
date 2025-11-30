source("functions/eigrs2.R") # Eigen with ascending order
source("functions/blockfact0.R") # CCA

ThreeMDFM1<-function(Yorig,groups,spec=1){

  Yorig <- scale(Yorig) # Standarize
  R<-list() # where everything is saved
  ##################################################################
  ##                    Get groups for level 2                    ##
  ##################################################################
  #Yorig<-Yorig[,order(colnames(Yorig))]
  #check<-data.frame(colnames(Yorig))
  names_Yorig<-colnames(Yorig) # To go back to orginal ordering
  R1 = Yorig[,which(groups[,2]==1)] #      % Africa
  idx_R1=which(groups[,2]==1)
  NR1 = ncol(R1)
  R2 = Yorig[,which(groups[,3]==1)]  #      % America
  idx_R2=which(groups[,3]==1)
  NR2 = ncol(R2)
  R3 = Yorig[,which(groups[,4]==1)]  #      % Asia
  idx_R3=which(groups[,4]==1)
  NR3 = ncol(R3)
  R4 = Yorig[,which(groups[,5]==1)]  #      % Europe
  idx_R4=which(groups[,5]==1)
  NR4 = ncol(R4)
  
  
  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                 OBTAIN ESTIMATES OF INITIAL FACTORS                 ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################
  beta_ols<-function(X,Y){
    t(Y) %*% X %*% solve(t(X) %*% X)
  }
  
  # Order data and inputs for CCA
  y = cbind(R1,R2,R3,R4)
  Nregio=c(NR1,NR2,NR3,NR4)
  r_glob=1
  r_reg=c(1,1,1,1)
  
  G0 = blockfact0(y, Nregio,r_glob,r_reg)
  R[["GO"]]=scale(G0);  #CCA GLOBAL FACTOR
  
  yG0 = y-G0 %*% t(beta_ols(G0,y)) #Resid Global
  colnames(y)
  # 
  Nregio=c(NR1,NR2,NR3,NR4)
  r_reg=c(1,1,1,1)
  g = length(Nregio)
  RegInd = c(0, cumsum(Nregio))
  fhatreg = c() 
  for(i in 1:g){
    evec<-eigrs2(t(yG0[,(RegInd[i]+1):RegInd[i+1]]) %*% yG0[,(RegInd[i]+1):RegInd[i+1]])$evec
    fhatreg<-cbind(fhatreg,yG0[,(RegInd[i]+1):RegInd[i+1]]%*%evec[,(ncol(evec)-r_reg[i]+1):ncol(evec)])
  }
  
  fhatreg<-fhatreg / kronecker(matrix(1,nrow = nrow(fhatreg),ncol=1),t(matrix(sqrt(diag(t(fhatreg)%*%fhatreg)))))
  
  
  #################################################################
  ##       Obtain estimates of initial factors (3rd level)       ##
  #################################################################
  
  y_r1 = yG0[,1:NR1]
  yR1 = y_r1-fhatreg[,1] %*% t(beta_ols(fhatreg[,1],y_r1))
  if(spec==2)  yR1 = y_r1
  y_r2=yG0[,(NR1+1):(NR1+NR2)]
  yR2 = y_r2-fhatreg[,2] %*% t(beta_ols(fhatreg[,2],y_r2))
  
  y_r3=yG0[,(NR1+NR2+1):(NR1+NR2+NR3)]
  yR3 = y_r3-fhatreg[,3] %*% t(beta_ols(fhatreg[,3],y_r3))
  if(spec==2)  yR3 = y_r3
  y_r4=yG0[,(NR1+NR2+NR3+1):(NR1+NR2+NR3+NR4)]
  yR4 = y_r4-fhatreg[,4] %*% t(beta_ols(fhatreg[,4],y_r4))
  
  
  Prev_Block_Lev3 = cbind(yR1,yR2,yR3,yR4) #
  #Prev_Block_Lev3 = cbind(y_r1,y_r2,y_r3,y_r4) # Error in matlab
  idx_Lev2 = c(idx_R1,idx_R2,idx_R3,idx_R4)
  yprevBlock3L = Prev_Block_Lev3[,order(idx_Lev2)] # missing in matlab
  #yprevBlock3L = Prev_Block_Lev3[,idx_Lev2] # Matlab error
  
  
  R5 = yprevBlock3L[,which(groups[,6]==1)] #      % Advanced
  idx_R5=which(groups[,6]==1)
  NR5 = ncol(R5)
  R6 = yprevBlock3L[,which(groups[,7]==1)]  #      % Emerging
  idx_R6 = which(groups[,7]==1)
  NR6 = ncol(R6)
  R7 = yprevBlock3L[,which(groups[,8]==1)]  #      % Low income
  idx_R7 = which(groups[,8]==1)
  NR7 = ncol(R7)
  
  
  idx_Lev3 = c(idx_R5,idx_R6,idx_R7)
  yBlock3L = cbind(R5,R6,R7)
  
  
  Nregio=c(NR5,NR6,NR7)
  r_reg=c(1,1,1)
  g = length(Nregio)
  RegInd = c(0,cumsum(Nregio))
  fBlockLev3 = c() 
  for (i in 1:g){
    evec<-eigrs2(t(yBlock3L[,(RegInd[i]+1):RegInd[i+1]]) %*% yBlock3L[,(RegInd[i]+1):RegInd[i+1]])$evec
    fBlockLev3<-cbind(fBlockLev3,yBlock3L[,(RegInd[i]+1):RegInd[i+1]]%*%evec[,(ncol(evec)-r_reg[i]+1):ncol(evec)])
  }
  
  fBlockLev3<-fBlockLev3 / kronecker(matrix(1,nrow = nrow(fBlockLev3),ncol=1),t(matrix(sqrt(diag(t(fBlockLev3)%*%fBlockLev3)))))
  
  # Initial values  for while
  uu1=100000000
  uu0=1100000000000
  conteo=0
  
  
  while ((uu0-uu1)> 0.000001){
    conteo = conteo+1
    lamG_aux = beta_ols(G0,y)
    uG0 = y-G0 %*% t(beta_ols(G0,y))
    
    ur1 = uG0[,1:NR1] 
    ur2 = uG0[,(NR1+1):(NR1+NR2)]
    ur3 = uG0[,(NR1+NR2+1):(NR1+NR2+NR3)]
    ur4 = uG0[,(NR1+NR2+NR3+1):(NR1+NR2+NR3+NR4)] 
    
    lamR1 = beta_ols(fhatreg[,1],ur1) 
    if(spec==2) lamR1 = beta_ols(fhatreg[,1],ur1)*0
    lamR2 = beta_ols(fhatreg[,2],ur2) 
    lamR3 = beta_ols(fhatreg[,3],ur3) 
    if(spec==2) lamR3 = beta_ols(fhatreg[,1],ur3)*0
    lamR4 = beta_ols(fhatreg[,4],ur4) 
    
    uR1L2 = ur1-fhatreg[,1] %*% t(beta_ols(fhatreg[,1],ur1))
    if(spec==2) uR1L2 = ur1
    uR2L2 = ur2-fhatreg[,2] %*% t(beta_ols(fhatreg[,2],ur2))
    uR3L2 = ur3-fhatreg[,3] %*% t(beta_ols(fhatreg[,3],ur3))
    if(spec==2) uR3L2 = ur3
    uR4L2 = ur4-fhatreg[,4] %*% t(beta_ols(fhatreg[,4],ur4))
    
    Prev_Block_Lev3=cbind(uR1L2,uR2L2,uR3L2,uR4L2)
    yprevBlock3L=Prev_Block_Lev3[,order(idx_Lev2)] # Correcto
    #yprevBlock3L=Prev_Block_Lev3[,idx_Lev2] # Error Matlab
    colnames(Prev_Block_Lev3)==colnames(Yorig[,idx_Lev2]) # check
    
    ur5 = yprevBlock3L[,which(groups[,6]==1)]      # Advanced
    ur6 = yprevBlock3L[,which(groups[,7]==1)]      # Emerging
    if(spec==2) ur6=0*ur6 
    ur7 = yprevBlock3L[,which(groups[,8]==1)]      # Low Income
    
    lamR5=beta_ols(fBlockLev3[,1],ur5)
    lamR6=beta_ols(fBlockLev3[,2],ur6)
    if(spec==2) lamR6=0*lamR6
    lamR7=beta_ols(fBlockLev3[,3],ur7)
    
    LAMBDA_2LEVEL_aux = cbind(c(lamR1,rep(0,NR2+NR3+NR4)),
                              c(rep(0,NR1),lamR2,rep(0,NR3+NR4)),
                              c(rep(0,NR1+NR2),lamR3,rep(0,NR4)),
                              c(rep(0,NR1+NR2+NR3),lamR4))
    
    lamG = lamG_aux[order(idx_Lev2),]
    #lamG = lamG_aux[idx_Lev2,]
    rownames(LAMBDA_2LEVEL_aux) = colnames(Prev_Block_Lev3) # add names
    LAMBDA_2LEVEL= LAMBDA_2LEVEL_aux[order(idx_Lev2),]
    #LAMBDA_2LEVEL= LAMBDA_2LEVEL_aux[idx_Lev2,]
    
    LAMBDA_3LEVEL_aux=cbind(c(lamR5,rep(0,NR6+NR7)),
                            c(rep(0,NR5),lamR6,rep(0,NR7)),
                            c(rep(0,NR5+NR6),lamR7))
    rownames(LAMBDA_3LEVEL_aux) = colnames(yBlock3L) # add names
    LAMBDA_3LEVEL= LAMBDA_3LEVEL_aux[order(idx_Lev3),]
    #LAMBDA_3LEVEL= LAMBDA_3LEVEL_aux[idx_Lev3,]
    
    
    if(spec==1){
      lam = cbind(lamG,LAMBDA_2LEVEL,LAMBDA_3LEVEL) 
      fhat = beta_ols(lam,t(Yorig))
    } else if(spec==2) {
      lam = cbind(lamG,LAMBDA_2LEVEL,LAMBDA_3LEVEL) 
      fhat = beta_ols(lam[,-c(2,4,7)],t(Yorig))
      lam = lam[,-c(2,4,7)]
    }
    #################################################################
    ##                      Orthogonalization                      ##
    #################################################################
    F_Global=fhat[,1]
    G0=fhat[,1] # Adición para que actualice el algoritmo
    if(spec==1) {
      F_R1 = fhat[,2]-F_Global %*% t(beta_ols(F_Global,fhat[,2]))
      F_R2 = fhat[,3]-F_Global %*% t(beta_ols(F_Global,fhat[,3]))
      F_R3 = fhat[,4]-F_Global %*% t(beta_ols(F_Global,fhat[,4]))
      F_R4 = fhat[,5]-F_Global %*% t(beta_ols(F_Global,fhat[,5]))

      FR5reg=beta_ols(cbind(F_Global,F_R1,F_R2,F_R3,F_R4),fhat[,6])
      F_R5=fhat[,6]- cbind(F_Global,F_R1,F_R2,F_R3,F_R4)  %*% t(FR5reg)
      FR6reg=beta_ols(cbind(F_Global,F_R1,F_R2,F_R3,F_R4),fhat[,7])
      F_R6=fhat[,7]- cbind(F_Global,F_R1,F_R2,F_R3,F_R4)  %*% t(FR6reg)
      FR7reg=beta_ols(cbind(F_Global,F_R1,F_R2,F_R3,F_R4),fhat[,8])
      F_R7=fhat[,8]- cbind(F_Global,F_R1,F_R2,F_R3,F_R4)  %*% t(FR7reg)
      ############ F'F=I
      # F_R1 = fhat[,2]-cbind(F_Global,fhat[,3],fhat[,4],fhat[,5]) %*% t(beta_ols(cbind(F_Global,fhat[,3],fhat[,4],fhat[,5]),fhat[,2]))
      # F_R2 = fhat[,3]-cbind(F_Global,F_R1,fhat[,4],fhat[,5]) %*% t(beta_ols(cbind(F_Global,F_R1,fhat[,4],fhat[,5]),fhat[,3]))
      # F_R3 = fhat[,4]-cbind(F_Global,F_R1,F_R2,fhat[,5]) %*% t(beta_ols(cbind(F_Global,F_R1,F_R2,fhat[,5]),fhat[,4]))
      # F_R4 = fhat[,5]-cbind(F_Global,F_R1,F_R2,F_R3) %*% t(beta_ols(cbind(F_Global,F_R1,F_R2,F_R3),fhat[,5]))
      # 
      # FR5reg=beta_ols(cbind(F_Global,F_R1,F_R2,F_R3,F_R4,fhat[,7],fhat[,8]),fhat[,6])
      # F_R5=fhat[,6]- cbind(F_Global,F_R1,F_R2,F_R3,F_R4,fhat[,7],fhat[,8])  %*% t(FR5reg)
      # FR6reg=beta_ols(cbind(F_Global,F_R1,F_R2,F_R3,F_R4,F_R5,fhat[,8]),fhat[,7])
      # F_R6=fhat[,7]- cbind(F_Global,F_R1,F_R2,F_R3,F_R4,F_R5,fhat[,8])  %*% t(FR6reg)
      # FR7reg=beta_ols(cbind(F_Global,F_R1,F_R2,F_R3,F_R4,F_R5,F_R6),fhat[,8])
      # F_R7=fhat[,8]- cbind(F_Global,F_R1,F_R2,F_R3,F_R4,F_R5,F_R6)  %*% t(FR7reg)
    }else if(spec==2){
      F_R2 = fhat[,2]-F_Global %*% t(beta_ols(F_Global,fhat[,2]))
      F_R4 = fhat[,3]-F_Global %*% t(beta_ols(F_Global,fhat[,3]))
      
      FR5reg=beta_ols(cbind(F_Global,F_R2,F_R4),fhat[,4])
      F_R5=fhat[,4]- cbind(F_Global,F_R2,F_R4)  %*% t(FR5reg)
      FR7reg=beta_ols(cbind(F_Global,F_R2,F_R4),fhat[,5])
      F_R7=fhat[,5]- cbind(F_Global,F_R2,F_R4)  %*% t(FR7reg) 
    }
   
    #################################################################
    ##                          Residuals                          ##
    #################################################################
    
    uu0 = uu1
    e = (y - fhat %*%  t(lam)) 
    uu1 = sum(diag(t(e) %*%  e))
    cat("Iteración:",conteo,"\n")
  }
  # round(cor(cbind(Global,F_R1,F_R2,F_R3,F_R4,F_R5,F_R7)),2)
  # FF<-cbind(Global,F_R1,F_R2,F_R3,F_R4,F_R5,F_R7)
  # Run F'F=I
  # round(cor(cbind(F_Global,F_R1,F_R2,F_R3,F_R4,F_R5,F_R7)),2)
  # FP<-cbind(Global,F_R1,F_R2,F_R3,F_R4,F_R5,F_R7)
  # plot(FF[,1]*-1,t="l")
  # lines(FP[,1]*-1,t="l",col=2)
  # plot(FF[,2]*-1,t="l")
  # lines(FP[,2],t="l",col=2)
  # plot(FF[,3]*1,t="l")
  # lines(FP[,3],t="l",col=2)
  # plot(FF[,4]*1,t="l")
  # lines(FP[,4],t="l",col=2)
  # plot(FF[,5]*1,t="l")
  # lines(FP[,5],t="l",col=2)
  # plot(FF[,6]*1,t="l")
  # lines(FP[,6],t="l",col=2)
  # plot(FF[,7]*1,t="l")
  # lines(FP[,7],t="l",col=2)
  # diag(round(cor(cbind(FF,FP))[1:7,8:14],2))
  
  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                      ORTHO., LOADINGS, FACTORS                      ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################
  
  if(spec==1) lam_inflation=matrix(0,nrow = 115,ncol = 8)
  if(spec==2) lam_inflation=matrix(0,nrow = 115,ncol = 5)
  t=nrow(y)
  # GLOBAL 
  V = (lam[,1]%*% t(F_Global) %*%  F_Global%*% t(lam[,1]))/t
  evec = eigrs2(V)$evec
  evec=t(lam[,1]) %*% evec[,ncol(evec)]
  Global = F_Global %*% evec
  Global=Global/sd(Global)
  lam_inflation[,1]= beta_ols(Global,Yorig)
  cor(Global,F_Global)
  
  # REGIONAL
  if (spec==1){
    V = (t(F_R1) %*% F_R1)/t
    evec = eigrs2(V)$evec
    FR1 = F_R1 %*%evec[length(evec)]
    FR1 = FR1/sd(FR1)
    lam_inflation[idx_R1,2]= beta_ols(FR1,Yorig[,idx_R1])
    
    V = (t(F_R2) %*% F_R2)/t
    evec = eigrs2(V)$evec
    FR2 = F_R2 %*%evec[length(evec)]
    FR2 = FR2/sd(FR2)
    lam_inflation[idx_R2,3]= beta_ols(FR2,Yorig[,idx_R2])
    
    V = (t(F_R3) %*% F_R3)/t
    evec = eigrs2(V)$evec
    FR3 = F_R3 %*%evec[length(evec)]
    FR3 = FR3/sd(FR3)
    lam_inflation[idx_R3,4]= beta_ols(FR3,Yorig[,idx_R3])
    
    V = (t(F_R4) %*% F_R4)/t
    evec = eigrs2(V)$evec
    FR4 = F_R4 %*%evec[length(evec)]
    FR4 = FR4/sd(FR4)
    lam_inflation[idx_R4,5]= beta_ols(FR4,Yorig[,idx_R4])
    
    V = (t(F_R5) %*% F_R5)/t
    evec = eigrs2(V)$evec
    FR5 = F_R5 %*%evec[length(evec)]
    FR5 = FR5/sd(FR5)
    lam_inflation[idx_R5,6]= beta_ols(FR5,Yorig[,idx_R5])
    
    V = (t(F_R6) %*% F_R6)/t
    evec = eigrs2(V)$evec
    FR6 = F_R6 %*%evec[length(evec)]
    FR6 = FR6/sd(FR6)
    lam_inflation[idx_R6,7]= beta_ols(FR6,Yorig[,idx_R6])
  
    V = (t(F_R7) %*% F_R7)/t
    evec = eigrs2(V)$evec
    FR7 = F_R7 %*%evec[length(evec)]
    FR7 = FR7/sd(FR7)
    lam_inflation[idx_R7,8]= beta_ols(FR7,Yorig[,idx_R7])
  } else if (spec==2){
    
    V = (t(F_R2) %*% F_R2)/t
    evec = eigrs2(V)$evec
    FR2 = F_R2 %*%evec[length(evec)]
    FR2 = FR2/sd(FR2)
    lam_inflation[idx_R2,2]= beta_ols(FR2,Yorig[,idx_R2])
    
    V = (t(F_R4) %*% F_R4)/t
    evec = eigrs2(V)$evec
    FR4 = F_R4 %*%evec[length(evec)]
    FR4 = FR4/sd(FR4)
    lam_inflation[idx_R4,3]= beta_ols(FR4,Yorig[,idx_R4])
    
    V = (t(F_R5) %*% F_R5)/t
    evec = eigrs2(V)$evec
    FR5 = F_R5 %*%evec[length(evec)]
    FR5 = FR5/sd(FR5)
    lam_inflation[idx_R5,4]= beta_ols(FR5,Yorig[,idx_R5])
    
    V = (t(F_R7) %*% F_R7)/t
    evec = eigrs2(V)$evec
    FR7 = F_R7 %*%evec[length(evec)]
    FR7 = FR7/sd(FR7)
    lam_inflation[idx_R7,5]= beta_ols(FR7,Yorig[,idx_R7])
    
    
    
    
    
  }
  
    
  if (spec==1){
    R[["Factors_Inflation"]] <- cbind(Global,FR1,FR2,FR3,FR4,FR5,FR6,FR7)
    R[["Lam_Inflation"]] <- lam_inflation
    
    colnames(R[["Factors_Inflation"]])<-c("Global","Africa","America",
                                          "Asia and Oceania","Europe","Advanced",
                                          "HMI-EMDEs","LI-EMDEs")
    rownames(R[["Lam_Inflation"]])<-names_Yorig
    }else if (spec==2){
    R[["Factors_Inflation"]] <- cbind(Global,FR2,FR4,FR5,FR7)
    R[["Lam_Inflation"]] <- lam_inflation
    
    colnames(R[["Factors_Inflation"]])<-c("Global","America",
                                          "Europe","Advanced",
                                          "LI-EMDEs")
    rownames(R[["Lam_Inflation"]])<-names_Yorig
    }
  R[["Residuals"]]=e
  
  return(R)
}



TwolevelMDFM1<-function(Yorig,groups){
  
  Yorig <- scale(Yorig) # Standarize
  R<-list() # where everything is saved
  ##################################################################
  ##                    Get groups for level 2                    ##
  ##################################################################
  #Yorig<-Yorig[,order(colnames(Yorig))]
  #check<-data.frame(colnames(Yorig))
  names_Yorig<-colnames(Yorig) # To go back to orginal ordering
  R1 = Yorig[,which(groups[,2]==1)] #      % Africa
  idx_R1=which(groups[,2]==1)
  NR1 = ncol(R1)
  R2 = Yorig[,which(groups[,3]==1)]  #      % America
  idx_R2=which(groups[,3]==1)
  NR2 = ncol(R2)
  R3 = Yorig[,which(groups[,4]==1)]  #      % Asia
  idx_R3=which(groups[,4]==1)
  NR3 = ncol(R3)
  R4 = Yorig[,which(groups[,5]==1)]  #      % Europe
  idx_R4=which(groups[,5]==1)
  NR4 = ncol(R4)
  
  
  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                 OBTAIN ESTIMATES OF INITIAL FACTORS                 ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################
  beta_ols<-function(X,Y){
    t(Y) %*% X %*% solve(t(X) %*% X)
  }
  
  # Order data and inputs for CCA
  y = cbind(R1,R2,R3,R4)
  Nregio=c(NR1,NR2,NR3,NR4)
  r_glob=1
  r_reg=c(1,1,1,1)
  
  G0 = blockfact0(y, Nregio,r_glob,r_reg)
  R[["GO"]]=scale(G0);  #CCA GLOBAL FACTOR
  
  yG0 = y-G0 %*% t(beta_ols(G0,y)) #Resid Global
  colnames(y)
  # 
  Nregio=c(NR1,NR2,NR3,NR4)
  r_reg=c(1,1,1,1)
  g = length(Nregio)
  RegInd = c(0, cumsum(Nregio))
  fhatreg = c() 
  for(i in 1:g){
    evec<-eigrs2(t(yG0[,(RegInd[i]+1):RegInd[i+1]]) %*% yG0[,(RegInd[i]+1):RegInd[i+1]])$evec
    fhatreg<-cbind(fhatreg,yG0[,(RegInd[i]+1):RegInd[i+1]]%*%evec[,(ncol(evec)-r_reg[i]+1):ncol(evec)])
  }
  
  fhatreg<-fhatreg / kronecker(matrix(1,nrow = nrow(fhatreg),ncol=1),t(matrix(sqrt(diag(t(fhatreg)%*%fhatreg)))))
  

  # Initial values  for while
  uu1=100000000
  uu0=1100000000000
  conteo=0
  
  
  while ((uu0-uu1)> 0.000001){
    conteo = conteo+1
    lamG_aux = beta_ols(G0,y)
    uG0 = y-G0 %*% t(beta_ols(G0,y))
    
    ur1 = uG0[,1:NR1] 
    ur2 = uG0[,(NR1+1):(NR1+NR2)]
    ur3 = uG0[,(NR1+NR2+1):(NR1+NR2+NR3)]
    ur4 = uG0[,(NR1+NR2+NR3+1):(NR1+NR2+NR3+NR4)] 
    
    lamR1 = beta_ols(fhatreg[,1],ur1) 
    lamR2 = beta_ols(fhatreg[,2],ur2) 
    lamR3 = beta_ols(fhatreg[,3],ur3) 
    lamR4 = beta_ols(fhatreg[,4],ur4) 
    
    uR1L2 = ur1-fhatreg[,1] %*% t(beta_ols(fhatreg[,1],ur1))
    uR2L2 = ur2-fhatreg[,2] %*% t(beta_ols(fhatreg[,2],ur2))
    uR3L2 = ur3-fhatreg[,3] %*% t(beta_ols(fhatreg[,3],ur3))
    uR4L2 = ur4-fhatreg[,4] %*% t(beta_ols(fhatreg[,4],ur4))
    
    Prev_Block_Lev3=cbind(uR1L2,uR2L2,uR3L2,uR4L2)
    idx_Lev2 = c(idx_R1,idx_R2,idx_R3,idx_R4)
    yprevBlock3L=Prev_Block_Lev3[,order(idx_Lev2)] # Correcto
    #yprevBlock3L=Prev_Block_Lev3[,idx_Lev2] # Error Matlab
    colnames(Prev_Block_Lev3)==colnames(Yorig[,idx_Lev2]) # check
   
    LAMBDA_2LEVEL_aux = cbind(c(lamR1,rep(0,NR2+NR3+NR4)),
                              c(rep(0,NR1),lamR2,rep(0,NR3+NR4)),
                              c(rep(0,NR1+NR2),lamR3,rep(0,NR4)),
                              c(rep(0,NR1+NR2+NR3),lamR4))
    
    lamG = lamG_aux[order(idx_Lev2),]
    #lamG = lamG_aux[idx_Lev2,]
    rownames(LAMBDA_2LEVEL_aux) = colnames(Prev_Block_Lev3) # add names
    LAMBDA_2LEVEL= LAMBDA_2LEVEL_aux[order(idx_Lev2),]
    #LAMBDA_2LEVEL= LAMBDA_2LEVEL_aux[idx_Lev2,]
    
    
      lam = cbind(lamG,LAMBDA_2LEVEL) 
      fhat = beta_ols(lam,t(Yorig))
    #################################################################
    ##                      Orthogonalization                      ##
    #################################################################
    F_Global=fhat[,1]
    G0=fhat[,1] # Adición para que actualice el algoritmo
      F_R1 = fhat[,2]-F_Global %*% t(beta_ols(F_Global,fhat[,2]))
      F_R2 = fhat[,3]-F_Global %*% t(beta_ols(F_Global,fhat[,3]))
      F_R3 = fhat[,4]-F_Global %*% t(beta_ols(F_Global,fhat[,4]))
      F_R4 = fhat[,5]-F_Global %*% t(beta_ols(F_Global,fhat[,5]))
      
    #################################################################
    ##                          Residuals                          ##
    #################################################################
    
    uu0 = uu1
    e = (y - fhat %*%  t(lam)) 
    uu1 = sum(diag(t(e) %*%  e))
    cat("Iteración:",conteo,"\n")
  }
  
  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                      ORTHO., LOADINGS, FACTORS                      ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################
  
  lam_inflation=matrix(0,nrow = 115,ncol = 5)
  
  t=nrow(y)
  # GLOBAL 
  V = (lam[,1]%*% t(F_Global) %*%  F_Global%*% t(lam[,1]))/t
  evec = eigrs2(V)$evec
  evec=t(lam[,1]) %*% evec[,ncol(evec)]
  Global = F_Global %*% evec
  Global=Global/sd(Global)
  lam_inflation[,1]= beta_ols(Global,Yorig)
  cor(Global,F_Global)
  
  # REGIONAL
    V = (t(F_R1) %*% F_R1)/t
    evec = eigrs2(V)$evec
    FR1 = F_R1 %*%evec[length(evec)]
    FR1 = FR1/sd(FR1)
    lam_inflation[idx_R1,2]= beta_ols(FR1,Yorig[,idx_R1])
    
    V = (t(F_R2) %*% F_R2)/t
    evec = eigrs2(V)$evec
    FR2 = F_R2 %*%evec[length(evec)]
    FR2 = FR2/sd(FR2)
    lam_inflation[idx_R2,3]= beta_ols(FR2,Yorig[,idx_R2])
    
    V = (t(F_R3) %*% F_R3)/t
    evec = eigrs2(V)$evec
    FR3 = F_R3 %*%evec[length(evec)]
    FR3 = FR3/sd(FR3)
    lam_inflation[idx_R3,4]= beta_ols(FR3,Yorig[,idx_R3])
    
    V = (t(F_R4) %*% F_R4)/t
    evec = eigrs2(V)$evec
    FR4 = F_R4 %*%evec[length(evec)]
    FR4 = FR4/sd(FR4)
    lam_inflation[idx_R4,5]= beta_ols(FR4,Yorig[,idx_R4])
  
    R[["Factors_Inflation"]] <- cbind(Global,FR1,FR2,FR3,FR4)
    R[["Lam_Inflation"]] <- lam_inflation
    
    colnames(R[["Factors_Inflation"]])<-c("Global","Africa","America",
                                          "Asia and Oceania","Europe")
    rownames(R[["Lam_Inflation"]])<-names_Yorig
  R[["Residuals"]]=e
  
  return(R)
}

TwolevelMDFM2<-function(Yorig,groups){
  
  Yorig <- scale(Yorig) # Standarize
  R<-list() # where everything is saved
  ##################################################################
  ##                    Get groups for level 2                    ##
  ##################################################################
  #Yorig<-Yorig[,order(colnames(Yorig))]
  #check<-data.frame(colnames(Yorig))
  names_Yorig<-colnames(Yorig) # To go back to orginal ordering
  R1 = Yorig[,which(groups[,6]==1)] #      % Africa
  idx_R1=which(groups[,6]==1)
  NR1 = ncol(R1)
  R2 = Yorig[,which(groups[,7]==1)]  #      % America
  idx_R2=which(groups[,7]==1)
  NR2 = ncol(R2)
  R3 = Yorig[,which(groups[,8]==1)]  #      % Asia
  idx_R3=which(groups[,8]==1)
  NR3 = ncol(R3)
  
  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                 OBTAIN ESTIMATES OF INITIAL FACTORS                 ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################
  beta_ols<-function(X,Y){
    t(Y) %*% X %*% solve(t(X) %*% X)
  }
  
  # Order data and inputs for CCA
  y = cbind(R1,R2,R3)
  Nregio=c(NR1,NR2,NR3)
  r_glob=1
  r_reg=c(1,1,1)
  
  G0 = blockfact0(y, Nregio,r_glob,r_reg)
  R[["GO"]]=scale(G0);  #CCA GLOBAL FACTOR
  
  yG0 = y-G0 %*% t(beta_ols(G0,y)) #Resid Global
  colnames(y)
  # 
  Nregio=c(NR1,NR2,NR3)
  r_reg=c(1,1,1)
  g = length(Nregio)
  RegInd = c(0, cumsum(Nregio))
  fhatreg = c() 
  for(i in 1:g){
    evec<-eigrs2(t(yG0[,(RegInd[i]+1):RegInd[i+1]]) %*% yG0[,(RegInd[i]+1):RegInd[i+1]])$evec
    fhatreg<-cbind(fhatreg,yG0[,(RegInd[i]+1):RegInd[i+1]]%*%evec[,(ncol(evec)-r_reg[i]+1):ncol(evec)])
  }
  
  fhatreg<-fhatreg / kronecker(matrix(1,nrow = nrow(fhatreg),ncol=1),t(matrix(sqrt(diag(t(fhatreg)%*%fhatreg)))))
  
  
  # Initial values  for while
  uu1=100000000
  uu0=1100000000000
  conteo=0
  
  
  while ((uu0-uu1)> 0.000001){
    conteo = conteo+1
    lamG_aux = beta_ols(G0,y)
    uG0 = y-G0 %*% t(beta_ols(G0,y))
    
    ur1 = uG0[,1:NR1] 
    ur2 = uG0[,(NR1+1):(NR1+NR2)]
    ur3 = uG0[,(NR1+NR2+1):(NR1+NR2+NR3)]
    
    lamR1 = beta_ols(fhatreg[,1],ur1) 
    lamR2 = beta_ols(fhatreg[,2],ur2) 
    lamR3 = beta_ols(fhatreg[,3],ur3) 
    
    uR1L2 = ur1-fhatreg[,1] %*% t(beta_ols(fhatreg[,1],ur1))
    uR2L2 = ur2-fhatreg[,2] %*% t(beta_ols(fhatreg[,2],ur2))
    uR3L2 = ur3-fhatreg[,3] %*% t(beta_ols(fhatreg[,3],ur3))
    
    Prev_Block_Lev3=cbind(uR1L2,uR2L2,uR3L2)
    idx_Lev2 = c(idx_R1,idx_R2,idx_R3)
    yprevBlock3L=Prev_Block_Lev3[,order(idx_Lev2)] # Correcto
    #yprevBlock3L=Prev_Block_Lev3[,idx_Lev2] # Error Matlab
    colnames(Prev_Block_Lev3)==colnames(Yorig[,idx_Lev2]) # check
    
    LAMBDA_2LEVEL_aux = cbind(c(lamR1,rep(0,NR2+NR3)),
                              c(rep(0,NR1),lamR2,rep(0,NR3)),
                              c(rep(0,NR1+NR2),lamR3))
    
    lamG = lamG_aux[order(idx_Lev2),]
    #lamG = lamG_aux[idx_Lev2,]
    rownames(LAMBDA_2LEVEL_aux) = colnames(Prev_Block_Lev3) # add names
    LAMBDA_2LEVEL= LAMBDA_2LEVEL_aux[order(idx_Lev2),]
    #LAMBDA_2LEVEL= LAMBDA_2LEVEL_aux[idx_Lev2,]
    
    
    lam = cbind(lamG,LAMBDA_2LEVEL) 
    fhat = beta_ols(lam,t(Yorig))
    #################################################################
    ##                      Orthogonalization                      ##
    #################################################################
    F_Global=fhat[,1]
    G0=fhat[,1] # Adición para que actualice el algoritmo
    F_R1 = fhat[,2]-F_Global %*% t(beta_ols(F_Global,fhat[,2]))
    F_R2 = fhat[,3]-F_Global %*% t(beta_ols(F_Global,fhat[,3]))
    F_R3 = fhat[,4]-F_Global %*% t(beta_ols(F_Global,fhat[,4]))
    #################################################################
    ##                          Residuals                          ##
    #################################################################
    
    uu0 = uu1
    e = (y - fhat %*%  t(lam)) 
    uu1 = sum(diag(t(e) %*%  e))
    cat("Iteración:",conteo,"\n")
  }
  
  ###########################################################################
  ###########################################################################
  ###                                                                     ###
  ###                      ORTHO., LOADINGS, FACTORS                      ###
  ###                                                                     ###
  ###########################################################################
  ###########################################################################
  
  lam_inflation=matrix(0,nrow = 115,ncol = 5)
  
  t=nrow(y)
  # GLOBAL 
  V = (lam[,1]%*% t(F_Global) %*%  F_Global%*% t(lam[,1]))/t
  evec = eigrs2(V)$evec
  evec=t(lam[,1]) %*% evec[,ncol(evec)]
  Global = F_Global %*% evec
  Global=Global/sd(Global)
  lam_inflation[,1]= beta_ols(Global,Yorig)
  cor(Global,F_Global)
  
  # REGIONAL
  V = (t(F_R1) %*% F_R1)/t
  evec = eigrs2(V)$evec
  FR1 = F_R1 %*%evec[length(evec)]
  FR1 = FR1/sd(FR1)
  lam_inflation[idx_R1,2]= beta_ols(FR1,Yorig[,idx_R1])
  
  V = (t(F_R2) %*% F_R2)/t
  evec = eigrs2(V)$evec
  FR2 = F_R2 %*%evec[length(evec)]
  FR2 = FR2/sd(FR2)
  lam_inflation[idx_R2,3]= beta_ols(FR2,Yorig[,idx_R2])
  
  V = (t(F_R3) %*% F_R3)/t
  evec = eigrs2(V)$evec
  FR3 = F_R3 %*%evec[length(evec)]
  FR3 = FR3/sd(FR3)
  lam_inflation[idx_R3,4]= beta_ols(FR3,Yorig[,idx_R3])
  
  
  R[["Factors_Inflation"]] <- cbind(Global,FR1,FR2,FR3)
  R[["Lam_Inflation"]] <- lam_inflation
  
  colnames(R[["Factors_Inflation"]])<-c("Global","Advanced",
                                        "HMI-EMDEs","LI-EMDEs")
  rownames(R[["Lam_Inflation"]])<-names_Yorig
  R[["Residuals"]]=e
  
  return(R)
}

