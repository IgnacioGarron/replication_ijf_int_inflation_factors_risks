##### OVERVIEW

# This function generates the test statistic(s) and bootstrap statistics from the paper
# "Out-of-Sample Tests for Conditional Quantile Coverage: An Application to Growth-at-Risk"
# by Valentina Corradi, Jack Fosten and Daniel Gutknecht

# The function performs an out-of-sample evaluation of linear quantile regression models
# for predicting a target variable h steps ahead using the direct forecasting method
# and the fixed estimation scheme

# The models are based on the linear QAR(1) + predictor variables specified by the user


##### INPUTS:

### yData and XData
# Data for the target variable and candidate predictor variables

### models
# List of two or more linear quantile models with model variable identifiers
# The X vector will be ordered with an AR term followed by all variables in XData
# e.g. if Xdata contains two variables X1 and X2, then models=list(c(1,2),c(1,3))
# will compare the QAR(1)+X1 with the QAR(1)+X2

# IMPORTANT: the first entry of "models" is the benchmark model

### tauvalues
# One or model quantile levels for the test e.g. tauvalues=c(0.1,0.25,0.5)
# N.B. this is for the one-sided interval (0,tau] discussed in the paper

### hn
# forecast horizon

### P
# The length of the forecast evaluation window 

### lvalues
# One or more block lengths to be used in the computation of the bootstrap e.g. lvalues=c(1,2,5)

### Other parameters can be left at defaults unless desired
# Bootstrap replications: S=1999, trimming value: trim=0.01, epanechnikov kernel order: kernorder=2

##### OUTPUTS:

### SP 
# The pairwise test statistic 
# When there are multiple quantiles and/or more than two models, 
# each pairwise comparison with the benchmark is computed
# Results are ordered first by quantile level and then by model comparison (i.e. the model 1/model 2 comparison comes first for all tau values, then model1/model3 etc.)

### SPboot
# The bootstrap test statistics for all specified block lengths
# For each pairwise comparison in SP, there are S bootstrap statistics produced
# SPboot is a list object based on the block lengths specified in lvalues
# So the first list item gives the S bootstrap statistics with a block length of lvalues[1] etc.

### SPmax
# The max statistic
# This produces the single max statistic over multiple quantiles and/or models

### SPmaxboot
# The bootstrap max statistics for all specified block lengths
# SPmaxboot is also a list object based on the block lengths specified in lvalues

  library(np)
  library(quantreg)
  library(sandwich)
  

CFG_Test_Stat_insample=function(yData,XData,models,tauvalues=0.5,hn=1,lvalues=1,S=1999,trim=0.01,kernorder=2){
  
  
  # Generate y and X for direct multi-step quantile regression with X lagged hn periods relative to y
  y=matrix(yData[(1+hn):nrow(yData),]) 
  X=cbind(yData[1:(nrow(yData)-hn),],XData[1:(nrow(yData)-hn),])
  
  # Obtain total sample size and compute in-sample window R based on out-of-sample window P
  P=0
  T=nrow(y)
  R=T-P
  
  # Obtain total number of tau levels and models
  kTau=length(tauvalues)
  kmodel=length(models)
  
  
  # Pre-allocate bootstrap statistics
  # SPboot is a list object for multiple block lengths
  # For each block length, all pairwise bootstrap statistics are computed, ordered by tau and then by model
  SPboot=lapply(1:length(lvalues),function(x){matrix(NA,S,(kTau*(kmodel-1)))})
  
  # Pre-allocate bootstrap max statistics
  # Depending on whether multiple taus and/or multiple models (>2) are entered, this will either be the single max or double max stat
  SPmaxboot=lapply(1:length(lvalues),function(x){matrix(NA,S,1)})
  
  
  # Pre-allocate lists of matrices for loss differentials and wild bootstrap components
  # to be used in calculating the final test statistic and bootstrap test statistic
  # For the test statistic this is named Loss and the wild bootstrap components are A, B and D as in the paper
  # They are to be grouped first by tau values, and then by model
  Losslist=matrix(NA,T,kTau*(kmodel-1))
  Alist=matrix(NA,T,kTau*(kmodel-1))
  Blist=matrix(NA,T,kTau*(kmodel-1))
  Dlist=matrix(NA,T,kTau*(kmodel-1))
  
  
  for (modelcount in 2:kmodel){
    
    # Locate variables for model j, benchmark model, and union of j and benchmark
    # Recall that the benchmark model is stored in models[[1]]
    xselect=models[[modelcount]]
    xselectbench=models[[1]]
    xselectunion=sort(unique(c(xselect,xselectbench)))
    
    # Compute trimming for X's in the IN-of-sample portion
    # This is done for the union of X_j and X_1 
    # Work out top and bottom 100trim% for each X over R+1:T and combine
    # Obtain variable for which observations to keep
    trimmed=apply(X[1:T,xselectunion],2,function(x){x<quantile(x,trim)|x>quantile(x,1-trim)})
    keep=as.matrix(1-(rowSums(trimmed)>0))
    
    
    # First compute kernel weights for each X_t in X[R+1:T,]
    # Also repeat for the in-sample data X[1:R] for the bootstrap PEE calculation
    # We use the union of information sets (i.e. the combined X^j variables) to compute the kernel weights for the out-of-sample
    # We use just the relevant X_j variable to compute the weights for the in-sample
    
    # Set bandwidth using the rule h=K*sd(X)*P^(-1/3) for second-order kernel out-of-sample
    # Set bandwidth using the rule h=K*sd(X)*P^(-1/6) for fourth-order kernel out-of-sample
    # Replace P with R for bandwidth on the in-sample
    
    # N.B. This has been set up to deal with max 3 variables. Additional variables will require changes to bandwidth constants
    
    # Each column of Xweight gives the weight for all X_s relative to X_t for all s in [R+1:T]
    # This is used to weight the hit matrix which has columns for the hits of y_s relative to q_t for all s in [R+1:T]
    
    
    if (kernorder==2){
      
      # In-sample model j
      # Weights calculated based on X_j and not union of information sets
      if (length(xselect)==1){ kerncons=1 }
      if (length(xselect)==2){ kerncons=2 }
      if (length(xselect)==3){ kerncons=3 }
      if (length(xselect)==4){ kerncons=4 }
      bwm_R=apply(as.matrix(X[1:R,xselect]),2,function(x){kerncons*(R^(-1/3))*sd(x)})
      Xweight_R=npksum(txdat=X[1:R,xselect],exdat=X[1:R,xselect],bws=bwm_R,ckertype="epanechnikov",ckerorder=2,return.kernel.weights=TRUE)$kw
      
      # In-sample benchmark model 1
      # Weights calculated based on X_1 and not union of information sets
      if (length(xselectbench)==1){ kerncons=1 }
      if (length(xselectbench)==2){ kerncons=2 }
      if (length(xselectbench)==3){ kerncons=3 }
      bwm_Rb=apply(as.matrix(X[1:R,xselectbench]),2,function(x){kerncons*(R^(-1/3))*sd(x)})
      Xweight_Rb=npksum(txdat=X[1:R,xselectbench],exdat=X[1:R,xselectbench],bws=bwm_Rb,ckertype="epanechnikov",ckerorder=2,return.kernel.weights=TRUE)$kw
      
      
    }
    if (kernorder==4){
      
      # In-sample model j
      # Weights calculated based on X_j and not union of information sets
      if (length(xselect)==1){ kerncons=0.75 }
      if (length(xselect)==2){ kerncons=1.5  }
      if (length(xselect)==3){ kerncons=2.25  }
      bwm_R=apply(as.matrix(X[1:R,xselect]),2,function(x){kerncons*(R^(-1/6))*sd(x)})
      Xweight_R=npksum(txdat=X[1:R,xselect],exdat=X[1:R,xselect],bws=bwm_R,ckertype="epanechnikov",ckerorder=4,return.kernel.weights=TRUE)$kw
      
      # In-sample benchmark model 1
      # Weights calculated based on X_1 and not union of information sets
      if (length(xselectbench)==1){ kerncons=0.75 }
      if (length(xselectbench)==2){ kerncons=1.5  }
      if (length(xselectbench)==3){ kerncons=2.25  }
      bwm_Rb=apply(as.matrix(X[1:R,xselectbench]),2,function(x){kerncons*(R^(-1/6))*sd(x)})
      Xweight_Rb=npksum(txdat=X[1:R,xselectbench],exdat=X[1:R,xselectbench],bws=bwm_Rb,ckertype="epanechnikov",ckerorder=4,return.kernel.weights=TRUE)$kw
      
      
    }
    
    
    # Generate all quantile regression and coverage results for all tauvalues
    # We need to do this both for model j and also the benchmark model for each j
    # This is because the results are generated using the union of information sets X_1 and X_j
    
    # Run quantile regression of y on X_j and X_1 over first R observations as in the fixed estimation scheme
    reg=rq(as.matrix(y[1:R,])~as.matrix(X[1:R,xselect]),tau=tauvalues)
    regb=rq(as.matrix(y[1:R,])~as.matrix(X[1:R,xselectbench]),tau=tauvalues)
    
    
    # Run quantile regressions for tau levels perturbed by +-h for the computation of the density
    
    # Hall and Sheather (1988) bandwidth as suggested by Qu (2008)
    # Scaled by 0.5 in accordance with a low quantile level and sample size
    # so that e.g. P=120, tau=0.1 have h around 2% 
    h=0.5*(T^(-1/3)*(qnorm(1-tauvalues/2))^(2/3))*(1.5*dnorm(qnorm(tauvalues))^2/((2*qnorm(tauvalues))^2+1))^(1/3)
    
    reg1=rq(as.matrix(y[1:R,])~as.matrix(X[1:R,xselect]),tau=(tauvalues-h))
    reg2=rq(as.matrix(y[1:R,])~as.matrix(X[1:R,xselect]),tau=(tauvalues+h))
    reg1b=rq(as.matrix(y[1:R,])~as.matrix(X[1:R,xselectbench]),tau=(tauvalues-h))
    reg2b=rq(as.matrix(y[1:R,])~as.matrix(X[1:R,xselectbench]),tau=(tauvalues+h))
    
    
    for (taucount in 1:kTau){
      
      tau=tauvalues[taucount]
      
      # Save predictions out-of-sample and in-sample for model j and benchmark
      # Also obtain results with tau values perturbed by +-h

      qhat_R=cbind(matrix(1,R,1),as.matrix(X[1:R,xselect]))%*%matrix(as.matrix(reg$coefficients)[,taucount])
      qhat1_R=cbind(matrix(1,R,1),as.matrix(X[1:R,xselect]))%*%matrix(as.matrix(reg1$coefficients)[,taucount])
      qhat2_R=cbind(matrix(1,R,1),as.matrix(X[1:R,xselect]))%*%matrix(as.matrix(reg2$coefficients)[,taucount])
      
      qhat_Rb=cbind(matrix(1,R,1),as.matrix(X[1:R,xselectbench]))%*%matrix(as.matrix(regb$coefficients)[,taucount])
      qhat1_Rb=cbind(matrix(1,R,1),as.matrix(X[1:R,xselectbench]))%*%matrix(as.matrix(reg1b$coefficients)[,taucount])
      qhat2_Rb=cbind(matrix(1,R,1),as.matrix(X[1:R,xselectbench]))%*%matrix(as.matrix(reg2b$coefficients)[,taucount])
      
      
      # Generate local "hits" for each value of q(X_t) in the sample from R+1:T
      
      # Repeat for the in-sample results using tau perturbed by +-h
      hit1_R=sapply(1:R,function(x){matrix(y[1:R,]<qhat1_R[x,])})
      hit2_R=sapply(1:R,function(x){matrix(y[1:R,]<qhat2_R[x,])})
      C1_R=colSums(hit1_R*Xweight_R)/(colSums(Xweight_R)+0.00001)
      C2_R=colSums(hit2_R*Xweight_R)/(colSums(Xweight_R)+0.00001)
      
      hit1_Rb=sapply(1:R,function(x){matrix(y[1:R,]<qhat1_Rb[x,])})
      hit2_Rb=sapply(1:R,function(x){matrix(y[1:R,]<qhat2_Rb[x,])})
      C1_Rb=colSums(hit1_Rb*Xweight_Rb)/(colSums(Xweight_Rb)+0.00001)
      C2_Rb=colSums(hit2_Rb*Xweight_Rb)/(colSums(Xweight_Rb)+0.00001)
      
      
      # For higher-order kernels, ensure that C estimates are in [0,1]
      C1_R[which(C1_R<0)]=0; C1_R[which(C1_R>1)]=1 ; C2_R[which(C2_R<0)]=0; C2_R[which(C2_R>1)]=1
      
      C1_Rb[which(C1_Rb<0)]=0; C1_Rb[which(C1_Rb>1)]=1 ; C2_Rb[which(C2_Rb<0)]=0; C2_Rb[which(C2_Rb>1)]=1
      
      # Compute terms required for the parameter estimation part of the bootstrap
      
      # First generate the qt-yt hits for the in-sample period 1:R
      # The out-of-sample hits R+1:T are already stored in diag(hit)
      hit_R=y[1:R,]<qhat_R
      hit_Rb=y[1:R,]<qhat_Rb
      
      # Generate the estimate of the density out-of-sample and in-sample
      f_R=(C2_R-C1_R)/(qhat2_R-qhat1_R+0.00001)
      
      f_Rb=(C2_Rb-C1_Rb)/(qhat2_Rb-qhat1_Rb+0.00001)
      
      
      
      # Loss Calculation for Test Statistic and bootstrap statistic
      
      # Compute coverage errors for model j and benchmark
      eps=C1_R-tau
      epsb=C1_Rb-tau
      # Compute differential of quadratic loss between benchmark and model j
      Loss=epsb^2-eps^2
      # Apply trimming
      Loss=keep*Loss
      
      # Include the recentering term. This is called Ahat in the paper
      A=(epsb^2-mean(epsb^2))-(eps^2-mean(eps^2))
      # Apply trimming
      A=keep*A
      
      
      
      # Loss Calculation for the terms in the bootstrap statistic
      
      # Generate the second part of the bootstrap statistic
      # This is called Bhat in the paper
      B=(2*epsb)*(diag(hit_Rb)-C1_Rb) - (2*eps)*(diag(hit_R)-C1_R)  
      # Apply trimming
      B=keep*B
      
      # Generate the third part of the bootstrap statistic (PEE)
      # This is called Dhat in the paper
      
      # First compute mu, this uses (trimmed) X and the density estimate f over R+1:T 
      mu=as.matrix(colMeans((keep%*%matrix(1,1,(1+length(xselect))))*cbind(matrix(1,R,1),X[(1):T,xselect])*matrix((2*eps)*f_R)%*%(matrix(1,1,(1+length(xselect))))))
      mub=as.matrix(colMeans((keep%*%matrix(1,1,(1+length(xselectbench))))*cbind(matrix(1,R,1),X[(1):T,xselectbench])*matrix((2*epsb)*f_Rb)%*%(matrix(1,1,(1+length(xselectbench))))))
      
      
      # Next compute H, this uses (non-trimmed) X and the density estimate f over 1:R
      H=(1/R)*t(cbind(matrix(1,R,1),as.matrix(X[1:R,xselect])))%*%diag(as.vector(f_R))%*%cbind(matrix(1,R,1),as.matrix(X[1:R,xselect]))
      Hb=(1/R)*t(cbind(matrix(1,R,1),as.matrix(X[1:R,xselectbench])))%*%diag(as.vector(f_Rb))%*%cbind(matrix(1,R,1),as.matrix(X[1:R,xselectbench]))
      
      # Add tolerance to H if it is close to singular
      if (det(H)<0.00001){H=H+diag(0.1,nrow=nrow(H))}
      if (det(Hb)<0.00001){Hb=Hb+diag(0.1,nrow=nrow(Hb))}
      
      
      # Finally compute the PEE term using X and hits from [1:R] as well as H and mu
      D=(cbind(matrix(1,R,1),as.matrix(X[1:R,xselectbench]))*matrix(hit_Rb-tau)%*%matrix(1,1,1+length(xselectbench)))%*%solve(Hb)%*%mub-(cbind(matrix(1,R,1),as.matrix(X[1:R,xselect]))*matrix(hit_R-tau)%*%matrix(1,1,1+length(xselect)))%*%solve(H)%*%mu
      
      
      # Save results
      Losslist[,((modelcount-2)*kTau+taucount)]=Loss
      Alist[,((modelcount-2)*kTau+taucount)]=A
      Blist[,((modelcount-2)*kTau+taucount)]=B
      Dlist[,((modelcount-2)*kTau+taucount)]=D
      
      # taucount loop end
    }
    
    
    
    
    # modelcount loop end
  }
  
  
  # Compute test statistic for equal conditional coverage error loss for each tau and j
  SP=sqrt(T)*colMeans(Losslist)
  
  # Compute max test statistic
  SPmax=sum(((SP>0)*SP)^2)
  
  # Compute slackness indicator from SP for the GMS in the wild bootstrap
  kappa=log(T)
  
  # Compute v using Newey-West variance covariance estimate
  # P*lrvar used in the calculation of HAC standard errors as lrvar reports the var-cov for the mean
  # We can ignore covariances with PEE as we are using the fixed estimation scheme, so only cov(A,B) is required
  # Therefore we need var(A)+var(B)+var(D)+2*cov(A,B) only
  # Set max lag to be the integer part of T^(1/4) to prevent very large lag values being used in some simulation replications by the default bwNeweyWest
  v=diag(T*lrvar(Alist,type="Newey-West",prewhite = FALSE,lag=round(T^(1/4))))+
    diag(T*lrvar(Blist,type="Newey-West",prewhite = FALSE,lag=round(T^(1/4))))+
    diag(R*lrvar(Dlist,type="Newey-West",prewhite = FALSE,lag=round(R^(1/4))))+
    2*diag(T*lrvar(cbind(Alist,Blist),type="Newey-West",prewhite = FALSE,lag=round(T^(1/4)))[(kTau*(kmodel-1)+1):(2*kTau*(kmodel-1)),1:(kTau*(kmodel-1))])
  
  
  removeslack=as.numeric(SP>(-kappa*sqrt(v)))
  
  
  # Compute wild bootstrap statistics for equal conditional coverage error loss
  for (s in 1:S){
    
    for (lcount in 1:length(lvalues)){
      
      l=lvalues[lcount]
      
      # Generate P-l+1 draws of N(0,1/l) for eps and R-l+1 draws for eta
      eps=matrix(rnorm((T-l+1),0,sqrt(1/l)))
      eta=matrix(rnorm((R-l+1),0,sqrt(1/l)))
      
      # Generate matrix to do overlapping wild boot 
      # Suppress warnings as we fill up a matrix with non-matching number of elements
      suppressWarnings({
        mult=cbind(matrix(rep(eps,each=l),ncol=l,byrow=TRUE),matrix(0,(T-l+1),(T-l+1)))
        mult=t(matrix(t(mult),nrow=T,ncol=(T-l+1)))
        multPEE=cbind(matrix(rep(eta,each=l),ncol=l,byrow=TRUE),matrix(0,(R-l+1),(R-l+1)))
        multPEE=t(matrix(t(multPEE),nrow=R,ncol=(R-l+1)))
      })
      
      # Generate test statistic using mult and multPEE
      #SPboot[[lcount]][s,]=sqrt(T)*(colMeans(mult%*%(Alist+Blist))+colMeans(multPEE%*%Dlist))
      SPboot[[lcount]][s,]=sqrt(T)*(colMeans(mult%*%(Alist+Blist+Dlist))) # P=T According to Valentina Corradi
      
      
      # Compute max test statistic
      SPmaxboot[[lcount]][s,]=sum(((SPboot[[lcount]][s,]*removeslack>0)*(SPboot[[lcount]][s,]*removeslack))^2)
      
      
      # lcount loop end   
    }
    
    
    
    
    if (S>10){
      if ((s %in% seq(10,10*floor(S/10),10))==TRUE){
        cat(s)
      } else{
        cat('.')
      }
    }
    
    
    
    # s loop end
  }
  
  
  
  
  if (kmodel==2 & kTau==1){
    out=list(SP=SP, SPboot=SPboot)  
  } else{
    out=list(SP=SP, SPboot=SPboot, SPmax=SPmax, SPmaxboot=SPmaxboot)  
  }
  
  
  
  
}
