library(quantreg) #quant reg
library(sn) #skew-t
library(tidyverse)

qreg_inf_best<-function(ind,Y,country,groups,h,alpha=0.10,
                   tau = c(0.05,0.25,0.50,0.75,0.95),...){
  
  #data preparation
  # Y=mat_h
  # ind=indmat[1,]
  # country=112
  # spec="m1"
  # model=3
  # tau = c(0.05,0.25,0.50,0.75,0.95)
  yin=Y[ind,country] # for estimation
  Xin=dplyr::lag(Y,h)
  Xin=Xin[ind,country] # for estimation
  Xout<-Y[ind[length(ind)],country] # for forecast
  yout<-Y[ind[length(ind)]+h,country] # for evaluation
  
  #X=R2$Factors_Inflation # Factors
  X <- ThreeMDFM1(Y[ind,],groups,spec=1)$Factors_Inflation
  X2in=dplyr::lag(as.matrix(X),h) # for estimation
  X2out<-X[nrow(X),] # for forecast check if works for h>1!!!!
  X2out<-X2out[seq(1,length(X2out))*c(1,unlist(groups[country,-1]))]
  Xout<-c(Xout,X2out)
    
  ### Estimate regression 
  XX<-data.frame(cbind("y"=yin,"X"=cbind(Xin,X2in))) 
  XX<-XX[,seq(1,ncol(XX))*c(1,1,1,unlist(groups[country,-1]))] # Select right factors
  XX<-rbind(XX,c("y"=yout,"X"=t(Xout)))
  XX<-XX[(-h:-1),] # Due to h
  #XX<-XX %>% as.tibble() mutate_all(.,scale())
  yy<-c()
  beta_list<-list()
  residuals_list<-list()
  for (tt in tau){
  eq1 = rq(y~.,data = XX[1:(nrow(XX)-1),],tau = tt) # estimate at t
  vc1 <- summary.rq(eq1,se = "ker",covariance = T)
  Isig<-c(T,vc1$coefficients[2:5,4]<alpha)
  XX2<-as.data.frame(XX[,Isig])
  if (ncol(XX2)>1) eq1 <- rq(y~.,data = XX2[1:(nrow(XX2)-1),],tau = tt)
  if (ncol(XX2)==1){
    ycons<-as.data.frame(XX2[1:(nrow(XX2)-1),])
    colnames(ycons)<-"y"
    eq1 <- rq(y~1,data = ycons,tau = tt)
  }
  beta<-eq1$coefficients
  residuals<-eq1$residuals
  ### forecast regression
  if (ncol(XX2)>1) forecast=beta%*%c(1,as.matrix(XX2[nrow(XX2),-1]))
  if (ncol(XX2)==1) forecast=beta%*%c(1)
  yy<-c(yy,forecast)
  beta_list[[paste0(tt)]]<-beta
  residuals_list[[paste0(tt)]]<-residuals
  }
  #Chernozhukov, V., I. Fernandez-Val, and A. Galichon, (2006) 
  #Quantile and Probability Curves without Crossing, Econometrica, forthcoming.
  yy<-sort(yy)
  #x_range<-seq(-20,250,by=0.05)
  # Initial values
  iqn=qnorm(0.75)-qnorm(0.25)
  l0=yy[3]  # LOCATION
  s0=(yy[4] - yy[2]) / iqn; # SCALE
  sh0=0 # Shape
  #v0=1 # DF
  #LB=c(-30,0,-30,1) # SIMILAR TO ADRIAN
  #UB=c(500,50,30,30)
  LB = c(   -10+l0,     1,   -100); #Omega must positive >=1
  UB = c(   +20+l0,    50,    100);
  
  
  skewt<-optim(c(l0, s0, sh0),fn=function(x){
    sum((as.numeric(yy)-qst(tau,xi=x[1],omega=x[2],alpha=x[3]))^2)
  }, lower=LB,upper=UB,  method="L-BFGS-B")
  
  #density <-dst(x_range,xi=skewt$par[1],omega=skewt$par[2],
  #                   alpha=skewt$par[3],nu=skewt$par[4])
  
  skt_q<-qst(tau,xi=skewt$par[1],omega=skewt$par[2],
             alpha=skewt$par[3])
  qs<-qs(yout,yy,alpha=tau) # QS scores *2
  crps_equal<-crps_qw(yout,yy,w,alpha = tau) #center
  crps_center<-crps_qw(yout,yy,w_c,alpha = tau) #center
  crps_tails<-crps_qw(yout,yy,w_t,alpha = tau) #tails
  crps_right<-crps_qw(yout,yy,w_r,alpha = tau) #right
  crps_left<-crps_qw(yout,yy,w_l, alpha = tau) #left
  
  
  prob<-pst(c(0,3),xi=skewt$par[1],omega=skewt$par[2],
            alpha=skewt$par[3],log=F)
  
  prob[2]<-1-prob[2] # Prob[2] inf>3% and Prob[1]<0% (convention<=)
  
  
  return(list(beta=beta_list,residuals=residuals_list,forecast_quantile=forecast,
              forecast_skt=skt_q,qs=qs,crps_equal = crps_equal,
              crps_center=crps_center,
              crps_tails=crps_tails,crps_right=crps_right,
              crps_left=crps_left, prob = prob))
}



qreg_inf<-function(ind,Y,spec="m1",country,groups,h,mod=1,
                      tau = c(0.05,0.25,0.50,0.75,0.95),...){

  #data preparation
  # Y=mat_h
  # ind=indmat[1,]
  # country=112
  # spec="m1"
  # model=3
  # tau = c(0.05,0.25,0.50,0.75,0.95)
  yin=Y[ind,country] # for estimation
  Xin=dplyr::lag(Y,h)
  Xin=Xin[ind,country] # for estimation
  Xout<-Y[ind[length(ind)],country] # for forecast
  yout<-Y[ind[length(ind)]+h,country] # for evaluation
  
  if (spec=="m1"){
    #X=R2$Factors_Inflation # Factors
    X <- ThreeMDFM1(Y[ind,],groups,spec=1)$Factors_Inflation
    X2in=dplyr::lag(as.matrix(X),h) # for estimation
    X2out<-X[nrow(X),] # for forecast check if works for h>1!!!!
    X2out<-X2out[seq(1,length(X2out))*c(1,unlist(groups[country,-1]))]
    Xout<-c(Xout,X2out)
    
    ### Estimate regression 
    XX<-data.frame(cbind("y"=yin,"X"=cbind(Xin,X2in))) 
    XX<-XX[,seq(1,ncol(XX))*c(1,1,1,unlist(groups[country,-1]))] # Select right factors
    XX<-rbind(XX,c("y"=yout,"X"=t(Xout)))
    XX<-XX[(-h:-1),] # Due to h
    #XX<-XX %>% as.tibble() mutate_all(.,scale())
    
    # Select 
    if (mod==2) XX<-XX[,c(T,T,T,T,F)] # Due to h
    if (mod==3) XX<-XX[,c(T,T,T,F,T)] # Due to h
    if (mod==4) XX<-XX[,c(T,T,T,F,F)] # Due to h
    
    eq1=rq(y~.,data = XX[1:(nrow(XX)-1),],tau = tau) # estimate at t 
  }else if (spec=="bench"){
    
  ### Estimate regression 
  XX<-data.frame(cbind("y"=yin,"X"=Xin)) 
  XX<-rbind(XX,c("y"=yout,"X"=t(Xout)))
  XX<-XX[(-h:-1),] # Due to h
  #XX<-XX %>% as.tibble() mutate_all(.,scale())
  eq1=rq(y~.,data = XX[1:(nrow(XX)-1),],tau = tau) # estimate at t 
  }
  beta<-eq1$coefficients
  residuals<-eq1$residuals
  ### forecast regression
  #fp=predict(eq1,newdata=na.omit(XX), stepfun = TRUE)
  #a=rearrange(fp) # non crossing quantiles
  #forecast=environment(a[[paste0(length(yin)+1)]])[["y"]][c(-1,-7)]
  
  #Chernozhukov, V., I. Fernandez-Val, and A. Galichon, (2006) 
  #Quantile and Probability Curves without Crossing, Econometrica, forthcoming.
  forecast<-t(beta) %*% matrix(c(1,as.matrix(XX[nrow(XX),-1])))
  forecast<-sort(forecast)
  yy<-forecast
  #x_range<-seq(-20,250,by=0.05)
  # Initial values
  iqn=qnorm(0.75)-qnorm(0.25)
  l0=yy[3]  # LOCATION
  s0=(yy[4] - yy[2]) / iqn; # SCALE
  sh0=0 # Shape
  #v0=1 # DF
  #LB=c(-30,0,-30,1) # SIMILAR TO ADRIAN
  #UB=c(500,50,30,30)
  LB = c(   -10+l0,     1,   -100); #Omega must positive >=1
  UB = c(   +20+l0,    50,    100);
  

  skewt<-optim(c(l0, s0, sh0),fn=function(x){
    sum((as.numeric(yy)-qst(tau,xi=x[1],omega=x[2],alpha=x[3]))^2)
  }, lower=LB,upper=UB,  method="L-BFGS-B")
  
  #density <-dst(x_range,xi=skewt$par[1],omega=skewt$par[2],
  #                   alpha=skewt$par[3],nu=skewt$par[4])
  
  skt_q<-qst(tau,xi=skewt$par[1],omega=skewt$par[2],
             alpha=skewt$par[3])
  qs<-qs(yout,yy,alpha=tau) # QS scores *2
  crps_equal<-crps_qw(yout,yy,w,alpha = tau) #center
  crps_center<-crps_qw(yout,yy,w_c,alpha = tau) #center
  crps_tails<-crps_qw(yout,yy,w_t,alpha = tau) #tails
  crps_right<-crps_qw(yout,yy,w_r,alpha = tau) #right
  crps_left<-crps_qw(yout,yy,w_l, alpha = tau) #left
  
  
  # prob<-pst(c(0,3),xi=skewt$par[1],omega=skewt$par[2],
  #                    alpha=skewt$par[3],log=F)
  # 
  # prob[2]<-1-prob[2] # Prob[2] inf>3% and Prob[1]<0% (convention<=)
  # 
  
  prob<-pst(c(0,3,1,2),xi=skewt$par[1],omega=skewt$par[2],
            alpha=skewt$par[3],log=F)
  
  prob[2]<-1-prob[2] # Prob[2] inf>3% and Prob[1]<0% (convention<=)
  prob[3]<-1-prob[3] # Prob[3] inf>1% 
  prob[4]<-1-prob[4] # Prob[4] inf>2%
  
  prob_pit<-pst(yout,xi=skewt$par[1],omega=skewt$par[2],
                alpha=skewt$par[3],log=F)
  
  return(list(beta=beta,residuals=residuals,forecast_quantile=forecast,
              forecast_skt=skt_q,qs=qs,crps_equal = crps_equal,
              crps_center=crps_center,
              crps_tails=crps_tails,crps_right=crps_right,
              crps_left=crps_left, prob = prob, prob_pit= prob_pit,
              X=XX))
}


qreg_inf_uncon<-function(ind,Y,country,groups,h,
                   tau = c(0.05,0.25,0.50,0.75,0.95),...){
  yin=data.frame(Y[ind,country]) # for estimation
  colnames(yin)=c("y")
  yout<-Y[ind[length(ind)]+h,country] # for evaluation
  ### Estimate regression 
  eq1=rq(y~1,data = yin,tau = tau) # estimate at t 

  beta<-eq1$coefficients
  residuals<-eq1$residuals
  ### forecast regression
  forecast=predict(eq1)[nrow(yin),]
  yy<-forecast
  #x_range<-seq(-20,250,by=0.05)
  # Initial values
  iqn=qnorm(0.75)-qnorm(0.25)
  l0=yy[3]  # LOCATION
  s0=(yy[4] - yy[2]) / iqn; # SCALE
  sh0=0 # Shape
  #v0=1 # DF
  #LB=c(-30,0,-30,1) # SIMILAR TO ADRIAN
  #UB=c(500,50,30,30)
  LB = c(   -10+l0,     1,   -100); #Omega must positive >=1
  UB = c(   +20+l0,    50,    100);
  
  
  skewt<-optim(c(l0, s0, sh0),fn=function(x){
    sum((as.numeric(yy)-qst(tau,xi=x[1],omega=x[2],alpha=x[3]))^2)
  }, lower=LB,upper=UB,  method="L-BFGS-B")
  
  #density <-dst(x_range,xi=skewt$par[1],omega=skewt$par[2],
  #                   alpha=skewt$par[3],nu=skewt$par[4])
  
  skt_q<-qst(tau,xi=skewt$par[1],omega=skewt$par[2],
             alpha=skewt$par[3])
  qs<-qs(yout,yy,alpha=tau) # QS scores *2
  crps_equal<-crps_qw(yout,yy,w,alpha = tau) #center
  crps_center<-crps_qw(yout,yy,w_c,alpha = tau) #center
  crps_tails<-crps_qw(yout,yy,w_t,alpha = tau) #tails
  crps_right<-crps_qw(yout,yy,w_r,alpha = tau) #right
  crps_left<-crps_qw(yout,yy,w_l, alpha = tau) #left
  
  
  prob<-pst(c(0,3),xi=skewt$par[1],omega=skewt$par[2],
            alpha=skewt$par[3],log=F)
  
  prob[2]<-1-prob[2] # Prob[2] inf>3% and Prob[1]<0% (convention<=)
  
  
  prob_pit<-pst(yout,xi=skewt$par[1],omega=skewt$par[2],
                alpha=skewt$par[3],log=F)
  
  
  return(list(beta=beta,residuals=residuals,forecast_quantile=forecast,
              forecast_skt=skt_q,qs=qs,crps_equal = crps_equal,
              crps_center=crps_center,
              crps_tails=crps_tails,crps_right=crps_right,
              crps_left=crps_left, prob = prob,prob_pit=prob_pit))
}
    # 
    # Y=mat_h
    # X=R1$Factors_Inflation
    # country=112 # US
    # h=1
    # tau = c(0.05,0.25,0.50,0.75,0.95)
  qreg_inf_insample<-function(Y,X,country,h,
                   tau = c(0.05,0.25,0.50,0.75,0.95),Sigma_F){
  #data preparation
  #Y=mat_h
  yin=Y[,country] # for estimation
  Xin=dplyr::lag(Y,n=h)
  Xin=Xin[,country] # for estimation

  #X=R1$Factors_Inflation # Factors
  X2in=dplyr::lag(X,n=h) # for estimation
  
  ### Estimate regression 
  XX<-data.frame(cbind("y"=yin,"X"=cbind(Xin,X2in))) 
  XX<-XX[,seq(1,ncol(XX))*c(1,1,1,unlist(groups[country,-1]))] # Select right factors
  XX<-XX[(-h:-1),] # Due to h
  
  colnames(XX)<-c("y","Ly","FGlobal","FRegional","FIncome")
  # betas
  eq1=rq(y~.,data = XX,tau = tau) # estimate at t
  beta<-eq1$coefficients
  
  #################################################################
  ##               In-sample evaluation statistics               ##
  #################################################################
  
  # residuals
  residuals<-eq1$residuals
  # tt=tau[1]
  #pvalue of coefficients and R2 of regression
  rho <- function(u,tau=.5)u*(tau - (u < 0)) # Koenker and Machado 1999
  pvalue=c()
  R2=c()
  Fest=c()
  Fpval=c()
  AICar1=c()
  BICar1=c()
  AICf=c()
  BICf=c()
  ICf=c() 
  ICar1=c()
  for (tt in tau){
    qqar1 <- rq(y~.,data = XX[,1:2],tau = tt)
    qq0 <- rq(y~1,data = XX,tau = tt)
    qq1 <- rq(y~.,data = XX,tau = tt)
    Xi <- density(qq1$fitted.values,n = 286)$y
    #Xi <- predict(qq1,type="fhat") #DENSITY
    vc1 <-  summary.rq(qq1,se = "ker",covariance = T)
    #################################################
    #################################################
    #vc1 <- summary.rq(qq1,se = "iid") #KB(1978) iid errors (Gonzales-Rivera et al.,2024).
    pvalue <- round(rbind(pvalue,vc1$coefficients[,4]),4)
    # V1 <- sum(rho(qq1$resid, qq1$tau)) es igual a qq1$rho
    # V0 <- sum(rho(qq0$resid, qq0$tau)) es igual a qq10$rho
    R2 <- c(R2,(1- qq1$rho/qq0$rho)*100)
    LLar1=logLik(qqar1)
    AICar1=c(AICar1,AIC(qqar1)[1])
    BICar1=c(BICar1,-2*LLar1[1]+length(qqar1$coefficients)*log(length(qqar1$y)))
    LL1=logLik(qq1)
    AICf=c(AICf,AIC(qq1)[1])
    BICf=c(BICf,-2*LL1[1]+length(qq1$coefficients)*log(length(qq1$y)))
    Fest=c(Fest,anova.rq(qq1,qqar1,se = "ker")$table$Tn)
    Fpval=c(Fpval,anova.rq(qq1,qqar1,se = "ker")$table$pvalue)
    
    ####################### Ando and Tsay (2011) IC QF#########################
    gamma=qq1$coefficients
    N=115
    Sigma_Ff = Sigma_F[,seq(1,ncol(Sigma_F))*c(1,unlist(groups[country,-1]))]
    Sigma_Ff = Sigma_Ff[seq(1,nrow(Sigma_F))*c(1,unlist(groups[country,-1])),]
    Sigma_Ff = rbind(rep(0,ncol(Sigma_Ff)),Sigma_Ff)
    Sigma_Ff = cbind(rep(0,nrow(Sigma_Ff)),Sigma_Ff)
    Sigma_Ff[1,1] = gamma[1]*gamma[1]
    Z = as.matrix(XX[,-1])
    Tt=nrow(Z)
    I_tau = (1/Tt)*(tt*(1-tt))*(t(Z) %*% Z)
    M = diag(Xi)
    J_tau = (1/Tt)*(t(Z) %*% M %*% Z)
    K_tau = gamma[-1] %*% t(gamma[-1]) 
    b_hat = c() 
      for (i in 1:length(Xi)) {
        b_hat <- sum(c(b_hat,Xi[i] * diag(K_tau %*% Sigma_Ff)))
      }
    b_hat = (1/Tt)*(sum(diag(solve(J_tau) %*% I_tau)))+(1/(Tt*N))*b_hat
    ICf = c(ICf, -2 *LL1 +2*Tt*b_hat)
    
    ####################### Ando and Tsay (2011) IC QAR1########################
    gamma=qqar1$coefficients
    N=115
    Z = as.matrix(XX[,2])
    Tt=nrow(Z)
    I_tau = (1/Tt)*(tt*(1-tt))*(t(Z) %*% Z)
    M = diag(Xi)
    J_tau = (1/Tt)*(t(Z) %*% M %*% Z)
    b_hat = (1/Tt)*(sum(diag(solve(J_tau) %*% I_tau)))
    ICar1 = c(ICar1, -2 *LLar1 +2*Tt*b_hat)
  }
  names(R2)<-tau
  R2<-t(as.matrix(R2))
  
  ### forecast regression
  fp=predict(eq1,newdata=na.omit(XX), stepfun = TRUE)
  a=rearrange(fp) # non crossing quantiles
  #Chernozhukov, V., I. Fernandez-Val, and A. Galichon, (2006) 
  #Quantile and Probability Curves without Crossing, Econometrica, forthcoming.
  
  forecast=c()
  for(i in 1:(length(a))){
    forecast=rbind(forecast,environment(a[[paste0(1+i)]])[["y"]][c(-1,-7)])
  }
  
  # plot(forecast[,1],t="l",ylim=c(-20,45),col="green")
  # lines(forecast[,3],t="l")
  # lines(forecast[,5],t="l",col="red")
  # 
  
  table_qs=c()
  table_crps_equal = c()
  table_crps_center = c()
  table_crps_tails = c()
  table_crps_right = c()
  table_crps_left = c()
  table_prob = c()
  table_forecast_skt=c()
  table_prob_pit=c()
  
  for (i in c(1:length(forecast[,1]))) {
    
  yy<-forecast[i,]
  yout<-XX[i,1]
  #x_range<-seq(-20,250,by=0.05)
  # Initial values
  iqn=qnorm(0.75)-qnorm(0.25)
  l0=yy[3]  # LOCATION
  s0=(yy[4] - yy[2]) / iqn; # SCALE
  sh0=0 # Shape
  #v0=1 # DF
  #LB=c(-30,0,-30,1) # SIMILAR TO ADRIAN
  #UB=c(500,50,30,30)
  LB = c(   -10+l0,     1,   -100); #Omega must positive >=1
  UB = c(   +20+l0,    50,    100);
  
  
  skewt<-optim(c(l0, s0, sh0),fn=function(x){
    sum((as.numeric(yy)-qst(tau,xi=x[1],omega=x[2],alpha=x[3]))^2)
  }, lower=LB,upper=UB,  method="L-BFGS-B")
  
  
  skt_q<-qst(tau,xi=skewt$par[1],omega=skewt$par[2],
             alpha=skewt$par[3])
  qs<-qs(yout,skt_q,alpha=tau) # QS scores *2
  crps_equal<-crps_qw(yout,skt_q,w,alpha = tau) #center
  crps_center<-crps_qw(yout,skt_q,w_c,alpha = tau) #center
  crps_tails<-crps_qw(yout,skt_q,w_t,alpha = tau) #tails
  crps_right<-crps_qw(yout,skt_q,w_r,alpha = tau) #right
  crps_left<-crps_qw(yout,skt_q,w_l, alpha = tau) #left
  
  table_forecast_skt<-rbind(table_forecast_skt,skt_q)
  prob<-pst(c(0,3,1,2),xi=skewt$par[1],omega=skewt$par[2],
            alpha=skewt$par[3],log=F)
  
  prob[2]<-1-prob[2] # Prob[2] inf>3% and Prob[1]<0% (convention<=)
  prob[3]<-1-prob[3] # Prob[3] inf>1% 
  prob[4]<-1-prob[4] # Prob[4] inf>2%
  
  prob_pit<-pst(yout,xi=skewt$par[1],omega=skewt$par[2],
                alpha=skewt$par[3],log=F)
  
  
  table_qs=rbind(table_qs,qs)
  table_crps_equal = rbind(table_crps_equal,crps_equal)
  table_crps_center=rbind(table_crps_center,crps_center)
  table_crps_tails=rbind(table_crps_tails,crps_tails)
  table_crps_right=rbind(table_crps_right,crps_right)
  table_crps_left=rbind(table_crps_left,crps_left)
  table_prob = rbind(table_prob,prob)
  table_prob_pit = rbind(table_prob_pit,prob_pit)
  
  }
  
  return(list(beta=beta,residuals=residuals,pvalue=pvalue,R=R2,
              forecast_quantiles=forecast,
              forecast_skt=table_forecast_skt,yin=XX[,1],qs=table_qs,crps_equal = table_crps_equal,
              crps_center=table_crps_center,X=XX,
              crps_tails=table_crps_tails,crps_right=table_crps_right,
              crps_left=table_crps_left, prob = table_prob,prob_pit=table_prob_pit,
              Fest=Fest,
              Fpval=Fpval,
              AICar1=AICar1,
              BICar1=BICar1,
              AICf=AICf,
              BICf=BICf,
              ICf=ICf,
              ICar1=ICar1))
}

  
  
  qreg_inf_insample_ar<-function(Y,country,h,
                              tau = c(0.05,0.25,0.50,0.75,0.95),...){
    #data preparation
    #Y=mat_h
    yin=Y[,country] # for estimation
    Xin=dplyr::lag(Y,n=h)
    Xin=Xin[,country] # for estimation
    

    
    ### Estimate regression 
    XX<-data.frame(cbind("y"=yin,"X"=cbind(Xin))) 
    XX<-XX[(-h:-1),] # Due to h
    
    colnames(XX)<-c("y","Ly")
    # betas
    eq1=rq(y~.,data = XX,tau = tau) # estimate at t
    beta<-eq1$coefficients
    
    # residuals
    residuals<-eq1$residuals  
    
    #pvalue of coefficients and R2 of regression
    rho <- function(u,tau=.5)u*(tau - (u < 0)) # Koenker and Machado 1999
    pvalue=c()
    R2=c()
    for (tt in tau){
      qq0 <- rq(y~1,data = XX,tau = tt)
      qq1 <- rq(y~.,data = XX,tau = tt)
      vc1 <- summary.rq(qq1,se = "iid") #KB(1978) iid errors (Gonzales-Rivera et al.,2024).
      pvalue <- round(rbind(pvalue,vc1$coefficients[,4]),4)
      # V1 <- sum(rho(qq1$resid, qq1$tau)) es igual a qq1$rho
      # V0 <- sum(rho(qq0$resid, qq0$tau)) es igual a qq10$rho
      R2 <- c(R2,(1- qq1$rho/qq0$rho)*100)
    }
    names(R2)<-tau
    R2<-t(as.matrix(R2))
    
    ### forecast regression
    fp=predict(eq1,newdata=na.omit(XX), stepfun = TRUE)
    a=rearrange(fp) # non crossing quantiles
    #Chernozhukov, V., I. Fernandez-Val, and A. Galichon, (2006) 
    #Quantile and Probability Curves without Crossing, Econometrica, forthcoming.
    
    forecast=c()
    for(i in 1:(length(a))){
      forecast=rbind(forecast,environment(a[[paste0(1+i)]])[["y"]][c(-1,-7)])
    }
    
    # plot(forecast[,1],t="l",ylim=c(-20,45),col="green")
    # lines(forecast[,3],t="l")
    # lines(forecast[,5],t="l",col="red")
    # 
    
    table_qs=c()
    table_crps_equal = c()
    table_crps_center = c()
    table_crps_tails = c()
    table_crps_right = c()
    table_crps_left = c()
    table_prob = c()
    table_forecast_skt=c()
    
    for (i in c(1:length(forecast[,1]))) {
      
      yy<-forecast[i,]
      yout<-XX[i,1]
      #x_range<-seq(-20,250,by=0.05)
      # Initial values
      iqn=qnorm(0.75)-qnorm(0.25)
      l0=yy[3]  # LOCATION
      s0=(yy[4] - yy[2]) / iqn; # SCALE
      sh0=0 # Shape
      #v0=1 # DF
      #LB=c(-30,0,-30,1) # SIMILAR TO ADRIAN
      #UB=c(500,50,30,30)
      LB = c(   -10+l0,     1,   -100); #Omega must positive >=1
      UB = c(   +20+l0,    50,    100);
      
      
      skewt<-optim(c(l0, s0, sh0),fn=function(x){
        sum((as.numeric(yy)-qst(tau,xi=x[1],omega=x[2],alpha=x[3]))^2)
      }, lower=LB,upper=UB,  method="L-BFGS-B")
      
      
      skt_q<-qst(tau,xi=skewt$par[1],omega=skewt$par[2],
                 alpha=skewt$par[3])
      qs<-qs(yout,skt_q,alpha=tau) # QS scores *2
      crps_equal<-crps_qw(yout,skt_q,w,alpha = tau) #center
      crps_center<-crps_qw(yout,skt_q,w_c,alpha = tau) #center
      crps_tails<-crps_qw(yout,skt_q,w_t,alpha = tau) #tails
      crps_right<-crps_qw(yout,skt_q,w_r,alpha = tau) #right
      crps_left<-crps_qw(yout,skt_q,w_l, alpha = tau) #left
      
      table_forecast_skt<-rbind(table_forecast_skt,skt_q)
      prob<-pst(c(0,3),xi=skewt$par[1],omega=skewt$par[2],
                alpha=skewt$par[3],log=F)
      
      prob[2]<-1-prob[2] # Prob[2] inf>3% and Prob[1]<0% (convention<=)
      
      
      table_qs=rbind(table_qs,qs)
      table_crps_equal = rbind(table_crps_equal,crps_equal)
      table_crps_center=rbind(table_crps_center,crps_center)
      table_crps_tails=rbind(table_crps_tails,crps_tails)
      table_crps_right=rbind(table_crps_right,crps_right)
      table_crps_left=rbind(table_crps_left,crps_left)
      table_prob = rbind(table_prob,prob)
      
    }
    
    return(list(beta=beta,residuals=residuals,pvalue=pvalue,R=R2,
                forecast_quantiles=forecast,
                forecast_skt=table_forecast_skt,yin=XX[,1],qs=table_qs,crps_equal = table_crps_equal,
                crps_center=table_crps_center,
                crps_tails=table_crps_tails,crps_right=table_crps_right,
                crps_left=table_crps_left, prob = table_prob))
  }
