library(tidyverse)
library(ggpubr) # ggarrrange
library(kableExtra) # latex tables
library(openxlsx)
library(forecast)
library(quantreg) #quant reg
library(sn) #skew-t

rm(list = ls()) # Limpiar environment

#source("functions/eigrs2.R") # Eigen with ascending order
#source("functions/blockfact0.R") # CCA
source("functions/ThreeLevelMDFM.R") # CCA
#source("functions/spca.R") # CCA
source("functions/CRPS.R")
source("functions/Quantile model.R")

##### important function!!!!
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

#################################################################
##                         Import data                         ##
#################################################################
data <- readxl::read_excel("../Data/Data_complete_rev.xlsx")
Yorig <- readxl::read_excel("../Data/X.xlsx",sheet = "fulldata_ordered")
groups <- readxl::read_excel("../Data/X.xlsx",sheet = "structure")
date <- (data %>%  filter(country=="United States"))[-1,6]
date<-as.Date(date$date)
R1 <- ThreeMDFM1(Yorig,groups,spec=1)

R1$Factors_Inflation
R1$Factors_Inflation <- R1$Factors_Inflation %*% diag(c(-1,1,-1,-1,-1,-1,1,1))
R1$Lam_Inflation <- R1$Lam_Inflation %*% diag(c(-1,1,-1,-1,-1,-1,1,1))
idiosyncratic<-R1$Residuals


t<-nrow(idiosyncratic)
N<-nrow(idiosyncratic)
PP<- solve((t(R1$Lam_Inflation) %*% R1$Lam_Inflation)/N)
# Assumption uncorrelated idiosincratic
sigma_e<-sum(diag(t(R1$Residuals) %*% R1$Residuals)/(N*t)) 
Gamma<-sigma_e * (t(R1$Lam_Inflation) %*% R1$Lam_Inflation)/N
Sigma_F<-(PP %*% (Gamma %*% PP))/N


bannerCommenter::banner("ROLLING WINDOW FORECASTING")
##################################################################
##                  ROLLING WINDOW FORECASTING                  ##
##################################################################


# Tables 
crqs<-data.frame(matrix(nrow=253,ncol=11))
colnames(crqs)<-c("Date","QS=0.05","QS=0.25","QS=0.50",
                  "QS=0.50","QS=0.75","QS=0.95","CRPS-c",
                  "CRPS-t","CRPS-r","CRPS-l")

forecast<-data.frame(matrix(nrow=253,ncol=7))
colnames(crqs)<-c("Date","QS=0.05","QS=0.25","QS=0.50",
                  "QS=0.50","QS=0.75","QS=0.95")


#data_trans<-data_prep(1:240,data_list,spec = 1)


# Quantile Regressions


# Rolling scheme

h=1

mat=Yorig/1200 # Not annualized
mat_h=matrix(NA,nrow = h-1, ncol = ncol(Yorig))
colnames(mat_h)=colnames(Yorig)
for (i in 1:(nrow(mat)-h+1)){
  mat_h<-rbind(mat_h,cumsum(mat[i:(i+h-1),])[h,]*(1200/h))
}

list_h1_m1=list()
list_h1_m2=list()
list_h1_bench=list()
i=1
j=1
start = Sys.time()

# Some names with dotes in Yorig (but are the same to groups, so make equal both)
#groups[,1]<-colnames(Yorig)
list_insample<-list()

#list_h1_spec3<-list()
idx1_R1<-c(which(groups[,2]==1)) # Africa
idx1_R2<-c(which(groups[,3]==1)) # America
idx1_R3<-c(which(groups[,4]==1)) # Asia and Oceania
idx1_R4<-c(which(groups[,5]==1)) # Europe


colnames(mat_h)[111]
colnames(mat_h)[112]
colnames(mat_h)[13]
colnames(mat_h)[19]
colnames(mat_h)[23]

# list_insample[["United States"]]<-qreg_inf_insample(Y=mat_h,X=R1$Factors_Inflation,country=112,h=h,
#                                                     Sigma_F=Sigma_F)

# list_insample <- loadRData("../Data/list_insample_h1.RData")

###################### QAR MODEL #############################
# start = Sys.time()
# for (j in c(idx1_R1,idx1_R2,idx1_R3,idx1_R4)){
#   cat("\n",colnames(mat_h)[j],Sys.time() - start,"\n" )
#   list_insample[[colnames(mat_h)[j]]]<-qreg_inf_insample_ar(Y=mat_h,country=j,h=h)
# }

# save(list_insample,file = "../Data/list_insample_ar_h1.RData")

###################### QF MODEL #############################

start = Sys.time()
for (j in c(idx1_R1,idx1_R2,idx1_R3,idx1_R4)){
  cat("\n",colnames(mat_h)[j],Sys.time() - start,"\n" )
  list_insample[[colnames(mat_h)[j]]]<-qreg_inf_insample(Y=mat_h,X=R1$Factors_Inflation,country=j,h=h,
                                                         Sigma_F=Sigma_F)
}


save(list_insample,file = "../Data/list_insample_h1_rr.RData")
# 



###################### QF MODEL ROLLING #############################

list_rolling_LB<-c()
list_rolling_UB<-c()
list_rolling_beta<-c()
country<-19
colnames(mat_h)[country]
date[155] #"2011-12-01"
start = Sys.time()
Y=mat_h
X=R1$Factors_Inflation
tau=c(0.05,0.25,0.50,0.75,0.95)
for (i in 0:(length(date)-155)){
  cat("\n",colnames(mat_h)[country],Sys.time() - start,"\n" )
  
  yin=Y[(1+i):(155+i),country] # for estimation
  Xin=dplyr::lag(Y[(1+i):(155+i),],n=h)
  Xin=Xin[,country] # for estimation
  
  #X=R1$Factors_Inflation # Factors
  X2in=dplyr::lag(X[(1+i):(155+i),],n=h) # for estimation
  
  ### Estimate regression 
  XX<-data.frame(cbind("y"=yin,"X"=cbind(Xin,X2in))) 
  XX<-XX[,seq(1,ncol(XX))*c(1,1,1,unlist(groups[country,-1]))] # Select right factors
  XX<-XX[(-h:-1),] # Due to h
  
  colnames(XX)<-c("y","Ly","FGlobal","FRegional","FIncome")
  # betas
  
  for (tt in tau) {
    eq1=rq(y~.,data = XX,tau = tt) # estimate at t
    reg<-summary.rq(eq1,se = "ker",covariance = T)
    list_rolling_UB[[paste0(i)]][[paste0(tt)]]<-reg$coefficients[,1]+diag(reg$cov^0.5)*2
    list_rolling_LB[[paste0(i)]][[paste0(tt)]]<-reg$coefficients[,1]-diag(reg$cov^0.5)*2
    list_rolling_beta[[paste0(i)]][[paste0(tt)]]<-reg$coefficients[,1]
  }
  
}

for (j in 1:5) {
  
  if (j==1) name ="Intercept"
  if (j==2) name ="Lagged inflation coefficient"
  if (j==3) name ="Global factor coefficient"
  if (j==4) name ="Regional factor coefficient"
  if (j==5) name ="Income factor coefficient"
  
  f1<-as.data.frame(cbind("date"=date[155:(287)],"beta"=as.data.frame(Reduce(rbind,lapply(list_rolling_beta,function(x)x$`0.05`)))[,j],
                          "LB"=as.data.frame(Reduce(rbind,lapply(list_rolling_LB,function(x)x$`0.05`)))[,j],
                          "UB"=as.data.frame(Reduce(rbind,lapply(list_rolling_UB,function(x)x$`0.05`)))[,j])) %>% 
    ggplot()+
    geom_line(aes(x=as.Date(date,origin='1970-01-01'),y=beta))+
    geom_line(aes(x=as.Date(date,origin='1970-01-01'),y=LB),linetype = "dashed",col="blue")+
    geom_line(aes(x=as.Date(date,origin='1970-01-01'),y=UB),linetype = "dashed",col="blue")+
    ylab("") +
    xlab(expression("")) + 
    labs(title=paste0(name," (q=0.05)"),
         caption = "") +
    theme_bw()
  
  f2<-as.data.frame(cbind("date"=date[155:(287)],"beta"=as.data.frame(Reduce(rbind,lapply(list_rolling_beta,function(x)x$`0.25`)))[,j],
                          "LB"=as.data.frame(Reduce(rbind,lapply(list_rolling_LB,function(x)x$`0.25`)))[,j],
                          "UB"=as.data.frame(Reduce(rbind,lapply(list_rolling_UB,function(x)x$`0.25`)))[,j])) %>% 
    ggplot()+
    geom_line(aes(x=as.Date(date,origin='1970-01-01'),y=beta))+
    geom_line(aes(x=as.Date(date,origin='1970-01-01'),y=LB),linetype = "dashed",col="blue")+
    geom_line(aes(x=as.Date(date,origin='1970-01-01'),y=UB),linetype = "dashed",col="blue")+
    ylab("") +
    xlab(expression("")) + 
    labs(title=paste0(name," (q=0.25)"),
         caption = "") +
    theme_bw()
  
  f3<-as.data.frame(cbind("date"=date[155:(287)],"beta"=as.data.frame(Reduce(rbind,lapply(list_rolling_beta,function(x)x$`0.5`)))[,j],
                          "LB"=as.data.frame(Reduce(rbind,lapply(list_rolling_LB,function(x)x$`0.5`)))[,j],
                          "UB"=as.data.frame(Reduce(rbind,lapply(list_rolling_UB,function(x)x$`0.5`)))[,j])) %>% 
    ggplot()+
    geom_line(aes(x=as.Date(date,origin='1970-01-01'),y=beta))+
    geom_line(aes(x=as.Date(date,origin='1970-01-01'),y=LB),linetype = "dashed",col="blue")+
    geom_line(aes(x=as.Date(date,origin='1970-01-01'),y=UB),linetype = "dashed",col="blue")+
    ylab("") +
    xlab(expression("")) + 
    labs(title=paste0(name," (q=0.50)"),
         caption = "") +
    theme_bw()
  
  
  f4<-as.data.frame(cbind("date"=date[155:(287)],"beta"=as.data.frame(Reduce(rbind,lapply(list_rolling_beta,function(x)x$`0.75`)))[,j],
                          "LB"=as.data.frame(Reduce(rbind,lapply(list_rolling_LB,function(x)x$`0.75`)))[,j],
                          "UB"=as.data.frame(Reduce(rbind,lapply(list_rolling_UB,function(x)x$`0.75`)))[,j])) %>% 
    ggplot()+
    geom_line(aes(x=as.Date(date,origin='1970-01-01'),y=beta))+
    geom_line(aes(x=as.Date(date,origin='1970-01-01'),y=LB),linetype = "dashed",col="blue")+
    geom_line(aes(x=as.Date(date,origin='1970-01-01'),y=UB),linetype = "dashed",col="blue")+
    ylab("") +
    xlab(expression("")) + 
    labs(title=paste0(name," (q=0.75)"),
         caption = "") +
    theme_bw()
  
  f5<-as.data.frame(cbind("date"=date[155:(287)],"beta"=as.data.frame(Reduce(rbind,lapply(list_rolling_beta,function(x)x$`0.95`)))[,j],
                          "LB"=as.data.frame(Reduce(rbind,lapply(list_rolling_LB,function(x)x$`0.95`)))[,j],
                          "UB"=as.data.frame(Reduce(rbind,lapply(list_rolling_UB,function(x)x$`0.95`)))[,j])) %>% 
    ggplot()+
    geom_line(aes(x=as.Date(date,origin='1970-01-01'),y=beta))+
    geom_line(aes(x=as.Date(date,origin='1970-01-01'),y=LB),linetype = "dashed",col="blue")+
    geom_line(aes(x=as.Date(date,origin='1970-01-01'),y=UB),linetype = "dashed",col="blue")+
    ylab("") +
    xlab(expression("")) + 
    labs(title=paste0(name," (q=0.95)"),
         caption = "") +
    theme_bw()
  
  
  ggsave(paste0("../Figures/Coefficient_",name,colnames(mat_h)[country],".png"), 
         ggarrange(f1,f2,f3,f4,f5,legend = "bottom",nrow = 3,ncol = 2,
                   common.legend = T),
         width = 8, height = 6)
  
}