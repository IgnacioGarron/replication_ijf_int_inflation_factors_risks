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
data <- readxl::read_excel("../input/Data_complete_rev.xlsx")
Yorig <- readxl::read_excel("../input/X.xlsx")
groups <- readxl::read_excel("../input/structure_dfm.xlsx")
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
# Assumption of homscedastic and uncorrelated idiosincratic
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



# Quantile Regressions
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


###################### QFAR MODEL #############################

set.seed(123)
start = Sys.time()
for (j in c(idx1_R1,idx1_R2,idx1_R3,idx1_R4)){
  cat("\n",colnames(mat_h)[j],Sys.time() - start,"\n" )
  list_insample[[colnames(mat_h)[j]]]<-qreg_inf_insample(Y=mat_h,X=R1$Factors_Inflation,country=j,h=h,
                                                         Sigma_F=Sigma_F)
}

save(list_insample,file = "../output/Data/list_insample_h1_rr.RData")
# 

###################### QAR MODEL #############################
set.seed(123)
start = Sys.time()
for (j in c(idx1_R1,idx1_R2,idx1_R3,idx1_R4)){
  cat("\n",colnames(mat_h)[j],Sys.time() - start,"\n" )
  list_insample[[colnames(mat_h)[j]]]<-qreg_inf_insample_ar(Y=mat_h,country=j,h=h)
}

save(list_insample,file = "../output/Data/list_insample_ar_h1_rr.RData")





