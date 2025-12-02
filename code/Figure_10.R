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
source("functions/rs_test_one_step.R")
source("functions/plotsofuniformity.R")

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
# Assumption uncorrelated idiosincratic
sigma_e<-sum(diag(t(R1$Residuals) %*% R1$Residuals)/(N*t)) 
Gamma<-sigma_e * (t(R1$Lam_Inflation) %*% R1$Lam_Inflation)/N
Sigma_F<-(PP %*% (Gamma %*% PP))/N


bannerCommenter::banner("IN-SAMPLE")
#################################################################
##                          IN-SAMPLE                          ##
#################################################################

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


colnames(mat_h)[111] #UK 
colnames(mat_h)[112] #US
colnames(mat_h)[13] # Brazil 
colnames(mat_h)[19] #Canada 
colnames(mat_h)[23] #China 
colnames(mat_h)[57] #Japan
colnames(mat_h)[40] #France
colnames(mat_h)[43] #Germany
colnames(mat_h)[55] #Italy
colnames(mat_h)[51] #India
colnames(mat_h)[97] #South Africa 
colnames(mat_h)[72] #Mexico

bannerCommenter::banner("ROLLING WINDOW FORECASTING")
##################################################################
##                  ROLLING WINDOW FORECASTING                  ##
##################################################################


# Rolling scheme
h=1

mat=Yorig/1200 # Not annualized
mat_h=matrix(NA,nrow = h-1, ncol = ncol(Yorig))
colnames(mat_h)=colnames(Yorig)
for (i in 1:(nrow(mat)-h+1)){
  mat_h<-rbind(mat_h,cumsum(mat[i:(i+h-1),])[h,]*(1200/h))
}


date[155] #"2011-12-01"

nwindow=287-132 #R=240-36-12+1 Real window size see data_prep()
ind=1:nrow(mat_h) # Due to Ham Filter
window_size=nrow(mat_h)-nwindow-h+1
indmat=matrix(NA,window_size,nwindow)
indmat[1,]=1:ncol(indmat)
for(i in 2:nrow(indmat)){
  indmat[i,]=indmat[i-1,]+1
}

list_h1_mb=list()
list_h1_m1=list()
list_h1_m2=list()
list_h1_m3=list()
list_h1_m4=list()
list_h1_bench=list()
list_h1_uncon=list()
i=1
j=1

set.seed(123)
start = Sys.time()
for (j in c(111,112,13,19,23,57,40,43,55,51,97,72)){
  cat("\n",colnames(mat_h)[j],Sys.time() - start,"\n" )
  for (i in 1:nrow(indmat)){
    cat(i, ",")

    list_h1_m1[[colnames(mat_h)[j]]][[i]]=qreg_inf(ind=indmat[i,],Y=mat_h,groups = groups,country=j,h=h,mod=1,
                                                   tau = c(0.05,0.25,0.50,0.75,0.95))
  }
}                                                                                                                                                                                                                     


set.seed(123)
for (c in 1:12) {
  
  # PIT
  pit<-as.data.frame(Reduce(rbind,lapply(list_h1_m1[[c]],FUN = function(x)x$prob_pit)))
  rvec = seq(0,1,0.001)
  crit_value<- rs_test_one_step(pit[,1],rvec)$crit_value
  
  png(filename = paste0("../output/Figures/Figure_10",names(list_h1_m1)[c],".png"), width = 600, height = 400)
  plotsofuniformity(pit[,1], figtitle=names(list_h1_m1)[c], crit_value_vec=crit_value, significance_level=5)
  dev.off()
}


