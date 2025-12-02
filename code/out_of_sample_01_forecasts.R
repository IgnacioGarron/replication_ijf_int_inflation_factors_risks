library(tidyverse)
library(ggpubr) # ggarrrange
library(kableExtra) # latex tables
library(openxlsx)
library(forecast)
library(quantreg) #quant reg
library(sn) #skew-t

rm(list = ls()) # Limpiar environment

source("functions/ThreeLevelMDFM.R") # CCA
source("functions/CRPS.R")
source("functions/Quantile model.R")


#################################################################
##                         Import data                         ##
#################################################################
data <- readxl::read_excel("../input/Data_complete_rev.xlsx")
Yorig <- readxl::read_excel("../input/X.xlsx")
groups <- readxl::read_excel("../input/structure_dfm.xlsx")
date <- (data %>%  filter(country=="United States"))[-1,6]
date<-as.Date(date$date)


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

idx1_R1<-c(which(groups[,2]==1)) # Africa
idx1_R2<-c(which(groups[,3]==1)) # America
idx1_R3<-c(which(groups[,4]==1)) # Asia and Oceania
idx1_R4<-c(which(groups[,5]==1)) # Europe



# Rolling scheme

h=1

mat=Yorig/1200 # Not annualized
mat_h=matrix(NA,nrow = h-1, ncol = ncol(Yorig))
colnames(mat_h)=colnames(Yorig)
for (i in 1:(nrow(mat)-h+1)){
  mat_h<-rbind(mat_h,cumsum(mat[i:(i+h-1),])[h,]*(1200/h))
}

#plot(mat_h[,"Mexico"],t="l")
#tail(mat_h[,"Mexico"])
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


start = Sys.time()
for (j in c(idx1_R1,idx1_R2,idx1_R3,idx1_R4)){
  cat("\n",colnames(mat_h)[j],Sys.time() - start,"\n" )
  for (i in 1:nrow(indmat)){
    cat(i, ",")
    
    list_h1_mb[[colnames(mat_h)[j]]][[i]]=qreg_inf_best(ind=indmat[i,],Y=mat_h,groups = groups,country=j,h=h,
                                                            tau = c(0.05,0.25,0.50,0.75,0.95))
    
    list_h1_m1[[colnames(mat_h)[j]]][[i]]=qreg_inf(ind=indmat[i,],Y=mat_h,groups = groups,country=j,h=h,mod=1,
                                                   tau = c(0.05,0.25,0.50,0.75,0.95))
    
    list_h1_m2[[colnames(mat_h)[j]]][[i]]=qreg_inf(ind=indmat[i,],Y=mat_h,groups = groups,country=j,h=h,mod=2,
                                                   tau = c(0.05,0.25,0.50,0.75,0.95))
    
    list_h1_m3[[colnames(mat_h)[j]]][[i]]=qreg_inf(ind=indmat[i,],Y=mat_h,groups = groups,country=j,h=h,mod=3,
                                                   tau = c(0.05,0.25,0.50,0.75,0.95))

    list_h1_m4[[colnames(mat_h)[j]]][[i]]=qreg_inf(ind=indmat[i,],Y=mat_h,groups = groups,country=j,h=h,mod=4,
                                                   tau = c(0.05,0.25,0.50,0.75,0.95))

    list_h1_bench[[colnames(mat_h)[j]]][[i]]=qreg_inf(ind=indmat[i,],Y=mat_h,groups = groups,country=j,h=h,spec="bench",
                                                      tau = c(0.05,0.25,0.50,0.75,0.95))

    list_h1_uncon[[colnames(mat_h)[j]]][[i]]=qreg_inf_uncon(ind=indmat[i,],Y=mat_h,groups = groups,country=j,h=h,
                                                            tau = c(0.05,0.25,0.50,0.75,0.95))
  }
}                                                                                                                                                                                                                     

# save
save(list_h1_mb,file ="../Data/list_h1_mb.RData")
save(list_h1_m1,file ="../output/Data/list_h1_m1rr.RData")
save(list_h1_m2,file ="../output/Data/list_h1_m2.RData")
save(list_h1_m3,file ="../output/Data/list_h1_m3.RData")
save(list_h1_m4,file ="../output/Data/list_h1_m4.RData")
save(list_h1_bench,file ="../output/Data/list_h1_bench.RData")
save(list_h1_uncon,file ="../output/Data/list_h1_uncon.RData")

