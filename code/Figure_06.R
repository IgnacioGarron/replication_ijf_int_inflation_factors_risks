
bannerCommenter::banner("LOAD PACKAGES AND FUNCTIONS")
#################################################################
##                 LOAD PACKAGES AND FUNCTIONS                 ##
#################################################################


rm(list = ls()) # Limpiar environment
library(forecast)
library(kableExtra) # latex tables
library(ggpubr) #ggarrane
library(tidyverse) #ggarrane
library(viridis)
library(writexl)
source("functions/CRPS.R")


##### important function!!!!
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
data <- readxl::read_excel("../input/Data_complete_rev.xlsx")

source("functions/ThreeLevelMDFM.R") # CCA

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

idx1_R1<-groups[groups[,2]==1,1] # Africa
idx1_R2<-groups[groups[,3]==1,1] # America
idx1_R3<-groups[groups[,4]==1,1] # Asia and Oceania
idx1_R4<-groups[groups[,5]==1,1] # Europe

idx1_R5<-groups[groups[,6]==1,1] # Adv
idx1_R6<-groups[groups[,7]==1,1] # Emde
idx1_R7<-groups[groups[,8]==1,1] # LI

geo_group<-c(rep("Africa",length(idx1_R1$Country)),
             rep("America",length(idx1_R2$Country)),
             rep("Asia and Oceania",length(idx1_R3$Country)),
             rep("Europe",length(idx1_R4$Country)))


mod1<-loadRData(file = paste0("../output/Data/list_h1_m",1,"rr.RData"))


#################################################################
##                         Import real factors                 ##
#################################################################
##############################



################################################################################
# Align signs to the original factors
sign_factor<-function(full_factor,Factors_vintages){
  window<-length(na.omit(Factors_vintages[,1]))
  tot_n_factors <-ncol(Factors_vintages)
  for (ff in 1:tot_n_factors) {
    inv_sample <- (-1) * Factors_vintages[(ff):(window+ff-1), ff]
    dif1 <- sum(abs(full_factor[(ff):(window+ff-1)] - Factors_vintages[(ff):(window+ff-1), ff]))
    dif2 <- sum(abs(full_factor[(ff):(window+ff-1)] - inv_sample))
    
    if (dif2 < dif1) {
      Factors_vintages[(ff):(window+ff-1), ff] <- inv_sample
    }
  }
  Real_time_factor<-c()
  for (i in 1:nrow(Factors_vintages)){
    Real_time_factor<-c(Real_time_factor,na.omit(Factors_vintages[i,])[1])
  }
  return(list(Factors_vintages=Factors_vintages,Real_time_factor=Real_time_factor))
}



#################################################################
##                       Factors vintages plot                 ##
#################################################################
i<-"Spain"
m<-1

matrix<-c(Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,3]))[,1],rep(NA,287-155-1))
for (t in 2:132){
  if(t<132) matrix<-cbind(matrix,c(rep(NA,t-1),Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,3]))[,t],rep(NA,287-t-155)))
  if(t==132) matrix<-cbind(matrix,c(rep(NA,t-1),Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,3]))[,t]))
}


Factors_vintages <-as.matrix(matrix)
F1_R <- sign_factor(full_factor=R1$Factors_Inflation[,1],Factors_vintages=Factors_vintages)
matrix<- F1_R$Factors_vintages

# save
png(filename = paste0("../output/Figures/Figure_06_Globalfactorvintages.png"),
  width = 1200, height = 800, res = 120)

plot(matrix[,1],x=date[-1],t="l",ylim=c(-6,6),ylab="",xlab="")
for (t in 2:132){
  lines(matrix[,t],x=date[-1],t="l",col=t)
}
dev.off()

f_matrix<-cbind(date[-1],data.frame(matrix)*-1)
colnames(f_matrix)<-c("date",(date[(length(date)-132+2):length(date)]))
write_xlsx(x = f_matrix,path = "../output/Data/Globalfactorvintages.xlsx")

##########################
#Europe Factor
i<-"Spain"

matrix<-c(Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,4]))[,1],rep(NA,287-155-1))
for (t in 2:132){
  if(t<132) matrix<-cbind(matrix,c(rep(NA,t-1),Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,4]))[,t],rep(NA,287-t-155)))
  if(t==132) matrix<-cbind(matrix,c(rep(NA,t-1),Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,4]))[,t]))
}

Factors_vintages <-as.matrix(matrix)
F5_R <- sign_factor(full_factor=R1$Factors_Inflation[,5],Factors_vintages=Factors_vintages)
matrix<- F5_R$Factors_vintages

# save
png(filename = paste0("../output/Figures/Figure_06_Europefactorvintages.png"),
    width = 1200, height = 800, res = 120)
plot(matrix[,1],x=date[-1],t="l",ylim=c(-6,6),ylab="",xlab="")
for (t in 2:132){
  lines(matrix[,t],x=date[-1],t="l",col=t)
}
dev.off()

f_matrix<-cbind(date[-1],data.frame(matrix)*-1)
colnames(f_matrix)<-c("date",(date[(length(date)-132+2):length(date)]))
write_xlsx(x = f_matrix,path = "../output/Data/Europefactorvintages.xlsx")

##########################
#America Factor
i<-"Mexico"
m<-1

matrix<-c(Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,4]))[,1],rep(NA,287-155-1))
for (t in 2:132){
  if(t<132) matrix<-cbind(matrix,c(rep(NA,t-1),Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,4]))[,t],rep(NA,287-t-155)))
  if(t==132) matrix<-cbind(matrix,c(rep(NA,t-1),Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,4]))[,t]))
}

Factors_vintages <-as.matrix(matrix)
F3_R <- sign_factor(full_factor=R1$Factors_Inflation[,3],Factors_vintages=Factors_vintages)
matrix<- F3_R$Factors_vintages

# save
png(filename = paste0("../output/Figures/Figure_06_Americafactorvintages.png"),
    width = 1200, height = 800, res = 120)
plot(matrix[,1],x=date[-1],t="l",ylim=c(-6,6),ylab="",xlab="")
for (t in 2:132){
  lines(matrix[,t],x=date[-1],t="l",col=t)
}
dev.off()

f_matrix<-cbind(date[-1],data.frame(matrix))
colnames(f_matrix)<-c("date",(date[(length(date)-132+2):length(date)]))
write_xlsx(x = f_matrix,path = "../output/Data/Americafactorvintages.xlsx")

##########################
#Asia Factor
i<-"Japan"
m<-1

matrix<-c(Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,4]))[,1],rep(NA,287-155-1))
for (t in 2:132){
  if(t<132) matrix<-cbind(matrix,c(rep(NA,t-1),Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,4]))[,t],rep(NA,287-t-155)))
  if(t==132) matrix<-cbind(matrix,c(rep(NA,t-1),Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,4]))[,t]))
}

Factors_vintages <-as.matrix(matrix)
F4_R <- sign_factor(full_factor=R1$Factors_Inflation[,4],Factors_vintages=Factors_vintages)
matrix <- F4_R$Factors_vintages

png(filename = paste0("../output/Figures/Figure_06_Asiafactorvintages.png"),
    width = 1200, height = 800, res = 120)
plot(matrix[,1]*-1,x=date[-1],t="l",ylim=c(-6,6),ylab="",xlab="")
for (t in 2:132){
  lines(matrix[,t]*-1,x=date[-1],t="l",col=t)
}
dev.off()

f_matrix<-cbind(date[-1],data.frame(matrix))
colnames(f_matrix)<-c("date",(date[(length(date)-132+2):length(date)]))
write_xlsx(x = f_matrix,path = "../output/Data/Asiafactorvintages.xlsx")


##########################
#Africa Factor
i<-"South Africa"
m<-1

matrix<-c(Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,4]))[,1],rep(NA,287-155-1))
for (t in 2:132){
  if(t<132) matrix<-cbind(matrix,c(rep(NA,t-1),Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,4]))[,t],rep(NA,287-t-155)))
  if(t==132) matrix<-cbind(matrix,c(rep(NA,t-1),Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,4]))[,t]))
}

Factors_vintages <-as.matrix(matrix)
F2_R <- sign_factor(full_factor=R1$Factors_Inflation[,2],Factors_vintages=Factors_vintages)
matrix <- F2_R$Factors_vintages


png(filename = paste0("../output/Figures/Figure_06_Africafactorvintages.png"),
    width = 1200, height = 800, res = 120)
plot(matrix[,1],x=date[-1],t="l",ylim=c(-6,6),ylab="",xlab="")
for (t in 2:132){
  lines(matrix[,t],x=date[-1],t="l",col=t)
}
dev.off()

f_matrix<-cbind(date[-1],data.frame(matrix)*-1)
colnames(f_matrix)<-c("date",(date[(length(date)-132+2):length(date)]))
write_xlsx(x = f_matrix,path = "../output/Data/Africafactorvintages.xlsx")


##########################
#Advanced Factor
i<-"Spain"
m<-1

matrix<-c(Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,5]))[,1],rep(NA,287-155-1))
for (t in 2:132){
  if(t<132) matrix<-cbind(matrix,c(rep(NA,t-1),Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,5]))[,t],rep(NA,287-t-155)))
  if(t==132) matrix<-cbind(matrix,c(rep(NA,t-1),Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,5]))[,t]))
}


Factors_vintages <- as.matrix(matrix)
F6_R <- sign_factor(full_factor=R1$Factors_Inflation[,6],Factors_vintages=Factors_vintages)
matrix <- F6_R$Factors_vintages

png(filename = paste0("../output/Figures/Figure_06_Advancedfactorvintages.png"),
    width = 1200, height = 800, res = 120)
plot(matrix[,1],x=date[-1],t="l",ylim=c(-6,6),ylab="",xlab="")
for (t in 2:132){
  lines(matrix[,t],x=date[-1],t="l",col=t)
}
dev.off()

f_matrix<-cbind(date[-1],data.frame(matrix)*-1)
colnames(f_matrix)<-c("date",(date[(length(date)-132+2):length(date)]))
write_xlsx(x = f_matrix,path = "../output/Data/Advancedfactorvintages.xlsx")


##########################
#HMI Factor
i<-"Mexico"

matrix<-c(Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,5]))[,1],rep(NA,287-155-1))
for (t in 2:132){
  if(t<132) matrix<-cbind(matrix,c(rep(NA,t-1),Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,5]))[,t],rep(NA,287-t-155)))
  if(t==132) matrix<-cbind(matrix,c(rep(NA,t-1),Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,5]))[,t]))
}

Factors_vintages <- as.matrix(matrix)
F7_R <- sign_factor(full_factor=R1$Factors_Inflation[,7],Factors_vintages=Factors_vintages)
matrix <- F7_R$Factors_vintages

png(filename = paste0("../output/Figures/Figure_06_HMIfactorvintages.png"),
    width = 1200, height = 800, res = 120)
plot(matrix[,1],x=date[-1],t="l",ylim=c(-6,6),ylab="",xlab="")
for (t in 2:132){
  lines(matrix[,t],x=date[-1],t="l",col=t)
}
dev.off()


f_matrix<-cbind(date[-1],data.frame(matrix)*-1)
colnames(f_matrix)<-c("date",(date[(length(date)-132+2):length(date)]))
write_xlsx(x = f_matrix,path = "../output/Data/HMIfactorvintages.xlsx")

##########################
#LI Factor
i<-"Bolivia"
m<-1

matrix<-c(Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,5]))[,1],rep(NA,287-155-1))
for (t in 2:132){
  if(t<132) matrix<-cbind(matrix,c(rep(NA,t-1),Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,5]))[,t],rep(NA,287-t-155)))
  if(t==132) matrix<-cbind(matrix,c(rep(NA,t-1),Reduce(cbind,lapply(mod1[[i]], function(x)x$X[,5]))[,t]))
}

Factors_vintages <- as.matrix(matrix)
F8_R <- sign_factor(full_factor=R1$Factors_Inflation[,8],Factors_vintages=Factors_vintages)
matrix <- F8_R$Factors_vintages

png(filename = paste0("../output/Figures/Figure_06_LIfactorvintages.png"),
    width = 1200, height = 800, res = 120)
plot(matrix[,1],x=date[-1],t="l",ylim=c(-6,6),ylab="",xlab="")
for (t in 2:132){
  lines(matrix[,t],x=date[-1],t="l",col=t)
}
dev.off()

f_matrix<-cbind(date[-1],data.frame(matrix)*-1)
colnames(f_matrix)<-c("date",(date[(length(date)-132+1):length(date)]))
write_xlsx(x = f_matrix,path = "../output/Data/LIfactorvintages.xlsx")
