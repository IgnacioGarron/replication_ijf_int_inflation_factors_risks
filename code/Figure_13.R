
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
source("functions/CFG_Test_Stat_in.R")
library(sn) #skew-t
library(murphydiagram) # Giacomini and Rossi (2010)
library(MCS)

##### important function!!!!
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
data <- readxl::read_excel("../input/Data_complete_rev.xlsx")

source("functions/ThreeLevelMDFM.R") # CCA
source("functions/get_nonparametric_variance.R") # CCA

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

date <- readxl::read_excel("../output/Data/Globalfactorvintages.xlsx")[,1]

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
################################################################################

# Global Factor
Factors_vintages <- readxl::read_excel("../output/Data/Globalfactorvintages.xlsx")
Factors_vintages <-as.matrix(Factors_vintages[,-1])
F1_R <- sign_factor(full_factor=R1$Factors_Inflation[,1],Factors_vintages=Factors_vintages)
F1_R <- F1_R$Real_time_factor


# Africa Factor
F2_R<-c()
Factors_vintages <- readxl::read_excel("../output/Data/Africafactorvintages.xlsx")
Factors_vintages <-as.matrix(Factors_vintages[,-1])
F2_R <- sign_factor(full_factor=R1$Factors_Inflation[,2],Factors_vintages=Factors_vintages)
F2_R <- F2_R$Real_time_factor


# America Factor
F3_R<-c()
Factors_vintages <- readxl::read_excel("../output/Data/Americafactorvintages.xlsx")
Factors_vintages <-as.matrix(Factors_vintages[,-1])
F3_R <- sign_factor(full_factor=R1$Factors_Inflation[,3],Factors_vintages=Factors_vintages)
F3_R <- F3_R$Real_time_factor


# Asia Factor
F4_R<-c()
Factors_vintages <- readxl::read_excel("../output/Data/Asiafactorvintages.xlsx")
Factors_vintages <- as.matrix(Factors_vintages[,-1])
F4_R <- sign_factor(full_factor=R1$Factors_Inflation[,4],Factors_vintages=Factors_vintages)
F4_R <- F4_R$Real_time_factor


#Europe Factor
F5_R<-c()
Factors_vintages <- readxl::read_excel("../output/Data/Europefactorvintages.xlsx")
Factors_vintages <- as.matrix(Factors_vintages[,-1])
F5_R <- sign_factor(full_factor=R1$Factors_Inflation[,5],Factors_vintages=Factors_vintages)
F5_R <- F5_R$Real_time_factor


#Advanced Factor
F6_R<-c()
Factors_vintages <- readxl::read_excel("../output/Data/Advancedfactorvintages.xlsx")
Factors_vintages <- as.matrix(Factors_vintages[,-1])
F6_R <- sign_factor(full_factor=R1$Factors_Inflation[,6],Factors_vintages=Factors_vintages)
F6_R <- F6_R$Real_time_factor


#HMI Factor
F7_R<-c()
Factors_vintages <- readxl::read_excel("../output/Data/HMIfactorvintages.xlsx")
Factors_vintages <- as.matrix(Factors_vintages[,-1])
F7_R <- sign_factor(full_factor=R1$Factors_Inflation[,7],Factors_vintages=Factors_vintages)
F7_R <- F7_R$Real_time_factor


#LI Factor
F8_R<-c()
Factors_vintages <- readxl::read_excel("../output/Data/LIfactorvintages.xlsx")
Factors_vintages <- as.matrix(Factors_vintages[,-1])
F8_R <- sign_factor(full_factor=R1$Factors_Inflation[,8],Factors_vintages=Factors_vintages)
F8_R <- F8_R$Real_time_factor


F_R<-cbind(F1_R,F2_R,F3_R,F4_R,F5_R,F6_R,F7_R,F8_R)


#################################################################
##                   AM  Test                                  ##
#################################################################
##############################

Table<-data.frame(matrix(nrow = 8,ncol=6))
Table[,1]<-c("Global","Africa","America","Asia and Oceania","Europe","Advance","HMI","LI")
colnames(Table)<-c("","QS(0.05)","QS(0.25)","QS(0.50)","QS(0.75)","QS(0.95)")

Table1<-data.frame(matrix(nrow = 115,ncol=5))
rownames(Table1)<-colnames(Yorig)
colnames(Table1)<-c("QS(0.05)","QS(0.25)","QS(0.50)","QS(0.75)","QS(0.95)")
Table2<-Table1
Table3<-Table1
Table4<-Table1
benchmark<-loadRData(file = paste0("../output/Data/list_h1_bench.RData"))

country<-"United Kingdom"
country

for(country in colnames(Yorig)){
  length(benchmark[[country]])
  OOSerror_modela<-c()
  OOSerror_modelb<-c()
  for (oos in 1:length(benchmark[[country]])){
    temp <- rep(tail(benchmark[[country]][[oos]]$X,1)$y,5)-benchmark[[country]][[oos]]$forecast_quantile
    OOSerror_modela <-rbind(OOSerror_modela,temp)
    
    temp <- rep(tail(mod1[[country]][[oos]]$X,1)$y,5)-mod1[[country]][[oos]]$forecast_quantile
    OOSerror_modelb <-rbind(OOSerror_modelb,temp)
  }
  
  # plot(OOSerror_modelb[,5],t="l",col=1)
  # lines(OOSerror_modela[,5],t="l",col=2)
  # Real time and full sample matrices
  
  F_fullsampl = R1$Factors_Inflation[,seq(1,ncol(R1$Factors_Inflation))*c(1,unlist(groups[groups$Country==country,-1]))][-287,]
  F_realtime  = F_R[,seq(1,ncol(F_R))*c(1,unlist(groups[groups$Country==country,-1]))]
  y_fullsamp = c(as.matrix(Yorig[-1,country]))
  x_fullsamp = as.matrix(cbind(Yorig[-nrow(Yorig[,country]),country],F_fullsampl))
  x_rt = as.matrix(cbind(Yorig[-nrow(Yorig[,country]),country],F_realtime))[-1:-154,]
  
  # Initialization
  R = nrow(benchmark[[country]][[1]]$X)-1 # h=1 Estimation window
  P = length(benchmark[[country]]) # OOS
  pi = P/R
  scheme = "rolling"
  direct = 1
  epsilon = 1/5
  h=1
  
  # QS(0.05)
  alpha = c(0.05)
  V=get_nonparametric_variance(y_fullsamp, x_fullsamp, x_rt, R, pi, h, alpha, OOSerror_modela=array(OOSerror_modela[,1]), OOSerror_modelb=array(OOSerror_modelb[,1]), scheme, epsilon, direct)
  loss_b=Reduce(rbind,lapply(mod1[[country]], function(x)x$qs[1]))
  loss_a=Reduce(rbind,lapply(benchmark[[country]], function(x)x$qs[1]))
  am_test=mean(loss_a-loss_b)/sqrt(V)
  Table1[country,1]<-(round(pnorm(am_test,lower.tail=F),3)<0.1)*1
  Table2[country,1]<- round(pnorm(am_test,lower.tail=F),3)
  Table3[country,1]<- mean(loss_b)/mean(loss_a)
  Table4[country,1]<- (dm.test(loss_a,loss_b,power=1,alternative = "greater"))$statistic/am_test
  # QS(0.25)
  alpha = c(0.25)
  V=get_nonparametric_variance(y_fullsamp, x_fullsamp, x_rt, R, pi, h, alpha, OOSerror_modela=array(OOSerror_modela[,2]), OOSerror_modelb=array(OOSerror_modelb[,2]), scheme, epsilon, direct)
  loss_b=Reduce(rbind,lapply(mod1[[country]], function(x)x$qs[2]))
  loss_a=Reduce(rbind,lapply(benchmark[[country]], function(x)x$qs[2]))
  am_test=mean(loss_a-loss_b)/sqrt(V)
  Table1[country,2]<-(round(pnorm(am_test,lower.tail=F),3)<0.1)*1
  Table2[country,2]<- round(pnorm(am_test,lower.tail=F),3)
  Table3[country,2]<- mean(loss_b)/mean(loss_a)
  Table4[country,2]<- (dm.test(loss_a,loss_b,power=1,alternative = "greater"))$statistic/am_test
  # QS(0.50)
  alpha = c(0.5)
  V=get_nonparametric_variance(y_fullsamp, x_fullsamp, x_rt, R, pi, h, alpha, OOSerror_modela=array(OOSerror_modela[,3]), OOSerror_modelb=array(OOSerror_modelb[,3]), scheme, epsilon, direct)
  loss_b=Reduce(rbind,lapply(mod1[[country]], function(x)x$qs[3]))
  loss_a=Reduce(rbind,lapply(benchmark[[country]], function(x)x$qs[3]))
  am_test=mean(loss_a-loss_b)/sqrt(V)
  Table1[country,3]<-(round(pnorm(am_test,lower.tail=F),3)<0.1)*1
  Table2[country,3]<- round(pnorm(am_test,lower.tail=F),3)
  Table3[country,3]<- mean(loss_b)/mean(loss_a)
  Table4[country,3]<- (dm.test(loss_a,loss_b,power=1,alternative = "greater"))$statistic/am_test
  # QS(0.75)
  alpha = c(0.75)
  V=get_nonparametric_variance(y_fullsamp, x_fullsamp, x_rt, R, pi, h, alpha, OOSerror_modela=array(OOSerror_modela[,4]), OOSerror_modelb=array(OOSerror_modelb[,4]), scheme, epsilon, direct)
  loss_b=Reduce(rbind,lapply(mod1[[country]], function(x)x$qs[4]))
  loss_a=Reduce(rbind,lapply(benchmark[[country]], function(x)x$qs[4]))
  am_test=mean(loss_a-loss_b)/sqrt(V)
  Table1[country,4]<-(round(pnorm(am_test,lower.tail=F),3)<0.1)*1
  Table2[country,4]<- round(pnorm(am_test,lower.tail=F),3)
  Table3[country,4]<- mean(loss_b)/mean(loss_a)
  Table4[country,4]<- (dm.test(loss_a,loss_b,power=1,alternative = "greater"))$statistic/am_test
  # QS(0.95)
  alpha = c(0.95)
  V=get_nonparametric_variance(y_fullsamp, x_fullsamp, x_rt, R, pi, h, alpha, OOSerror_modela=array(OOSerror_modela[,5]), OOSerror_modelb=array(OOSerror_modelb[,5]), scheme, epsilon, direct)
  loss_b=Reduce(rbind,lapply(mod1[[country]], function(x)x$qs[5]))
  loss_a=Reduce(rbind,lapply(benchmark[[country]], function(x)x$qs[5]))
  am_test=mean(loss_a-loss_b)/sqrt(V)
  Table1[country,5]<-(round(pnorm(am_test,lower.tail=F),3)<0.1)*1
  Table2[country,5]<- round(pnorm(am_test,lower.tail=F),3)
  Table3[country,5]<- mean(loss_b)/mean(loss_a)
  Table4[country,5]<- (dm.test(loss_a,loss_b,power=1,alternative = "greater"))$statistic/am_test
}


Table[1,2]<-round(mean(Table1[,1]),3)*100
Table[1,3]<-round(mean(Table1[,2]),3)*100
Table[1,4]<-round(mean(Table1[,3]),3)*100
Table[1,5]<-round(mean(Table1[,4]),3)*100
Table[1,6]<-round(mean(Table1[,5]),3)*100

Table[2,2]<-round(mean(Table1[idx1_R1$Country,1]),3)*100
Table[2,3]<-round(mean(Table1[idx1_R1$Country,2]),3)*100
Table[2,4]<-round(mean(Table1[idx1_R1$Country,3]),3)*100
Table[2,5]<-round(mean(Table1[idx1_R1$Country,4]),3)*100
Table[2,6]<-round(mean(Table1[idx1_R1$Country,5]),3)*100

Table[3,2]<-round(mean(Table1[idx1_R2$Country,1]),3)*100
Table[3,3]<-round(mean(Table1[idx1_R2$Country,2]),3)*100
Table[3,4]<-round(mean(Table1[idx1_R2$Country,3]),3)*100
Table[3,5]<-round(mean(Table1[idx1_R2$Country,4]),3)*100
Table[3,6]<-round(mean(Table1[idx1_R2$Country,5]),3)*100

Table[4,2]<-round(mean(Table1[idx1_R3$Country,1]),3)*100
Table[4,3]<-round(mean(Table1[idx1_R3$Country,2]),3)*100
Table[4,4]<-round(mean(Table1[idx1_R3$Country,3]),3)*100
Table[4,5]<-round(mean(Table1[idx1_R3$Country,4]),3)*100
Table[4,6]<-round(mean(Table1[idx1_R3$Country,5]),3)*100

Table[5,2]<-round(mean(Table1[idx1_R4$Country,1]),3)*100
Table[5,3]<-round(mean(Table1[idx1_R4$Country,2]),3)*100
Table[5,4]<-round(mean(Table1[idx1_R4$Country,3]),3)*100
Table[5,5]<-round(mean(Table1[idx1_R4$Country,4]),3)*100
Table[5,6]<-round(mean(Table1[idx1_R4$Country,5]),3)*100

Table[6,2]<-round(mean(Table1[idx1_R5$Country,1]),3)*100
Table[6,3]<-round(mean(Table1[idx1_R5$Country,2]),3)*100
Table[6,4]<-round(mean(Table1[idx1_R5$Country,3]),3)*100
Table[6,5]<-round(mean(Table1[idx1_R5$Country,4]),3)*100
Table[6,6]<-round(mean(Table1[idx1_R5$Country,5]),3)*100

Table[7,2]<-round(mean(Table1[idx1_R6$Country,1]),3)*100
Table[7,3]<-round(mean(Table1[idx1_R6$Country,2]),3)*100
Table[7,4]<-round(mean(Table1[idx1_R6$Country,3]),3)*100
Table[7,5]<-round(mean(Table1[idx1_R6$Country,4]),3)*100
Table[7,6]<-round(mean(Table1[idx1_R6$Country,5]),3)*100

Table[8,2]<-round(mean(Table1[idx1_R7$Country,1]),3)*100
Table[8,3]<-round(mean(Table1[idx1_R7$Country,2]),3)*100
Table[8,4]<-round(mean(Table1[idx1_R7$Country,3]),3)*100
Table[8,5]<-round(mean(Table1[idx1_R7$Country,4]),3)*100
Table[8,6]<-round(mean(Table1[idx1_R7$Country,5]),3)*100


################################################################################

africa<-groups[groups[,2]==1,1]$Country # Africa
america<-groups[groups[,3]==1,1]$Country # America
asia<-groups[groups[,4]==1,1]$Country # Asia and Oceania
europe<-groups[groups[,5]==1,1]$Country # Europe

h<-1

names<-c(europe,america,asia,africa)

Table1<-cbind(rownames(Table1),Table1)
colnames(Table1)<-c("country","q=0.05","q=0.25","q=0.50","q=0.75","q=0.95")
Table1$country <- factor(Table1$country,levels=(Table1$country)[order(Table1$country,decreasing = T)])


f1<-Table1 %>% filter(country %in% europe) %>% 
  pivot_longer(names_to="variable",values_to="value",cols=c(-country)) %>% 
  mutate(value=case_when(value == 1 ~ 'Rejection',
                         value == 0 ~ 'Non-rejection')) %>% 
  ggplot(aes(x=variable, y=country, fill= as.factor(value)))+
  geom_tile() +
  scale_fill_manual(values = c("white","lightskyblue"),name="P. value<0.10")+
  ylab("") +
  xlab(expression("")) + 
  labs(title=paste0("Europe"),
       caption = "") +
  theme_bw()

f2<-Table1  %>% filter(country %in% america) %>% 
  pivot_longer(names_to="variable",values_to="value",cols=c(-country)) %>% 
  mutate(value=case_when(value == 1 ~ 'Rejection',
                         value == 0 ~ 'Non-rejection')) %>% 
  ggplot(aes(x=variable, y=country, fill= as.factor(value)))+
  geom_tile() +
  scale_fill_manual(values = c("white","lightskyblue"),name="P. value<0.10")+
  ylab("") +
  xlab(expression("")) + 
  labs(title=paste0("America"),
       caption = "") +
  theme_bw()


f4<-Table1  %>% filter(country %in% africa) %>% 
  pivot_longer(names_to="variable",values_to="value",cols=c(-country)) %>% 
  mutate(value=case_when(value == 1 ~ 'Rejection',
                         value == 0 ~ 'Non-rejection')) %>% 
  ggplot(aes(x=variable, y=country, fill= as.factor(value)))+
  geom_tile() +
  scale_fill_manual(values = c("white","lightskyblue"),name="P. value<0.10")+
  ylab("") +
  xlab(expression("")) + 
  labs(title=paste0("Africa"),
       caption = "") +
  theme_bw()

f5<-Table1  %>% filter(country %in% asia) %>% 
  pivot_longer(names_to="variable",values_to="value",cols=c(-country)) %>% 
  mutate(value=case_when(value == 1 ~ 'Rejection',
                         value == 0 ~ 'Non-rejection')) %>% 
  ggplot(aes(x=variable, y=country, fill= as.factor(value)))+
  geom_tile() +
  scale_fill_manual(values = c("white","lightskyblue"),name="P. value<0.10")+
  ylab("") +
  xlab(expression("")) + 
  labs(title=paste0("Asia and Oceania"),
       caption = "") +
  theme_bw()


ggsave(paste0("../output/Figures/Figure_13.png"), 
       ggarrange(f1,f4,f5,f2,legend = "bottom",nrow = 2,ncol = 2,
                 common.legend = T),
       width = 8, height = 10)

