rm(list = ls()) # Limpiar environment
library(tidyverse)
library(ggpubr) # ggarrrange
library(kableExtra) # latex tables
library(openxlsx)
library(forecast)
library(forcats)


source("functions/ThreeLevelMDFM.R") # CCA


#################################################################
##                         Import data                         ##
#################################################################
data <- readxl::read_excel("../input/Data_complete_rev.xlsx")
Yorig <- readxl::read_excel("../input/X.xlsx")
groups <- readxl::read_excel("../input/structure_dfm.xlsx")

#Rcheck <- spca(scale(Yorig))

R1 <- ThreeMDFM1(Yorig,groups,spec=1)

R1$Factors_Inflation <- R1$Factors_Inflation %*% diag(c(-1,1,-1,-1,-1,-1,1,1))
R1$Lam_Inflation <- R1$Lam_Inflation %*% diag(c(-1,1,-1,-1,-1,-1,1,1))
idiosyncratic<-R1$Residuals

# MSE CI
t<-nrow(idiosyncratic)
N<-ncol(idiosyncratic)
PP<- solve((t(R1$Lam_Inflation) %*% R1$Lam_Inflation)/N)
# Assumption uncorrelated idiosincratic
sigma_e<-sum(diag(t(R1$Residuals) %*% R1$Residuals)/(N*t)) 
Gamma<-sigma_e * (t(R1$Lam_Inflation) %*% R1$Lam_Inflation)/N
SD<-sqrt(diag(PP %*% (Gamma %*% PP))/N)



loadings<-R1$Lam_Inflation
colnames(loadings)<-c("Global","Africa","America",
                      "Asia and Oceania","Europe","Advanced",
                      "HMI-EMDEs","LI-EMDEs") 

loadings<-loadings[colnames(idiosyncratic),] # It is necessary to reorganize

rownames(loadings)==colnames(idiosyncratic) # Check

Loading.New<-c()
for (i in 1:115) {
  Loading.New<-rbind(Loading.New,loadings[i,which(loadings[i,]!=0)])
}
rownames(Loading.New)<-rownames(loadings)
colnames(Loading.New)<-c("Global","Region","Income")

N<-nrow(Loading.New)
Loads.ConInt<-matrix(0,N,3)
list.Loads.ConInt<-list(Loads.ConInt,Loads.ConInt,Loads.ConInt)
for (ll in 1:length(list.Loads.ConInt)) {
  for (ii in 1:N) {
    list.Loads.ConInt[[ll]][ii,1]<--sqrt(var(idiosyncratic[,ii])/nrow(idiosyncratic))*2+Loading.New[ii,ll]
    list.Loads.ConInt[[ll]][ii,2]<-Loading.New[ii,ll]
    list.Loads.ConInt[[ll]][ii,3]<-sqrt(var(idiosyncratic[,ii])/nrow(idiosyncratic))*2+Loading.New[ii,ll]
  }
}



region<-c(rep("Africa",33),rep("America",21),rep("Asia and Oceania",22),rep("Europe",39))

Big.loadings<-cbind("Factor"=rep("Global",115),region,"Country"=colnames(idiosyncratic),data.frame(list.Loads.ConInt[[1]]))
for (i in 2:3) {
  Big.loadings<-rbind(Big.loadings,cbind("Factor"=rep(colnames(Loading.New)[i],115),region,"Country"=colnames(idiosyncratic),data.frame(list.Loads.ConInt[[i]])))
}

for (i in 1:nrow(Big.loadings)){
  name<-Big.loadings[i,"Country"]
  if(Big.loadings[i,"Factor"]=="Global"){
    Big.loadings[i,"Income"]<-"Global"
  }else if(Big.loadings[i,"Factor"]=="Region"){
    Big.loadings[i,"Income"]<-"Region"
  }else if(Big.loadings[i,"Factor"]=="Income"){
    Big.loadings[i,"Income"]<-names(loadings[name,which(loadings[name,]!=0)])[3]
  }
}

unique(Big.loadings[,"Factor"])

unique(Big.loadings$Income)
colnames(Big.loadings)<-c("Factor","Region","Country","Lower bound","Loading","Upper bound","Income")


g1<-Big.loadings %>% filter(Region=="Africa") %>% 
  ggplot(aes(x=fct_rev(Country), y=Loading,fill=Income)) +
  geom_bar(  stat="identity", alpha=0.7) +
  geom_hline(yintercept = 0,col="red")+
  geom_errorbar( aes(x=fct_rev(Country), ymin=`Lower bound`, ymax=`Upper bound`), width=0.5, colour="black", alpha=1, size=0.2)+
  facet_wrap(vars(Factor))+
  coord_flip()+
  theme_bw()+
  theme(legend.position="bottom")+
  scale_fill_manual("", values = c("Advanced" = "royalblue1", "HMI-EMDEs" = "green4","LI-EMDEs"="orange"))+
  labs(colour = paste0(""),title = "a) Africa")+ylab("Loadings")+xlab(NULL)

g2<-Big.loadings %>% filter(Region=="America") %>% 
  ggplot(aes(x=fct_rev(Country), y=Loading,fill=Income)) +
  geom_bar(  stat="identity", alpha=0.7) +
  geom_hline(yintercept = 0,col="red")+
  geom_errorbar( aes(x=fct_rev(Country), ymin=`Lower bound`, ymax=`Upper bound`), width=0.5, colour="black", alpha=1, size=0.2)+
  facet_wrap(vars(Factor))+
  coord_flip()+
  theme_bw()+
  theme(legend.position="bottom")+
  scale_fill_manual("", values = c("Advanced" = "royalblue1", "HMI-EMDEs" = "green4","LI-EMDEs"="orange"))+
  labs(colour = paste0(""),title = "b) America")+ylab("Loadings")+xlab(NULL)


g3<-Big.loadings %>% filter(Region=="Asia and Oceania") %>% 
  ggplot(aes(x=fct_rev(Country), y=Loading,fill=Income)) +
  geom_bar(  stat="identity", alpha=0.7) +
  geom_hline(yintercept = 0,col="red")+
  geom_errorbar( aes(x=fct_rev(Country), ymin=`Lower bound`, ymax=`Upper bound`), width=0.5, colour="black", alpha=1, size=0.2)+
  facet_wrap(vars(Factor))+
  coord_flip()+
  theme_bw()+
  theme(legend.position="bottom")+
  scale_fill_manual("", values = c("Advanced" = "royalblue1", "HMI-EMDEs" = "green4","LI-EMDEs"="orange"))+
  labs(colour = paste0(""),title = "c) Asia and Oceania")+ylab("Loadings")+xlab(NULL)


g4<-Big.loadings %>% filter(Region=="Europe") %>% 
  ggplot(aes(x=fct_rev(Country), y=Loading,fill=Income)) +
  geom_bar(  stat="identity", alpha=0.7) +
  geom_hline(yintercept = 0,col="red")+
  geom_errorbar( aes(x=fct_rev(Country), ymin=`Lower bound`, ymax=`Upper bound`), width=0.5, colour="black", alpha=1, size=0.2)+
  facet_wrap(vars(Factor))+
  coord_flip()+
  theme_bw()+
  theme(legend.position="bottom")+
  scale_fill_manual("", values = c("Advanced" = "royalblue1", "HMI-EMDEs" = "green4","LI-EMDEs"="orange"))+
  labs(colour = paste0(""),title = "d) Europe")+ylab("Loadings")+xlab(NULL)


ggsave(paste0("../output/Figures/Figure_B3.png"),
       ggarrange(g1,g2,g3,g4,ncol = 2,nrow = 2,common.legend = T,legend = "bottom"), width = 12, height = 12)
