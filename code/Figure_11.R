
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

# list_h1_bench<-loadRData("../Data/list_h1_bench.RData")
# list_h1_m1<-loadRData("../Data/list_h1_m1.RData")
# list_h1_m2<-loadRData("../Data/list_h1_m2.RData")
# list_h1_m3<-loadRData("../Data/list_h1_m3.RData")
# list_h1_m4<-loadRData("../Data/list_h1_m4.RData")

date <- (data %>%  filter(country=="United States"))[-1,6]
date<-as.Date(date$date)
groups <- readxl::read_excel("../input/structure_dfm.xlsx")


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


names<-c(idx1_R1$Country,idx1_R2$Country,idx1_R3$Country,idx1_R4$Country)


mod1<-loadRData(file = paste0("../output/Data/list_h1_m",1,"rr.RData"))
benchmark<-loadRData(file = paste0("../output/Data/list_h1_bench.RData"))


bannerCommenter::banner("Ratios figures")
##################################################################
##                        Ratios figures                        ##
##################################################################


# Forecast CRPS equal
table_f<-data_frame(c())
table_1<-data_frame(c())
table_2<-data_frame(c())
table_3<-data_frame(c())
table_4<-data_frame(c())
table_5<-data_frame(c())
table_6<-data_frame(c())

for (i in names){
  table_1=rbind(table_1,mean(Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_equal)))))/
                  mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_equal))))))
  
  table_2=rbind(table_2,mean(Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_left)))))/
                  mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_left))))))
  
  table_3=rbind(table_3,mean(Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_right)))))/
                  mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_right))))))
  
  table_4=rbind(table_4,mean(Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[1])))/
                  mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[1]))))
  
  table_5=rbind(table_5,mean(Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[3])))/
                  mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[3]))))
  
  table_6=rbind(table_6,mean(Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[5])))/
                  mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[5]))))
  
}   
colnames(table_1)<-"value"
colnames(table_2)<-"value"
colnames(table_3)<-"value"
colnames(table_4)<-"value"
colnames(table_5)<-"value"
colnames(table_6)<-"value"
table_1<-cbind("Country"=names,"Region"=geo_group,"Metric"=rep("CRPS-equal",115),table_1)
table_2<-cbind("Country"=names,"Region"=geo_group,"Metric"=rep("CRPS-left",115),table_2)
table_3<-cbind("Country"=names,"Region"=geo_group,"Metric"=rep("CRPS-right",115),table_3)
table_4<-cbind("Country"=names,"Region"=geo_group,"Metric"=rep("QS(0.05)",115),table_4)
table_5<-cbind("Country"=names,"Region"=geo_group,"Metric"=rep("QS(0.50)",115),table_5)
table_6<-cbind("Country"=names,"Region"=geo_group,"Metric"=rep("QS(0.95)",115),table_6)

table_f<-rbind(table_1,table_2,table_3,table_4,table_5,table_6)


g1<- ggplot() + 
  geom_point(aes(x=table_f[table_f$Region=="Africa","Country"],
                 y=table_f[table_f$Region=="Africa","value"],
                 shape=table_f[table_f$Region=="Africa","Metric"],
                 col=table_f[table_f$Region=="Africa","Metric"]), size=2) +   # Draw points
  geom_hline(yintercept = 1,linetype = 2)+
  labs(title="a) Africa",x="",y="Ratio",shape = "Metric",col="Metric") +  
  theme_bw()+
  ylim(0.5, 1.5)+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))

g2<-ggplot() + 
  geom_point(aes(x=table_f[table_f$Region=="America","Country"],
                 y=table_f[table_f$Region=="America","value"],
                 shape=table_f[table_f$Region=="America","Metric"],
                 col=table_f[table_f$Region=="America","Metric"]), size=2) +   # Draw points
  geom_hline(yintercept = 1,linetype = 2)+
  labs(title="b) America",x="",y="Ratio",shape = "Metric",col="Metric") +  
  theme_bw()+
  ylim(0.5, 1.5)+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))

g3<-ggplot() + 
  geom_point(aes(x=table_f[table_f$Region=="Asia and Oceania","Country"],
                 y=table_f[table_f$Region=="Asia and Oceania","value"],
                 shape=table_f[table_f$Region=="Asia and Oceania","Metric"],
                 col=table_f[table_f$Region=="Asia and Oceania","Metric"]), size=2) +   # Draw points
  geom_hline(yintercept = 1,linetype = 2)+
  labs(title="c) Asia and Oceania",x="",y="Ratio",shape = "Metric",col="Metric") +  
  theme_bw()+
  ylim(0.5, 1.5)+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))

g4<-ggplot() + 
  geom_point(aes(x=table_f[table_f$Region=="Europe","Country"],
                 y=table_f[table_f$Region=="Europe","value"],
                 shape=table_f[table_f$Region=="Europe","Metric"],
                 col=table_f[table_f$Region=="Europe","Metric"]), size=2) +   # Draw points
  geom_hline(yintercept = 1,linetype = 2)+
  labs(title="d) Europe",x="",y="Ratio",shape = "Metric",col="Metric") +  
  theme_bw()+
  ylim(0.5, 1.5)+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))

ggsave(paste0("../output/Figures/Figure_11.png"), 
       ggarrange(g1,g2,g3,g4,legend = "bottom",nrow = 2,ncol = 2,
                 common.legend = T),
       width = 10, height = 8)
