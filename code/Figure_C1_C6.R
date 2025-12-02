
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

modb<-loadRData(file = paste0("../output/Data/list_h1_mb.RData"))
mod1<-loadRData(file = paste0("../output/Data/list_h1_m",1,"rr.RData"))
mod2<-loadRData(file = paste0("../output/Data/list_h1_m",2,".RData"))
mod3<-loadRData(file = paste0("../output/Data/list_h1_m",3,".RData"))
mod4<-loadRData(file = paste0("../output/Data/list_h1_m",4,".RData"))
mod5<-loadRData(file = paste0("../output/Data/list_h1_uncon.RData"))
benchmark<-loadRData(file = paste0("../output/Data/list_h1_bench.RData"))


for (metric in c("CRPS-equal","CRPS-left","CRPS-right","QS(0.05)",
                 "QS(0.50)","QS(0.95)")){
  
  # Forecast CRPS equal
  table_f<-data_frame(c())
  table_b<-data_frame(c())
  table_1<-data_frame(c())
  table_2<-data_frame(c())
  table_3<-data_frame(c())
  table_4<-data_frame(c())
  table_5<-data_frame(c())
  
  for (i in names){
    if (metric=="CRPS-equal"){
      
      table_b=rbind(table_b,mean(Reduce(rbind,lapply(modb[[i]], function(x)mean(head(x$crps_equal)))))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_equal))))))
      table_1=rbind(table_1,mean(Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_equal)))))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_equal))))))
      table_2=rbind(table_2,mean(Reduce(rbind,lapply(mod2[[i]], function(x)mean(head(x$crps_equal)))))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_equal))))))
      table_3=rbind(table_3,mean(Reduce(rbind,lapply(mod3[[i]], function(x)mean(head(x$crps_equal)))))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_equal))))))
      table_4=rbind(table_4,mean(Reduce(rbind,lapply(mod4[[i]], function(x)mean(head(x$crps_equal)))))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_equal))))))
      table_5=rbind(table_5,mean(Reduce(rbind,lapply(mod5[[i]], function(x)mean(head(x$crps_equal)))))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_equal))))))
    } else if (metric=="CRPS-left"){
      table_b=rbind(table_b,mean(Reduce(rbind,lapply(modb[[i]], function(x)mean(head(x$crps_left)))))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_left))))))
      table_1=rbind(table_1,mean(Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_left)))))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_left))))))
      table_2=rbind(table_2,mean(Reduce(rbind,lapply(mod2[[i]], function(x)mean(head(x$crps_left)))))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_left))))))
      table_3=rbind(table_3,mean(Reduce(rbind,lapply(mod3[[i]], function(x)mean(head(x$crps_left)))))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_left))))))
      table_4=rbind(table_4,mean(Reduce(rbind,lapply(mod4[[i]], function(x)mean(head(x$crps_left)))))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_left))))))
      table_5=rbind(table_5,mean(Reduce(rbind,lapply(mod5[[i]], function(x)mean(head(x$crps_left)))))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_left))))))
      
    }else if (metric=="CRPS-right"){
      table_b=rbind(table_b,mean(Reduce(rbind,lapply(modb[[i]], function(x)mean(head(x$crps_right)))))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_right))))))
      table_1=rbind(table_1,mean(Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_right)))))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_right))))))
      table_2=rbind(table_2,mean(Reduce(rbind,lapply(mod2[[i]], function(x)mean(head(x$crps_right)))))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_right))))))
      table_3=rbind(table_3,mean(Reduce(rbind,lapply(mod3[[i]], function(x)mean(head(x$crps_right)))))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_right))))))
      table_4=rbind(table_4,mean(Reduce(rbind,lapply(mod4[[i]], function(x)mean(head(x$crps_right)))))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_right))))))
      table_5=rbind(table_5,mean(Reduce(rbind,lapply(mod5[[i]], function(x)mean(head(x$crps_right)))))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_right))))))
      
    }else if (metric=="QS(0.05)"){
      table_b=rbind(table_b,mean(Reduce(rbind,lapply(modb[[i]], function(x)x$qs[1])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[1]))))
      table_1=rbind(table_1,mean(Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[1])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[1]))))
      table_2=rbind(table_2,mean(Reduce(rbind,lapply(mod2[[i]], function(x)x$qs[1])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[1]))))
      table_3=rbind(table_3,mean(Reduce(rbind,lapply(mod3[[i]], function(x)x$qs[1])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[1]))))
      table_4=rbind(table_4,mean(Reduce(rbind,lapply(mod4[[i]], function(x)x$qs[1])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[1]))))
      table_5=rbind(table_5,mean(Reduce(rbind,lapply(mod5[[i]], function(x)x$qs[1])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[1]))))
      
    }else if (metric=="QS(0.25)"){
      table_b=rbind(table_b,mean(Reduce(rbind,lapply(modb[[i]], function(x)x$qs[2])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[2]))))
      table_1=rbind(table_1,mean(Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[2])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[2]))))
      table_2=rbind(table_2,mean(Reduce(rbind,lapply(mod2[[i]], function(x)x$qs[2])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[2]))))
      table_3=rbind(table_3,mean(Reduce(rbind,lapply(mod3[[i]], function(x)x$qs[2])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[2]))))
      table_4=rbind(table_4,mean(Reduce(rbind,lapply(mod4[[i]], function(x)x$qs[2])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[2]))))
      table_5=rbind(table_5,mean(Reduce(rbind,lapply(mod5[[i]], function(x)x$qs[2])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[2]))))
      
    }else if (metric=="QS(0.50)"){
      table_b=rbind(table_b,mean(Reduce(rbind,lapply(modb[[i]], function(x)x$qs[3])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[3]))))
      table_1=rbind(table_1,mean(Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[3])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[3]))))
      table_2=rbind(table_2,mean(Reduce(rbind,lapply(mod2[[i]], function(x)x$qs[3])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[3]))))
      table_3=rbind(table_3,mean(Reduce(rbind,lapply(mod3[[i]], function(x)x$qs[3])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[3]))))
      table_4=rbind(table_4,mean(Reduce(rbind,lapply(mod4[[i]], function(x)x$qs[3])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[3]))))
      table_5=rbind(table_5,mean(Reduce(rbind,lapply(mod5[[i]], function(x)x$qs[3])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[3]))))
      
    }else if (metric=="QS(0.75)"){
      table_b=rbind(table_b,mean(Reduce(rbind,lapply(modb[[i]], function(x)x$qs[4])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[4]))))
      table_1=rbind(table_1,mean(Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[4])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[4]))))
      table_2=rbind(table_2,mean(Reduce(rbind,lapply(mod2[[i]], function(x)x$qs[4])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[4]))))
      table_3=rbind(table_3,mean(Reduce(rbind,lapply(mod3[[i]], function(x)x$qs[4])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[4]))))
      table_4=rbind(table_4,mean(Reduce(rbind,lapply(mod4[[i]], function(x)x$qs[4])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[4]))))
      table_5=rbind(table_5,mean(Reduce(rbind,lapply(mod5[[i]], function(x)x$qs[4])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[4]))))
      
    }else if (metric=="QS(0.95)"){
      table_b=rbind(table_b,mean(Reduce(rbind,lapply(modb[[i]], function(x)x$qs[5])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[5]))))
      table_1=rbind(table_1,mean(Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[5])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[5]))))
      table_2=rbind(table_2,mean(Reduce(rbind,lapply(mod2[[i]], function(x)x$qs[5])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[5]))))
      table_3=rbind(table_3,mean(Reduce(rbind,lapply(mod3[[i]], function(x)x$qs[5])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[5]))))
      table_4=rbind(table_4,mean(Reduce(rbind,lapply(mod4[[i]], function(x)x$qs[5])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[5]))))
      table_5=rbind(table_5,mean(Reduce(rbind,lapply(mod5[[i]], function(x)x$qs[5])))/
                      mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[5]))))
    }
  }
  colnames(table_b)<-metric
  colnames(table_1)<-metric
  colnames(table_2)<-metric
  colnames(table_3)<-metric
  colnames(table_4)<-metric
  colnames(table_5)<-metric
  table_b<-cbind("Country"=names,"Region"=geo_group,table_b)
  table_1<-cbind("Country"=names,"Region"=geo_group,table_1)
  table_2<-cbind("Country"=names,"Region"=geo_group,table_2)
  table_3<-cbind("Country"=names,"Region"=geo_group,table_3)
  table_4<-cbind("Country"=names,"Region"=geo_group,table_4)
  table_5<-cbind("Country"=names,"Region"=geo_group,table_5)
  
  table_f<-rbind(cbind("Model"=rep("MB",115),table_b),
                 cbind("Model"=rep("M1",115),table_1),
                 cbind("Model"=rep("M2",115),table_2),
                 cbind("Model"=rep("M3",115),table_3),
                 cbind("Model"=rep("M4",115),table_4),
                 cbind("Model"=rep("M5",115),table_5))
  
  
  if (metric=="CRPS-equal") table_f[table_f$`CRPS-equal`>2,"CRPS-equal"]<-2
  if (metric=="CRPS-left") table_f[table_f$`CRPS-left`>2,"CRPS-left"]<-2
  if (metric=="CRPS-right") table_f[table_f$`CRPS-right`>2,"CRPS-right"]<-2
  if (metric=="QS(0.05)") table_f[table_f$`QS(0.05)`>2,"QS(0.05)"]<-2
  if (metric=="QS(0.25)") table_f[table_f$`QS(0.25)`>2,"QS(0.25)"]<-2
  if (metric=="QS(0.50)") table_f[table_f$`QS(0.50)`>2,"QS(0.50)"]<-2
  if (metric=="QS(0.75)") table_f[table_f$`QS(0.75)`>2,"QS(0.75)"]<-2
  if (metric=="QS(0.95)") table_f[table_f$`QS(0.95)`>2,"QS(0.95)"]<-2
  
  #openxlsx::write.xlsx(data.frame(table_f),paste0(paste0("../Temp/crps_equal_h_1.xlsx")))
  
  g1<- ggplot() + 
    geom_point(aes(x=table_f[table_f$Region=="Africa","Country"],
                   y=table_f[table_f$Region=="Africa",metric],
                   shape=table_f[table_f$Region=="Africa","Model"],
                   col=table_f[table_f$Region=="Africa","Model"]), size=2) +   # Draw points
    geom_hline(yintercept = 1,linetype = 2)+
    labs(title="Africa",x="",y=metric,shape = "Model",col="Model") +  
    theme_bw()+
    ylim(0.5, 2)+
    theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
  
  g2<-ggplot() + 
    geom_point(aes(x=table_f[table_f$Region=="America","Country"],
                   y=table_f[table_f$Region=="America",metric],
                   shape=table_f[table_f$Region=="America","Model"],
                   col=table_f[table_f$Region=="America","Model"]), size=2) +   # Draw points
    geom_hline(yintercept = 1,linetype = 2)+
    labs(title="America",x="",y=metric,shape = "Model",col="Model") +  
    theme_bw()+
    ylim(0.5, 2)+
    theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
  
  g3<-ggplot() + 
    geom_point(aes(x=table_f[table_f$Region=="Asia and Oceania","Country"],
                   y=table_f[table_f$Region=="Asia and Oceania",metric],
                   shape=table_f[table_f$Region=="Asia and Oceania","Model"],
                   col=table_f[table_f$Region=="Asia and Oceania","Model"]), size=2) +   # Draw points
    geom_hline(yintercept = 1,linetype = 2)+
    labs(title="Asia and Oceania",x="",y=metric,shape = "Model",col="Model") +  
    theme_bw()+
    ylim(0.5, 2)+
    theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
  
  g4<-ggplot() + 
    geom_point(aes(x=table_f[table_f$Region=="Europe","Country"],
                   y=table_f[table_f$Region=="Europe",metric],
                   shape=table_f[table_f$Region=="Europe","Model"],
                   col=table_f[table_f$Region=="Europe","Model"]), size=2) +   # Draw points
    geom_hline(yintercept = 1,linetype = 2)+
    labs(title="Europe",x="",y=metric,shape = "Model",col="Model") +  
    theme_bw()+
    ylim(0.5, 2)+
    theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
  
  if (metric=="QS(0.05)") dir = "C1"
  if (metric=="QS(0.50)") dir = "C2"
  if (metric=="QS(0.95)") dir = "C3"
  if (metric=="CRPS-equal") dir = "C4"
  if (metric=="CRPS-left") dir = "C5"
  if (metric=="CRPS-right") dir = "C6"
      
  ggsave(paste0("../output/Figures/Figure_",dir,".png"), 
         ggarrange(g1,g2,g3,g4,legend = "bottom",nrow = 2,ncol = 2,
                   common.legend = T),
         width = 10, height = 8)
  
}   