
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


#################################################################
##                        Tables                               ##
#################################################################


Table<-data.frame(matrix(nrow = 8,ncol=7))
Table[,1]<-c("Global","Africa","America","Asia and Oceania","Europe","Advance","HMI","LI")
colnames(Table)<-c("","QS(0.05)","QS(0.50)","QS(0.95)","CRPS-E", "CRPS-L", "CRPS-R")

Table1<-list()
for (j in 1){
  Table1[[j]]<-data.frame(matrix(nrow = 115,6))
  row.names(Table1[[j]])<-names
}
Table2<-Table1
j<-1

for (i in names){
  Table1[[j]][i,1] = ((dm.test(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[1])),
                               Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[1])),
                               alternative=c("greater"),power=1)$p.value)<0.1)*1
  Table1[[j]][i,2] = ((dm.test(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[3])),
                               Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[3])),
                               alternative=c("greater"),power=1)$p.value)<0.1)*1
  Table1[[j]][i,3] = ((dm.test(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[5])),
                               Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[5])),
                               alternative=c("greater"),power=1)$p.value)<0.1)*1
  Table1[[j]][i,4] = ((dm.test(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_equal)))),
                               Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_equal)))),
                               alternative=c("greater"),power=1)$p.value)<0.1)*1
  Table1[[j]][i,5] = ((dm.test(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_left)))),
                               Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_left)))),
                               alternative=c("greater"),power=1)$p.value)<0.1)*1
  Table1[[j]][i,6] = ((dm.test(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_right)))),
                               Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_right)))),
                               alternative=c("greater"),power=1)$p.value)<0.1)*1
  
  # Scores
  Table2[[j]][i,1] = round(mean(Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[1])))/
                             mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[1]))),3)
  Table2[[j]][i,2] = round(mean(Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[3])))/
                             mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[3]))),3)
  Table2[[j]][i,3] = round(mean(Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[5])))/
                             mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[5]))),3)
  
  Table2[[j]][i,4] = round(mean(Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_equal)))))/
                             mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_equal))))),3)
  Table2[[j]][i,5] = round(mean(Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_left)))))/
                             mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_left))))),3)
  Table2[[j]][i,6] = round(mean(Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_right)))))/
                             mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_right))))),3)
  
}
colnames(Table1[[1]])<-c("QS(0.05)-DMpval<0.10","QS(0.50)-DMpval<0.10","QS(0.95)-DMpval<0.10","CRPS-E-DMpval<0.10", "CRPS-L-DMpval<0.10", "CRPS-R-DMpval<0.10")
colnames(Table2[[1]])<-c("QS(0.05)","QS(0.50)","QS(0.95)","CRPS-E", "CRPS-L", "CRPS-R")


Table[1,2]<-round(mean(Table1[[j]][,1]),3)*100
Table[1,3]<-round(mean(Table1[[j]][,2]),3)*100
Table[1,4]<-round(mean(Table1[[j]][,3]),3)*100
Table[1,5]<-round(mean(Table1[[j]][,4]),3)*100
Table[1,6]<-round(mean(Table1[[j]][,5]),3)*100
Table[1,7]<-round(mean(Table1[[j]][,6]),3)*100

Table[2,2]<-round(mean(Table1[[j]][idx1_R1$Country,1]),3)*100
Table[2,3]<-round(mean(Table1[[j]][idx1_R1$Country,2]),3)*100
Table[2,4]<-round(mean(Table1[[j]][idx1_R1$Country,3]),3)*100
Table[2,5]<-round(mean(Table1[[j]][idx1_R1$Country,4]),3)*100
Table[2,6]<-round(mean(Table1[[j]][idx1_R1$Country,5]),3)*100
Table[2,7]<-round(mean(Table1[[j]][idx1_R1$Country,6]),3)*100

Table[3,2]<-round(mean(Table1[[j]][idx1_R2$Country,1]),3)*100
Table[3,3]<-round(mean(Table1[[j]][idx1_R2$Country,2]),3)*100
Table[3,4]<-round(mean(Table1[[j]][idx1_R2$Country,3]),3)*100
Table[3,5]<-round(mean(Table1[[j]][idx1_R2$Country,4]),3)*100
Table[3,6]<-round(mean(Table1[[j]][idx1_R2$Country,5]),3)*100
Table[3,7]<-round(mean(Table1[[j]][idx1_R2$Country,6]),3)*100

Table[4,2]<-round(mean(Table1[[j]][idx1_R3$Country,1]),3)*100
Table[4,3]<-round(mean(Table1[[j]][idx1_R3$Country,2]),3)*100
Table[4,4]<-round(mean(Table1[[j]][idx1_R3$Country,3]),3)*100
Table[4,5]<-round(mean(Table1[[j]][idx1_R3$Country,4]),3)*100
Table[4,6]<-round(mean(Table1[[j]][idx1_R3$Country,5]),3)*100
Table[4,7]<-round(mean(Table1[[j]][idx1_R3$Country,6]),3)*100

Table[5,2]<-round(mean(Table1[[j]][idx1_R4$Country,1]),3)*100
Table[5,3]<-round(mean(Table1[[j]][idx1_R4$Country,2]),3)*100
Table[5,4]<-round(mean(Table1[[j]][idx1_R4$Country,3]),3)*100
Table[5,5]<-round(mean(Table1[[j]][idx1_R4$Country,4]),3)*100
Table[5,6]<-round(mean(Table1[[j]][idx1_R4$Country,5]),3)*100
Table[5,7]<-round(mean(Table1[[j]][idx1_R4$Country,6]),3)*100

Table[6,2]<-round(mean(Table1[[j]][idx1_R5$Country,1]),3)*100
Table[6,3]<-round(mean(Table1[[j]][idx1_R5$Country,2]),3)*100
Table[6,4]<-round(mean(Table1[[j]][idx1_R5$Country,3]),3)*100
Table[6,5]<-round(mean(Table1[[j]][idx1_R5$Country,4]),3)*100
Table[6,6]<-round(mean(Table1[[j]][idx1_R5$Country,5]),3)*100
Table[6,7]<-round(mean(Table1[[j]][idx1_R5$Country,6]),3)*100

Table[7,2]<-round(mean(Table1[[j]][idx1_R6$Country,1]),3)*100
Table[7,3]<-round(mean(Table1[[j]][idx1_R6$Country,2]),3)*100
Table[7,4]<-round(mean(Table1[[j]][idx1_R6$Country,3]),3)*100
Table[7,5]<-round(mean(Table1[[j]][idx1_R6$Country,4]),3)*100
Table[7,6]<-round(mean(Table1[[j]][idx1_R6$Country,5]),3)*100
Table[7,7]<-round(mean(Table1[[j]][idx1_R6$Country,6]),3)*100

Table[8,2]<-round(mean(Table1[[j]][idx1_R7$Country,1]),3)*100
Table[8,3]<-round(mean(Table1[[j]][idx1_R7$Country,2]),3)*100
Table[8,4]<-round(mean(Table1[[j]][idx1_R7$Country,3]),3)*100
Table[8,5]<-round(mean(Table1[[j]][idx1_R7$Country,4]),3)*100
Table[8,6]<-round(mean(Table1[[j]][idx1_R7$Country,5]),3)*100
Table[8,7]<-round(mean(Table1[[j]][idx1_R7$Country,6]),3)*100


colnames(Table)<-c("","QS(0.05)","QS(0.50)","QS(0.95)","CRPS-E", "CRPS-L", "CRPS-R")

TableDM <- data.frame(cbind(rownames(Table1[[1]]),Table1[[1]],Table2[[1]]))

africa<-groups[groups[,2]==1,1]$Country # Africa
america<-groups[groups[,3]==1,1]$Country # America
asia<-groups[groups[,4]==1,1]$Country # Asia and Oceania
europe<-groups[groups[,5]==1,1]$Country # Europe
h<-1


TableDM<-TableDM[,c(1,2,3,4,5,6,7)]
colnames(TableDM)<-c("country","q=0.05","q=0.50","q=0.95","CRPS-E","CRPS-L","CRPS-R")
TableDM$country <- factor(TableDM$country,levels=(TableDM$country)[order(TableDM$country,decreasing = T)])


f1<-TableDM %>% filter(country %in% europe) %>% 
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

f2<-TableDM  %>% filter(country %in% america) %>% 
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


f4<-TableDM  %>% filter(country %in% africa) %>% 
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

f5<-TableDM  %>% filter(country %in% asia) %>% 
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

ggsave(paste0("../output/Figures/Figure_12.png"), 
       ggarrange(f1,f4,f5,f2,legend = "bottom",nrow = 2,ncol = 2,
                 common.legend = T),
       width = 10, height = 10)

