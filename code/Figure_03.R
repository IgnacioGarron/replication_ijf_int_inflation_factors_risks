library(tidyverse)
library(ggpubr) # ggarrrange
library(kableExtra) # latex tables
library(openxlsx)
library(forecast)
library(quantreg) #quant reg

rm(list = ls()) # Limpiar environment


source("functions/ThreeLevelMDFM.R") # CCA
source("functions/CRPS.R")
source("functions/Quantile model.R")

##### important function!!!!
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

data <- readxl::read_excel("../input/Data_complete_rev.xlsx")
list_insample<-loadRData("../output/Data/list_insample_h1_rr.RData")
list_insample_ar<-loadRData("../output/Data/list_insample_ar_h1_rr.RData")
date <- (data %>%  filter(country=="United States"))[-1,6]
date<-as.Date(date$date)
groups <- readxl::read_excel("../input/structure_dfm.xlsx")



# F_pval
Fpval<-cbind(names(list_insample),as.data.frame(Reduce(rbind,lapply(list_insample,FUN = function(x)x$Fpval))))


#################################################################
##                        HEAT MAPS   FPAL                     ##
#################################################################
h<-1
africa =   names(list_insample)[1:33]
america =  names(list_insample)[34:54] 
asia = names(list_insample)[55:76] 
europe = names(list_insample)[77:115] 

names<-c(europe,america,asia,africa)


colnames(Fpval)<-c("country","q=0.05","q=0.25","q=0.50","q=0.75","q=0.95")

Fpval$country <- factor(Fpval$country,levels=(Fpval$country)[order(Fpval$country,decreasing = T)])

Fpval[,-1]<-(Fpval[,-1]<0.10)*1
f1<-Fpval %>% filter(country %in% europe) %>% 
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

f2<-Fpval  %>% filter(country %in% america) %>% 
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


f4<-Fpval  %>% filter(country %in% africa) %>% 
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

f5<-Fpval  %>% filter(country %in% asia) %>% 
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


ggsave(paste0("../output/Figures/Figure_03.png"), 
       ggarrange(f1,f4,f5,f2,legend = "bottom",nrow = 2,ncol = 2,
                 common.legend = T),
       width = 8, height = 10)

