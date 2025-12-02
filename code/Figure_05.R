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



################################################################
##                    Probabilities                            ##
#################################################################


# P(X<0%)
prob_low<-cbind("country"=names(list_insample),as.data.frame(Reduce(rbind,lapply(list_insample,FUN = function(x)x$prob[,1]))))
# P(X>3%)
prob_high<-cbind("country"=names(list_insample),as.data.frame(Reduce(rbind,lapply(list_insample,FUN = function(x)x$prob[,2]))))
# P(X>1%)
prob_1<-cbind("country"=names(list_insample),as.data.frame(Reduce(rbind,lapply(list_insample,FUN = function(x)x$prob[,3]))))
# P(X>2%)
prob_2<-cbind("country"=names(list_insample),as.data.frame(Reduce(rbind,lapply(list_insample,FUN = function(x)x$prob[,4]))))


# NBER recessions dates
recessions.df = read.table(textConnection(
  "Peak, Trough
  2001-03-01, 2001-11-01
  2007-12-01, 2009-06-01
  2020-02-01, 2020-04-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)


# Russia-Ukraine war
recessions.df2 = read.table(textConnection(
  "Peak, Trough
  2022-02-01, 2022-12-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)


#list_h1_spec3<-list()
idx1_R1<-c(which(groups[,2]==1)) # Africa
idx1_R2<-c(which(groups[,3]==1)) # America
idx1_R3<-c(which(groups[,4]==1)) # Asia and Oceania
idx1_R4<-c(which(groups[,5]==1)) # Europe

# Group refion
group_region=c(rep("Africa",1,length(idx1_R1)),
               rep("America",1,length(idx1_R2)),
               rep("Asia and Oceania",1,length(idx1_R3)),
               rep("Europe",1,length(idx1_R4)))




#################################################################
##                    Prob>3%.  (high)                         ##
#################################################################


prob<-cbind("Region"=group_region,prob_high) %>% pivot_longer(cols=-c(1,2),values_to = "prob") %>% 
  group_by(country) %>% 
  mutate(date=date[-1])

pp1<-c(prob[prob$country=="South Africa","prob"])$prob

pp2<-c(prob[prob$country=="United States","prob"])$prob
pp3<-c(prob[prob$country=="Brazil","prob"])$prob
pp4<-c(prob[prob$country=="Canada","prob"])$prob
pp5<-c(prob[prob$country=="Mexico","prob"])$prob

pp6<-c(prob[prob$country=="United Kingdom","prob"])$prob
pp7<-c(prob[prob$country=="France","prob"])$prob
pp8<-c(prob[prob$country=="Italy","prob"])$prob
pp9<-c(prob[prob$country=="Germany","prob"])$prob

pp10<-c(prob[prob$country=="China","prob"])$prob
pp11<-c(prob[prob$country=="Japan","prob"])$prob
pp12<-c(prob[prob$country=="India","prob"])$prob

prob<-prob %>% group_by(country) %>% 
  mutate(South_Africa=pp1,
         United_States=pp2,Brazil=pp3,Canada=pp4,Mexico=pp5,
         United_Kingdom=pp6,France=pp7,Italy=pp8,Germany=pp9,
         China=pp10,Japan=pp11,India=pp12)



g1h<-prob %>% filter(Region=="Africa") %>% 
  ggplot(aes(x=as.Date(date),y=prob,ymin=-0,ymax=1)) +
  geom_line(linewidth=0.2, show.legend = T,col="gray",position =  position_dodge(1)) +
  geom_line(aes(x=as.Date(date),y=South_Africa),col="blue")+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='green', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='red', alpha=0.2)+
  labs(x = "",
       y = expression(IaR[t+1,t](3*"%")),caption = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "")

g2h<-prob %>% filter(Region=="America") %>% 
  ggplot(aes(x=as.Date(date),y=prob,col=country,ymin=-0,ymax=1)) +
  geom_line(linewidth=0.2, show.legend = T,col="gray",position =  position_dodge(1)) +
  geom_line(aes(x=as.Date(date),y=United_States),col="blue")+
  geom_line(aes(x=as.Date(date),y=Brazil),col="black")+
  geom_line(aes(x=as.Date(date),y=Canada),col="purple")+
  geom_line(aes(x=as.Date(date),y=Mexico),col="orange")+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='green', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='red', alpha=0.2)+
  labs(x = "",
       y = expression(IaR[t+1,t](3*"%")),caption = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "")

g3h<-prob %>% filter(Region=="Asia and Oceania") %>% 
  ggplot(aes(x=as.Date(date),y=prob,col=country,ymin=-0,ymax=1)) +
  geom_line(linewidth=0.2, show.legend = T,col="gray",position =  position_dodge(1)) +
  geom_line(aes(x=as.Date(date),y=China),col="blue")+
  geom_line(aes(x=as.Date(date),y=Japan),col="black")+
  geom_line(aes(x=as.Date(date),y=India),col="purple")+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='green', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='red', alpha=0.2)+
  labs(x = "",
       y = expression(IaR[t+1,t](3*"%")),caption = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "")



g4h<-prob %>% filter(Region=="Europe") %>% 
  ggplot(aes(x=as.Date(date),y=prob,col=country,ymin=-0,ymax=1)) +
  geom_line(linewidth=0.2, show.legend = T,col="gray",position =  position_dodge(1)) +
  geom_line(aes(x=as.Date(date),y=United_Kingdom),col="blue")+
  geom_line(aes(x=as.Date(date),y=France),col="black")+
  geom_line(aes(x=as.Date(date),y=Italy),col="purple")+
  geom_line(aes(x=as.Date(date),y=Germany),col="orange")+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='green', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='red', alpha=0.2)+
  labs(x = "",
       y = expression(IaR[t+1,t](3*"%")),caption = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "")

#################################################################
##                    Prob low.(X<0%)                          ##
#################################################################

prob<-cbind("Region"=group_region,prob_low) %>% pivot_longer(cols=-c(1,2),values_to = "prob") %>% 
  group_by(country) %>% 
  mutate(date=date[-1])

pp1<-c(prob[prob$country=="South Africa","prob"])$prob

pp2<-c(prob[prob$country=="United States","prob"])$prob
pp3<-c(prob[prob$country=="Brazil","prob"])$prob
pp4<-c(prob[prob$country=="Canada","prob"])$prob
pp5<-c(prob[prob$country=="Mexico","prob"])$prob

pp6<-c(prob[prob$country=="United Kingdom","prob"])$prob
pp7<-c(prob[prob$country=="France","prob"])$prob
pp8<-c(prob[prob$country=="Italy","prob"])$prob
pp9<-c(prob[prob$country=="Germany","prob"])$prob

pp10<-c(prob[prob$country=="China","prob"])$prob
pp11<-c(prob[prob$country=="Japan","prob"])$prob
pp12<-c(prob[prob$country=="India","prob"])$prob

prob<-prob %>% group_by(country) %>% 
  mutate(South_Africa=pp1,
         United_States=pp2,Brazil=pp3,Canada=pp4,Mexico=pp5,
         United_Kingdom=pp6,France=pp7,Italy=pp8,Germany=pp9,
         China=pp10,Japan=pp11,India=pp12)



g1l<-prob %>% filter(Region=="Africa") %>% 
  ggplot(aes(x=as.Date(date),y=prob,ymin=-0,ymax=1)) +
  geom_line(linewidth=0.2, show.legend = T,col="gray",position =  position_dodge(1)) +
  geom_line(aes(x=as.Date(date),y=South_Africa),col="blue")+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='green', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='red', alpha=0.2)+
  labs(x = "",
       y = expression(DaR[t+1,t](0*"%")),caption = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "")

g2l<-prob %>% filter(Region=="America") %>% 
  ggplot(aes(x=as.Date(date),y=prob,col=country,ymin=-0,ymax=1)) +
  geom_line(linewidth=0.2, show.legend = T,col="gray",position =  position_dodge(1)) +
  geom_line(aes(x=as.Date(date),y=United_States),col="blue")+
  geom_line(aes(x=as.Date(date),y=Brazil),col="black")+
  geom_line(aes(x=as.Date(date),y=Canada),col="purple")+
  geom_line(aes(x=as.Date(date),y=Mexico),col="orange")+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='green', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='red', alpha=0.2)+
  labs(x = "",
       y = expression(DaR[t+1,t](0*"%")),caption = "
       ") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "")

g3l<-prob %>% filter(Region=="Asia and Oceania") %>% 
  ggplot(aes(x=as.Date(date),y=prob,col=country,ymin=-0,ymax=1)) +
  geom_line(linewidth=0.2, show.legend = T,col="gray",position =  position_dodge(1)) +
  geom_line(aes(x=as.Date(date),y=China),col="blue")+
  geom_line(aes(x=as.Date(date),y=Japan),col="black")+
  geom_line(aes(x=as.Date(date),y=India),col="purple")+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='green', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='red', alpha=0.2)+
  labs(x = "",
       y = expression(DaR[t+1,t](0*"%")),caption = "
       ") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "")



g4l<-prob %>% filter(Region=="Europe") %>% 
  ggplot(aes(x=as.Date(date),y=prob,col=country,ymin=-0,ymax=1)) +
  geom_line(linewidth=0.2, show.legend = T,col="gray",position =  position_dodge(1)) +
  geom_line(aes(x=as.Date(date),y=United_Kingdom),col="blue")+
  geom_line(aes(x=as.Date(date),y=France),col="black")+
  geom_line(aes(x=as.Date(date),y=Italy),col="purple")+
  geom_line(aes(x=as.Date(date),y=Germany),col="orange")+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='green', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='red', alpha=0.2)+
  labs(x = "",
       y = expression(DaR[t+1,t](0*"%")),caption = "
       ") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "")



# ggsave(paste0("../Figures/Prob_1.png"), 
#        ggarrange(g1h,g1l,g2h,g2l,g3h,g3l,g4h,g4l,
#                  legend = "none",nrow = 4,ncol = 2,
#                  common.legend = T),
#        width = 9, height = 10)

#################################################################
##                    Prob X>1%                                ##
#################################################################


prob<-cbind("Region"=group_region,prob_1) %>% pivot_longer(cols=-c(1,2),values_to = "prob") %>% 
  group_by(country) %>% 
  mutate(date=date[-1])

pp1<-c(prob[prob$country=="South Africa","prob"])$prob

pp2<-c(prob[prob$country=="United States","prob"])$prob
pp3<-c(prob[prob$country=="Brazil","prob"])$prob
pp4<-c(prob[prob$country=="Canada","prob"])$prob
pp5<-c(prob[prob$country=="Mexico","prob"])$prob

pp6<-c(prob[prob$country=="United Kingdom","prob"])$prob
pp7<-c(prob[prob$country=="France","prob"])$prob
pp8<-c(prob[prob$country=="Italy","prob"])$prob
pp9<-c(prob[prob$country=="Germany","prob"])$prob

pp10<-c(prob[prob$country=="China","prob"])$prob
pp11<-c(prob[prob$country=="Japan","prob"])$prob
pp12<-c(prob[prob$country=="India","prob"])$prob

prob<-prob %>% group_by(country) %>% 
  mutate(South_Africa=pp1,
         United_States=pp2,Brazil=pp3,Canada=pp4,Mexico=pp5,
         United_Kingdom=pp6,France=pp7,Italy=pp8,Germany=pp9,
         China=pp10,Japan=pp11,India=pp12)



g11<-prob %>% filter(Region=="Africa") %>% 
  ggplot(aes(x=as.Date(date),y=prob,ymin=-0,ymax=1)) +
  geom_line(linewidth=0.2, show.legend = T,col="gray",position =  position_dodge(1)) +
  geom_line(aes(x=as.Date(date),y=South_Africa),col="blue")+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='green', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='red', alpha=0.2)+
  labs(x = "",
       y = expression(IaR[t+1,t](1*"%")),caption = "Note: South Africa (blue), other countries (gray area).") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "a) Africa")

g21<-prob %>% filter(Region=="America") %>% 
  ggplot(aes(x=as.Date(date),y=prob,col=country,ymin=-0,ymax=1)) +
  geom_line(linewidth=0.2, show.legend = T,col="gray",position =  position_dodge(1)) +
  geom_line(aes(x=as.Date(date),y=United_States),col="blue")+
  geom_line(aes(x=as.Date(date),y=Brazil),col="black")+
  geom_line(aes(x=as.Date(date),y=Canada),col="purple")+
  geom_line(aes(x=as.Date(date),y=Mexico),col="orange")+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='green', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='red', alpha=0.2)+
  labs(x = "",
       y = expression(IaR[t+1,t](1*"%")),caption = "Note: United States (blue), Brazil (black), Canada (purple), 
       Mexico (orange), other countries (gray area).") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "b) America")

g31<-prob %>% filter(Region=="Asia and Oceania") %>% 
  ggplot(aes(x=as.Date(date),y=prob,col=country,ymin=-0,ymax=1)) +
  geom_line(linewidth=0.2, show.legend = T,col="gray",position =  position_dodge(1)) +
  geom_line(aes(x=as.Date(date),y=China),col="blue")+
  geom_line(aes(x=as.Date(date),y=Japan),col="black")+
  geom_line(aes(x=as.Date(date),y=India),col="purple")+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='green', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='red', alpha=0.2)+
  labs(x = "",
       y = expression(IaR[t+1,t](1*"%")),caption = "Note: China (blue), Japan (black), India (purple), 
       other countries (gray area).") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "c) Asia and Oceania")



g41<-prob %>% filter(Region=="Europe") %>% 
  ggplot(aes(x=as.Date(date),y=prob,col=country,ymin=-0,ymax=1)) +
  geom_line(linewidth=0.2, show.legend = T,col="gray",position =  position_dodge(1)) +
  geom_line(aes(x=as.Date(date),y=United_Kingdom),col="blue")+
  geom_line(aes(x=as.Date(date),y=France),col="black")+
  geom_line(aes(x=as.Date(date),y=Italy),col="purple")+
  geom_line(aes(x=as.Date(date),y=Germany),col="orange")+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='green', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='red', alpha=0.2)+
  labs(x = "",
       y = expression(IaR[t+1,t](1*"%")),caption = "Note: United Kingdom (blue), France (black), Italy (purple), 
       Germany (orange), other countries (gray area).") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "d) Europe")


#################################################################
##                    Prob X>2%                                 ##
#################################################################

prob<-cbind("Region"=group_region,prob_2) %>% pivot_longer(cols=-c(1,2),values_to = "prob") %>% 
  group_by(country) %>% 
  mutate(date=date[-1])

pp1<-c(prob[prob$country=="South Africa","prob"])$prob

pp2<-c(prob[prob$country=="United States","prob"])$prob
pp3<-c(prob[prob$country=="Brazil","prob"])$prob
pp4<-c(prob[prob$country=="Canada","prob"])$prob
pp5<-c(prob[prob$country=="Mexico","prob"])$prob

pp6<-c(prob[prob$country=="United Kingdom","prob"])$prob
pp7<-c(prob[prob$country=="France","prob"])$prob
pp8<-c(prob[prob$country=="Italy","prob"])$prob
pp9<-c(prob[prob$country=="Germany","prob"])$prob

pp10<-c(prob[prob$country=="China","prob"])$prob
pp11<-c(prob[prob$country=="Japan","prob"])$prob
pp12<-c(prob[prob$country=="India","prob"])$prob

prob<-prob %>% group_by(country) %>% 
  mutate(South_Africa=pp1,
         United_States=pp2,Brazil=pp3,Canada=pp4,Mexico=pp5,
         United_Kingdom=pp6,France=pp7,Italy=pp8,Germany=pp9,
         China=pp10,Japan=pp11,India=pp12)



g12<-prob %>% filter(Region=="Africa") %>% 
  ggplot(aes(x=as.Date(date),y=prob,ymin=-0,ymax=1)) +
  geom_line(linewidth=0.2, show.legend = T,col="gray",position =  position_dodge(1)) +
  geom_line(aes(x=as.Date(date),y=South_Africa),col="blue")+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='green', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='red', alpha=0.2)+
  labs(x = "",
       y = expression(IaR[t+1,t](2*"%")),caption = "")+  
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "")

g22<-prob %>% filter(Region=="America") %>% 
  ggplot(aes(x=as.Date(date),y=prob,col=country,ymin=-0,ymax=1)) +
  geom_line(linewidth=0.2, show.legend = T,col="gray",position =  position_dodge(1)) +
  geom_line(aes(x=as.Date(date),y=United_States),col="blue")+
  geom_line(aes(x=as.Date(date),y=Brazil),col="black")+
  geom_line(aes(x=as.Date(date),y=Canada),col="purple")+
  geom_line(aes(x=as.Date(date),y=Mexico),col="orange")+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='green', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='red', alpha=0.2)+
  labs(x = "",
       y = expression(IaR[t+1,t](2*"%")),caption = "
       ") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "")

g32<-prob %>% filter(Region=="Asia and Oceania") %>% 
  ggplot(aes(x=as.Date(date),y=prob,col=country,ymin=-0,ymax=1)) +
  geom_line(linewidth=0.2, show.legend = T,col="gray",position =  position_dodge(1)) +
  geom_line(aes(x=as.Date(date),y=China),col="blue")+
  geom_line(aes(x=as.Date(date),y=Japan),col="black")+
  geom_line(aes(x=as.Date(date),y=India),col="purple")+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='green', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='red', alpha=0.2)+
  labs(x = "",
       y = expression(IaR[t+1,t](2*"%")),caption = "
       ") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "")



g42<-prob %>% filter(Region=="Europe") %>% 
  ggplot(aes(x=as.Date(date),y=prob,col=country,ymin=-0,ymax=1)) +
  geom_line(linewidth=0.2, show.legend = T,col="gray",position =  position_dodge(1)) +
  geom_line(aes(x=as.Date(date),y=United_Kingdom),col="blue")+
  geom_line(aes(x=as.Date(date),y=France),col="black")+
  geom_line(aes(x=as.Date(date),y=Italy),col="purple")+
  geom_line(aes(x=as.Date(date),y=Germany),col="orange")+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='green', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=0, ymax=1), fill='red', alpha=0.2)+
  labs(x = "",
       y = expression(IaR[t+1,t](2*"%")),caption = "
       ") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "")



ggsave(paste0("../output/Figures/Figure_05.png"), 
       ggarrange(g11,g12,g1h,g1l,g21,g22,g2h,g2l,g31,g32,g3h,g3l,g41,g42,g4h,g4l,
                 legend = "none",nrow = 4,ncol = 4,
                 common.legend = T),
       width = 16, height = 10)


