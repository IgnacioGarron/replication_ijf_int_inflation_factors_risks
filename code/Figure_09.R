
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

##### important function!!!!
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
data <- readxl::read_excel("../input/Data_complete_rev.xlsx")

date <- (data %>%  filter(country=="United States"))[-1,6]
date<-as.Date(date$date)
date<-date[(length(date)-132+1):length(date)] # date OOS
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


set.seed(123)
bannerCommenter::banner("Probabilites out-of-sample")
##################################################################
##                  Probabilites out-of-sample                  ##
##################################################################
prob_low<-c()
prob_high<-c()
prob_1<-c()
prob_2<-c()

for (c in names) {
  
  # P(X<0%)
  prob_low<-rbind(prob_low,Reduce(cbind,lapply(mod1[[c]],FUN = function(x)x$prob[1])))
  # P(X>3%)
  prob_high<-rbind(prob_high,Reduce(cbind,lapply(mod1[[c]],FUN = function(x)x$prob[2])))
  # P(X>1%)
  prob_1<-rbind(prob_1,Reduce(cbind,lapply(mod1[[c]],FUN = function(x)x$prob[3])))
  # P(X>2%)
  prob_2<-rbind(prob_2,Reduce(cbind,lapply(mod1[[c]],FUN = function(x)x$prob[4])))
  
}

prob_low <- as.data.frame(prob_low)
prob_high <- as.data.frame(prob_high)
prob_1 <- as.data.frame(prob_1)
prob_2 <- as.data.frame(prob_2)

prob_low <- cbind("country"=names,prob_low)
prob_high <- cbind("country"=names,prob_high)
prob_1 <- cbind("country"=names,prob_1)
prob_2 <- cbind("country"=names,prob_2)


#################################################################
##                    Probabilities                            ##
#################################################################


# NBER recessions dates
recessions.df = read.table(textConnection(
  "Peak, Trough
  2020-02-01, 2020-04-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)


# Russia-Ukraine war
recessions.df2 = read.table(textConnection(
  "Peak, Trough
  2022-02-01, 2022-12-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)

# Group refion
group_region=c(rep("Africa",1,length(idx1_R1$Country)),
               rep("America",1,length(idx1_R2$Country)),
               rep("Asia and Oceania",1,length(idx1_R3$Country)),
               rep("Europe",1,length(idx1_R4$Country)))



#################################################################
##                    Prob>3%.  (high)                         ##
#################################################################


prob<-cbind("Region"=group_region,"country"=names,prob_high)  %>% pivot_longer(cols=-c(1,2),values_to = "prob") %>% 
  group_by(country) %>% 
  mutate(date=date)

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

prob<-cbind("Region"=group_region,"country"=names,prob_low)  %>% pivot_longer(cols=-c(1,2),values_to = "prob") %>% 
  group_by(country) %>% 
  mutate(date=date)

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


#################################################################
##                    Prob X>1%                                ##
#################################################################

prob<-cbind("Region"=group_region,"country"=names,prob_1)  %>% pivot_longer(cols=-c(1,2),values_to = "prob") %>% 
  group_by(country) %>% 
  mutate(date=date)

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


prob<-cbind("Region"=group_region,"country"=names,prob_2)  %>% pivot_longer(cols=-c(1,2),values_to = "prob") %>% 
  group_by(country) %>% 
  mutate(date=date)

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



ggsave(paste0("../output/Figures/Figure_09.png"), 
       ggarrange(g11,g12,g1h,g1l,g21,g22,g2h,g2l,g31,g32,g3h,g3l,g41,g42,g4h,g4l,
                 legend = "none",nrow = 4,ncol = 4,
                 common.legend = T),
       width = 16, height = 10)



