
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
h=1
j=1
for (i in c("United States","Canada","Japan","United Kingdom","France",
            "Germany","Italy","India","Brazil","China","South Africa","Mexico")) {
  
  # Giacomini test
  
  score_m1 <- Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_equal))))
  score_m1b <- Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_equal))))
  score_m2 <- Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_left))))
  score_m2b <- Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_left))))
  score_m3 <- Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_right))))
  score_m3b <- Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_right))))
  score_m4 <- Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[1]))
  score_m4b <- Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[1]))
  score_m5 <- Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[3]))
  score_m5b <- Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[3]))
  score_m6 <- Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[5]))
  score_m6b <- Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[5]))
  
  fluct_test1 <- fluctuation_test(c(score_m1), c(score_m1b),mu=0.5, 
                                  time_labels =date[156:287], lag_truncate = 3,conf_level = 0.10)
  fluct_test2 <- fluctuation_test(c(score_m2), c(score_m2b),mu=0.5, 
                                  time_labels =date[156:287], lag_truncate = 3,conf_level = 0.10)
  fluct_test3 <- fluctuation_test(c(score_m3), c(score_m3b),mu=0.5, 
                                  time_labels =date[156:287], lag_truncate = 3,conf_level = 0.10)
  fluct_test4 <- fluctuation_test(c(score_m4), c(score_m4b),mu=0.5, 
                                  time_labels =date[156:287], lag_truncate = 3,conf_level = 0.10)
  fluct_test5 <- fluctuation_test(c(score_m5), c(score_m5b),mu=0.5, 
                                  time_labels =date[156:287], lag_truncate = 3,conf_level = 0.10)
  fluct_test6 <- fluctuation_test(c(score_m6), c(score_m6b),mu=0.5, 
                                  time_labels =date[156:287], lag_truncate = 3,conf_level = 0.10)
  
  g2<-ggplot()+
    geom_line(aes(x=fluct_test1$df$time,y=fluct_test1$df$dmstat,col="CRPS-equal"))+
    geom_line(aes(x=fluct_test2$df$time,y=fluct_test2$df$dmstat,col="CRPS-left"))+
    geom_line(aes(x=fluct_test3$df$time,y=fluct_test3$df$dmstat,col="CRPS-right"))+
    geom_line(aes(x=fluct_test4$df$time,y=fluct_test4$df$dmstat,col="QS(0.05)"))+
    geom_line(aes(x=fluct_test5$df$time,y=fluct_test5$df$dmstat,col="QS(0.50)"))+
    geom_line(aes(x=fluct_test6$df$time,y=fluct_test6$df$dmstat,col="QS(0.95)"))+
    geom_hline(yintercept = -2.168,col="red",linetype = "dashed")+ # Critical value for one sided test 10% (see Table 1 GR,2010)
    ylim(c(-6,6)) +
    labs(title=i,caption = "",x="", y="")+
    scale_colour_manual(name = "",values=c("CRPS-equal"="black","CRPS-left"="blue","CRPS-right"="green",
                                           "QS(0.05)"="purple","QS(0.50)"="red",
                                           "QS(0.95)"="orange"))+
    theme(legend.box.spacing = unit(-10, "pt"),
          legend.margin=margin(0,0,0,0),
          legend.position="bottom", #enviar leyenda abajo
          legend.title = element_text(size=1))+  #tamaño leyenda
    theme_bw()
  
  ggsave(paste0("../output/Figures/Figure_14_",i,".png"), 
         ggarrange(g2,common.legend = F,
                   nrow=1,ncol=1),
         width = 6, height = 3)
}

