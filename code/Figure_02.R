library(tidyverse)
library(ggpubr) # ggarrrange
library(kableExtra) # latex tables
library(openxlsx)
library(forecast)
rm(list = ls()) # Limpiar environment

#source("functions/eigrs2.R") # Eigen with ascending order
#source("functions/blockfact0.R") # CCA
#source("functions/ThreeLevelMDFM.R") # CCA
#source("functions/spca.R") # CCA


#################################################################
##                         Import data                         ##
#################################################################
data_complete <- readxl::read_excel("../input//Data_complete_rev.xlsx")


bannerCommenter::banner("Inflation figures by country")
##################################################################
##                 Inflation figures by country                 ##
##################################################################

start_date="1999-01-01"
end_date="2022-12-01"



bannerCommenter::banner("Figure: Inflation across income groups")
##################################################################
##            Figure: Inflation across income groups            ##
##################################################################


unique(data_complete$group_income)

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


g1<-data_complete %>% filter(group_income=="Advanced Economies" & as.Date(date)>"1999-01-01") %>% 
  ggplot(aes(x=as.Date(date),y=inf_sa,col=country,ymin=-150,ymax=150)) +
  geom_line(linewidth=0.5,position =  position_dodge(0.4), show.legend = F) +
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='gray10', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='red', alpha=0.2)+
  viridis::scale_color_viridis(discrete=TRUE,option="magma") +
  labs(x = "",
       y = "Monthly annualized inflation (%)",col="") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "a) Advanced Economies")

g2<-data_complete %>% filter(group_income=="Emerging and Developing Economies" & as.Date(date)>"1999-01-01") %>% 
  ggplot(aes(x=as.Date(date),y=inf_sa,col=country,ymin=-150,ymax=150)) +
  geom_line(linewidth=0.5,position =  position_dodge(0.4), show.legend = F) +
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='gray10', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='red', alpha=0.2)+
  viridis::scale_color_viridis(discrete=TRUE,option="magma") +
  labs(x = "",
       y = "Monthly annualized inflation (%)",col="") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "b) Middle and high income EMDEs")

g3<-data_complete %>% filter(group_income=="Low income EMDEs" & as.Date(date)>"1999-01-01") %>% 
  ggplot(aes(x=as.Date(date),y=inf_sa,col=country,ymin=-150,ymax=150)) +
  geom_line(linewidth=0.5,position =  position_dodge(0.4), show.legend = F) +
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='gray10', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='red', alpha=0.2)+
  viridis::scale_color_viridis(discrete=TRUE,option="magma") +
  labs(x = "",
       y = "Monthly annualized inflation (%)",col="") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.caption = element_text(hjust = 0))+
  labs(colour="",title = "c) Low income EMDEs")

 ggsave(paste0("../output/Figures/Figure_02.png"),
        ggarrange(g1,g2,g3,ncol = 1,nrow=3,
                  common.legend = F,legend = "none"), width = 8, height = 10)

 

 
