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

factors<-R1$Factors_Inflation
colnames(factors)<-c("Global","Africa","America",
                     "Asia and Oceania","Europe","Advanced",
                     "HMI-EMDEs","LI-EMDEs") 

data_plot <- (data %>% filter(country=="United States") %>% 
                select(date))[-1,]
data_plot <- cbind(data_plot,factors)


data_plot <- data_plot %>%  pivot_longer(cols = -"date",values_to = "value",
                                         names_to = "Factors") %>% 
  arrange(Factors) %>% 
  mutate(LB=case_when(Factors =="Global" ~ value-2*SD[1],
                      Factors =="Africa" ~ value-2*SD[2],
                      Factors =="America" ~ value-2*SD[3],
                      Factors =="Asia and Oceania" ~ value-2*SD[4],
                      Factors =="Europe" ~ value-2*SD[5],
                      Factors =="Advanced" ~ value-2*SD[6],
                      Factors =="HMI-EMDEs" ~ value-2*SD[7],
                      Factors =="LI-EMDEs" ~ value-2*SD[8]),
         UB=case_when(Factors =="Global" ~ value+2*SD[1],
                      Factors =="Africa" ~ value+2*SD[2],
                      Factors =="America" ~ value+2*SD[3],
                      Factors =="Asia and Oceania" ~ value+2*SD[4],
                      Factors =="Europe" ~ value+2*SD[5],
                      Factors =="Advanced" ~ value+2*SD[6],
                      Factors =="HMI-EMDEs" ~ value+2*SD[7],
                      Factors =="LI-EMDEs" ~ value+2*SD[8]))


g31 <- data_plot %>% filter(Factors=="Global") %>%
  ggplot() + geom_line(aes(x = as.Date(date),y = value),col="blue",alpha=0.5) +
  geom_ribbon(aes(x = as.Date(date), ymax = UB,ymin=LB),alpha=0.3) +
  xlab("")+ # título xlab 
  ylab("")+
  theme_bw()+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='gray10', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='red', alpha=0.2)+
  facet_wrap(vars(Factors),nrow = 3,) + 
  theme(legend.box.spacing = unit(-10, "pt"),
        legend.margin=margin(0,0,0,0),
        legend.position="none", #enviar leyenda abajo
        legend.title = element_text(size=1))+  #tamaño leyenda
  labs(colour = paste0(""),title = "a) Global factor")


g32<- data_plot %>% filter(Factors!="Global" & Factors!="HMI-EMDEs" & Factors!="LI-EMDEs" & Factors!="Advanced") %>% 
  ggplot() + geom_line(aes(x = as.Date(date),y = value),col="blue",alpha=0.5,linewidth=0.5) +
  geom_ribbon(aes(x = as.Date(date), ymax = UB,ymin=LB),alpha=0.3) +
  xlab("")+ # título xlab 
  ylab("")+
  theme_bw()+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='gray10', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='red', alpha=0.2)+
  facet_wrap(vars(Factors),nrow = 5,) + 
  theme(legend.box.spacing = unit(-10, "pt"),
        legend.margin=margin(0,0,0,0),
        legend.position="none", #enviar leyenda abajo
        legend.title = element_text(size=1))+  #tamaño leyenda
  labs(colour = paste0(""),title = "b) Geographical factors")



g33<- data_plot %>% filter(Factors!="Global" & Factors!="Africa" & Factors!="America" & 
                             Factors!="Asia and Oceania" & Factors!="Europe") %>% 
  ggplot() + geom_line(aes(x = as.Date(date),y = value),col="blue",alpha=0.5,linewidth=0.5) +
  geom_ribbon(aes(x = as.Date(date), ymax = UB,ymin=LB),alpha=0.3) +
  xlab("")+ # título xlab 
  ylab("")+
  theme_bw()+
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='gray10', alpha=0.2)+
  geom_rect(data = recessions.df2, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='red', alpha=0.2)+
  facet_wrap(vars(Factors),nrow = 5,) + 
  theme(legend.box.spacing = unit(-10, "pt"),
        legend.margin=margin(0,0,0,0),
        legend.position="none", #enviar leyenda abajo
        legend.title = element_text(size=1))+  #tamaño leyenda
  labs(colour = paste0(""),title = "c) Income factors")

ggsave(paste0("../output/Figures/Figure_B2.png"),
       ggarrange(g31,ggarrange(g32,g33,nrow = 1,common.legend = F),nrow = 2,common.legend = F),
       width = 8, height = 8)
