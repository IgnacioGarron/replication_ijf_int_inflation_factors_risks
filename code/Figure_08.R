
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




bannerCommenter::banner("Densifity out-of-sample forecasts")
#################################################################
##              Densifity out-of-sample forecasts              ##
#################################################################

set.seed(123)
for (name in c("China","United States","Mexico","Germany")) {
  
  table_oos<-data.frame(c())
  table_yout<-data.frame(c())
  
  table_oos=rbind(table_oos,Reduce(rbind,lapply(mod1[[name]], function(x)x$forecast_quantile)))
  table_yout=rbind(table_yout,Reduce(rbind,lapply(mod1[[name]], function(x)tail(x$X$y,1))))
  
  
  dens <- NULL # Store densities
  val5 <- NULL # Store 5% info
  densb <- NULL # Store benchmark density
  tau = c(0.05,0.25,0.50,0.75,0.95)
  
  for (i in c(1:length(table_oos[,1]))) {
    
    
    x   <- seq(-55,55,0.05)                        # Evaluate fitted density over this interval
    
    yy<-table_oos[i,]
    #x_range<-seq(-20,250,by=0.05)
    # Initial values
    iqn=qnorm(0.75)-qnorm(0.25)
    l0=yy[3]  # LOCATION
    s0=(yy[4] - yy[2]) / iqn; # SCALE
    sh0=0 # Shape
    #v0=1 # DF
    #LB=c(-30,0,-30,1) # SIMILAR TO ADRIAN
    #UB=c(500,50,30,30)
    LB = c(   -20+l0,     1,   -100); #Omega must positive >=1
    UB = c(   +20+l0,    50,    100);
    
    
    skewt<-optim(c(l0, s0, sh0),fn=function(x){
      sum((as.numeric(yy)-qst(tau,xi=x[1],omega=x[2],alpha=x[3]))^2)
    }, lower=LB,upper=UB,  method="L-BFGS-B")
    
    #density <-dst(x_range,xi=skewt$par[1],omega=skewt$par[2],
    #                   alpha=skewt$par[3],nu=skewt$par[4])
    
    y  <-dst(x,xi=skewt$par[1],omega=skewt$par[2],
             alpha=skewt$par[3])
    dens <- bind_rows(dens, tibble(x=x,y=y,yin=rep(table_yout$V1[i],length(y)),
                                   date=rep(date[i+1],length(y))))
  }
  
  sc   <- 1500                           # Scale factor
  dens$date<-as.Date(dens$date)
  
  g1<-ggplot() + 
    ggridges::geom_ridgeline(data=dens,aes(x=x, height=y, y=date, group=date), 
                             fill="forestgreen",col="white",scale=sc,alpha=1) +
    geom_point(data=dens,aes(x=yin, y=date, group=date),size=0.2) +
    theme_bw() + 
    labs(x="Monthly annualized Inflation (%)", y="", title = paste0(name))+
    scale_color_manual(name="",values = c("Realized value" = "black"))+
    coord_flip()+
    theme(legend.position="bottom")+
    xlim(-20,20)
  #xlim(min(list_insample[[name]]$yin)-5,max(list_insample[[name]]$yin)+5)
  
  
  ggsave(paste0("../output/Figures/Figure_08_Density_OOS",name,".png"),
         ggarrange(g1,ncol = 1,nrow = 1,common.legend = F,legend = "bottom"), 
         width = 8, height = 4)
}
