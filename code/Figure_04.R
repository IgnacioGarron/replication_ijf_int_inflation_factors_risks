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





for (name in c("United States", "Germany", "Mexico", "China")){
  
  dens <- NULL # Store densities
  val5 <- NULL # Store 5% info
  densb <- NULL # Store benchmark density
  tau = c(0.05,0.25,0.50,0.75,0.95)
  
  x   <- seq(-55,55,0.05)                        # Evaluate fitted density over this interval
  
  for (i in c(1:length(list_insample[[name]]$forecast_quantiles[,1]))) {
    
    yy<-list_insample[[name]]$forecast_quantiles[i,]
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
    dens <- bind_rows(dens, tibble(x=x,y=y,yin=rep(list_insample[[name]]$yin[i],length(y)),
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
  
  
  ggsave(paste0("../output/Figures/Figure_04",name,".png"),
         ggarrange(g1,ncol = 1,nrow = 1,common.legend = F,legend = "bottom"), 
         width = 8, height = 4)
  
}

