# Outputs: 
# "Data_complete_kalman_1999.xlsx"
# Figures of country's inflation using BSM.

library(tidyverse)
library(ggpubr) # ggarrrange
library(kableExtra) # latex tables
#library(SLBDD)
library(openxlsx)
library(forecast)
rm(list = ls()) # Limpiar environment

source("functions/data_complete.R")
source("functions/replace_outliers.R")
library(UComp)


bannerCommenter::banner("Preliminaries")
#################################################################
##                        Preliminaries                        ##
#################################################################


start_date="1999-01-01"
end_date="2022-12-01"

data<-data_inf(start_date=start_date,end_date=end_date)  

unique(data$data_complete$region)

list_meast<-unique(data$data_complete[data$data_complete$region=="Middle East & North Africa","country"])
list_europe<-unique(data$data_complete[data$data_complete$region=="Europe & Central Asia","country"])
list_lamerica<-unique(data$data_complete[data$data_complete$region=="Latin America & Caribbean","country"])
list_subafrica<-unique(data$data_complete[data$data_complete$region=="Sub-Saharan Africa","country"])
list_namerica<-unique(data$data_complete[data$data_complete$region=="North America","country"])
list_easia<-unique(data$data_complete[data$data_complete$region=="East Asia & Pacific","country"])
list_sasia<-unique(data$data_complete[data$data_complete$region=="South Asia" ,"country"])

data_complete <-data$data_complete
data_complete %>% 
  mutate(level=NA,slope=NA,sae=NA)

bannerCommenter::banner("BSM")
#################################################################
##                             BSM                             ##
#################################################################

country_name="Mexico"
data_country<-data$data_complete %>% filter(country==country_name) %>% select(cpi)
ex<-ts((data_country[,1]),start=c(1999,1),f=12)

m1<-UC(ex,model="llt/equal/arma(0,0)") ## local linear time trend  + fixed seasonal dummies
m2<-UC(ex,stepwise = T) ## local linear time trend + automatic selection

# summary(m1)
# summary(m2)
# 
# autoplot(ex-m1$comp[,3])+
#   autolayer(ex)
# 
# autoplot(diff(log(ex-m1$comp[,3])))+
#   autolayer(diff(log(ex)))



for (country_name in unique(data$data_complete$country)){
  data_country<-data$data_complete %>% filter(country==country_name) %>% select(cpi)
  ex<-ts((data_country[,1]),start=c(1999,1),f=12)
  fit<-UC(ex,model="llt/equal/arma(0,0)",h=0)
  data_complete[data_complete$country==country_name,"level_bsm"] <-fit$comp[,1]
  data_complete[data_complete$country==country_name,"slope_bsm"] <-fit$comp[,2]
  data_complete[data_complete$country==country_name,"sea_bsm"] <-fit$comp[,3]
}


bannerCommenter::banner("Outlier replace")
#################################################################
##                       Outlier replace                       ##
#################################################################

temp<-data_complete %>% group_by(country) %>% mutate(cpi_sa=cpi-sea_bsm) %>%
  mutate(inf_sa=log(cpi_sa/lag(cpi_sa))*1200) %>% 
  select(country,date,inf_sa) %>%
  pivot_wider(id_cols = "date",names_from = "country",
              values_from = "inf_sa")
temp2<-temp
temp2[,-1]<-replace_outliers(temp[,-1])
date<-temp2$date

for (j in 1:115){
  if(length(which((temp[,j+1]==temp2[,j+1])==F))>0) {
    #j=55
    IQR<-quantile(as.matrix(temp[-1,j+1]),0.75)+10*(quantile(as.matrix(temp[-1,j+1]),0.75)-quantile(as.matrix(temp[-1,j+1]),0.25))
    outlier<-temp[which((temp[,j+1]==temp2[,j+1])==F),j+1]/IQR
    print(paste0(j,": ",colnames(temp[,-1])[j],": ",
                 date[which((temp[,j+1]==temp2[,j+1])==F)],", Outlier/(Q3+10*IQR):",
                 round(outlier,3)))
  }
}




# plot(y=temp[-1,33+1]$Ecuador,x=date[-1],col="black",t="l",xlab = "",ylab="",main = "Ecuador")
# lines(y=temp2[-1,33+1]$Ecuador,x=date[-1],col="red",t="l")
# plot(y=temp[-1,55+1]$Italy,x=date[-1],col="black",t="l",xlab = "",ylab="",main = "Italy")
# lines(y=temp2[-1,55+1]$Italy,x=date[-1],col="red",t="l")

# (temp[,55+1]==temp2[,33+1])
# (quantile(as.matrix(temp[-1,33+1]),.75)-quantile(as.matrix(temp[-1,33+1]),.25))*10+quantile(as.matrix(temp[-1,33+1]),.75)
# cbind(temp[,33+1],temp2[,33+1])


temp2<-temp2 %>% pivot_longer(cols = -"date",names_to = "country",values_to = "inf_sa")
data_complete <-left_join(data_complete,temp2,by = c("date","country"))  

temp<-data_complete %>% group_by(country) %>% mutate(cpi_sa=cpi-sea_bsm) %>%
  mutate(inf_sa12=log(cpi_sa/lag(cpi_sa,12))*100) %>%
  select(country,date,inf_sa12) %>%
  pivot_wider(id_cols = "date",names_from = "country",
              values_from = "inf_sa12")
temp2<-temp
temp2[,-1]<-replace_outliers(temp[,-1])

temp2<-temp2 %>% pivot_longer(cols = -"date",names_to = "country",values_to = "inf_sa12")
data_complete <-left_join(data_complete,temp2,by = c("date","country"))

# Consider just CPI_SA for inflation
data_complete <- data_complete %>% mutate(cpi_sa=cpi-sea_bsm) %>% 
  select("country","code","group_geo","group_income","region","date","cpi","cpi_sa","inf_sa","inf_sa12",
         "level_bsm","slope_bsm","sea_bsm" )

# Save new data
write.xlsx(data_complete,file = "../input/Data_complete_rev.xlsx")

