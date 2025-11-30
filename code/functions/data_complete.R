library(tidyverse)
library(ggpubr) # ggarrrange
library(kableExtra) # latex tables

#############################################
#             Preliminaries
#############################################


data_inf <- function(start_date="1980-01-01",end_date="2022-12-01",drop=TRUE){
  
  data<-readxl::read_excel("../input/raw_data/Inflation-data.xlsx",sheet = "hcpi_m")
  wbclass<-readxl::read_excel("../input/raw_data/CLASS_WB.xlsx",sheet = "List of economies")
  
  data <- data %>% rename(country=Country,code=`Country Code`)
  wbclass <- wbclass %>% rename(code=Code,region=Region,income_group=`Income group`)
  
  data_w <- data %>% pivot_longer(cols = -c(1:5,645:648),names_to = "date",values_to = "values") %>% 
    pivot_wider(values_from = "values",names_from = "country",id_cols = c("date")) %>% 
    mutate (date=seq(as.Date("1970-1-1"), by = "month", length.out = ncol(data)-9)) %>%
    pivot_longer(cols = -c(1),names_to = "country",values_to = "cpi") %>%  # 186 countries
    arrange(country,date) %>%  
    group_by(country) %>% 
    mutate(cpi=cpi+1) %>% # Some countries have a cpi very close to 0.
    mutate(inf12=(1200/12)*(log(cpi/lag(cpi,12))),
           inf1=(1200/1)*(log(cpi/lag(cpi,1))),
           inf3=(1200/3)*(log(cpi/lag(cpi,3))),
           inf6=(1200/6)*(log(cpi/lag(cpi,6)))) %>% 
    filter(date>=start_date & date<=end_date)
  
  Data_available<-data_w  %>% select(date,country,inf12,inf6,inf3,inf1) %>% 
    group_by(country,inf1) %>% 
    filter(complete.cases(inf1,date)) %>% 
    group_by(country) %>% 
    summarise("Start"=first(zoo::as.yearmon((date))),
              "End"=last(zoo::as.yearmon((date))),"Tobs"=n(),
              "Treal"=round((last(zoo::as.yearmon((date)))-first(zoo::as.yearmon((date))))*12+1)) %>% 
    mutate("Complete"=case_when(Start == zoo::as.yearmon((start_date)) &
                                  End == zoo::as.yearmon((end_date)) &
                                  Tobs == Treal ~ "Yes", .default = "No"))
  
  Data_available<-left_join(Data_available,data[,c("country","code")],by="country")
  Data_available<-left_join(Data_available,wbclass[,c("code","group_geo","group_income","region")],by="code")
  
  data_c <-left_join(keep = F,Data_available[Data_available$Start==start_date & 
                                               Data_available$End==end_date &
                                               Data_available$Complete=="Yes",]
                     ,data_w,by="country")[,-c(2:6)]
  
  if (drop==TRUE){
    drop_countries<-c(
      "Belarus",
      "Haiti",
      "Sudan",
      "Malawi", 
      "Tanzania, United Rep.",
      "Gambia, The",
      "Maldives", 
      "Hong Kong SAR, China")
    
    data_c <- data_c %>% filter(!country %in% drop_countries)
  }
  Data_available <- Data_available %>%  filter(country %in% unique(data_c$country))
  
  return(list("Data_available"=Data_available,"data_complete"=data_c))
}

