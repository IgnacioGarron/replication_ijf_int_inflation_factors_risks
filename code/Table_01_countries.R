library(tidyverse)
library(ggpubr) # ggarrrange
library(kableExtra) # latex tables
library(openxlsx)
rm(list = ls()) # Limpiar environment


source("functions/data_complete.R")

#############################################
#             Preliminaries
#############################################

start_date="1999-01-01"
end_date="2022-12-01"

data<-data_inf(start_date=start_date,end_date=end_date)  

# FULL SAMPLE
kbl(data$Data_available %>% filter(group_income=="Advanced Economies") %>% select(country,group_geo,code) %>%
      arrange(country), caption = "Availability of inflation data",booktabs = T,
    linesep = c(""),
    format="latex") %>%
  kable_classic(full_width = F) %>% 
  kable_styling(latex_options = "scale_down") %>% 
  save_kable(paste('../output/Tables/Table_01_Advanced Economies.tex'),float = FALSE)

kbl(data$Data_available %>% filter(group_income=="Emerging and Developing Economies") %>% select(country,group_geo,code) %>%
      arrange(country), caption = "Availability of inflation data",booktabs = T,
    linesep = c(""),
    format="latex") %>%
  kable_classic(full_width = F) %>% 
  kable_styling(latex_options = "scale_down") %>% 
  save_kable(paste('../output/Tables/Table_01_Middle-High_Emerging.tex'),float = FALSE)

kbl(data$Data_available %>% filter(group_income=="Low income EMDEs") %>% select(country,group_geo,code) %>%
      arrange(country), caption = "Availability of inflation data",booktabs = T,
    linesep = c(""),
    format="latex") %>%
  kable_classic(full_width = F) %>% 
  kable_styling(latex_options = "scale_down") %>% 
  save_kable(paste('../output/Tables/Table_01_Low_Emerging.tex'),float = FALSE)


