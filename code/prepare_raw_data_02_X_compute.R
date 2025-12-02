library(tidyverse)
library(openxlsx)
rm(list = ls()) # Limpiar environment

source("functions/data_complete.R")

# Importa datos de finales
data <- readxl::read_excel("../input/Data_complete_rev.xlsx")

X<-data %>% pivot_wider(names_from = "country",values_from = "inf_sa",id_cols = "date")
X<-as.matrix(X[,-1])
X<-as.matrix(X[-1,])

X<-X[,order(colnames(X))]

write.xlsx(as.data.frame(X),"../input/X.xlsx",keepNA = TRUE, colNames = TRUE)
