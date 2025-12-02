
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

idx1_R1<-groups[groups[,2]==1,1] # Africa
idx1_R2<-groups[groups[,3]==1,1] # America
idx1_R3<-groups[groups[,4]==1,1] # Asia and Oceania
idx1_R4<-groups[groups[,5]==1,1] # Europe

idx1_R5<-groups[groups[,6]==1,1] # Adv
idx1_R6<-groups[groups[,7]==1,1] # Emde
idx1_R7<-groups[groups[,8]==1,1] # LI



# R2
names(list_insample)
Reduce(rbind,lapply(list_insample,FUN = names))
R<-cbind(names(list_insample),as.data.frame(Reduce(rbind,lapply(list_insample,FUN = function(x)x$R))))

# F_pval
Fpval<-cbind(names(list_insample),as.data.frame(Reduce(rbind,lapply(list_insample,FUN = function(x)x$Fpval))))

# AIC
AICmodel<-cbind(names(list_insample),as.data.frame(Reduce(rbind,lapply(list_insample,FUN = function(x)x$AICf))))
AICar1<-cbind(names(list_insample),as.data.frame(Reduce(rbind,lapply(list_insample,FUN = function(x)x$AICar1))))

# BIC
BICmodel<-cbind(names(list_insample),as.data.frame(Reduce(rbind,lapply(list_insample,FUN = function(x)x$BICf))))
BICar1<-cbind(names(list_insample),as.data.frame(Reduce(rbind,lapply(list_insample,FUN = function(x)x$BICar1))))

# IC
ICmodel<-cbind(names(list_insample),as.data.frame(Reduce(rbind,lapply(list_insample,FUN = function(x)x$ICf))))
ICar1<-cbind(names(list_insample),as.data.frame(Reduce(rbind,lapply(list_insample,FUN = function(x)x$ICar1))))


# Forecast
forecast_0.05<-Reduce(cbind,lapply(list_insample,FUN = function(x)x$forecast_skt[,1]))
colnames(forecast_0.05)<-names(list_insample)

forecast_0.25<-Reduce(cbind,lapply(list_insample,FUN = function(x)x$forecast_skt[,2]))
colnames(forecast_0.25)<-names(list_insample)

forecast_0.50<-Reduce(cbind,lapply(list_insample,FUN = function(x)x$forecast_skt[,3]))
colnames(forecast_0.50)<-names(list_insample)

forecast_0.75<-Reduce(cbind,lapply(list_insample,FUN = function(x)x$forecast_skt[,4]))
colnames(forecast_0.75)<-names(list_insample)

forecast_0.95<-Reduce(cbind,lapply(list_insample,FUN = function(x)x$forecast_skt[,5]))
colnames(forecast_0.95)<-names(list_insample)

# Scores
qs_0.05<-Reduce(cbind,lapply(list_insample,FUN = function(x)x$qs[,1]))/Reduce(cbind,lapply(list_insample_ar,FUN = function(x)x$qs[,1]))
qs_0.25<-Reduce(cbind,lapply(list_insample,FUN = function(x)x$qs[,2]))/Reduce(cbind,lapply(list_insample_ar,FUN = function(x)x$qs[,2]))
qs_0.50<-Reduce(cbind,lapply(list_insample,FUN = function(x)x$qs[,3]))/Reduce(cbind,lapply(list_insample_ar,FUN = function(x)x$qs[,3]))
qs_0.75<-Reduce(cbind,lapply(list_insample,FUN = function(x)x$qs[,4]))/Reduce(cbind,lapply(list_insample_ar,FUN = function(x)x$qs[,4]))
qs_0.95<-Reduce(cbind,lapply(list_insample,FUN = function(x)x$qs[,5]))/Reduce(cbind,lapply(list_insample_ar,FUN = function(x)x$qs[,5]))

colnames(qs_0.05)<-names(list_insample)
colnames(qs_0.25)<-names(list_insample)
colnames(qs_0.50)<-names(list_insample)
colnames(qs_0.75)<-names(list_insample)
colnames(qs_0.95)<-names(list_insample)

crps_equal<-Reduce(cbind,lapply(list_insample,FUN = function(x)rowMeans(x$crps_equal)))/Reduce(cbind,lapply(list_insample_ar,FUN = function(x)rowMeans(x$crps_equal)))
crps_center<-Reduce(cbind,lapply(list_insample,FUN = function(x)rowMeans(x$crps_center)))/Reduce(cbind,lapply(list_insample_ar,FUN = function(x)rowMeans(x$crps_center)))
crps_tails<-Reduce(cbind,lapply(list_insample,FUN = function(x)rowMeans(x$crps_tails)))/Reduce(cbind,lapply(list_insample_ar,FUN = function(x)rowMeans(x$crps_tails)))
crps_right<-Reduce(cbind,lapply(list_insample,FUN = function(x)rowMeans(x$crps_right)))/Reduce(cbind,lapply(list_insample_ar,FUN = function(x)rowMeans(x$crps_right)))
crps_left<-Reduce(cbind,lapply(list_insample,FUN = function(x)rowMeans(x$crps_left)))/Reduce(cbind,lapply(list_insample_ar,FUN = function(x)rowMeans(x$crps_left)))

colnames(crps_equal)<-names(list_insample)
colnames(crps_center)<-names(list_insample)
colnames(crps_tails)<-names(list_insample)
colnames(crps_right)<-names(list_insample)
colnames(crps_left)<-names(list_insample)

# betas
# Helper: build dataframe with numeric columns + country
build_df <- function(x_list, slot, q_index) {
  out_num <- Reduce(rbind, lapply(x_list, function(x) x[[slot]][, q_index]))
  df <- data.frame(
    country = names(x_list),
    out_num,
    stringsAsFactors = FALSE
  )
  # ensure numeric columns
  for (j in 2:ncol(df)) df[[j]] <- as.numeric(df[[j]])
  df
}

# BETA TABLES
betas_0.05 <- build_df(list_insample,      "beta", 1)
betas_0.25 <- build_df(list_insample,      "beta", 2)
betas_0.50 <- build_df(list_insample,      "beta", 3)
betas_0.75 <- build_df(list_insample,      "beta", 4)
betas_0.95 <- build_df(list_insample,      "beta", 5)

# PVALUE TABLES
pvalue_0.05 <- build_df(list_insample, "pvalue", 1)
pvalue_0.25 <- build_df(list_insample, "pvalue", 2)
pvalue_0.50 <- build_df(list_insample, "pvalue", 3)
pvalue_0.75 <- build_df(list_insample, "pvalue", 4)
pvalue_0.95 <- build_df(list_insample, "pvalue", 5)

# P(X<0%)
prob_low<-cbind("country"=names(list_insample),as.data.frame(Reduce(rbind,lapply(list_insample,FUN = function(x)x$prob[,1]))))
# P(X>3%)
prob_high<-cbind("country"=names(list_insample),as.data.frame(Reduce(rbind,lapply(list_insample,FUN = function(x)x$prob[,2]))))
# P(X>1%)
prob_1<-cbind("country"=names(list_insample),as.data.frame(Reduce(rbind,lapply(list_insample,FUN = function(x)x$prob[,3]))))
# P(X>2%)
prob_2<-cbind("country"=names(list_insample),as.data.frame(Reduce(rbind,lapply(list_insample,FUN = function(x)x$prob[,4]))))




Table<-data.frame(matrix(nrow=13,ncol = 21))
i<-1
colnames(Table) <-c(rep(c("0.05","0.5","0.95"),7))
rownames(Table)<-c("Intercept","pval1","Ly","pval2","FGlobal","pval3","FRegional","pval4","FIncome",
                   "pval5","R1","AIC","AIC_AR")
for (name in c("Canada","France","United States","Italy","Germany","United Kingdom","Japan")) {

  Table[1:10,1+i-1]<-unlist((rbind(
    c(betas_0.05[betas_0.05$country==name,][2]),
    c(pvalue_0.05[pvalue_0.05$country==name,][2]),
    c(betas_0.05[betas_0.05$country==name,][3]),
    c(pvalue_0.05[pvalue_0.05$country==name,][3]),
    c(betas_0.05[betas_0.05$country==name,][4]),
    c(pvalue_0.05[pvalue_0.05$country==name,][4]),
    c(betas_0.05[betas_0.05$country==name,][5]),
    c(pvalue_0.05[pvalue_0.05$country==name,][5]),
    c(betas_0.05[betas_0.05$country==name,][6]),
    c(pvalue_0.05[pvalue_0.05$country==name,][6]))))
  
  Table[11,1+i-1]<-R[R$`names(list_insample)`==name,2]/100
  Table[12,1+i-1]<-ICmodel[ICmodel$`names(list_insample)`==name,2]
  Table[13,1+i-1]<-ICar1[ICar1$`names(list_insample)`==name,2]
  
  Table[1:10,2+i-1]<-unlist((rbind(
    c(betas_0.50[betas_0.50$country==name,][2]),
    c(pvalue_0.50[pvalue_0.50$country==name,][2]),
    c(betas_0.50[betas_0.50$country==name,][3]),
    c(pvalue_0.50[pvalue_0.50$country==name,][3]),
    c(betas_0.50[betas_0.50$country==name,][4]),
    c(pvalue_0.50[pvalue_0.50$country==name,][4]),
    c(betas_0.50[betas_0.50$country==name,][5]),
    c(pvalue_0.50[pvalue_0.50$country==name,][5]),
    c(betas_0.50[betas_0.50$country==name,][6]),
    c(pvalue_0.50[pvalue_0.50$country==name,][6]))))
  
  Table[11,2+i-1]<-R[R$`names(list_insample)`==name,4]/100
  Table[12,2+i-1]<-ICmodel[ICmodel$`names(list_insample)`==name,4]
  Table[13,2+i-1]<-ICar1[ICar1$`names(list_insample)`==name,4]
  
  Table[1:10,3+i-1]<-unlist((rbind(
    c(betas_0.95[betas_0.95$country==name,][2]),
    c(pvalue_0.95[pvalue_0.95$country==name,][2]),
    c(betas_0.95[betas_0.95$country==name,][3]),
    c(pvalue_0.95[pvalue_0.95$country==name,][3]),
    c(betas_0.95[betas_0.95$country==name,][4]),
    c(pvalue_0.95[pvalue_0.95$country==name,][4]),
    c(betas_0.95[betas_0.95$country==name,][5]),
    c(pvalue_0.95[pvalue_0.95$country==name,][5]),
    c(betas_0.95[betas_0.95$country==name,][6]),
    c(pvalue_0.95[pvalue_0.95$country==name,][6]))))
  
  Table[11,3+i-1]<-R[R$`names(list_insample)`==name,6]/100
  Table[12,3+i-1]<-ICmodel[ICmodel$`names(list_insample)`==name,6]
  Table[13,3+i-1]<-ICar1[ICar1$`names(list_insample)`==name,6]
  i<-i+3
}


kbl(round(Table,3), caption = "Table B2",booktabs = T,
    linesep = c(""),
    format="latex") %>%
  kable_classic(full_width = F) %>% 
  kable_styling(latex_options = "scale_down") %>% 
  save_kable(paste('../output/Tables/Table_B2.tex'),float = FALSE)



