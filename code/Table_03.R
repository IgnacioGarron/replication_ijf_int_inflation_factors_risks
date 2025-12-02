
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
library(pcse)


##### important function!!!!
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
data <- readxl::read_excel("../input/Data_complete_rev.xlsx")

# list_h1_bench<-loadRData("../Data/list_h1_bench.RData")
# list_h1_m1<-loadRData("../Data/list_h1_m1.RData")
# list_h1_m2<-loadRData("../Data/list_h1_m2.RData")
# list_h1_m3<-loadRData("../Data/list_h1_m3.RData")
# list_h1_m4<-loadRData("../Data/list_h1_m4.RData")

date <- (data %>%  filter(country=="United States"))[-1,6]
date<-as.Date(date$date)
date<-format(as.Date(date),"%Y") # Get year dates
date<-as.numeric(date) # as numeric

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


bannerCommenter::banner("Get data")
##################################################################
##                           Get data                           ##
##################################################################

# Forecast tables
table_f<-data_frame(c())
data_crps_equal<-c()
data_crps_left<-c()
data_crps_right<-c()
data_qs_05<-c()
data_qs_50<-c()
data_qs_95<-c()

for (i in names){
  data_crps_equal=cbind(data_crps_equal,-Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_equal))))+
                                        Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_equal)))))
  
  data_crps_left=cbind(data_crps_left,-Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_left))))+
                                      Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_left)))))
  
  data_crps_right=cbind(data_crps_right,-Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_right))))+
                                        Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_right)))))
  
  data_qs_05=cbind(data_qs_05,-Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[1]))+
                              Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[1])))
  
  data_qs_50=cbind(data_qs_50,-Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[3]))+
                              Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[3])))
  
  data_qs_95=cbind(data_qs_95,-Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[5]))+
                              Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[5])))
  
}

data_crps_equal<-data.frame(data_crps_equal)
data_crps_left<-data.frame(data_crps_left)
data_crps_right<-data.frame(data_crps_right)
data_qs_05<-data.frame(data_qs_05)
data_qs_50<-data.frame(data_qs_50)
data_qs_95<-data.frame(data_qs_95)


colnames(data_crps_equal)<-names
colnames(data_crps_left)<-names
colnames(data_crps_right)<-names
colnames(data_qs_05)<-names
colnames(data_qs_50)<-names
colnames(data_qs_95)<-names

data_crps_equal <- cbind("year"=date[(length(date)-132+1):length(date)],data_crps_equal)
data_crps_left  <- cbind("year"=date[(length(date)-132+1):length(date)],data_crps_left)
data_crps_right <- cbind("year"=date[(length(date)-132+1):length(date)],data_crps_right)
data_qs_05      <- cbind("year"=date[(length(date)-132+1):length(date)],data_qs_05)
data_qs_50      <- cbind("year"=date[(length(date)-132+1):length(date)],data_qs_50)
data_qs_95      <- cbind("year"=date[(length(date)-132+1):length(date)],data_qs_95)

# Collapse  
data_crps_equal <- data_crps_equal %>% pivot_longer(cols = -1,names_to = "country",values_to = "d") %>% group_by(year,country) %>% summarise(d=mean(d))
data_crps_left <- data_crps_left %>% pivot_longer(cols = -1,names_to = "country",values_to = "d") %>% group_by(year,country) %>% summarise(d=mean(d))
data_crps_right <- data_crps_right %>% pivot_longer(cols = -1,names_to = "country",values_to = "d") %>% group_by(year,country) %>% summarise(d=mean(d))
data_qs_05 <- data_qs_05 %>% pivot_longer(cols = -1,names_to = "country",values_to = "d") %>% group_by(year,country) %>% summarise(d=mean(d))
data_qs_50 <- data_qs_50 %>% pivot_longer(cols = -1,names_to = "country",values_to = "d") %>% group_by(year,country) %>% summarise(d=mean(d))
data_qs_95 <- data_qs_95 %>% pivot_longer(cols = -1,names_to = "country",values_to = "d") %>% group_by(year,country) %>% summarise(d=mean(d))


# Merge with cross determinants
data_cross <- readxl::read_excel("../input/WDD.xlsx",sheet = "Data")


# setdiff(unique(data_crps_equal$country),data$country)

data_crps_equal <- left_join(data_crps_equal,data_cross,by = c("year","country"),keep = F)
data_crps_left  <- left_join(data_crps_left,data_cross,by = c("year","country"),keep = F)
data_crps_right <- left_join(data_crps_right,data_cross,by = c("year","country"),keep = F)
data_qs_05 <- left_join(data_qs_05,data_cross,by = c("year","country"),keep = F)
data_qs_50 <- left_join(data_qs_50,data_cross,by = c("year","country"),keep = F)
data_qs_95 <- left_join(data_qs_95,data_cross,by = c("year","country"),keep = F)

# Missing GDP_pc_growth
unique(data_crps_equal[is.na(data_crps_equal$gdp_pc_growth),"country"])

# Missing GDP_growth
unique(data_crps_equal[is.na(data_crps_equal$gdp_growth),"country"])

# Missing unemployment_rate
unique(data_crps_equal[is.na(data_crps_equal$unemployment_rate),c("country")])

# Missing openess_percentage
unique(data_crps_equal[is.na(data_crps_equal$openess_percentage),c("country")])

# Missing inflation
unique(data_crps_equal[is.na(data_crps_equal$inflation_rate),c("country")])

# No regular unemployment data for Antigua and Barbuda and St. Kitts and Nevis


# openxlsx::write.xlsx(data_crps_equal,paste0(paste0("../Data/data_crps_equal_complete.xlsx")),rowNames=F)
# openxlsx::write.xlsx(data_crps_left,paste0(paste0("../Data/data_crps_left_complete.xlsx")),rowNames=F)
# openxlsx::write.xlsx(data_crps_right,paste0(paste0("../Data/data_crps_right_complete.xlsx")),rowNames=F)
# openxlsx::write.xlsx(data_qs_05,paste0(paste0("../Data/data_qs_05_complete.xlsx")),rowNames=F)
# openxlsx::write.xlsx(data_qs_50,paste0(paste0("../Data/data_qs_50_complete.xlsx")),rowNames=F)
# openxlsx::write.xlsx(data_qs_95,paste0(paste0("../Data/data_qs_95_complete.xlsx")),rowNames=F)


data_crps_equal <- data_crps_equal %>% filter(country!="Antigua and Barbuda" & country!="St. Kitts and Nevis")
data_crps_left  <- data_crps_left %>% filter(country!="Antigua and Barbuda" & country!="St. Kitts and Nevis")
data_crps_right <- data_crps_right %>% filter(country!="Antigua and Barbuda" & country!="St. Kitts and Nevis")
data_qs_05 <- data_qs_05 %>% filter(country!="Antigua and Barbuda" & country!="St. Kitts and Nevis")
data_qs_50 <- data_qs_50 %>% filter(country!="Antigua and Barbuda" & country!="St. Kitts and Nevis")
data_qs_95 <- data_qs_95 %>% filter(country!="Antigua and Barbuda" & country!="St. Kitts and Nevis")


####################### GPR (for appendix) #####################################

data_gpr <- readxl::read_excel("../input/data_gpr.xls")
data_cbie <- readxl::read_excel("../input/CBIData_Romelli_2025.xlsx",sheet = "CBI data") %>% 
  select(country,year,cbie_index,cbie_cbiconstitution)

data_gpr <- data_gpr %>% pivot_longer(cols = -c(1,2),names_to = "country",values_to = "gpr") %>% 
  arrange(country,year) %>% group_by(country) %>% mutate(gpr=scale(gpr)) %>% group_by(country,year) %>% 
      summarise(gpr=mean(gpr))


data_crps_equal_gpr <- left_join(data_crps_equal,data_gpr,by = c("year","country"),keep = F)
data_crps_equal_gpr <- data_crps_equal_gpr[!is.na(data_crps_equal_gpr)[,12],]
print(unique(data_crps_equal_gpr[,2]),n = length(unique(data_crps_equal_gpr[,2])$country))

data_crps_left_gpr <- left_join(data_crps_left,data_gpr,by = c("year","country"),keep = F)
data_crps_left_gpr <- data_crps_left_gpr[!is.na(data_crps_left_gpr)[,12],]
print(unique(data_crps_left_gpr[,2]),n = length(unique(data_crps_left_gpr[,2])$country))

data_crps_right_gpr <- left_join(data_crps_right,data_gpr,by = c("year","country"),keep = F)
data_crps_right_gpr <- data_crps_right_gpr[!is.na(data_crps_right_gpr)[,12],]
print(unique(data_crps_right_gpr[,2]),n = length(unique(data_crps_right_gpr[,2])$country))

data_qs_05_gpr <- left_join(data_qs_05,data_gpr,by = c("year","country"),keep = F)
data_qs_05_gpr <- data_qs_05_gpr[!is.na(data_qs_05_gpr)[,12],]
print(unique(data_qs_05_gpr[,2]),n = length(unique(data_qs_05_gpr[,2])$country))

data_qs_50_gpr <- left_join(data_qs_50,data_gpr,by = c("year","country"),keep = F)
data_qs_50_gpr <- data_qs_50_gpr[!is.na(data_qs_50_gpr)[,12],]
print(unique(data_qs_50_gpr[,2]),n = length(unique(data_qs_50_gpr[,2])$country))

data_qs_95_gpr <- left_join(data_qs_95,data_gpr,by = c("year","country"),keep = F)
data_qs_95_gpr <- data_qs_95_gpr[!is.na(data_qs_95_gpr)[,12],]
print(unique(data_qs_95_gpr[,2]),n = length(unique(data_qs_95_gpr[,2])$country))

# MPI data
data_crps_equal_gpr_mpi <- left_join(data_crps_equal_gpr,data_cbie,by = c("year","country"),keep = F) %>% filter(country!="Israel")
data_crps_left_gpr_mpi <- left_join(data_crps_left_gpr,data_cbie,by = c("year","country"),keep = F) %>% filter(country!="Israel")
data_crps_right_gpr_mpi <- left_join(data_crps_right_gpr,data_cbie,by = c("year","country"),keep = F) %>% filter(country!="Israel")
data_qs_05_gpr_mpi <- left_join(data_qs_05_gpr,data_cbie,by = c("year","country"),keep = F) %>% filter(country!="Israel")
data_qs_50_gpr_mpi <- left_join(data_qs_50_gpr,data_cbie,by = c("year","country"),keep = F) %>% filter(country!="Israel")
data_qs_95_gpr_mpi <- left_join(data_qs_95_gpr,data_cbie,by = c("year","country"),keep = F) %>% filter(country!="Israel")


data_crps_equal_gpr %>% ggplot() + geom_line(aes(x=year,y=gpr,col=country))+
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = 2012:2022)


bannerCommenter::banner("Regressions 3 (with MPI) 1")
##################################################################
##                  Regressions 3 (with MPI) 1                  ##
##################################################################

data_crps_equal_gpr_mpi$year<-as.integer(data_crps_equal_gpr_mpi$year)
data_crps_equal_gpr_mpi$country<-as.factor(data_crps_equal_gpr_mpi$country)

data_crps_left_gpr_mpi$year<-as.integer(data_crps_left_gpr_mpi$year)
data_crps_left_gpr_mpi$country<-as.factor(data_crps_left_gpr_mpi$country)

data_crps_right_gpr_mpi$year<-as.integer(data_crps_right_gpr_mpi$year)
data_crps_right_gpr_mpi$country<-as.factor(data_crps_right_gpr_mpi$country)

data_qs_05_gpr_mpi$year<-as.integer(data_qs_05_gpr_mpi$year)
data_qs_05_gpr_mpi$country<-as.factor(data_qs_05_gpr_mpi$country)

data_qs_50_gpr_mpi$year<-as.integer(data_qs_50_gpr_mpi$year)
data_qs_50_gpr_mpi$country<-as.factor(data_qs_50_gpr_mpi$country)

data_qs_95_gpr_mpi$year<-as.integer(data_qs_95_gpr_mpi$year)
data_qs_95_gpr_mpi$country<-as.factor(data_qs_95_gpr_mpi$country)


lm1 <- lm( d ~ openess_percentage+gdp_growth+gdp_pc_growth+inflation_rate+unemployment_rate+gpr+cbie_index+as.factor(country)+as.factor(year), data = data_crps_equal_gpr_mpi)
lm1.pcse <- pcse(lm1, groupN=data_crps_equal_gpr_mpi$country, groupT=data_crps_equal_gpr_mpi$year,pairwise = F) # BALANCED DATA pairwase = F
robust_se1 <- lm1.pcse$se

lm2 <- lm( d ~ openess_percentage+gdp_growth+gdp_pc_growth+inflation_rate+unemployment_rate+gpr+cbie_index +as.factor(country)+as.factor(year), data = data_crps_right_gpr_mpi)
lm2.pcse <- pcse(lm2, groupN=data_crps_right_gpr_mpi$country, groupT=data_crps_right_gpr_mpi$year,pairwise = F)
robust_se2 <- lm2.pcse$se

lm3 <- lm( d ~ openess_percentage+gdp_growth+gdp_pc_growth+inflation_rate+unemployment_rate+gpr+cbie_index +as.factor(country)+as.factor(year), data = data_crps_left_gpr_mpi)
lm3.pcse <- pcse(lm3, groupN=data_crps_left_gpr_mpi$country, groupT=data_crps_left_gpr_mpi$year,pairwise = F)
robust_se3 <- lm3.pcse$se

lm4 <- lm( d ~ openess_percentage+gdp_growth+gdp_pc_growth+inflation_rate+unemployment_rate+gpr+cbie_index +as.factor(country)+as.factor(year), data = data_qs_05_gpr_mpi)
lm4.pcse <- pcse(lm4, groupN=data_qs_05_gpr_mpi$country, groupT=data_qs_05_gpr_mpi$year,pairwise = F)
robust_se4 <- lm4.pcse$se

lm5 <- lm( d ~ openess_percentage+gdp_growth+gdp_pc_growth+inflation_rate+unemployment_rate+gpr+cbie_index +as.factor(country)+as.factor(year), data = data_qs_50_gpr_mpi)
lm5.pcse <- pcse(lm5, groupN=data_qs_50_gpr_mpi$country, groupT=data_qs_50_gpr_mpi$year,pairwise = F)
robust_se5 <- lm5.pcse$se

lm6 <- lm( d ~ openess_percentage+gdp_growth+gdp_pc_growth+inflation_rate+unemployment_rate+gpr+cbie_index +as.factor(country)+as.factor(year), data = data_qs_95_gpr_mpi)
lm6.pcse <- pcse(lm6, groupN=data_qs_95_gpr_mpi$country, groupT=data_qs_95_gpr_mpi$year,pairwise = F)
robust_se6 <- lm6.pcse$se

stargazer::stargazer(
  lm1, lm2, lm3, lm4, lm5, lm6,
  title       = "Table 3",
  align       = FALSE,
  se          = list(robust_se1, robust_se2, robust_se3,
                     robust_se4, robust_se5, robust_se6),
  no.space    = TRUE,
  omit        = c("country", "year"),
  column.sep.width = "3pt",

  column.labels = c("CRPS-equal","CRPS-right","CRPS-left",
                    "QS(0.05)","QS(0.50)","QS(0.95)"),
  dep.var.caption = "",
  covariate.labels = c(
    "Openness (%)","GDP growth (%)","GDP per capita growth (%)",
    "Inflation (%)","Unemployment (%)",
    "GPR (standardized)","CBIE (index)"
  ),
  out = "../output/Tables/Table_03.tex"   # <<<< SAVE HERE
)

