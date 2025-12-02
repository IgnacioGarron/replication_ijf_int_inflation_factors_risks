
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
library(pEPA) # Tests of Equal Predictive Accuracy for Panels of Forecasts

##### important function!!!!
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

source("functions/ThreeLevelMDFM.R") # CCA
# source("functions/get_nonparametric_variance.R") # CCA

#################################################################
##                         Import data                         ##
#################################################################
data <- readxl::read_excel("../input/Data_complete_rev.xlsx")
Yorig <- readxl::read_excel("../input/X.xlsx")
groups <- readxl::read_excel("../input/structure_dfm.xlsx")
date <- (data %>%  filter(country=="United States"))[-1,6]
date<-as.Date(date$date)
R1 <- ThreeMDFM1(Yorig,groups,spec=1)

# R1$Factors_Inflation
R1$Factors_Inflation <- R1$Factors_Inflation %*% diag(c(-1,1,-1,-1,-1,-1,1,1))

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

set.seed(123)

Table_PANEL<-matrix(nrow = 4, ncol = 6)

rownames(Table_PANEL)<-c("S^{(1)}_{nT}","",
                         "S^{(3)}_{nT}","")

colnames(Table_PANEL)<-c("QS(0.05)","QS(0.50)","QS(0.95)","CRPS-E","CRPS-L","CRPS-R")


Table_a<-c()
Table_b<-c()
y <-matrix(nrow = 115, ncol = 132)
i<-0
for (country in names) {
  i<-i+1
  loss_a=Reduce(cbind,lapply(mod1[[country]], function(x)x$qs[1]))
  loss_b=Reduce(cbind,lapply(benchmark[[country]], function(x)x$qs[1]))
  Table_a<-rbind(Table_a,loss_a)
  Table_b<-rbind(Table_b,loss_b)
  y[i,] <- rep(0,132) 
  }

# Already QS is computed so no need for realized (E.G., e_1=QS_1-0=QS_1). 
# Original code computes loss function.

t1 <- pool_av.S1.test(evaluated1=Table_b,evaluated2=Table_a,realized=y,loss.type="AE")
t3 <- pool_av.S3.test(evaluated1=Table_b,evaluated2=Table_a,realized=y,loss.type="AE")

Table_PANEL[1,1]<-t1$statistic
Table_PANEL[2,1]<-t1$p.value
Table_PANEL[3,1]<-t3$statistic
Table_PANEL[4,1]<-t3$p.value

Table_a<-c()
Table_b<-c()
y <-matrix(nrow = 115, ncol = 132)
i<-0
for (country in names) {
  i<-i+1
  loss_a=Reduce(cbind,lapply(mod1[[country]], function(x)x$qs[3]))
  loss_b=Reduce(cbind,lapply(benchmark[[country]], function(x)x$qs[3]))
  Table_a<-rbind(Table_a,loss_a)
  Table_b<-rbind(Table_b,loss_b)
  y[i,] <- rep(0,132) 
}

# Already QS is computed so no need for realized (E.G., e_1=QS_1-0=QS_1). 
# Original code computes loss function.

t1 <- pool_av.S1.test(evaluated1=Table_b,evaluated2=Table_a,realized=y,loss.type="AE")
t3 <- pool_av.S3.test(evaluated1=Table_b,evaluated2=Table_a,realized=y,loss.type="AE")

Table_PANEL[1,2]<-t1$statistic
Table_PANEL[2,2]<-t1$p.value
Table_PANEL[3,2]<-t3$statistic
Table_PANEL[4,2]<-t3$p.value


Table_a<-c()
Table_b<-c()
y <-matrix(nrow = 115, ncol = 132)
i<-0
for (country in names) {
  i<-i+1
  loss_a=Reduce(cbind,lapply(mod1[[country]], function(x)x$qs[5]))
  loss_b=Reduce(cbind,lapply(benchmark[[country]], function(x)x$qs[5]))
  Table_a<-rbind(Table_a,loss_a)
  Table_b<-rbind(Table_b,loss_b)
  y[i,] <- rep(0,132) 
}

# Already QS is computed so no need for realized (E.G., e_1=QS_1-0=QS_1). 
# Original code computes loss function.

t1 <- pool_av.S1.test(evaluated1=Table_b,evaluated2=Table_a,realized=y,loss.type="AE")
t3 <- pool_av.S3.test(evaluated1=Table_b,evaluated2=Table_a,realized=y,loss.type="AE")

Table_PANEL[1,3]<-t1$statistic
Table_PANEL[2,3]<-t1$p.value
Table_PANEL[3,3]<-t3$statistic
Table_PANEL[4,3]<-t3$p.value


Table_a<-c()
Table_b<-c()
y <-matrix(nrow = 115, ncol = 132)
i<-0
for (country in names) {
  i<-i+1
  loss_a=Reduce(cbind,lapply(mod1[[country]], function(x)mean(head(x$crps_equal))))
  loss_b=Reduce(cbind,lapply(benchmark[[country]], function(x)mean(head(x$crps_equal))))
  Table_a<-rbind(Table_a,loss_a)
  Table_b<-rbind(Table_b,loss_b)
  y[i,] <- rep(0,132) 
}

# Already QS is computed so no need for realized (E.G., e_1=QS_1-0=QS_1). 
# Original code computes loss function.

t1 <- pool_av.S1.test(evaluated1=Table_b,evaluated2=Table_a,realized=y,loss.type="AE")
t3 <- pool_av.S3.test(evaluated1=Table_b,evaluated2=Table_a,realized=y,loss.type="AE")

Table_PANEL[1,4]<-t1$statistic
Table_PANEL[2,4]<-t1$p.value
Table_PANEL[3,4]<-t3$statistic
Table_PANEL[4,4]<-t3$p.value


Table_a<-c()
Table_b<-c()
y <-matrix(nrow = 115, ncol = 132)
i<-0
for (country in names) {
  i<-i+1
  loss_a=Reduce(cbind,lapply(mod1[[country]], function(x)mean(head(x$crps_left))))
  loss_b=Reduce(cbind,lapply(benchmark[[country]], function(x)mean(head(x$crps_left))))
  Table_a<-rbind(Table_a,loss_a)
  Table_b<-rbind(Table_b,loss_b)
  y[i,] <- rep(0,132) 
}

# Already QS is computed so no need for realized (E.G., e_1=QS_1-0=QS_1). 
# Original code computes loss function.

t1 <- pool_av.S1.test(evaluated1=Table_b,evaluated2=Table_a,realized=y,loss.type="AE")
t3 <- pool_av.S3.test(evaluated1=Table_b,evaluated2=Table_a,realized=y,loss.type="AE")

Table_PANEL[1,5]<-t1$statistic
Table_PANEL[2,5]<-t1$p.value
Table_PANEL[3,5]<-t3$statistic
Table_PANEL[4,5]<-t3$p.value


Table_a<-c()
Table_b<-c()
y <-matrix(nrow = 115, ncol = 132)
i<-0
for (country in names) {
  i<-i+1
  loss_a=Reduce(cbind,lapply(mod1[[country]], function(x)mean(head(x$crps_right))))
  loss_b=Reduce(cbind,lapply(benchmark[[country]], function(x)mean(head(x$crps_right))))
  Table_a<-rbind(Table_a,loss_a)
  Table_b<-rbind(Table_b,loss_b)
  y[i,] <- rep(0,132) 
}

# Already QS is computed so no need for realized (E.G., e_1=QS_1-0=QS_1). 
# Original code computes loss function.

t1 <- pool_av.S1.test(evaluated1=Table_b,evaluated2=Table_a,realized=y,loss.type="AE")
t3 <- pool_av.S3.test(evaluated1=Table_b,evaluated2=Table_a,realized=y,loss.type="AE")

Table_PANEL[1,6]<-t1$statistic
Table_PANEL[2,6]<-t1$p.value
Table_PANEL[3,6]<-t3$statistic
Table_PANEL[4,6]<-t3$p.value


table_panel_final<-round(Table_PANEL[1:4,],2)


#################################################################
##                        Tables                               ##
#################################################################


Table<-data.frame(matrix(nrow = 8,ncol=7))
Table[,1]<-c("Global","Africa","America","Asia and Oceania","Europe","Advance","HMI","LI")
colnames(Table)<-c("","QS(0.05)","QS(0.50)","QS(0.95)","CRPS-E", "CRPS-L", "CRPS-R")

Table1<-list()
for (j in 1){
  Table1[[j]]<-data.frame(matrix(nrow = 115,6))
  row.names(Table1[[j]])<-names
}
Table2<-Table1
j<-1

for (i in names){
  Table1[[j]][i,1] = ((dm.test(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[1])),
                               Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[1])),
                               alternative=c("greater"),power=1)$p.value)<0.1)*1
  Table1[[j]][i,2] = ((dm.test(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[3])),
                               Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[3])),
                               alternative=c("greater"),power=1)$p.value)<0.1)*1
  Table1[[j]][i,3] = ((dm.test(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[5])),
                               Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[5])),
                               alternative=c("greater"),power=1)$p.value)<0.1)*1
  Table1[[j]][i,4] = ((dm.test(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_equal)))),
                               Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_equal)))),
                               alternative=c("greater"),power=1)$p.value)<0.1)*1
  Table1[[j]][i,5] = ((dm.test(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_left)))),
                               Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_left)))),
                               alternative=c("greater"),power=1)$p.value)<0.1)*1
  Table1[[j]][i,6] = ((dm.test(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_right)))),
                               Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_right)))),
                               alternative=c("greater"),power=1)$p.value)<0.1)*1
  
  # Scores
  Table2[[j]][i,1] = round(mean(Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[1])))/
                             mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[1]))),3)
  Table2[[j]][i,2] = round(mean(Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[3])))/
                             mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[3]))),3)
  Table2[[j]][i,3] = round(mean(Reduce(rbind,lapply(mod1[[i]], function(x)x$qs[5])))/
                             mean(Reduce(rbind,lapply(benchmark[[i]], function(x)x$qs[5]))),3)
  
  Table2[[j]][i,4] = round(mean(Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_equal)))))/
                             mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_equal))))),3)
  Table2[[j]][i,5] = round(mean(Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_left)))))/
                             mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_left))))),3)
  Table2[[j]][i,6] = round(mean(Reduce(rbind,lapply(mod1[[i]], function(x)mean(head(x$crps_right)))))/
                             mean(Reduce(rbind,lapply(benchmark[[i]], function(x)mean(head(x$crps_right))))),3)
  
}
colnames(Table1[[1]])<-c("QS(0.05)-DMpval<0.10","QS(0.50)-DMpval<0.10","QS(0.95)-DMpval<0.10","CRPS-E-DMpval<0.10", "CRPS-L-DMpval<0.10", "CRPS-R-DMpval<0.10")
colnames(Table2[[1]])<-c("QS(0.05)","QS(0.50)","QS(0.95)","CRPS-E", "CRPS-L", "CRPS-R")


Table[1,2]<-round(mean(Table1[[j]][,1]),3)*100
Table[1,3]<-round(mean(Table1[[j]][,2]),3)*100
Table[1,4]<-round(mean(Table1[[j]][,3]),3)*100
Table[1,5]<-round(mean(Table1[[j]][,4]),3)*100
Table[1,6]<-round(mean(Table1[[j]][,5]),3)*100
Table[1,7]<-round(mean(Table1[[j]][,6]),3)*100

Table[2,2]<-round(mean(Table1[[j]][idx1_R1$Country,1]),3)*100
Table[2,3]<-round(mean(Table1[[j]][idx1_R1$Country,2]),3)*100
Table[2,4]<-round(mean(Table1[[j]][idx1_R1$Country,3]),3)*100
Table[2,5]<-round(mean(Table1[[j]][idx1_R1$Country,4]),3)*100
Table[2,6]<-round(mean(Table1[[j]][idx1_R1$Country,5]),3)*100
Table[2,7]<-round(mean(Table1[[j]][idx1_R1$Country,6]),3)*100

Table[3,2]<-round(mean(Table1[[j]][idx1_R2$Country,1]),3)*100
Table[3,3]<-round(mean(Table1[[j]][idx1_R2$Country,2]),3)*100
Table[3,4]<-round(mean(Table1[[j]][idx1_R2$Country,3]),3)*100
Table[3,5]<-round(mean(Table1[[j]][idx1_R2$Country,4]),3)*100
Table[3,6]<-round(mean(Table1[[j]][idx1_R2$Country,5]),3)*100
Table[3,7]<-round(mean(Table1[[j]][idx1_R2$Country,6]),3)*100

Table[4,2]<-round(mean(Table1[[j]][idx1_R3$Country,1]),3)*100
Table[4,3]<-round(mean(Table1[[j]][idx1_R3$Country,2]),3)*100
Table[4,4]<-round(mean(Table1[[j]][idx1_R3$Country,3]),3)*100
Table[4,5]<-round(mean(Table1[[j]][idx1_R3$Country,4]),3)*100
Table[4,6]<-round(mean(Table1[[j]][idx1_R3$Country,5]),3)*100
Table[4,7]<-round(mean(Table1[[j]][idx1_R3$Country,6]),3)*100

Table[5,2]<-round(mean(Table1[[j]][idx1_R4$Country,1]),3)*100
Table[5,3]<-round(mean(Table1[[j]][idx1_R4$Country,2]),3)*100
Table[5,4]<-round(mean(Table1[[j]][idx1_R4$Country,3]),3)*100
Table[5,5]<-round(mean(Table1[[j]][idx1_R4$Country,4]),3)*100
Table[5,6]<-round(mean(Table1[[j]][idx1_R4$Country,5]),3)*100
Table[5,7]<-round(mean(Table1[[j]][idx1_R4$Country,6]),3)*100

Table[6,2]<-round(mean(Table1[[j]][idx1_R5$Country,1]),3)*100
Table[6,3]<-round(mean(Table1[[j]][idx1_R5$Country,2]),3)*100
Table[6,4]<-round(mean(Table1[[j]][idx1_R5$Country,3]),3)*100
Table[6,5]<-round(mean(Table1[[j]][idx1_R5$Country,4]),3)*100
Table[6,6]<-round(mean(Table1[[j]][idx1_R5$Country,5]),3)*100
Table[6,7]<-round(mean(Table1[[j]][idx1_R5$Country,6]),3)*100

Table[7,2]<-round(mean(Table1[[j]][idx1_R6$Country,1]),3)*100
Table[7,3]<-round(mean(Table1[[j]][idx1_R6$Country,2]),3)*100
Table[7,4]<-round(mean(Table1[[j]][idx1_R6$Country,3]),3)*100
Table[7,5]<-round(mean(Table1[[j]][idx1_R6$Country,4]),3)*100
Table[7,6]<-round(mean(Table1[[j]][idx1_R6$Country,5]),3)*100
Table[7,7]<-round(mean(Table1[[j]][idx1_R6$Country,6]),3)*100

Table[8,2]<-round(mean(Table1[[j]][idx1_R7$Country,1]),3)*100
Table[8,3]<-round(mean(Table1[[j]][idx1_R7$Country,2]),3)*100
Table[8,4]<-round(mean(Table1[[j]][idx1_R7$Country,3]),3)*100
Table[8,5]<-round(mean(Table1[[j]][idx1_R7$Country,4]),3)*100
Table[8,6]<-round(mean(Table1[[j]][idx1_R7$Country,5]),3)*100
Table[8,7]<-round(mean(Table1[[j]][idx1_R7$Country,6]),3)*100


colnames(Table)<-c("","QS(0.05)","QS(0.50)","QS(0.95)","CRPS-E", "CRPS-L", "CRPS-R")

table_DM<-Table[,-1]
colnames(table_DM)<-c("QS(0.05)","QS(0.50)","QS(0.95)","CRPS-E", "CRPS-L", "CRPS-R")
rownames(table_DM)<-Table[,1]
colnames(table_panel_final)<-c("QS(0.05)","QS(0.50)","QS(0.95)","CRPS-E", "CRPS-L", "CRPS-R")
rownames(table_panel_final)<-c("S^1","","S^3","")
save_table<-rbind(table_DM,table_panel_final)


kbl(save_table, caption = "Table 2",booktabs = T,
    linesep = c(""),
    format="latex") %>%
  kable_classic(full_width = F) %>% 
  kable_styling(latex_options = "scale_down") %>% 
  save_kable(paste('../output/Tables/Table_02.tex'),float = FALSE)


