rm(list = ls()) # Limpiar environment

library(tidyverse)
library(ggpubr) # ggarrrange
library(kableExtra) # latex tables
library(spData) # Spatial Data
library(sf) # the spatial workhorse
library(openxlsx)


source("functions/data_complete.R")

#############################################
#             Preliminaries
#############################################

start_date="1999-01-01"
end_date="2022-12-01"

data<-data_inf(start_date=start_date,end_date=end_date)  

mapdata <- map_data('world') %>% rename(country=region)
mapdatax<-left_join(mapdata,data$Data_available,by='country')


bannerCommenter::banner("Match names in both data sets")
#################################################################
##                Match names in both data sets                ##
#################################################################

sort(unique(mapdata$country))

mapdata$country[mapdata$country=="Antigua"]<-"Antigua and Barbuda"
mapdata$country[mapdata$country=="Barbuda"]<-"Antigua and Barbuda"
mapdata$country[mapdata$country=="USA"]<-"United States"
mapdata$country[mapdata$country=="Virgin Islands"]<-"British Virgin Islands"
mapdata$country[mapdata$country=="Brunei"]<-"Brunei Darussalam"
mapdata$country[mapdata$country=="Cape Verde"]<-"Cabo Verde" 
mapdata$country[mapdata$country=="Russia"]<-"Russian Federation"
mapdata$country[mapdata$country=="UK"]<-"United Kingdom"
mapdata$country[mapdata$country=="Venezuela"]<-"Venezuela, RB"
mapdata$country[mapdata$country=="Democratic Republic of the Congo"]<-"Congo, Dem. Rep."
mapdata$country[mapdata$country=="Republic of Congo"]<-"Congo, Rep."
mapdata$country[mapdata$country=="Ivory Coast"]<-"Cote d'Ivoire"
mapdata$country[mapdata$country=="Egypt"]<-"Egypt, Arab Rep."
mapdata$country[mapdata$country=="Gambia"]<-"Gambia, The"
mapdata$country[mapdata$country=="Iran"]<-"Iran, Islamic Rep."
mapdata$country[mapdata$country=="South Korea"]<-"Korea, Rep."
mapdata$country[mapdata$country=="Kyrgyzstan" ]<-"Kyrgyz Republic" 
mapdata$country[mapdata$country=="Laos"]<-"Lao, PDR" 
mapdata$country[mapdata$country=="Moldova" ]<-"Moldova, Rep."
mapdata$country[mapdata$country=="Montserrat"]<-"Montserratit"
mapdata$country[mapdata$country=="Saint Kitts"]<-"St. Kitts and Nevis"
mapdata$country[mapdata$country=="Saint Lucia"]<-"St. Lucia"
mapdata$country[mapdata$country=="Saint Vincent"]<-"St. Vincent and the Grenadines"
mapdata$country[mapdata$country=="Syria"]<-"Syrian Arab Republic"
mapdata$country[mapdata$country== "Taiwan" ]<-"Taiwan, China"
mapdata$country[mapdata$country=="Tanzania"]<-"Tanzania, United Rep."
#mapdata$country[mapdata$country==]<-"Trinidad and Tobago"
mapdata$country[mapdata$country== "Western Sahara"]<-"West Bank and Gaza"   
mapdata$country[mapdata$country=="Yemen"  ]<-"Yemen, Rep."
mapdata$country[mapdata$country=="Swaziland"  ]<-"Eswatini"
mapdata$country[mapdata$country=="Trinidad" ]<-"Trinidad and Tobago" 




mapdatax<-left_join(mapdata,data$Data_available,by='country')

mapdatax[is.na(mapdatax$group_income),"group_income"]<-"Missing"

map1<-mapdatax%>%
  ggplot(aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=as.factor(group_income)),color="white",size = 0.2,
               show.legend = T,na.rm = T)+ 
  geom_polygon(fill=NA,color="black",size = 0.2,
               show.legend = F,na.rm = T)+ 
  labs(fill= paste0("Countries (",length(unique(data$data_complete$country)),"): from ",year(start_date)," to ",year(end_date)))+
  theme_bw()+
  theme(axis.title=element_blank(),axis.text=element_blank(),
        axis.ticks=element_blank(),legend.position = "bottom")+
  scale_fill_manual(values = c("red","forestgreen","blue","white"),
                    labels=c(paste0("Advanced Economies (",
                            length(unique(mapdatax[mapdatax$group_income=="Advanced Economies","country"]))+1,")"), # Macao in China
                            paste0( "EMDEs (",
                                    length(unique(mapdatax[mapdatax$group_income=="Emerging and Developing Economies","country"])),")"),
                            paste0("Low income EMDEs (",
                                   length(unique(mapdatax[mapdatax$group_income=="Low income EMDEs","country"])),")"),
                            ""))

sort(unique(mapdatax[mapdatax$group_income=="Advanced Economies","country"]))

mapdatax[is.na(mapdatax$group_geo),"group_geo"]<-"Missing"


map2<-mapdatax%>%
  ggplot(aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=as.factor(group_geo)),color="white",size = 0.2,
               show.legend = T,na.rm = T)+ 
  geom_polygon(fill=NA,color="black",size = 0.2,
               show.legend = F,na.rm = T)+ 
  labs(fill= paste0("Countries (",length(unique(data$data_complete$country)),"): from ",year(start_date)," to ",year(end_date)))+
  theme_bw()+
  theme(axis.title=element_blank(),axis.text=element_blank(),
        axis.ticks=element_blank(),legend.position = "bottom")+
  scale_fill_manual(values = c("orange","purple","brown",
                               "yellow","white"),
                    labels=c(paste0("Africa (",
                              length(unique(mapdatax[mapdatax$group_geo=="Africa","country"])),")"),
                             paste0("America (",
                              length(unique(mapdatax[mapdatax$group_geo=="America","country"])),")"),
                             paste0("Asia & Oceania(",
                              length(unique(mapdatax[mapdatax$group_geo=="Asia","country"]))+1,")"),
                             paste0("Europe (",
                             length(unique(mapdatax[mapdatax$group_geo=="Europe","country"])),")"),
                              ""))



ggsave(paste0("../output/Figures/Figure_01_map_a.png"),
       ggarrange(map1, ncol = 1,nrow = 1), width = 8, height = 4)


ggsave(paste0("../output/Figures/Figure_01_map_b.png"),
       ggarrange(map2, ncol = 1,nrow = 1), width = 8, height = 4)



