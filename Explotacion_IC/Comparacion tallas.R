
################################################################# #
# este código sirve para comparar las distribciones de tallas extrapoladas del muestreo en puerto y del muestreo abordo
#
################################################################# #

rm(list=(ls()))
options(digits=2, scipen = 999)

library (stringr)
library(doBy)
library(dplyr)
library(data.table)
library(tidyverse)



################################################################### #
#   Leer los ficheros                                           #####
################################################################### #

#Año
Ano <- 2020

# market
###########
# fichero IC, que coge las distriuciones de tallas medidas en puerto para Catchcategoy==L, y las distribuciones de tallas medidas en la mar (descartadas) para CatchCategory==D
# le llamamos con el sufijo "_m"
x <- read.table("0_Datos/IC/2021 DC WGBIE hke.3a46-8abd ES AZTI_muestreo_puerto.csv", header = FALSE, stringsAsFactors=FALSE, sep = ",", na.strings=c(NA, ""),
                col.names = paste0("V",seq_len(33)), fill = TRUE)
head(x); dim(x)

HI_m<-subset(x[,1:12], V1=="HI") 
colnames(HI_m)<-c("RecordType", "Country", "Year", "SeasonType", "Season","Fleet", "AreaType", "FishingArea", "DepthRange", "UnitEffort", "Effort", "AreaQualifier")

SI_m<-subset(x[,1:24], V1=="SI") 
colnames(SI_m)<-c("RecordType", "Country", "Year", "SeasonType", "Season","Fleet", "AreaType", "FishingArea", "DepthRange", "Species", "Stock", "CatchCategory", "ReportingCategory", "DataToFrom", "Usage",  "SamplesOrigin", "QualityFlag", "UnitCATON", "CATON", "OffLandings", "varCATON", "InfoFleet",  "InfoStockCoordinator", "InfoGeneral")


if("SD" %in% unique(x$V1)){
  SD_m<-subset(x[,1:33], V1=="SD") 
  colnames(SD_m)<-c("RecordType", "Country", "Year", "SeasonType", "Season","Fleet", "AreaType", "FishingArea", "DepthRange", "Species", "Stock", "CatchCategory", "ReportingCategory", "Sex", "CANUMtype", "AgeLength", "PlusGroup", "SampledCatch", "NumSamplesLngt", "NumLngtMeas", "NumSamplesAge",  "NumAgeMeas", "unitMeanWeight", "unitCANUM", "UnitAgeOrLength", "UnitMeanLength", "Maturity", "NumberCaught", "MeanWeight", "MeanLength", "varNumLanded", "varWGTLanded", "varLGTLanded")  
  
  SD_m$NumSamplesLngt[ SD_m$NumSamplesLngt==-9]<-0
  SD_m$NumLngtMeas[ SD_m$NumLngtMeas==-9]<-0
  SD_m$AgeLength <- as.numeric(SD_m$AgeLength )
  SD_m$WeightLength <- SD_m$NumberCaught * SD_m$MeanWeight
  
} 

head(SD_m)



# on board
#############
# fichero IC, que coge las distriuciones de tallas medidas en la mar (retenidas) para Catchcategoy==L, y las distribuciones de tallas medidas en la mar (descartadas) para CatchCategory==D
# le llamamos con el sufijo "_s"
x <- read.table("0_Datos/IC/2021 DC WGBIE hke.3a46-8abd ES AZTI_muestreo_abordo.csv", header = FALSE, stringsAsFactors=FALSE, sep = ",", na.strings=c(NA, ""),
                col.names = paste0("V",seq_len(33)), fill = TRUE)
head(x); dim(x)

HI_s<-subset(x[,1:12], V1=="HI") 
colnames(HI_s)<-c("RecordType", "Country", "Year", "SeasonType", "Season","Fleet", "AreaType", "FishingArea", "DepthRange", "UnitEffort", "Effort", "AreaQualifier")

SI_s<-subset(x[,1:24], V1=="SI") 
colnames(SI_s)<-c("RecordType", "Country", "Year", "SeasonType", "Season","Fleet", "AreaType", "FishingArea", "DepthRange", "Species", "Stock", "CatchCategory", "ReportingCategory", "DataToFrom", "Usage",  "SamplesOrigin", "QualityFlag", "UnitCATON", "CATON", "OffLandings", "varCATON", "InfoFleet",  "InfoStockCoordinator", "InfoGeneral")


if("SD" %in% unique(x$V1)){
  SD_s<-subset(x[,1:33], V1=="SD") 
  colnames(SD_s)<-c("RecordType", "Country", "Year", "SeasonType", "Season","Fleet", "AreaType", "FishingArea", "DepthRange", "Species", "Stock", "CatchCategory", "ReportingCategory", "Sex", "CANUMtype", "AgeLength", "PlusGroup", "SampledCatch", "NumSamplesLngt", "NumLngtMeas", "NumSamplesAge",  "NumAgeMeas", "unitMeanWeight", "unitCANUM", "UnitAgeOrLength", "UnitMeanLength", "Maturity", "NumberCaught", "MeanWeight", "MeanLength", "varNumLanded", "varWGTLanded", "varLGTLanded")  
  
  SD_s$NumSamplesLngt[ SD_s$NumSamplesLngt==-9]<-0
  SD_s$NumLngtMeas[ SD_s$NumLngtMeas==-9]<-0
  SD_s$AgeLength <- as.numeric(SD_s$AgeLength )
  SD_s$WeightLength <- SD_s$NumberCaught * SD_s$MeanWeight
} 

head(SD_s)



# Reviso datos
####
t1 <- SD_s %>% group_by(Year, Season, Fleet, FishingArea, CatchCategory) %>% summarize( NumSamplesLngt = unique( NumSamplesLngt),
                                                                                     NumLngtMeas    = unique(NumLngtMeas) ) 
data.frame(t1)

subset(t1, Fleet == "PTB_DEF_>=70_0_0" & NumSamplesLngt>0)



# Comparo capturas (CATON) Catchcategory==L, market vs onboard
####
#market
comp_m <- subset(SI_m, Fleet == "OTB_DEF_>=70_0_0" & CatchCategory == "L" & ReportingCategory == "R" & FishingArea == "27.8.b")
comp_m$Sampling <- "market"
comp_m

#onboard
comp_s <- subset(SI_s, Fleet == "OTB_DEF_>=70_0_0" & CatchCategory == "L"  & ReportingCategory == "R" & FishingArea == "27.8.b")
comp_s$Sampling <- "onboard"
comp_s

comp <- rbind (comp_m, comp_s)

windows(7,10)
ggplot(comp, aes(x = Sampling, y = CATON)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~ Season)


# Comparo distribuciones de tallas Catchcategory==L, market vs onboard - 8abd - Peso por talla
####
#market
comp_m <- subset(SD_m, Fleet == "OTB_DEF_>=70_0_0" & NumSamplesLngt > 0 & CatchCategory == "L")
comp_m$Sampling <- "market"
comp_m %>% group_by(Year, Season, Fleet, FishingArea, CatchCategory) %>% summarize( NumSamplesLngt = unique( NumSamplesLngt),
                                                                                    NumLngtMeas    = unique(NumLngtMeas)) 
#onboard
comp_s <- subset(SD_s, Fleet == "OTB_DEF_>=70_0_0" & NumSamplesLngt > 0 & CatchCategory == "L")
comp_s$Sampling <- "onboard"
comp_s %>% group_by(Year, Season, Fleet, FishingArea, CatchCategory) %>% summarize( NumSamplesLngt = unique( NumSamplesLngt),
                                                                                    NumLngtMeas    = unique(NumLngtMeas) ) 

comp <- rbind (comp_m, comp_s)

windows(10,7)
ggplot(comp, aes(x = MeanLength, y = WeightLength, colour = Sampling)) + 
  geom_line() + 
  facet_wrap(~ Season)


# Comparo distribuciones de tallas Catchcategory==L, market vs onboard - 8abd - numero por talla
####
#market
comp_m <- subset(SD_m, Fleet == "OTB_DEF_>=70_0_0" & NumSamplesLngt > 0 & CatchCategory == "L")
comp_m$Sampling <- "market"
comp_m %>% group_by(Year, Season, Fleet, FishingArea, CatchCategory) %>% summarize( NumSamplesLngt = unique( NumSamplesLngt),
                                                                                    NumLngtMeas    = unique(NumLngtMeas)) 
#onboard
comp_s <- subset(SD_s, Fleet == "OTB_DEF_>=70_0_0" & NumSamplesLngt > 0 & CatchCategory == "L")
comp_s$Sampling <- "onboard"
comp_s %>% group_by(Year, Season, Fleet, FishingArea, CatchCategory) %>% summarize( NumSamplesLngt = unique( NumSamplesLngt),
                                                                                    NumLngtMeas    = unique(NumLngtMeas) ) 

comp <- rbind (comp_m, comp_s)

windows(10,7)
ggplot(comp, aes(x = MeanLength, y = NumberCaught, colour = Sampling)) + 
  geom_line() + 
  facet_wrap(~ Season)



# Comparo distribuciones de tallas Catchcategory==L, market vs onboard - 6a - numero por talla
####
#market
comp_m <- subset(SD_m, Fleet == "OTB_DEF_100-119_0_0" & NumSamplesLngt > 0 & CatchCategory == "L")
comp_m$Sampling <- "market"
comp_m %>% group_by(Year, Season, Fleet, FishingArea, CatchCategory) %>% summarize( NumSamplesLngt = unique( NumSamplesLngt),
                                                                                    NumLngtMeas    = unique(NumLngtMeas)) 
#onboard
comp_s <- subset(SD_s, Fleet == "OTB_DEF_100-119_0_0" & NumSamplesLngt > 0 & CatchCategory == "L")
comp_s$Sampling <- "onboard"
comp_s %>% group_by(Year, Season, Fleet, FishingArea, CatchCategory) %>% summarize( NumSamplesLngt = unique( NumSamplesLngt),
                                                                                    NumLngtMeas    = unique(NumLngtMeas) ) 

comp <- rbind (comp_m, comp_s)

windows(10,7)
ggplot(comp, aes(x = MeanLength, y = NumberCaught, colour = Sampling)) + 
  geom_line() + 
  facet_wrap(~ Season)


# Comparo distribuciones de talla D vs L
####

comp <- subset(SD_s, Fleet == "OTB_DEF_>=70_0_0" & NumSamplesLngt > 0)
comp %>% group_by(Year, Season, Fleet, FishingArea, CatchCategory) %>% summarize( NumSamplesLngt = unique( NumSamplesLngt),
                                                                                     NumLngtMeas    = unique(NumLngtMeas) ) 

windows(10,7)
ggplot(comp, aes(x = MeanLength, y = NumberCaught, colour = CatchCategory)) + 
  geom_line() + 
  facet_wrap(~ Season)






