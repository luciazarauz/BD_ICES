
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
x <-read.table("0_Datos/IC/2020_hke_market.csv", sep=",",dec=".",header=F, stringsAsFactors = FALSE)
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
} 

head(SD_m)



# on board
#############
x <-read.table("0_Datos/IC/2020_hke_onboard.csv", sep=",",dec=".",header=F, stringsAsFactors = FALSE)
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
} 

head(SD_s)



# Reviso datos
####
t1 <- SD_m %>% group_by(Year, Season, Fleet, FishingArea, CatchCategory) %>% summarize( NumSamplesLngt = unique( NumSamplesLngt),
                                                                                     NumLngtMeas    = unique(NumLngtMeas) ) 
data.frame(t1)

subset(t1, substr(Fleet,1,3) == "OTB" & NumSamplesLngt>0)



# Comparo distribuciones de talla market vs onboard
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


# Comparo capturas de talla market vs onboard
####
#market
comp_m <- subset(SI_m, Fleet == "OTB_DEF_>=70_0_0" & NumSamplesLngt > 0 & CatchCategory == "L")
comp_m$Sampling <- "market"
comp_m %>% group_by(Year, Season, Fleet, FishingArea, CatchCategory) %>% summarize( NumSamplesLngt = unique( NumSamplesLngt),
                                                                                    NumLngtMeas    = unique(NumLngtMeas)) 
#onboard
comp_s <- subset(SI_s, Fleet == "OTB_DEF_>=70_0_0" & NumSamplesLngt > 0 & CatchCategory == "L")
comp_s$Sampling <- "onboard"
comp_s %>% group_by(Year, Season, Fleet, FishingArea, CatchCategory) %>% summarize( NumSamplesLngt = unique( NumSamplesLngt),
                                                                                    NumLngtMeas    = unique(NumLngtMeas) ) 

comp <- rbind (comp_m, comp_s)

windows(7,10)
ggplot(comp, aes(x = MeanLength, y = NumberCaught, colour = Sampling)) + 
  geom_line() + 
  facet_wrap(~ Season)


# Comparo distribuciones de talla D vs L
####

comp <- subset(SD_m, Fleet == "OTB_DEF_>=70_0_0" & NumSamplesLngt > 0)
comp %>% group_by(Year, Season, Fleet, FishingArea, CatchCategory) %>% summarize( NumSamplesLngt = unique( NumSamplesLngt),
                                                                                     NumLngtMeas    = unique(NumLngtMeas) ) 

windows(7,10)
ggplot(comp, aes(x = MeanLength, y = NumberCaught, colour = CatchCategory)) + 
  geom_line() + 
  facet_wrap(~ Season)






