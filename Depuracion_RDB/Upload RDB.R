
# R version 3.3.0 (2016-05-03)


# R version 3.3.0 (2016-05-03)
# Revisar versiones tar.gz de la libreria COST. Parece que hay una version 4
# 

# Instalar librerias ####
#########################


rm(list=ls())
Fun_CountUnique <- function (x) { length(unique(x))}


# Instalar librerias ####
#########################

#install.packages("installr") 
#installr::installr("Rtools") #archivo en la carpeta fishPi. # Type: 3
#install.packages("devtools") 
library(devtools)
#library(tcltk2)
library(COSTcore)
library(COSTeda)

library(pander)
library(ggplot2)
library(data.table)
library(doBy)
require(mapplots)

library(fishPifct)


# Leer ficheros ####
####################

codes.path <- "C:/use/0_Lucia/1_Proyectos/AA_SegPes/RDB/Data 2019/codes"  
res.path <- "C:/use/0_Lucia/1_Proyectos/AA_SegPes/RDB/Data 2019/QA results/" 

data.path <- "C:/use/0_Lucia/1_Proyectos/AA_SegPes/RDB/Data 2019/RCG_NA/"  

options(scipen=999)

# AZTI codes
setwd(codes.path)
buquesAZTI <- read.table("AZTI_CodigoBuques.txt", sep="\t", header=TRUE, stringsAsFactors =FALSE)
speciesAZTI <- read.table("AZTI_Species.csv", sep=";", header=TRUE, stringsAsFactors =FALSE)
lenwt <- read.csv("AZTI_Length_weight_relationship.csv", stringsAsFactors =FALSE, strip.white=TRUE)
stocks <- read.csv("ICES__stocks_code_list.csv", stringsAsFactors =FALSE, strip.white=TRUE)
dim(speciesAZTI)

# RDB codes
#harbour <- read.table("Harbour.csv", sep=",", header=TRUE, stringsAsFactors =FALSE)
metier <- read.table("FishingActivityCategory.csv", sep=",", header=TRUE, stringsAsFactors =FALSE)
species <- read.table("Species.csv", sep=";", header=TRUE, stringsAsFactors =FALSE, na.strings=c(""," ","NULL"))
area <- read.table("Area.csv", sep=",", header=TRUE, stringsAsFactors =FALSE, na.strings=c(""," ","NULL"))

# Data
setwd(data.path)

cs <- new('csData')
tr <- read.table("TR.csv", sep=",", header=TRUE, stringsAsFactors =FALSE, strip.white=TRUE)
tr <- tr[,-1]
hh <- read.table("HH.csv", sep=",", header=TRUE, stringsAsFactors =FALSE, strip.white=TRUE)
hh <- hh[, -c(1,28)]
sl <- read.table("SL.csv", sep=",", header=TRUE, stringsAsFactors =FALSE, strip.white=TRUE)
sl <- sl[,-1]
hl <- read.table("HL.csv", sep=",", header=TRUE, stringsAsFactors =FALSE, strip.white=TRUE)
hl <- hl[,-c(1, 15)]
ca <- read.table("CA.csv", sep=",", header=TRUE, stringsAsFactors =FALSE, strip.white=TRUE)
ca <- ca[,-1]

cs <- csData(tr=tr, hh=hh, sl=sl, hl=hl, ca=ca) 
#csPi <- csDataTocsPi(cs)


## Create new variables ####
############################

hh<- data.table (hh)
hh$Date <- as.Date(hh$Date)
DateSum <- hh[,list(Date=max(Date)), by=Trip_code]
AreaSum <- hh[, list(Area=names(sort(table(Area), decreasing=TRUE))[1]), by=Trip_code]  
AreaSum$Area2 <- AreaSum$Area
AreaSum$Area2[AreaSum$Area2 %in% c("8a", "8b", "8c")] <- "8abd"
hh<- as.data.frame(hh)

tr$NombreBuque<-buquesAZTI$NombreBuque[match(tr$Vessel_identifier, buquesAZTI$CodBuque)]
tr$Date<-DateSum$Date[match(tr$Trip_code, DateSum$Trip_code)]
tr$HarbourName<-harbour$Description[match(tr$Harbour, harbour$Code )]
tr$FAC_EC_lvl6<-hh$FAC_EC_lvl6[match(tr$Trip_code, hh$Trip_code)]
tr$Area2<-AreaSum$Area2[match(tr$Trip_code, AreaSum$Trip_code)]

hh$Vessel_identifier<-tr$Vessel_identifier[match(hh$Trip_code, tr$Trip_code)]
hh$NombreBuque<-buquesAZTI$NombreBuque[match(hh$Vessel_identifier, buquesAZTI$CodBuque)]
hh$StatRect_lon<- NA
hh$StatRect_lat <- NA
hh$StatRect_lon[hh$Statistical_rectangle!="99x9"] <- ices.rect(hh$Statistical_rectangle[hh$Statistical_rectangle!="99x9"])[,1]
hh$StatRect_lat[hh$Statistical_rectangle!="99x9"]<- ices.rect(hh$Statistical_rectangle[hh$Statistical_rectangle!="99x9"])[,2]

sl$Vessel_identifier<-tr$Vessel_identifier[match(sl$Trip_code, tr$Trip_code)]
sl$NombreBuque<-buquesAZTI$NombreBuque[match(sl$Vessel_identifier, buquesAZTI$CodBuque)]
sl$Date<-hh$Date[match(paste(sl$Trip_code,sl$Station_number), paste(hh$Trip_code, hh$Station_number))]
sl$SpeciesName<-speciesAZTI$Nombre.Oficial [match(sl$Species, speciesAZTI$WORMS)]
sl$SpeciesSciName<-speciesAZTI$Nombre.Cientifico[match(sl$Species, speciesAZTI$WORMS)]
sl$FAC_EC_lvl6<-hh$FAC_EC_lvl6[match(sl$Trip_code, hh$Trip_code)]
sl$Area2<-AreaSum$Area2[match(sl$Trip_code, AreaSum$Trip_code)]

hl$Vessel_identifier<-tr$Vessel_identifier[match(hl$Trip_code, tr$Trip_code)]
hl$NombreBuque<-buquesAZTI$NombreBuque[match(hl$Vessel_identifier, buquesAZTI$CodBuque)]
hl$Date<-hh$Date[match(paste(hl$Trip_code,hl$Station_number), paste(hh$Trip_code, hh$Station_number))]
hl$SpeciesName<-speciesAZTI$Nombre.Oficial [match(hl$Species, speciesAZTI$WORMS)]
hl$SpeciesSciName<-speciesAZTI$Nombre.Cientifico[match(hl$Species, speciesAZTI$WORMS)]
hl$FAC_EC_lvl6<-hh$FAC_EC_lvl6[match(hl$Trip_code, hh$Trip_code)]
hl$Area2<-AreaSum$Area2[match(hl$Trip_code, AreaSum$Trip_code)]

ca$Vessel_identifier<-tr$Vessel_identifier[match(ca$Trip_code, tr$Trip_code)]
ca$NombreBuque<-buquesAZTI$NombreBuque[match(ca$Vessel_identifier, buquesAZTI$CodBuque)]
ca$SpeciesName<-speciesAZTI$Nombre.Oficial [match(ca$Species, speciesAZTI$WORMS)]
ca$SpeciesSciName<-speciesAZTI$Nombre.Cientifico[match(ca$Species, speciesAZTI$WORMS)]


## general checkings ####
#########################

tr_varsum <-c("Sampling_type","Trip_code","NombreBuque","Date",  "Harbour", "Vessel_flag_country", "Landing_country")
hh_varsum <-c("Sampling_type","Trip_code","NombreBuque","Date", "Station_number","FAC_EC_lvl6","Fishing_validity","Fishing_duration", "Species_registration")
sl_varsum <-c("Sampling_type","Trip_code","Station_number","Species","Catch_category","Comm_size_cat_scale","Comm_size_cat","Weight","Subsample_weight","NombreBuque","Date","SpeciesName")


## identificar registros problem?ticos ####
###########################################

ca[ca$Trip_code=="S646577" & ca$Species=="126426" & ca$Single_fish_number %in% c("3648") ,]
sl[sl$Trip_code=="S654775" & sl$Station_number==1 , sl_varsum]
hh[hh$Trip_code=="S646857"  , hh_varsum ]
tr[tr$Trip_code=="S647442"  , tr_varsum ]



hh[hh$Trip_code=="M519664" , hh_varsum]

hh[hh$FAC_EC_lvl6=="OTB_DEF_>=70_0_0" & hh$Area=="27.8.c.e", ]

sl[sl$NombreBuque=="Mar Mares" & sl$Date=="2016-11-14" & 
     sl$SpeciesName=="Chicharro Negro" & sl$Station_number=="3", sl_varsum]

hl[hl$NombreBuque=="Mar Mares" & hl$Date=="2016-11-14" & 
     hl$SpeciesName=="Chicharro Negro" & hl$Station_number=="3" , ]





