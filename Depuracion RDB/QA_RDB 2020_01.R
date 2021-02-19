
# R version 3.3.0 (2016-05-03)
# Revisar versiones tar.gz de la libreria COST. Parece que hay una version 4
# 


rm(list=ls())
Fun_CountUnique <- function (x) { length(unique(x))}


# Instalar librerias ####
#########################

#install.packages("installr") 
#installr::installr("Rtools") #archivo en la carpeta fishPi. # Type: 3
#install.packages("devtools") 
library(devtools)
#library(tcltk2)
#library(COSTcore)
#library(COSTeda)

#library(pander)
library(ggplot2)
library(data.table)
library(doBy)
require(mapplots)

#library(fishPifct)


# Leer ficheros ####
####################

codes.path <- "C:/use/0_Lucia/1_Proyectos/AA_SegPes/RDB/Data 2019/codes"  
res.path <- "C:/use/0_Lucia/1_Proyectos/AA_SegPes/RDB/Data 2019/QA results/" 


data.path <- "C:/use/0_Lucia/1_Proyectos/AA_SegPes/RDB/Data 2019/RCG_NA"  
data.path <- "C:/use/0_Lucia/1_Proyectos/AA_SegPes/RDB/Data 2019/RCG_NSEA"  

options(scipen=999)

# AZTI codes
setwd(codes.path)
buquesAZTI <- read.table("AZTI_CodigoBuques.txt", sep="\t", header=TRUE, stringsAsFactors =FALSE)
speciesAZTI <- read.table("AZTI_Species.csv", sep=";", header=TRUE, stringsAsFactors =FALSE)
#lenwt <- read.csv("AZTI_Length_weight_relationship.csv", stringsAsFactors =FALSE, strip.white=TRUE)
#stocks <- read.csv("ICES__stocks_code_list.csv", stringsAsFactors =FALSE, strip.white=TRUE)
dim(speciesAZTI)

# RDB codes
harbour <- read.table("Harbour.csv", sep=",", header=TRUE, stringsAsFactors =FALSE)
metier <- read.table("FishingActivityCategory.csv", sep=",", header=TRUE, stringsAsFactors =FALSE)
species <- read.table("Species.csv", sep=";", header=TRUE, stringsAsFactors =FALSE, na.strings=c(""," ","NULL"))
area <- read.table("Area.csv", sep=",", header=TRUE, stringsAsFactors =FALSE, na.strings=c(""," ","NULL"))

# Data
setwd(data.path)

#cs <- new('csData')
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

# cs <- csData(tr=tr, hh=hh, sl=sl, hl=hl, ca=ca) 
# #csPi <- csDataTocsPi(cs)


## Check codes ####
###################

subset(tr, Trip_code == "S640396")

setwd(res.path)

# harbours in tr but not in the RDB 
  t1 <- unique(tr$Harbour[!tr$Harbour %in% harbour$Code ])
  t1
  subset(tr, Harbour=="")
  
# metiers in hh but not in the RDB 
  t2 <- unique(hh$FAC_EC_lvl6[!hh$FAC_EC_lvl6 %in% metier$Code ])
  t2

  # areas in hh but not in the RDB 
  unique(hh$Area)
  t3 <- unique(hh$Area[!hh$Area %in% area$Code ])
    hh$Area[hh$Area %in% c("27.8.c.e","27.8.c.e.1", "27.8.c.e.2")] <- "27.8.c"
    hh$Area[hh$Area %in% c("27.6")] <- "27.6.a"
    hh$Area[hh$Area %in% c("27.8.d.2")] <- "27.8.d"
  t3
# species in sl but not in the RDB 
  t4 <- unique(sl$Species[!sl$Species %in% species$AphiaID ])
  t4
  
# species in our data base but not in the RDB 
  speciesAZTI [!speciesAZTI$WORMS %in% species$AphiaID,]

  

# write tables
  write.table("harbours in tr but not in the RDB", "0_corregir.csv", row.names = FALSE, sep=",")
  write.table(t1, "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
  write.table("", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
  
  write.table("metiers in hh but not in the RDB", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
  write.table(t2, "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
  write.table("", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
  
  write.table("areas in hh but not in the RDB", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
  write.table(t3, "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
  write.table("", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
  
  write.table("species in sl but not in the RDB" , "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
  write.table(t4, "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
  write.table("", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
  
  
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
hh$HarbourName<-tr$HarbourName[match(hh$Trip_code, tr$Trip_code)]
hh$StatRect_lon<- NA
hh$StatRect_lat <- NA
hh$StatRect_lon[hh$Statistical_rectangle!="99x9"] <- ices.rect(hh$Statistical_rectangle[hh$Statistical_rectangle!="99x9"])[,1]
hh$StatRect_lat[hh$Statistical_rectangle!="99x9"]<- ices.rect(hh$Statistical_rectangle[hh$Statistical_rectangle!="99x9"])[,2]

sl$Vessel_identifier<-tr$Vessel_identifier[match(sl$Trip_code, tr$Trip_code)]
sl$NombreBuque<-buquesAZTI$NombreBuque[match(sl$Vessel_identifier, buquesAZTI$CodBuque)]
sl$Date<-hh$Date[match(paste(sl$Trip_code,sl$Station_number), paste(hh$Trip_code, hh$Station_number))]
sl$SpeciesName<-speciesAZTI$Nombre.Oficial [match(sl$Species, speciesAZTI$WORMS)]
sl$SpeciesSciName<-speciesAZTI$Nombre.Cientifico[match(sl$Species, speciesAZTI$WORMS)]
unique(sl$Species[is.na(sl$SpeciesName)])
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

tr_varsum <-c("Sampling_type","Trip_code","NombreBuque","Date", "HarbourName")
hh_varsum <-c("Sampling_type","Trip_code","NombreBuque","Date", "Station_number","FAC_EC_lvl6","Fishing_validity","Fishing_duration", "Species_registration")
sl_varsum <-c("Sampling_type","Trip_code","Station_number","Species","Catch_category","Comm_size_cat","Weight","Subsample_weight","NombreBuque","Date","SpeciesName")
hl_varsum <-c("Sampling_type","Trip_code","Station_number","Species","Catch_category","Comm_size_cat","NombreBuque","Date","SpeciesName","Length_class", "Number_at_length")


## Revise summary: data class, codes, ranges.. 
###############################################

apply(subset(tr, select=c(Sampling_type,Landing_country,Vessel_flag_country,Year,
                          Project,Vessel_type,Harbour,Sampling_country,Sampling_method)), 2, unique)
apply(subset(hh, select=c(Sampling_type,Year,Station_number,Fishing_validity,Aggregation_level,Catch_registration,
                          Species_registration,Area,Statistical_rectangle,FAC_EC_lvl6)), 2, unique)  
apply(subset(hh, select=c(Fishing_duration, Pos_Start_Lat_dec, Pos_Start_Lon_dec, 
                          Pos_Stop_Lat_dec, Pos_Stop_Lon_dec, Main_fishing_depth)), 2, summary)  
summary(as.Date(hh$Date))

tapply(hh$Trip_code, list(hh$FAC_EC_lvl6, hh$Sampling_type), Fun_CountUnique)  #hemos quitado las mareas de cerco - muestreo a bordo. 

corr <-subset(hh, Area=="27.6", select=hh_varsum)

corr <-subset(tr, Harbour=="", select=tr_varsum)
corr


subset(hh,as.Date(hh$Date)=="2018-12-01" )

# Weigth=0 
##########
weigth0 <- sl[sl$Weight==0, sl_varsum]
weigth0


sl[sl$Weight>0 & sl$SpeciesName =="Pota voladora", sl_varsum]


    write.table("trips with weigth=0", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
    write.table(weigth0, "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
    write.table("", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")


# Duplicates
############
temp<- aggregate(Station_number ~ Trip_code + Landing_country, FUN = "sum", data= hh)
dupli <- temp[duplicated(temp$Trip_code)==TRUE,]
dupli

    write.table("trips duplicados", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
    write.table(dupli, "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
    write.table("", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")

    
# fishing duration
##################
  # PTB: 6-8h  -> margen de 2h (4,10)
  # OTB: 3-4h  -> margen de 2h (1,6)
fish1<- subset(hh[substr(hh$FAC_EC_lvl6,1,3)=="PTB", ],Fishing_duration<4*60 | Fishing_duration>10*60,hh_varsum)
fish2<- subset(hh[substr(hh$FAC_EC_lvl6,1,3)=="OTB", ],Fishing_duration<1*60 | Fishing_duration>6*60,hh_varsum)
fish3<- subset(hh[substr(hh$FAC_EC_lvl6,1,2)=="PS", ],Fishing_duration<1*30 | Fishing_duration>5*60,hh_varsum)
fishdur <- rbind(fish1, fish2, fish3)

fishdur
subset(fishdur, Fishing_duration<60)


fd_otb <- subset(hh, substr(hh$FAC_EC_lvl6,1,3)=="OTB")
fd_ptb <- subset(hh, substr(hh$FAC_EC_lvl6,1,3)=="PTB")

fd_ps <- subset(hh, substr(hh$FAC_EC_lvl6,1,2)=="PS")


png(filename="Fishing duration.png",  width = 900, height = 450)
    #windows(10,5)
    par(mfrow=c(1,3))
    dotchart(fd_ptb$Fishing_duration, groups=as.factor(fd_ptb$Fishing_validity) , main="PTB", xlab="Fishing duration (min)")
    abline(v=240, col="red")
    abline(v=600, col="red")
    dotchart(fd_otb$Fishing_duration, groups=as.factor(fd_otb$Fishing_validity), main="OTB", xlab="Fishing duration (min)")
     abline(v=60, col="red")
    abline(v=360, col="red")
    dotchart(fd_ps$Fishing_duration, groups=as.factor(fd_ps$Fishing_validity), main="PS", xlab="Fishing duration (min)")
    #abline(v=60, col="red")
    #abline(v=360, col="red")
  dev.off()

  dotchart(hh$Fishing_duration[substr(hh$FAC_EC_lvl6,1,3)=="OTB"], groups = as.factor(hh$Fishing_validity[substr(hh$FAC_EC_lvl6,1,3)=="OTB"]), main="OTB", xlab="Fishing duration (min)")
  
  
      write.table("fishing duration. PTB (4,10), OTB (1,6), PS (0.5, 5)", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
      write.table(fishdur, "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
      write.table("", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
  

# Dates not in 2018
###################
   datetemp <- subset(tr, Date< "2019-01-01" | Date > "2019-12-31", select=tr_varsum)   
   datetemp
       write.table("dates", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
       write.table(datetemp, "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
       write.table("", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
       
# Maps
######
require(maps);require(mapdata);
 
  # start haul
       latlim <- c(min(hh$Pos_Start_Lat_dec, na.rm=T)-1, max(hh$Pos_Start_Lat_dec, na.rm=T)+1)
       lonlim <- c(min(-hh$Pos_Start_Lon_dec, na.rm=T)-1, max(-hh$Pos_Start_Lon_dec, na.rm=T)+1)
     
       windows(8,10)
         map("worldHires",xlim=lonlim,ylim=latlim,fill=T,col="darkgreen", main="Map start haul")
         points(-hh$Pos_Start_Lon_dec,hh$Pos_Start_Lat_dec, col="red")
         map.axes()
         points <- identify(-hh$Pos_Start_Lon_dec, hh$Pos_Start_Lat_dec, labels = hh$Trip_code, plot=TRUE)
      
       dev.copy(png, "Map start haul.png")
       dev.off() 
       
       positemp <- hh[points, c(hh_varsum,"Pos_Start_Lon_dec","Pos_Start_Lat_dec")]
           write.table("posiciones start", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
           write.table(positemp, "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
           write.table("", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
       
   # stop haul
       latlim <- c(min(hh$Pos_Stop_Lat_dec, na.rm=T)-1, max(hh$Pos_Stop_Lat_dec, na.rm=T)+1)
       lonlim <- c(min(-hh$Pos_Stop_Lon_dec, na.rm=T)-1, max(-hh$Pos_Stop_Lon_dec, na.rm=T)+1)
       
       windows(8,10)
         map("worldHires",xlim=lonlim,ylim=latlim,fill=T,col="darkgreen", main="Map stop haul")
         points(-hh$Pos_Stop_Lon_dec,hh$Pos_Stop_Lat_dec, col="blue")
         map.axes()
         points <- identify(-hh$Pos_Stop_Lon_dec, hh$Pos_Stop_Lat_dec, labels = hh$Trip_code, plot=TRUE)
       
       dev.copy(png, "Map stop haul.png")
       dev.off() 
       
       positemp <- hh[points, c(hh_varsum,"Pos_Stop_Lon_dec","Pos_Stop_Lat_dec")]
           write.table("posiciones stop", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
           write.table(positemp, "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
           write.table("", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
       
       
    # Stat rect
       latlim <- c(min(hh$StatRect_lat, na.rm=T)-1, max(hh$StatRect_lat, na.rm=T)+1)
       lonlim <- c(min(hh$StatRect_lon, na.rm=T)-1, max(hh$StatRect_lon, na.rm=T)+1)
       windows(10,7)
       map("worldHires",xlim=lonlim,ylim=latlim,fill=T,col="darkgreen")
       points(hh$StatRect_lon,hh$StatRect_lat, col="red", pch=19)
       map.axes()
       points <- identify(-hh$StatRect_lon, hh$StatRect_lat, labels = hh$Trip_code, plot=TRUE)
       
       
       
       subset(hh, StatRect_lat>55 & StatRect_lat<57)
       subset(tr, Trip_code=="S536445" )

       # comprobar si alguna especie tiene fichero SL pero no HL
#########################################################
sl_tr <- unique(sl$Trip_code[sl$SpeciesName %in%"Calamar comun"])
hl_tr <- unique(hl$Trip_code[hl$SpeciesName %in%"Calamar comun"])
aaa<- sl_tr[!(sl_tr %in% hl_tr)]
unique(sl$SpeciesName[(sl$Trip_code %in% aaa) & sl$SpeciesName=="Calamar comun" ])


# codigos buque
###############
  # csbuques_code <- sort(unique(tr$Vessel_identifier))
  # csbuques <- buquesAZTI[buquesAZTI$CodBuque %in% csbuques_code,]
  # head(csbuques)
  # csbuques$codIEO <- buquesIEO$BARCOD[match(csbuques$CodigoUE, buquesIEO$CODIGO_UE)]
  # qu? hacemos con esto?? puedo preguntar a jose si los codigos ieo son estables e incluirlos en nuestra BD. 
  #                       o cambiarlos por fuera en el TR (solo estan en el TR). pero perdemso trazabilidad


# Landed weight and subsample weight
####################################
options(scipen=999)
max(sl$Weight)
wM <- subset(sl, Weight==max(sl$Weight[sl$Sampling_type=="M"]) & sl$Sampling_type=="M",sl_varsum); wM
wS <- subset(sl, Weight==max(sl$Weight[sl$Sampling_type=="S"]) & sl$Sampling_type=="S",sl_varsum); wS

  windows()
  par(mfrow=c(1,2))
  boxplot(sl$Weight, main= "Weight")
  boxplot(sl$Weight~sl$Sampling_type, main= "Weight")

  
  windows()
  par(mfrow=c(1,2))
  boxplot(sl$Weight[sl$SpeciesName=="Estornino del Pacifico"], main= "Weight")
  boxplot(sl$Weight[sl$SpeciesName=="Estornino del Pacifico"]~sl$Sampling_type[sl$SpeciesName=="Estornino del Pacifico"], main= "Weight")
  
  
max(sl$Subsample_weight)
swM <- subset(sl, Subsample_weight==max(sl$Subsample_weight[sl$Sampling_type=="M"]) & sl$Sampling_type=="M",sl_varsum); swM
swS <- subset(sl, Subsample_weight==max(sl$Subsample_weight[sl$Sampling_type=="S"]) & sl$Sampling_type=="S",sl_varsum); swS

subset(sl, Subsample_weight>10000000,sl_varsum)
  windows()
  par(mfrow=c(1,2))
  boxplot(sl$Subsample_weight, main= "subsample Weight")
  boxplot(sl$Subsample_weight~sl$Sampling_type, main= "subsample Weight")

  windows()
  par(mfrow=c(1,2))
  boxplot(sl$Weight[sl$SpeciesName=="Rape blanco"], main= "Weight")
  boxplot(sl$Weight[sl$SpeciesName=="Rape blanco"]~sl$Sampling_type[sl$SpeciesName=="Rape blanco"], main= "Weight")
  
  swS <- subset(sl, Subsample_weight>180000 & sl$Sampling_type=="M",sl_varsum); swS
  
  
sl0<-subset(sl, Weight>0 )
temp <- (sl0$Weight-sl0$Subsample_weight)/sl0$Weight
temp2 <- temp<(-2)
wdif<-sl0[temp2,sl_varsum]
wdif
  
    write.table("max weigth and subsample weigth", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
    write.table(rbind(wM, wS, sw), "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
    #write.table(0, "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
    write.table("", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
    
    write.table("subsample weigth higher tan weigth (2%)", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
    write.table(wdif, "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
    write.table("", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
    
    # Subsample_weight=0
x<-   subset(sl, Subsample_weight==0,sl_varsum)
write.table("subsample weigth  = 0 ", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
write.table(x, "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
write.table("", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")

## Number_at_length>500 ####
##################    
x<- subset(hl, Number_at_length>1000,hl_varsum)
write.table("Number_at_length>500", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
write.table(x, "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")
write.table("", "0_corregir.csv", append=TRUE, row.names = FALSE, sep=",")

sort(unique(X$Trip_code))
## Identificar mareas ####
##################
subset(hl, Trip_code=="S536452" & Species==126822 & Station_number==7,hl_varsum)

subset(tr, Sampling_type=="S" & FAC_EC_lvl6=="PS_SPF_0_0_0" ,tr_varsum)
subset(sl, Sampling_type=="S" & FAC_EC_lvl6=="PS_SPF_0_0_0" & Trip_code=="S539260" ,sl_varsum)

subset(sl, Sampling_type=="S" & NombreBuque=="Mater Bi" ,sl_varsum)

subset(sl, Trip_code=="M467849" & Species==126484,sl_varsum)


## Save files ####
##################
setwd(data.path)  
#save(tr,hh,sl,hl,ca,cs,buquesAZTI,speciesAZTI, lenwt, stocks, file="RDB2020_NSEA.RData")  
save(tr,hh,sl,hl,ca,buquesAZTI,speciesAZTI, file="RDB2020_NSEA.RData")  


head(hl)  

hl%>% group_by(SpeciesSciName)%>%summarise(Nind=sum(Number_at_length), Ntrip=length(unique(Trip_code)))
  
  
