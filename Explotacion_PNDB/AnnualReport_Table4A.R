############################################################################### #
#                    Estimas
############################################################################### #
############################################################################### 

#  Datos Oficiales 
#  Laboratorio AZTI 
#  csv
#
#  Tallas (Informe IEO)
#  Todos los metiers GNS/GTR/LLS/FPO/LHM/LTL/LHP/OTB/PTB/PS/MIS
#  Convertir a csv 
#

############################################################################# #

#      Librerias y datos      #####
################################### #

rm(list=(ls()))

#Librerias

library(lubridate)
library (stringr)
#library(plyr)
library(dplyr)
library(tidyr)

library(fishPiCodes)
data("ASFIS_WoRMS")

#Funciones
Fun_CountUnique <- function (x) { length(unique(x))}   

mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}


#      Leer los ficheros       ####
################################## #

año         <- "2020"

#Desembarco
DB0 <-read.table("0_Datos/Ventas_2020.csv", sep=",",dec=".",header=T, stringsAsFactors = FALSE)#, skip=10)# comment.char = "#")
DB<- DB0
head(DB); dim(DB)

#Tallas
LN0 <-read.table("0_Datos\\Tallas_2020.csv", sep=";",dec=",",header=T, stringsAsFactors = FALSE)
LN<- LN0
head(LN); dim(LN)

#maestros
conv_sp<- read.csv("0_Maestros\\Especies_2020.txt",header=T,sep="\t", stringsAsFactors = FALSE); head(conv_sp)
  names(conv_sp) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(conv_sp))

conv_censo<- read.csv("0_Maestros\\Buques_2020.txt",header=T,sep="\t", dec=",",  stringsAsFactors = FALSE); head(conv_censo)
  names(conv_censo) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(conv_censo))
  conv_censo$Buque <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), conv_censo$Buque)
  conv_censo$Puerto.base  <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), conv_censo$Puerto.base )
  conv_censo$CensoPorModalidad  <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), conv_censo$CensoPorModalidad )
  conv_censo$CensoPorModalidad  <- toupper(conv_censo$CensoPorModalidad) 


#Funciones
source("Explotacion_PNDB\\01_functions.r")

#     Datos Oficiales - Mareas          #####
################################################################# #

#    Acentos
names(DB) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(DB))
DB$Nombre_Buque <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), DB$Nombre_Buque)


#  Zonas
###
sort(unique(DB$Zona))

DB$Zona[grepl("27.8.a|27.8.b|27.8.d|27.8.abd", DB$Zona)] <- "27.8.abd"
DB$Zona[grepl("27.8.c", DB$Zona)] <- "27.8.c"
DB$Zona[grepl("27.6", DB$Zona)] <- "27.6"
DB$Zona[grepl("27.7", DB$Zona)] <- "27.7"
DB$Zona[grepl("27.9", DB$Zona)] <- "27.9"
DB$Zona[grepl("27.10", DB$Zona)] <- "27.10"


# censo
###
DB$censo  <-  conv_censo$CensoPorModalidad[match(DB$Cod_UE , conv_censo$Codigo.UE)]
unique(DB$Nombre_Buque[is.na(DB$censo)])
table(DB$censo)

DB <- subset(DB, censo!="PABELLON EXTRANJERO")

# Reviso parejas y crear unidad pesquera
unique(DB$Nombre_Buque[grepl("PTB",DB$Metier)])

# variable unidad pesquera. A partir de ahora lo utilizamos en vez de buque
ptb <- data.frame(cbind(Nombre_Buque = c("Aketxe", "Gaztelugatxe", "Andrekala Berria", "Arretxu Berria", "Gure Gaskuña", "Gure Kantabriko", "Kaxu", "Kaxarra"),
                        Pareja = rep(c("Aketxe - Gaztelugatxe", "Andrekala Berria - Arretxu Berria", "Gure Gaskuña - Gure Kantabriko", "Kaxu - Kaxarra"), each=2)),
                  stringsAsFactors = F)


DB$UnidadPesquera[DB$Metier %in% c("PTB_MPD_>=55_0_0", "PTB_DEF_>=70_0_0")] <- ptb$Pareja[match(DB$Nombre_Buque[DB$Metier %in% c("PTB_MPD_>=55_0_0", "PTB_DEF_>=70_0_0")], ptb$Nombre_Buque)]
DB$UnidadPesquera [is.na(DB$UnidadPesquera)] <- DB$Nombre_Buque[is.na(DB$UnidadPesquera)]



# estratos de muestreo
###
DB$strata<-    getStrata(DB, censo="censo", metier="Metier", nombre_buque = "Nombre_Buque") 
unique(DB$Nombre_Buque[is.na(DB$strata)])



# variables necesarias para las tablas
###
DB$Dia_Desembarco[is.na(DB$Dia_Desembarco)] <- DB$Dia[is.na(DB$Dia_Desembarco)]
DB$Mes_Desembarco[is.na(DB$Mes_Desembarco)] <- DB$Mes[is.na(DB$Mes_Desembarco)]
DB$Trimestre_Desembarco[is.na(DB$Trimestre_Desembarco)] <- DB$Trimestre[is.na(DB$Trimestre_Desembarco)]
DB$date <- as.Date(format(ISOdate(DB$Año,DB$Mes_Desembarco,DB$Dia_Desembarco),"%d/%m/%Y"),"%d/%m/%Y")

DB$voyageId <- paste(DB$UnidadPesquera, DB$date, sep="_")
DB$market_day <- paste(DB$Puerto_Venta , DB$date, sep="_")
DB$market_day_event<- paste(DB$Puerto_Venta , DB$voyageId, sep="_")

DB$date<- as.Date(DB$date,"%d/%m/%Y")
DB$weekday<- weekdays(DB$date)
DB$weekdayNum <- wday(DB$date)-1
DB$weekdayNum[DB$weekdayNum==0] <- 7
DB$week<- week(DB$date)
DB$vesselweek<- paste(DB$week, DB$Nombre_Buque)





#      Crear variables nuevas  en LN:  Tallas                 #########
###################################################################### #


#    Acentos
names(LN) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(LN))
LN$Barco.real <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), LN$Barco.real)


#  Zona
###
sort(unique(LN$Origen))

LN$Origen[grepl("27.8.a|27.8.b|27.8.d|27.8.abd", LN$Origen)] <- "27.8.abd"
LN$Origen[grepl("27.8.c", LN$Origen)] <- "27.8.c"
LN$Origen[grepl("27.6", LN$Origen)] <- "27.6"
LN$Origen[grepl("27.7", LN$Origen)] <- "27.7"
LN$Origen[grepl("27.9", LN$Origen)] <- "27.9"
LN$Origen[grepl("27.10", LN$Origen)] <- "27.10"


#especies: simplificamos los nombres comunes
###
LN$Especie.muestreada_Oficial  <-  conv_sp$Nombre.Oficial[match(LN$Especie.muestreada, conv_sp$Cod..ALFA.3)]

unique(LN$Especie.muestreada[is.na(LN$Especie.muestreada_Oficial)])


#incluimos información sobre eslora y puerto base
###
DBinfo <- unique(as.data.frame(DB)[,c("Nombre_Buque" ,"Puerto_Base", "Eslora_total")])

LN$Puerto_Base  <-  DBinfo$Puerto_Base[match(LN$Barco.real , DBinfo$Nombre_Buque)]
unique(LN$Barco.real[is.na(LN$Puerto_Base)])
LN$Eslora_total  <-  DBinfo$Eslora_total[match(LN$Barco.real , DBinfo$Nombre_Buque)]
unique(LN$Barco.real[is.na(LN$Eslora_total)])

# censo
###
LN$censo  <-  DB$censo[match(LN$Barco.real, DB$Nombre_Buque)]
table(LN$Barco.real[is.na(LN$censo)])
unique(LN$censo)

  
  # Reviso parejas
  unique(LN$Barco.real[grepl("PTB",LN$Arte)])
  
  # variable unidad pesquera. A partir de ahora lo utilizamso en vez de buque
  ptb <- data.frame(cbind(Barco.real = c("Aketxe", "Gaztelugatxe", "Andrekala Berria", "Arretxu Berria", "Gure Gaskuña", "Gure Kantabriko", "Kaxu", "Kaxarra"),
                          Pareja = rep(c("Aketxe - Gaztelugatxe", "Andrekala Berria - Arretxu Berria", "Gure Gaskuña - Gure Kantabriko", "Kaxu - Kaxarra"), each=2)),
                    stringsAsFactors = F)
  
  
  LN$UnidadPesquera[LN$Arte %in% c("PTB_MPD_>=55_0_0", "PTB_DEF_>=70_0_0")] <- ptb$Pareja[match(LN$Barco.real[LN$Arte %in% c("PTB_MPD_>=55_0_0", "PTB_DEF_>=70_0_0")], ptb$Barco.real)]
  LN$UnidadPesquera [is.na(LN$UnidadPesquera)] <- LN$Barco.real[is.na(LN$UnidadPesquera)]
  
  
  #estratos de muestreo
  ###
  LN$strata <- getStrata(LN, censo="censo", metier="Arte", nombre_buque = "Barco.real")
  LN[is.na(LN$strata),]
  sort(unique(LN$strata))
  
  
  
  # variables necesarias para las estimas
###
LN$date <- as.Date(LN$Fecha.venta,format="%d/%m/%Y" )
LN$quarter <- quarter(LN$date)
LN$month <- as.numeric(format(LN$date, "%m"))
LN$year <- as.numeric(format(LN$date, "%Y"))
LN$date <- format(LN$date,"%d/%m/%Y")
LN$voyageId <- paste(LN$UnidadPesquera, LN$date, sep="_")
LN$market_day <- paste(LN$Puerto.venta , LN$date, sep="_")
LN$market_day_event<- paste(LN$Puerto.venta , LN$voyageId, sep="_")

LN$date<- as.Date(LN$date,"%d/%m/%Y")
LN$weekday<- weekdays(LN$date)
LN$weekdayNum <- wday(LN$date)-1
LN$weekdayNum[LN$weekdayNum==0] <- 7
LN$week<- week(LN$date)
LN$vesselweek<- paste(LN$week, LN$UnidadPesquera)


#      Calcular tabla muestreo              #########
#################################################### #



# Crear tablas ####
LandingStrata<- DB %>% group_by(strata) %>% summarise(Nportday_LAN=length(unique(market_day)),
                                                      Nvesselweek_LAN= length (unique(vesselweek)),
                                                      Ntrip_LAN=length(unique(market_day_event)),
                                                      Nvessel_LAN=length(unique(UnidadPesquera)),
                                                      NSpecies_LAN= length(unique(Especie_ALFA3)))
SamplingStrata<- LN %>% group_by(strata) %>% summarise(Nportday_SAMP=length(unique(market_day)),
                                                      Nvesselweek_SAMP= length (unique(vesselweek)),
                                                      Ntrip_SAMP=length(unique(market_day_event)),
                                                      Nvessel_SAMP=length(unique(UnidadPesquera)),
                                                      NSpecies_SAMP= length(unique(Especie.muestreada)),
                                                      NLength_SAMP= sum(Nº.Individuos))

AR_SamplingDesignTable <- full_join(LandingStrata, SamplingStrata, by="strata") %>% select(strata, Nportday_LAN, Nportday_SAMP, 
                                                                                           Nvesselweek_LAN, Nvesselweek_SAMP,
                                                                                           Ntrip_LAN, Ntrip_SAMP,
                                                                                           Nvessel_LAN, Nvessel_SAMP,
                                                                                           NSpecies_LAN, NSpecies_SAMP,
                                                                                           NLength_SAMP)



#guardar t<blas
write.table(AR_SamplingDesignTable, "Explotacion_PNDB\\AR_4A_SamplingDesignTable.csv", sep=";", dec=".", row.names=F)




