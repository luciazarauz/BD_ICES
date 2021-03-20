# hago una prieba para ver cómo se linkan las NV y lso logboosk por codig de marea


################################################################### #
#   Cargar librerias y funciones                                #####
################################################################### #

rm(list=(ls()))
options(digits=2, scipen = 999)

library (stringr)
library(doBy)
library(dplyr)
library(data.table)
library(tidyverse)
library(lubridate)

# count unique
Fun_CountUnique <- function (x) { length(unique(x))} 

# mgsub: function for multiple replacement of accents
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



################################################################### #
#   Leer los ficheros                                           #####
################################################################### #

#Año
Ano <- 2020

# Ventas
DB <-read.table("0_Datos/Ventas_2020.csv", sep=",",dec=".",header=T, stringsAsFactors = FALSE)
head(DB); dim(DB)
names(DB)       <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), names(DB))
DB$Nombre_Buque <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), DB$Nombre_Buque)
DB$Puerto_Base  <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), DB$Puerto_Base)

# Notas de Venta
NV <-read.table("0_Datos/N1V_2020.csv", sep=";",dec=",",header=T, stringsAsFactors = FALSE)

head(NV); dim(NV)
names(NV)       <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), names(NV))
names(NV)       <- mgsub(c("\\."), c("\\_"), names(NV))
NV$Nombre_Buque <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), NV$Nombre_Buque)
NV$Puerto_Base  <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), NV$Puerto_Base)

NV <- NV %>% rename( Fecha_Desembarco      = Fecha_Regreso_N1V,
                     Dia                   = Dia_Venta,
                     Mes                   = Mes_Venta,
                     Trimestre             = Trimestre_Venta,
                     Kg_Desemb_Peso_Vivo   = Peso_Neto)

# Datos oficiales
DO <-read.table("0_Datos/DO_2020.csv", sep=";",dec=",",header=T, stringsAsFactors = FALSE)

head(DO); dim(DO)
names(DO)       <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), names(DO))
names(DO)       <- mgsub(c("\\."), c("\\_"), names(DO))
DO$Nombre_Buque <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), DO$Nombre_Buque)
DO$Puerto_Base  <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), DO$Puerto_Base)

names(DB)[!names(DB) %in%  names(DO)]

DO <- DO %>% rename( Cod_UE                = Cod__UE_Buque,
                     IdVenta               = IdMarea,
                     Dia_Desembarco        = Dia_desemb,
                     Mes_Desembarco        = Mes_desemb,
                     Trimestre_Desembarco  = Trimestre_desemb,
                     Dia                   = Dia_venta,
                     Mes                   = Mes_venta,
                     Trimestre             = Trimestre_venta,
                     Kg_Desemb_Peso_Vivo   = Peso,
                     MetierDO              = Nombre_Arte,
                     CMarea                = CODIGOMAREAOFICIAL)

#

# Ventas - Fecha desembarco
DB$Dia_Desembarco[is.na(DB$Dia_Desembarco)] <- DB$Dia[is.na(DB$Dia_Desembarco)]
DB$Mes_Desembarco[is.na(DB$Mes_Desembarco)] <- DB$Mes[is.na(DB$Mes_Desembarco)]
DB$Trimestre_Desembarco[is.na(DB$Trimestre_Desembarco)] <- DB$Trimestre[is.na(DB$Trimestre_Desembarco)]
DB$Fecha_Desembarco <- make_date(DB$Ano,DB$Mes_Desembarco,DB$Dia_Desembarco)
head(DB)

#NV - Fecha desembarco
NV$Fecha_Venta <- as.character(make_date(NV$Ano,NV$Mes,NV$Dia))
#NV$Fecha_Desembarco <- ymd_hms(NV$Fecha_Desembarco, tz = "Europe/Madrid")
NV$Fecha_Desembarco <- floor_date(ymd_hms(NV$Fecha_Desembarco, tz = "Europe/Madrid"), "day")
sf <- stamp("2030-12-31")
NV$Fecha_Desembarco <- sf(NV$Fecha_Desembarco )
NV$Fecha_Desembarco[is.na(NV$Fecha_Desembarco)] <- NV$Fecha_Venta[is.na(NV$Fecha_Desembarco)]
head(NV$Fecha_Desembarco)


#DO - Fecha desembarco
DO$Dia_Desembarco[is.na(DO$Dia_Desembarco)] <- DO$Dia[is.na(DO$Dia_Desembarco)]
DO$Mes_Desembarco[is.na(DO$Mes_Desembarco)] <- DO$Mes[is.na(DO$Mes_Desembarco)]
DO$Trimestre_Desembarco[is.na(DO$Trimestre_Desembarco)] <- DO$Trimestre[is.na(DO$Trimestre_Desembarco)]
DO$Fecha_Desembarco <- make_date(DO$Ano,DO$Mes_Desembarco,DO$Dia_Desembarco)
head(DO)


# link por codigo de marea oficial
NV_trip <- NV %>% group_by( Nombre_Buque, CMarea , Fecha_Desembarco, Metier, Zona) %>% summarise(Peso=sum(Kg_Desemb_Peso_Vivo, na.rm=T))
NV_trip$CMarea[is.na(NV_trip$CMarea)] <- paste(NV_trip$Nombre_Buque[is.na(NV_trip$CMarea)] , NV_trip$Fecha_Desembarco[is.na(NV_trip$CMarea)] , sep="_")

DO_trip <- DO %>% group_by( Nombre_Buque, CMarea , Fecha_Desembarco, Metier, Zona) %>% summarise(Peso=sum(Kg_Desemb_Peso_Vivo, na.rm=T))
DO_trip$CMarea[is.na(DO_trip$CMarea)] <- paste(DO_trip$Nombre_Buque[is.na(DO_trip$CMarea)] , DO_trip$Fecha_Desembarco[is.na(DO_trip$CMarea)] , sep="_")

link_trip <- NV_trip %>% inner_join(DO_trip, by=c("Nombre_Buque", "CMarea"), suffix = c(".NV", ".DO")) %>% data.frame()
head(link_trip)


# link por barco fecha
nolink_NV <- NV_trip %>% anti_join(DO_trip, by=c("Nombre_Buque", "CMarea"), suffix = c(".NV", ".DO")) %>% data.frame()
nolink_NV$CMarea <- paste(nolink_NV$Nombre_Buque , nolink_NV$Fecha_Desembarco, sep="_")

nolink_DO <- DO_trip %>% anti_join(NV_trip, by=c("Nombre_Buque", "CMarea"), suffix = c(".NV", ".DO")) %>% data.frame()
nolink_DO$CMarea <- paste(nolink_DO$Nombre_Buque , nolink_DO$Fecha_Desembarco, sep="_")

link_trip2 <- nolink_NV %>% full_join(nolink_DO, by=c("Nombre_Buque", "CMarea"), suffix = c(".NV", ".DO")) %>% data.frame()
head(link_trip2)
dim(link_trip2)


link <- rbind(link_trip, link_trip2)
  





dim(subset(link, is.na(Peso.DO)))
head(subset(link, is.na(Peso.DO)))

dim(subset(link, is.na(Peso.NV)))
head(subset(link, is.na(Peso.NV)))

subset(link, Nombre_Buque=="Aketxe")
subset(link, Nombre_Buque=="Aketxe" & is.na(Peso.NV))
subset(link, Nombre_Buque=="Aketxe" & is.na(Peso.DO))


#   Guardar ficheros                            ####
################################################## #
write.table(link, "Depuracion_Metiers\\Output\\link_test.csv", row.names = FALSE, sep=";", dec=",")


