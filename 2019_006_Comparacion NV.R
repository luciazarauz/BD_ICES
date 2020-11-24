# ----------------------------------------------------------------- #
# Readme:                                                           #
# ----------------------------------------------------------------- # 
# En este script: comparamos los datos con las Nv de Hazi
# salida BD: Notas de venta + detalle. Puerto base españa. Agregar por tamaños
#
# INDICE:
#
# Functions
# Libraries.
# Cargar la tabla maestra (dori) y las NV.
# Comprobaciones
#

#
# ----------------------------------------------------------------- # 
# R version 3.6.0 (2019-04-26) -- "Planting of a Tree"
# ----------------------------------------------------------------- # 


# # ################## #
# # Libraries          #
# # ################## #

rm(list=ls())

library(fishPiCodes)
data("UNLOCODE")
library(lubridate)
library(data.table)
library(reshape2)
library(dplyr)
#library(plyr)
library(scales)


# Funciones  ####
################ #

Fun_unique <- function(x){length(unique(x))}

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



# # ####################################### #
# # Load                                 ####
# # ####################################### #

load(file="Datos/Dori2019_v3.Rdata"   )

ventas <- read.csv("Datos/Ventas2019.csv", sep=",", dec=".", stringsAsFactors = F, skip=11)
head(ventas)

MaestrosBuque <- read.csv("Auxtables/MaestroBuquesAZTI_2019.csv", sep=",", dec=".", stringsAsFactors = F)
head(MaestrosBuque)
names(MaestrosBuque) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(MaestrosBuque))


# Añadir variable Censo Por Modalidad ####
ventas$CensoPorModalidad <- MaestrosBuque$Caladero.principal[match(ventas$Nombre_Buque, MaestrosBuque$Buque)]
sort(unique(ventas$Nombre_Buque[is.na(ventas$CensoPorModalidad)]))
ventas$CensoPorModalidad[ventas$CensoPorModalidad == "ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII y VIIIa"] <- "ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII y VIIIabde."

# Añadir variable CodigoCFR ####
ventas$CodigoCFR <- MaestrosBuque$Codigo.UE[match(ventas$Nombre_Buque, MaestrosBuque$Buque)]
sort(unique(ventas$Nombre_Buque[is.na(ventas$CodigoCFR)]))


# Añadir variable Laboratorio ####
ventas$Puerto_Base <- toupper(ventas$Puerto_Base )
ventas$Puerto_Venta <- toupper(ventas$Puerto_Venta )


PuertoVasco <- c("ALGORTA","ARMINTZA", "BILBAO", "BERMEO", "CIERVANA", "DONOSTIA", "ELANCHOVE",
                 "FUENTERRABIA","GETARIA", "GUETARIA", "GETARIA", "HONDARRIBIA", "LEKEITIO", "LEQUEITIO","MOTRICO",
                 "MUNDAKA","MUTRIKU", "ONDARROA", "ORIO","PASAIA", "PASAJES","PLENTZIA", "PLENCIA","SAN SEBASTIAN",
                 "SANTURTZI", "SANTURCE","ZIERBENA", "ZUMAYA")

sort(unique(ventas$Puerto_Venta))
ventas$LAB_PuertoDesembarco <- NA
ventas$LAB_PuertoDesembarco [ventas$Puerto_Venta %in% PuertoVasco] <- "EUS"
ventas$LAB_PuertoDesembarco [is.na(ventas$LAB_PuertoDesembarco)] <- "ESP"
ventas$LAB_PuertoDesembarco [ventas$Puerto_Venta %in% c("TROMSØ")] <- "OTH"
table(ventas$LAB_PuertoDesembarco)

sort(unique(ventas$Puerto_Base))
ventas$LAB_PuertoBase <- NA
ventas$LAB_PuertoBase [ventas$Puerto_Base %in% PuertoVasco] <- "EUS"
ventas$LAB_PuertoBase [is.na(ventas$LAB_PuertoBase)] <- "ESP"
table(ventas$LAB_PuertoBase)

ventas$LABORATORIO<- NA
ventas$LABORATORIO [is.na(ventas$LABORATORIO) & ventas$LAB_PuertoDesembarco == "EUS"] <-  "AZTI"
ventas$LABORATORIO [is.na(ventas$LABORATORIO) & ventas$LAB_PuertoDesembarco == "OTH" & ventas$LAB_PuertoBase=="EUS"] <-  "AZTI"
ventas$LABORATORIO [is.na(ventas$LABORATORIO) ]<- "IEO"
#ventas$LABORATORIO [ ventas$Nombre == "NUEVO CHAROLAIS" ]<- "IEO"

ventas %>% group_by(IdVenta) %>% summarize(n=Fun_unique(LABORATORIO )) %>% filter(n > 1)
ventas %>% group_by(IdVenta) %>% summarize(n=Fun_unique(LABORATORIO )) %>% filter(n > 1)

tapply(ventas$Peso_Neto, list(ventas$LAB_PuertoBase, ventas$LAB_PuertoDesembarco,ventas$LABORATORIO), sum, na.rm=TRUE)


unique(ventas$Puerto_Venta[ventas$LABORATORIO=="AZTI"])
unique(ventas$Puerto_Venta[ventas$LABORATORIO=="IEO"])



## comparacion por censo
comp_dori <- Dori_FIN %>% group_by(CensoPorModalidad) %>% summarise(Ntrip = length(unique(IdMarea)),
                                                               Peso = sum(PesoConsumoTotal, na.rm=T))
comp_nv <- ventas %>% group_by(CensoPorModalidad) %>% summarise(Ntrip = length(unique(IdVenta)),
                                                                            Peso = sum(Peso_Neto, na.rm=T))

comp <- comp_nv  %>% full_join(comp_dori, by=c("CensoPorModalidad"), suffix = c("_NV", "_DO")) %>% data.frame()
comp <- comp %>% arrange(CensoPorModalidad)
comp


## comparacion por censo y barco
comp_dori <- Dori_FIN %>% group_by(CensoPorModalidad, CodigoCFR) %>% summarise(Ntrip = length(unique(IdMarea)),
                                                                                            Peso = sum(PesoConsumoTotal, na.rm=T))
comp_nv <- ventas %>% group_by(CensoPorModalidad, CodigoCFR,Nombre_Buque, Puerto_Base) %>% summarise(Ntrip = length(unique(IdVenta)),
                                                                                        Peso = sum(Peso_Neto, na.rm=T))

comp <- comp_nv  %>% full_join(comp_dori, by=c("CensoPorModalidad", "CodigoCFR"), suffix = c("_NV", "_DO")) %>% data.frame()
comp$Nombre_Buque[is.na(comp$Nombre_Buque)] <- Dori_FIN$Nombre[match(comp$CodigoCFR[is.na(comp$Nombre_Buque)], Dori_FIN$CodigoCFR)]
comp$Puerto_Base[is.na(comp$Puerto_Base)] <- Dori_FIN$PuertoBase[match(comp$CodigoCFR[is.na(comp$Puerto_Base)], Dori_FIN$CodigoCFR)]
comp <- comp %>% arrange(CensoPorModalidad, LABORATORIO, CodigoCFR)
head(comp)

write.table(comp, "Comparacion_NV_Dori.csv", sep=",", dec=".", row.names = F)




