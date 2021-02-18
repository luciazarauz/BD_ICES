################################################################################
#                   chequear mareas duplicadas
################################################################################
################################################################################

#  Desembarcos 
#  Puerto Base: españa:
#  Unidad Temporal:	Dia;    
#
#  Datos Oficiales (informe de datso oficiales - desembarcos)

# link ventas mareas.listado y actualizacion de metiers mareas (origen ventas) 

################################################################################

#      Previo                  #####
####################################
rm(list=(ls()))


library (stringr)
library(doBy)
library(plyr)
library(dplyr)
library(data.table)

Fun_CountUnique <- function (x) { length(unique(x))}   

dat.path <- file.path("C:/use/0_Lucia/1_Proyectos/AA_SegPes/BD_AZTI_Metiers//data/" ) #Base de datos en mi C:
res.path  <- "C:/use/0_Lucia/1_Proyectos/AA_SegPes/BD_AZTI_Metiers/results/"  #Base de datos en mi C:
setwd(dat.path)


#      Leer los ficheros       ####
###################################


#Desembarcos
DB <-read.table("2019_ventas.csv", sep=";",dec=",",header=T, stringsAsFactors = FALSE)
#DB <- as.data.table(DB0)
head(DB); dim(DB)


#      chequear       ####
###################################
DB$Fecha_venta <- as.Date(format(ISOdate(DB$Ano,DB$Mes,DB$Dia),"%d/%m/%Y"),"%d/%m/%Y")
head(DB$Fecha_venta )
DBcheck <- DB %>% group_by(Nombre_Buque, Fecha_venta, Puerto_Venta) %>%
                  summarise(KgTot=sum(Kg_Desembarcados)) %>%
                  arrange(Nombre_Buque, Fecha_venta, Puerto_Venta) 
DBcheck$dif <- DBcheck$KgTot-lag(DBcheck$KgTot)
DBcheck_fin <- subset(DBcheck, dif==0)

setwd(res.path)
write.table(DBcheck_fin,"revisar.csv", sep=";", dec=",", row.names = FALSE)



