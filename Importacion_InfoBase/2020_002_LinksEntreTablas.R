# ----------------------------------------------------------------- #
# Readme:                                                           #
# ----------------------------------------------------------------- # 
# De este script no sale ningun fichero nuevo ni se crean variables.
# sirve para chequear las relaciones entre las diferentes tablas y 
# explorar un poco los datos.
#
# INDICE:
#
# Libraries.
# Load data from depuracion.
# Explore links between tables.
#
#
# ----------------------------------------------------------------- # 
# R version 3.6.0 (2019-04-26) -- "Planting of a Tree"
# ----------------------------------------------------------------- # 

# # ################## #
# # Libraries          #
# # ################## #
library(dplyr)
library(lubridate)
library(data.table)
library(reshape2)


# # ################## #
# # Load               #
# # ################## #
# Se cargan los datos derivados de la depuracion.

rm(list=ls())
load(file="0_Datos/Infobase/2020/Infobase2020_Unique_20210312.Rdata"   )


# # ############################# #
# # EXPLORE LINKS OF THE TABLES   #
# # ############################# #

head(InfoBase)
head(InfoBuquesUnique)
head(InfoVentas)
head(InfoOrigenLineas)
head(InfoParametrosArteCapturas_wide)
head(InfoCapturasCalculadas)
head(InfoCaptura0)
head(InfoDescartes)


# ----------------------------------------------- #
# InfoCapturas - InfoCapturasCalculadas           #
# ----------------------------------------------- #



# InfoOrigenLineas - InfoCapturasCalculadas             #####
######################################################### #
# Todas las InfoCapturasCalculadas tienen un Id en InfoOrigenLineas. 
# Todas las InfoOrigenLineas tienen un id en InfoCapturasCalculadas 

length(unique(InfoCapturasCalculadas$IdInfoOrigenLinea[unique(InfoCapturasCalculadas$IdInfoOrigenLinea) %in% unique(InfoOrigenLineas$IdInfoOrigenLinea)]))
length(unique(InfoCapturasCalculadas$IdInfoOrigenLinea))
length(unique(InfoOrigenLineas$IdInfoOrigenLinea))

temp<- InfoCapturasCalculadas %>% anti_join(InfoOrigenLineas, by=c("IdInfoOrigenLinea"="IdInfoOrigenLinea"))  ; dim(temp)
temp<- InfoOrigenLineas %>% anti_join(InfoCapturasCalculadas, by=c("IdInfoOrigenLinea"="IdInfoOrigenLinea"))  ; dim(temp)



# InfoOrigenLineas - InfoDiarios                    #####
######################################################### #
# Todas las InfoOrigenLineas tienen un Id en InfoDiarios 
# pero no todas las todas las InfoDiarios tienen un id en InfoOrigenLineas 
#   puede ser que vengan de descarte o de infocaptura 0

length(unique(InfoDiarios$IdDiario[unique(InfoDiarios$IdDiario) %in% unique(InfoOrigenLineas$IdDiario)]))
length(unique(InfoDiarios$IdDiario))
length(unique(InfoOrigenLineas$IdDiario))

temp<- InfoOrigenLineas %>% anti_join(InfoDiarios, by="IdDiario")  ; dim(temp)
temp<- InfoDiarios %>% anti_join(InfoOrigenLineas, by="IdDiario")  ; dim(temp)


# InfoDescartes - InfoDiarios                 #####
######################################################### #
# Todas las InfoDescartes tienen un Id en InfoDiarios 
# pero no todas las todas las InfoDiarios tienen un id en InfoDescartes
#   puede ser que vengan de infocapturascalculadas o de infocaptura 0

length(unique(InfoDescartes$IdDiario[unique(InfoDescartes$IdDiario) %in% unique(InfoDiarios$IdDiario)]))
length(unique(InfoDescartes$IdDiario))
length(unique(InfoDiarios$IdDiario))

temp<- InfoDescartes %>% anti_join(InfoDiarios, by="IdDiario")  ; dim(temp)
temp<- InfoDiarios %>% anti_join(InfoDescartes, by="IdDiario")  ; dim(temp)


# InfoCaptura0 - InfoDiarios                    #####
######################################################### #
# Todas las InfoCapturaLance0 tienen un Id en InfoDiarios 
# pero no todas las todas las InfoDiarios tienen un id en InfoCapturaLance0

length(unique(InfoCaptura0$IdDiario[unique(InfoCaptura0$IdDiario) %in% unique(InfoDiarios$IdDiario)]))
length(unique(InfoCaptura0$IdDiario))
length(unique(InfoDiarios$IdDiario))

temp<- InfoCaptura0 %>% anti_join(InfoDiarios, by="IdDiario")  ; dim(temp)
temp<- InfoDiarios %>% anti_join(InfoCaptura0, by="IdDiario")  ; dim(temp)


# InfoOrigenLineas/InfoDescartes/ InfoCapturaLance0/ - InfoDiariosUnique             #####
########################################################################################## #
# Todas los IdDiario de InfoDiarios tienen un Id en InfoOrigenLineas o InfoDescartes o InfoCapturaLance0 

allDiario <- data.frame(IdDiario=c(InfoCapturasCalculadas$IdDiario, InfoOrigenLineas$IdDiario, InfoDescartes$IdDiario, InfoCaptura0$IdDiario ))

temp<- InfoDiarios %>% anti_join(allDiario, by="IdDiario")  ; dim(temp)



# Comprobaciones problemas del ano pasado            #### #
######################################################### #

# Codigo de diario duplicado en algunas ventas

head(subset(InfoVentas, IdDiario<0))
head(subset(InfoVentas, IdDiario<0))
MareasNeg <-(-1)*(InfoDiarios$IdDiario[InfoDiarios$IdDiario<0]) # MareasNeg= IdDiario de las Mareas negativas en InfoDiarios

InfoVentas_check <- (InfoVentas[InfoVentas$IdVenta %in% MareasNeg & InfoVentas$IdDiario>0,]) # en InfoVentas: registros que tienen IdVenta en MareasNeg y IdDiario>0
dim(InfoVentas_check)  # en InfoVentas: No hay registros que tienen IdVenta en MareasNeg y IdDiario>0
InfoOrigenLineas_check <- (InfoOrigenLineas[InfoOrigenLineas$IdNotaDeVenta %in% MareasNeg & InfoOrigenLineas$IdDiario>0,])
dim(InfoOrigenLineas_check)  # en InfoOrigenLineas: No hay registros que tienen IdVenta en MareasNeg y IdDiario>0


# Mareas huerfanas: mareas con esloramayor de 10m, sin diario de pesca pero con NV, que en peso consumo aparece 0. SGM nos dice que mantengamos el peso Consumo=0

InfoCapturasCalculadas_check <- InfoCapturasCalculadas %>% left_join(select(InfoDiarios, IdDiario , IdBuque), by= c("IdDiario" = "IdDiario")) %>%
                                                           left_join(select(InfoBuquesUnique, IdBuque,Nombre,EsloraTotal), by= c("IdBuque" = "IdBuque")) 
                                                          
dim(InfoCapturasCalculadas_check)
a <- filter(InfoCapturasCalculadas_check,EsloraTotal<10 ); head(a); dim(a)
a <- filter(InfoCapturasCalculadas_check,EsloraTotal<10 & PesoConsumo>0 ); head(a); dim(a)
a <- filter(InfoCapturasCalculadas_check,EsloraTotal<10 & PesoConsumo>0 & PesoNotaVenta>0); head(a); dim(a)
a <- filter(InfoCapturasCalculadas_check,EsloraTotal<10 & PesoConsumo==0 ); head(a); dim(a)
a <- filter(InfoCapturasCalculadas_check,EsloraTotal>10 ); dim(a); head(a); dim(a)
