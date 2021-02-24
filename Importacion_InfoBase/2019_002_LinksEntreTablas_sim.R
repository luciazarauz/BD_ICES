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
load(file="0_Datos/Infobase/Infobase2019_Unique_20200724.Rdata"   )


# # ############################# #
# # EXPLORE LINKS OF THE TABLES   #
# # ############################# #

head(InfoBase)
head(InfoBuquesUnique)
head(InfoVentas)
head(InfoOrigenLineas)
head(InfoCapturas)
head(InfoParametrosArteCapturas_wide)
head(InfoCapturasCalculadas)
head(InfoCapturaLance0)
head(InfoDescartes)


# ----------------------------------------------- #
# InfoCapturas - InfoCapturasCalculadas           #
# ----------------------------------------------- #

  # Todas las IdCaptura de InfoCapturas tienen un Idcaptura en InfoCapturasCalculadas.
  # Pero no todas las IdCaptura en InfoCapturasCalculadas tienen un Idcaptura en InfoCapturas
  # Esto tiene sentido porque en InfoCapturasCalculadas se combinan datos de logbooks (que tienen informacion de esfuerzo) 
  #  con notas de venta (que no tienen información de esfuerzo)

temp<- InfoCapturas %>% anti_join(InfoCapturasCalculadas, by="IdCaptura")  ; dim(temp)
temp<- InfoCapturasCalculadas %>% anti_join(InfoCapturas, by="IdCaptura")  ; dim(temp)

dim(temp)
dim(subset(temp, is.na(IdCaptura)))
dim(subset(temp, !is.na(IdNotaDeVenta)))
dim(subset(temp, is.na(IdNotaDeVenta)))
dim(subset(temp, is.na(IdCaptura) & is.na(IdNotaDeVenta) & !is.na(IdDesembarqueEspecie)))
dim(subset(temp, is.na(IdCaptura) & is.na(IdNotaDeVenta) & is.na(IdDesembarqueEspecie)))
dim(subset(temp, is.na(IdCaptura) & is.na(IdNotaDeVenta) & is.na(IdDesembarqueEspecie)) %>% filter(PesoConsumo>0))
dim(subset(temp, is.na(IdCaptura) & is.na(IdNotaDeVenta) & is.na(IdDesembarqueEspecie)) %>% filter(PesoCapturado>0))
dim(subset(temp, is.na(IdCaptura) & is.na(IdNotaDeVenta) & is.na(IdDesembarqueEspecie)) %>% filter(PesoNotaVenta>0)) 
dim(subset(temp, is.na(IdCaptura) & is.na(IdNotaDeVenta) & is.na(IdDesembarqueEspecie) & is.na(IdMareaOrigen)))
length(unique(temp$IdMareaOrigen[is.na(temp$IdCaptura) & is.na(temp$IdNotaDeVenta) & is.na(temp$IdDesembarqueEspecie)]))
# todos tienen un peso NV positivo, pero peso consumo cero. Todos tienen un codigo marea
# son 9408 mareas


# InfoOrigenLineas - InfoCapturasCalculadas             #####
######################################################### #
# Todas las InfoCapturasCalculadas tienen un Id en InfoOrigenLineas. 
# pero no todas las todas las InfoOrigenLineas tienen un id en InfoCapturasCalculadas 
# @@ preguntar SGP. Por qué hay InfoOrigenLineas sin un id en InfoCapturasCalculadas? 
#    es información que estamos perdiendo, pero no sabemos de dónde viene

temp<- InfoCapturasCalculadas %>% anti_join(InfoOrigenLineas, by=c("IdInfoOrigenLinea"="IdInfoOrigenLinea"))  ; dim(temp)
temp<- InfoOrigenLineas %>% anti_join(InfoCapturasCalculadas, by=c("IdInfoOrigenLinea"="IdInfoOrigenLinea"))  ; dim(temp)

temp2<- temp %>% anti_join(InfoCapturaLance0, by=c("IdDiario"="IdDiario"))  ; dim(temp2)


head(temp)
table(substr(temp$OrigenIdentificador,1,2))
aa<- unique(temp$IdDiario)

subset(InfoOrigenLineas, IdInfoOrigenLinea==54656811)
subset(InfoCapturasCalculadas, IdInfoOrigenLinea==54656811)
subset(InfoDescartes, IdDiario==448524)
subset(InfoCapturaLance0, IdDiario==448524)
subset(InfoCapturasCalculadas, IdDiario==448524)

subset(InfoOrigenLineas, IdInfoOrigenLinea==54833890 )
subset(InfoCapturasCalculadas, IdInfoOrigenLinea==54833890 )
subset(InfoDescartes, IdDiario==468693)
subset(InfoCapturaLance0, IdDiario==468693)
subset(InfoCapturasCalculadas, IdDiario==468693)



# InfoOrigenLineas - InfoDiarios                    #####
######################################################### #
# Todas las InfoOrigenLineas tienen un Id en InfoDiarios 
# pero no todas las todas las InfoDiarios tienen un id en InfoOrigenLineas 
#   puede ser que vengan de descarte o de infocaptura lance 0

temp<- InfoOrigenLineas %>% anti_join(InfoDiarios, by="IdDiario")  ; dim(temp)
temp<- InfoDiarios %>% anti_join(InfoOrigenLineas, by="IdDiario")  ; dim(temp)


# InfoDescartes - InfoDiarios                 #####
######################################################### #
# Todas las InfoDescartes tienen un Id en InfoDiarios 
# pero no todas las todas las InfoDiarios tienen un id en InfoDescartes 

temp<- InfoDescartes %>% anti_join(InfoDiarios, by="IdDiario")  ; dim(temp)
temp<- InfoDiarios %>% anti_join(InfoDescartes, by="IdDiario")  ; dim(temp)


# InfoCapturaLance0 - InfoDiariosUnique             #####
######################################################### #
# Todas las InfoCapturaLance0 tienen un Id en InfoDiarios 
# pero no todas las todas las InfoDiarios tienen un id en InfoCapturaLance0

temp<- InfoCapturaLance0 %>% anti_join(InfoDiarios, by="IdDiario")  ; dim(temp)
temp<- InfoDiarios %>% anti_join(InfoCapturaLance0, by="IdDiario")  ; dim(temp)


# InfoOrigenLineas/InfoDescartes/ InfoCapturaLance0/ - InfoDiariosUnique             #####
########################################################################################## #
# Todas los IdDiario de InfoDiarios tienen un Id en InfoOrigenLineas o InfoDescartes o InfoCapturaLance0 

allDiario <- data.frame(IdDiario=c(InfoCapturasCalculadas$IdDiario, InfoOrigenLineas$IdDiario, InfoDescartes$IdDiario, InfoCapturaLance0$IdDiario ))

temp<- InfoDiarios %>% anti_join(allDiario, by="IdDiario")  ; dim(temp)

temp2 <- temp %>% anti_join(InfoVentas, by="IdDiario")  ; dim(temp2)



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
                                                          

a <- filter(InfoCapturasCalculadas_check,EsloraTotal<10 ); head(a); dim(a)
a <- filter(InfoCapturasCalculadas_check,EsloraTotal<10 & PesoConsumo>0 ); head(a); dim(a)
a <- filter(InfoCapturasCalculadas_check,EsloraTotal<10 & PesoConsumo==0 ); head(a); dim(a)
a <- filter(InfoCapturasCalculadas_check,EsloraTotal>10 ); dim(a); head(a); dim(a)
