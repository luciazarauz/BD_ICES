# ----------------------------------------------------------------- #
# Readme:                                                           #
# ----------------------------------------------------------------- # 
# En este script: corregimos los datso segun los criterios definidos
#
# INDICE:
#
# Functions
# Libraries.
# Cargar la tabla maestra (dori).
# Correcciones
#
# en el fondo hacemso lo mismo que en Comprobar_Corregir_explorar, pero sin tantas comprobaciones. 
# asumiendo que ya sabemos lo que estamos haciendo
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

Fun_unique <- function(x){length(unique(x))}


# # ####################################### #
# # Load                                 ####
# # ####################################### #

load(file="Datos/Dori2019_v2.Rdata"   )
load(file="Datos/Infobase2019_Unique_20200724.Rdata"   )

Dori_ini <- Dori
namevar <- c("IdDori", "IdDiario", "Nombre", "EsloraTotal", "CensoPorModalidad", "PuertoBase", "IdMarea", 
             "FcSalida","CodigoPuertoSalida_AL5", "C_FcRegresoFloor","C_FcRegreso",  "C_CodigoPuertoDesembarque_AL5", "C_FcVentaMax", "C_CodigoPuertoVenta_AL5",
             "CodigoOrigen", "OrigenIdentificador", "CatchCategory", "Desembarcado",
             "FcCaptura", "Posicion", "CodigoDivision", "RectanguloEstadistico", "CodigoArte_FaoAL3", "TiempoPescaMin", "NumOperaciones",
             "Especie_AL3", "Presentacion_AL3", "PesoConsumo", "PesoConsumoBajoTalla", "FactorConversion",  "PesoDesembarcado","PesoCapturado", "PesoRetenido" ,"PesoNotaVenta",  "PesoDescarte", "MotivoDescarte")


# # ####################################### #
# # tablas para el chequeo               ####
# # ####################################### #

funMareasTotales <- function(x) { 
  MareasTotales <- x %>% group_by( CensoPorModalidad, Nombre, EsloraTotal, IdMarea, C_FcRegresoFloor , CatchCategory) %>% 
    summarise(PesoTotal = sum(PesoConsumoTotal+ PesoDescarte, na.rm=T)) 
  MareasTotales <- MareasTotales %>% dcast(CensoPorModalidad + Nombre + EsloraTotal + C_FcRegresoFloor + IdMarea ~ CatchCategory,  value.var = "PesoTotal")
  MareasTotales <- MareasTotales %>% arrange(Nombre, C_FcRegresoFloor, CensoPorModalidad, EsloraTotal, IdMarea)
  return(MareasTotales)
}

funCodigoOrigen <- function(x) { 
  CodigoOrigen<- x %>% group_by( CensoPorModalidad, Nombre, EsloraTotal, IdMarea, C_FcRegresoFloor , CodigoOrigen) %>% summarise(PesoTotal = sum(PesoConsumoTotal+ PesoDescarte, na.rm=T)) %>%
    dcast(CensoPorModalidad + Nombre + EsloraTotal + C_FcRegresoFloor + IdMarea ~ CodigoOrigen,  value.var = "PesoTotal")
  CodigoOrigen <- CodigoOrigen %>% arrange(Nombre, C_FcRegresoFloor, CensoPorModalidad, EsloraTotal, IdMarea)
  names(CodigoOrigen) [names(CodigoOrigen) == 0] <- "X0"
  return(CodigoOrigen)
}

funDesembarcado <- function(x) { 
  Desembarcado<- x %>% group_by( CensoPorModalidad, Nombre, EsloraTotal, IdMarea, C_FcRegresoFloor , Desembarcado) %>% summarise(PesoTotal = sum(PesoConsumoTotal+ PesoDescarte, na.rm=T)) %>%
    dcast(CensoPorModalidad + Nombre + EsloraTotal + C_FcRegresoFloor + IdMarea ~ Desembarcado,  value.var = "PesoTotal")
  Desembarcado <- Desembarcado %>% arrange(Nombre, C_FcRegresoFloor, CensoPorModalidad, EsloraTotal, IdMarea)
  names(Desembarcado) [names(Desembarcado)==0] <- "False"
  names(Desembarcado) [names(Desembarcado)==1] <- "True"
  return(Desembarcado)
}


MareasTotales <- funMareasTotales (Dori); head(MareasTotales)
CodigoOrigen  <- funCodigoOrigen (Dori);  head(CodigoOrigen)
Desembarcado  <- funDesembarcado (Dori);  head(Desembarcado)


Dori_ref <- Dori

# # ####################################### #
# # Paso 1 - InfoCapturasLance0          ####
# # ####################################### #
#   - las marcamos para que no se incorporen a la BD. Las mantenemos en la BD original porque son mareas reales, pero no las contabilizamos
#

Dori_lance0 <- subset(Dori, CatchCategory=="CapturasLance0")
Dori <- subset(Dori, CatchCategory!="CapturasLance0")

  length(unique(Dori_ref$IdMarea))
  length(unique(c(Dori$IdMarea, Dori_lance0$IdMarea)))
  sum(Dori_ref$PesoConsumoTotal); sum(Dori_ref$PesoDescarte)
  sum(Dori$PesoConsumoTotal);     sum(Dori$PesoDescarte)

MareasTotales <- funMareasTotales (Dori); dim(MareasTotales)
CodigoOrigen  <- funCodigoOrigen (Dori); dim(MareasTotales)
Desembarcado  <- funDesembarcado (Dori); dim(MareasTotales) 


# # ####################################### #
# # Paso 2 - Mareas Huerfanas            ####
# # ####################################### #
#   - Eslora >10 & Sin Id Marea oficial  & PesoConsumo== 0 -> eliminar

check <- MareasTotales %>% filter (substr(IdMarea,1,3)!="ESP") %>% filter (EsloraTotal>=10)
  head(check)
  dim(check)  # 82
  sum(check$CapturasCalculadas)
Dori_huerfanas <- subset(Dori, IdMarea %in% check$IdMarea) # las sguardo para comprobaciones, pero no la vamos a incorporar a la BD
Dori <- subset(Dori, !IdMarea %in% check$IdMarea)

  length(unique(Dori_ref$IdMarea))
  length(unique(c(Dori$IdMarea, Dori_lance0$IdMarea, Dori_huerfanas$IdMarea)))
  sum(Dori_ref$PesoConsumoTotal); sum(Dori_ref$PesoDescarte)
  sum(Dori$PesoConsumoTotal);     sum(Dori$PesoDescarte)

MareasTotales <- funMareasTotales (Dori); dim(MareasTotales)
CodigoOrigen  <- funCodigoOrigen (Dori); dim(MareasTotales)
Desembarcado  <- funDesembarcado (Dori); dim(MareasTotales)  


# # ####################################################################################### #
# # Paso 3 - Mareas en las que se ha hecho el link entre Nv y logbooks                  #####
# #          y en las que se asigna Peso Consumo =0 a las líneas que vienen de NV       ### #
# # ####################################################################################### #
#   - eliminamos de la BD original. son lineas ficticias readas al combinar diferentes fuentes de información

check <- CodigoOrigen %>% filter ((!is.na(CA)) | (!is.na(DE) ) | (!is.na(Descartes) )) # identificamos mareas que  tienen pesos con origen CA/ DE/ Descartes
  head(check)
  dim(check)
temp <- Dori %>% filter(IdMarea %in% check$IdMarea & !CodigoOrigen %in% c("CA", "DE", "Descartes"))  # dentro de las anteriores, identificamos las lineas con origen NV/ 0
Dori <- subset(Dori, !IdDori %in% temp$IdDori)

  length(unique(Dori_ref$IdMarea))
  length(unique(c(Dori$IdMarea, Dori_lance0$IdMarea, Dori_huerfanas$IdMarea)))
  sum(Dori_ref$PesoConsumoTotal); sum(Dori_ref$PesoDescarte)
  sum(Dori$PesoConsumoTotal);     sum(Dori$PesoDescarte)

MareasTotales <- funMareasTotales (Dori); dim(MareasTotales)
CodigoOrigen  <- funCodigoOrigen (Dori);  dim(MareasTotales)
Desembarcado  <- funDesembarcado (Dori);  dim(MareasTotales) 



# # ######################################################## #
# # Paso 4 - Peso Consumo = 0 en toda la marea           #####
# # ######################################################## #
#   - si tienen PesoConsumo= 0 pero tienen descarte -> mantenemos
#   - si tienen PesoConsumo= 0 pero peso desembarcado/Capturado > 0 & censo= CERCO -> mantenemos
#   - el resto -> las marcamos para que no se incorporen a la BD. Las mantenemos en la BD original porque son mareas reales, pero no las contabilizamos
#

check <-  MareasTotales %>% filter(CapturasCalculadas==0 & is.na(Descartes))  # identificamos las mareas con pesoConsumo total en toda la marea que tampoco tengan descartes 
  head(check)
  dim(check)
temp <- subset (Dori, IdMarea %in% check$IdMarea & !(PesoCapturado>0 | PesoDesembarcado>0) )
  length(unique(temp$IdMarea))
  table(temp$CodigoOrigen)
  table(temp$CensoPorModalidad)
  unique(substr(temp$IdMarea,1,3))
Dori_captura0 <- subset(Dori, IdDori %in% temp$IdDori)
Dori <- subset(Dori, !IdDori %in% temp$IdDori)

  length(unique(Dori_ref$IdMarea))
  length(unique(c(Dori$IdMarea, Dori_lance0$IdMarea, Dori_huerfanas$IdMarea, Dori_captura0$IdMarea)))
  sum(Dori_ref$PesoConsumoTotal); sum(Dori_ref$PesoDescarte)
  sum(Dori$PesoConsumoTotal);     sum(Dori$PesoDescarte)

# 
#   xx <- subset (Dori, IdMarea %in% check$IdMarea  ) 
#   xx <- subset (Dori,  FactorConversion==0) 
#   subset(xx, IdMarea %in% check$IdMarea )


# # ####################################### #
# # base de datos Final                  ####
# # ####################################### #


Dori$AZTI_BD <- "Incorporar"
Dori_captura0$AZTI_BD <- "Captura0"
Dori_lance0$AZTI_BD <- "Lance0"

Dori_FIN <- rbind(Dori, Dori_captura0, Dori_lance0)

  sum(Dori_ref$PesoConsumoTotal); sum(Dori_ref$PesoDescarte)
  sum(Dori$PesoConsumoTotal);     sum(Dori$PesoDescarte)

save(Dori_FIN, file="Datos/Dori2019_v3.RData")






# # ** hay otra forma mas facil de hacerlo        
# # ** lo unico perdemos la trazabilidad de los lances marcados como "Captura0
# # ** entiendo que esto más o menos lo que hace el IEO
# # ####################################### #

Dori_2<- Dori_ref

Dori_2$AZTI_BD <- NA
Dori_2$AZTI_BD[Dori_2$CatchCategory=="CapturasLance0"]<- "Lance0"
Dori_2$AZTI_BD[is.na(Dori_2$AZTI_BD) & 
                Dori_2$PesoConsumo>0 | Dori_2$PesoConsumoBajoTalla>0 | Dori_2$PesoDescarte>0 | 
                Dori_2$PesoDesembarcado>0 | Dori_2$PesoCapturado>0]<- "Incorporar"
Dori_2$AZTI_BD[is.na(Dori_2$AZTI_BD) ] <- "Eliminar"



dim(Dori_FIN[Dori_FIN$AZTI_BD=="Lance0",])
dim(Dori_2[Dori_2$AZTI_BD=="Lance0",]) 

dim(Dori_FIN[Dori_FIN$AZTI_BD=="Incorporar",])
dim(Dori_2[Dori_2$AZTI_BD=="Incorporar",])   

dim(Dori_FIN[Dori_FIN$AZTI_BD=="Captura0",])
dim(Dori_2[Dori_2$AZTI_BD=="Captura0",])




