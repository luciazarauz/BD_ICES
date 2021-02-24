# R version 3.6.0 (2019-04-26) -- "Planting of a Tree"

# Path                     ####
############################# #
rm(list=ls())


# Libraries             #####
############################ #

library(fishPiCodes)
data("UNLOCODE")
data("ASFIS")
library(lubridate)
library(data.table)
library(reshape2)
library(dplyr)
#library(plyr)



# Leer fichero                          ####
############################################### #
setwd(path.data)
load(file="0_Datos/Infobase/Dori2019_v3.Rdata" )

# Preparar Tabla Importación  #####
################################# #


#Fuente
##
Dori$Fuente[Dori$CodigoOrigen %in% c("CA", "DE",0)] <- "DP2019"
Dori$Fuente[Dori$CodigoOrigen %in% c("NV")] <- "NV2019"
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(Fuente )) %>% filter(n > 1)




# Final Check

Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(Nombre )) %>% filter(n > 1)
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(C_PuertoDesembarque )) %>% filter(n > 1)
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(C_PuertoVenta )) %>% filter(n > 1)
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(C_Area )) %>% filter(n > 1)
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(LABORATORIO )) %>% filter(n > 1)



table(Dori$C_PuertoDesembarque [Dori$LABORATORIO =="IEO" ])
table(Dori$C_PuertoDesembarque [Dori$LABORATORIO =="AZTI" ])
table(Dori$PuertoBase [Dori$LABORATORIO =="AZTI" & Dori$LAB_PuertoDesembarco=="OTH"])
table(Dori$Nombre     [Dori$LABORATORIO =="AZTI" & Dori$LAB_PuertoDesembarco=="OTH"])
                                                                           
table(Dori$PuertoBase [Dori$LABORATORIO =="IEO" & Dori$LAB_PuertoDesembarco=="OTH"])
table(Dori$Nombre     [Dori$LABORATORIO =="IEO" & Dori$LAB_PuertoDesembarco=="OTH"])
table(Dori$PuertoDesembarque [Dori$LABORATORIO =="IEO" & Dori$LAB_PuertoDesembarco=="OTH"])



Dori_ini <- Dori


# Crear tabla importación    #####
################################# #

names(Dori)[names(Dori)=="IdMareaIEO"] <- "C_IDMAREA"
Dori$CALADERO <- NA
Dori$MODALIDAD <- NA
Dori$UG <- NA
Dori$METIER_IEO <- NA
Dori$ESTRATO_RIM <- NA
Dori$METIER_DCF <- NA
names(Dori)[names(Dori)=="C_Area"] <- "C_DIVISION"
Dori$FLEET_SEGMENT <- NA
Dori$FISHING_GROUND <- NA
Dori$PUERTOBASE_AZTI <- "AZTI"
names(Dori)[names(Dori)=="Fuente"] <- "C_FUENTE"
names(Dori)[names(Dori)=="Nombre"] <- "C_NOMBRE_BUQUE"
names(Dori)[names(Dori)=="CodigoCFR"] <- "C_CODIGO_UE"
names(Dori)[names(Dori)=="C_FcRegreso"] <- "C_FECHA_DESEMBARQUE"
names(Dori)[names(Dori)=="C_PuertoDesembarque_AL5"] <- "C_PUERTO_DESEMBARQUE"
names(Dori)[names(Dori)=="C_FcVenta"] <- "C_FECHA_VENTA"
names(Dori)[names(Dori)=="C_PuertoVenta_AL5"] <- "C_PUERTO_VENTA"



names(Dori)[names(Dori)=="IdDori"] <- "IDDORI"                # nuevo

names(Dori)[names(Dori)=="CodigoMarea"] <- "CODIGOMAREA"
names(Dori)[names(Dori)=="IdDiario"] <- "IDDIARIO"
names(Dori)[names(Dori)=="OrigenIdentificador"] <- "HOJA_DIARIO"           # pasa a ser alfanumerico
names(Dori)[names(Dori)=="CatchCategory"] <- "ORIGEN_TABLA"                # nuevo

Dori$ORDENACION <- 1
names(Dori)[names(Dori)=="IdBuque"] <- "CODIGO_BUQUE"
names(Dori)[names(Dori)=="FcSalida"] <- "FECHA_SALIDA"
names(Dori)[names(Dori)=="FcRegreso"] <- "FECHA_REGRESO"
names(Dori)[names(Dori)=="FcCaptura"] <- "FECHA_CAPTURA"
names(Dori)[names(Dori)=="FcDesembarque"] <- "FECHA_DESEMBARQUE"
Dori$FECHA_VENTA <- Dori$C_FECHA_VENTA                                       ## NUEVO
names(Dori)[names(Dori)=="FcDescarte"] <- "FECHA_DESCARTE"                   ## NUEVO
names(Dori)[names(Dori)=="MotivoDescarte"] <- "MOTIVODESCARTE"                   ## NUEVO
names(Dori)[names(Dori)=="PuertoSalida_AL5"] <- "PUERTO_SALIDA"              ## NUEVO
names(Dori)[names(Dori)=="PuertoRegreso_AL5"] <- "PUERTO_REGRESO"            ## NUEVO
names(Dori)[names(Dori)=="PuertoDesembarque_AL5"] <- "PUERTO_DESEMBARQUE"
names(Dori)[names(Dori)=="CodigoArte_FaoAL3"] <- "ARTE"
Dori$DIMENSION1 <- NA
Dori$DIMENSION2 <- NA
Dori$DIMENSION3 <- NA
Dori$MALLA <- NA
Dori$NUM_ARTES_LANZADAS <- NA
names(Dori)[names(Dori)=="FcInicio"] <- "INICIO_OPERACION"
names(Dori)[names(Dori)=="FcFin"] <- "FIN_OPERACION"
names(Dori)[names(Dori)=="TiempoPescaMin"] <- "TIEMPO_PESCA"
names(Dori)[names(Dori)=="NumOperaciones"] <- "NUM_OPERACIONES"
names(Dori)[names(Dori)=="Lat"] <- "LATITUD"
names(Dori)[names(Dori)=="Lon"] <- "LONGITUD"
names(Dori)[names(Dori)=="CodigoDivision"] <- "DIVISION"
names(Dori)[names(Dori)=="RectanguloEstadistico"] <- "R_ESTADISTICO"
Dori$PROFUNDIDAD <- NA
names(Dori)[names(Dori)=="Especie_Sci"] <- "CIENTIFICO"
names(Dori)[names(Dori)=="Especie_AL3"] <- "3A_CODE"
Dori$PESO <- Dori$PesoConsumo
Dori$PESOBAJOTALLA <- Dori$PesoConsumoBajoTalla
Dori$PESODESCARTESR <- Dori$PesoDescarte


sapply(Dori[grepl("Fc", names(Dori))], function(x) sum(is.na(x))) # Priorizamos el Puerto de Desembarque porque hay menos NAs


var_order <- c("IDDORI", "C_IDMAREA", "C_FUENTE", "C_NOMBRE_BUQUE", "C_CODIGO_UE", 
                "C_FECHA_DESEMBARQUE", "C_PUERTO_DESEMBARQUE", "C_FECHA_VENTA", "C_PUERTO_VENTA",
               "LABORATORIO",  "C_DIVISION", "METIER_DCF",
               "FLEET_SEGMENT", "FISHING_GROUND", "CALADERO", "MODALIDAD", "UG", "METIER_IEO", "ESTRATO_RIM",  "PUERTOBASE_AZTI",

               "CODIGOMAREA", "IDDIARIO", "HOJA_DIARIO", "ORIGEN_TABLA" , "ORDENACION", "CODIGO_BUQUE", 
               "FECHA_SALIDA", "FECHA_REGRESO", "FECHA_CAPTURA", "FECHA_DESEMBARQUE", "FECHA_VENTA",  "FECHA_DESCARTE", "MOTIVODESCARTE",
               "PUERTO_SALIDA", "PUERTO_REGRESO", "PUERTO_DESEMBARQUE", 
               "ARTE", "DIMENSION1", "DIMENSION2", "DIMENSION3", "MALLA", 
               "NUM_ARTES_LANZADAS", "INICIO_OPERACION", "FIN_OPERACION", "TIEMPO_PESCA", "NUM_OPERACIONES",
               "LATITUD", "LONGITUD", "DIVISION", "R_ESTADISTICO", "PROFUNDIDAD",
               "CIENTIFICO", "3A_CODE", "PESO", "PESOBAJOTALLA", "PESODESCARTESR",

               "InfoBase", "IdDiario" , "IdMarea", "IdCapturaCalculada", "IdCaptura", "IdCapturaEspecie", "IdDesembarqueEspecie", "IdNotaDeVenta",  "Id", 
               "IdBuque",
               "PesoConsumo", "PesoConsumoBajoTalla", "PesoConsumoTotal", 
               "PesoCapturado", "NumPiezasCapturadas", "PesoCapturadoBajoTalla", "NumPiezasCapturadasBajoTalla", 
               "Presentacion_AL3", "Conservacion_AL3", "FactorConversion", 
               "PesoDesembarcado", "NumPiezasDesembarcadas", "PesoDesembarcadoBajoTalla", "NumPiezasDesembarcadasBajoTalla", 
               "PesoDesembarcadoVivo", "PesoDesembarcadoVivoBajoTalla",
               "PesoNotaVenta", "PesoNotaVentaVivo", "NumPiezasNotaVenta",
               "PesoRetenido", "PesoRetenidoBajoTalla", "PesoTransferido", "NumPiezasTransferidas", "PesoTransferidoBajoTalla", "NumPiezasTransferidasBajoTalla", 
               "PesoDescarte","NumPiezasDescarte",
               "Desembarcado",  "Stock" ,
               "PaisCaptura_AL3", "ProvinciaPuertoBase", "CensoPorModalidad", "EsloraTotal")

var_order[!var_order %in% names(Dori)]

DoriBD <- Dori[,var_order]


sum(Dori$PesoConsumo, na.rm=T)
sum(DoriBD$PesoConsumo, na.rm=T)
sum(DoriBD$PESO, na.rm=T)

# Grabar Tabla

setwd(path.res)
save(DoriBD, file="0_Datos/Infobase/DoriBD_Importar.RData")
write.table(DoriBD, file="0_Datos/Infobase/DoriBD_Importar.csv", sep=";", dec=",", row.names = F)



