# ----------------------------------------------------------------- #
# Readme:                                                           #
# ----------------------------------------------------------------- # 
# En este script: revisamos los datos par establecer criterios de depuración
#
# INDICE:
#
# Functions
# Libraries.
# Cargar la tabla maestra (dori).
# Chequeos
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
  names(CodigoOrigen) [names(CodigoOrigen) == 0] <- "X0"
  return(CodigoOrigen)
}

funDesembarcado <- function(x) { 
  Desembarcado<- x %>% group_by( CensoPorModalidad, Nombre, EsloraTotal, IdMarea, C_FcRegresoFloor , Desembarcado) %>% summarise(PesoTotal = sum(PesoConsumoTotal+ PesoDescarte, na.rm=T)) %>%
    dcast(CensoPorModalidad + Nombre + EsloraTotal + C_FcRegresoFloor + IdMarea ~ Desembarcado,  value.var = "PesoTotal")
  names(Desembarcado) [names(Desembarcado)==0] <- "False"
  names(Desembarcado) [names(Desembarcado)==1] <- "True"
  return(Desembarcado)
}


MareasTotales <- funMareasTotales (Dori)
CodigoOrigen  <- funCodigoOrigen (Dori)
Desembarcado  <- funDesembarcado (Dori)

colSums(MareasTotales[,c("CapturasCalculadas", "Descartes")], na.rm = TRUE)
sum(Dori$PesoConsumoTotal[Dori$CatchCategory=="CapturasCalculadas"], na.rm = TRUE )
sum(Dori$PesoConsumoTotal[Dori$CatchCategory=="CapturasLance0"], na.rm = TRUE )
sum(Dori$PesoDescarte[Dori$CatchCategory=="Descartes"], na.rm = TRUE )

sum(CodigoOrigen[,c("CA", "DE", "NV")], na.rm = TRUE)  

sum(Desembarcado[,c("False", "True")], na.rm = TRUE) 


MareasTotales_ini <- MareasTotales
CodigoOrigen_ini  <- CodigoOrigen
Desembarado_ini   <- Desembarcado



# # ####################################### #
# # Paso 1 - InfoCapturasLance0          ####
# # ####################################### #

# Todas las mareas con Capturalance0 tienen un IdMarea oficial (3518).
subset(MareasTotales, substr(IdMarea,1,3) != "ESP" & !is.na(CapturasLance0)) 
dim(subset(MareasTotales, substr(IdMarea,1,3) == "ESP" & !is.na(CapturasLance0)) )


# mareas con captura lance 0 (3518)
    check <- subset(MareasTotales, !is.na(CapturasLance0) )
    head(check)
    dim(check)
# mareas con captura lance 0 y capturas ==0 (80)
    check <- subset(MareasTotales, !is.na(CapturasLance0) &!is.na(CapturasCalculadas) & CapturasCalculadas==0 )
    head(check)
    dim(check)
# mareas con captura lance 0 y capturas >0 (2045)
    check <- subset(MareasTotales, !is.na(CapturasLance0) &!is.na(CapturasCalculadas) & CapturasCalculadas>0 )
    head(check)
    dim(check)
# mareas con captura lance 0 y capturas NA (1293)
    check <- subset(MareasTotales, !is.na(CapturasLance0) &is.na(CapturasCalculadas) )
    dim(check)
    table(check$CensoPorModalidad)
      head(check[check$CensoPorModalidad=="CERCO EN CANTABRICO NW",])
      head(check[check$CensoPorModalidad=="ARTES MENORES EN CANTABRICO NW",])
      head(check[check$CensoPorModalidad=="PALANGRE DE FONDO EN CANTABRICO NW",])



# Comparativa en N de mareas Total y N de mareas con captura lance 0 (3518)
check <- subset(MareasTotales, !is.na(CapturasLance0) )
res <- as.data.frame(table(MareasTotales$CensoPorModalidad)) %>% 
  left_join(as.data.frame(table(check$CensoPorModalidad)), by="Var1")
names(res) <- c("CensoPorModalidad", "MareasTotales", "MareasLance0")
res$Percent <- percent(res$MareasLance0/res$MareasTotales)
res



# revisamos algunos ejemplos
# cerco
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02674120191026174905") 
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02674120191028124208") 
    subset(MareasTotales, Nombre=="ABRA DE AGUIÑO" & month(C_FcRegresoFloor)%in% c(9,10)) 
    # la primera marea viaja de ESSNI a ESOND. Sin esfuerzo ni capturas
    # @@ eliminar

    subset(Dori[,namevar], IdMarea=="ESP-TRP-02220920190416181256") 
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02220920190416195150") 
    subset(MareasTotales, Nombre=="AGARIMO DOUS" & month(C_FcRegresoFloor)%in% c(4,5)) 
    # la segunda marea viaja de ESEWE a ESOND. Sin esfuerzo ni capturas
    # @@ eliminar

    
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02220920190422072814") 
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02220920190423123050") 
    subset(MareasTotales, Nombre=="AGARIMO DOUS" & month(C_FcRegresoFloor)%in% c(4,5)) 
    # no esta clara la razon
    # @@ eliminar

# artes menores
    subset(Dori[,namevar], IdMarea=="ESP-99007272") 
    subset(Dori[,namevar], IdMarea=="ESP-99007273") 
    subset(Dori[,namevar], IdMarea=="ESP-99007274") 
    subset(Dori[,namevar], IdMarea=="ESP-99007275") 
    subset(MareasTotales, Nombre=="ANTIGUOTARRAK" & month(C_FcRegresoFloor)%in% c(8)) 
    # no esta clara la razon. costera tunidos. esfuerzo busqueda pescado?
    
    subset(Dori[,namevar], IdMarea=="ESP-13702706") 
    subset(Dori[,namevar], IdMarea=="ESP-13702707") 
    subset(Dori[,namevar], IdMarea=="ESP-13702708") 
    subset(Dori[,namevar], IdMarea=="ESP-13702709") 
    subset(MareasTotales, Nombre=="ATXURRA ANAIAK" & month(C_FcRegresoFloor)%in% c(4)) 
    # viaje lekeitio laredo durante la costera?



# CRITERIOS y CORRECCIÓN
######################## #
#   - las marcamos para que no se incorporen a la BD. Las mantenemso en la BD original porque son mareas reales, pero no las contabilizamos
#

Dori_lance0 <- subset(Dori, CatchCategory=="CapturasLance0")
Dori <- subset(Dori, CatchCategory!="CapturasLance0")
sum(Dori$PesoConsumo)
sum(Dori_ini$PesoConsumo)

MareasTotales <- funMareasTotales (Dori)
CodigoOrigen  <- funCodigoOrigen (Dori)
Desembarcado  <- funDesembarcado (Dori)  

 
# # ####################################### #
# # Paso 2 - Mareas Huerfanas            ####
# # ####################################### #


# Mareas huérfanas: aquellas lineas readas a partir de NV que pertenecen a buques con eslora de 10 metros o más pero que no han sido linkadas con un diario de pesca. 
#                   Se crea la línea en la tabla de consumos, pero no se tiene en cuenta el consumo (PesoConsumo = 0)
# Eliminar: Está contabilizada en el DEA, pero no se ha establecido el link correctamente
# 82 mareas

check <- MareasTotales %>% filter (substr(IdMarea,1,3)!="ESP") %>% filter (EsloraTotal>=10)
check
sum(check$CapturasCalculadas)
length(unique(check$IdMarea))

  subset(MareasTotales, Nombre=="KALAMENDI" & month(C_FcRegresoFloor)==1)
  subset(Dori[,namevar], IdMarea=="16288817_2019-01-07") %>% summarise(PesoNotaVenta = sum(PesoNotaVenta * FactorConversion))
  subset(Dori[,namevar], IdMarea=="16288817_2019-01-07")
  
  subset(MareasTotales, Nombre=="JON KURTZIO" & month(C_FcRegresoFloor)%in% c(9,10))
  subset(Dori, IdMarea=="12467136_2019-10-08")
  
# revisamos el origen del resto de mareas menores de 10 m.
# todos los origenes son NV: correcto
  CodigoOrigen %>% filter (substr(IdMarea,1,3)!="ESP") %>% filter (EsloraTotal>=10)  # huerfanas
  CodigoOrigen %>% filter (substr(IdMarea,1,3)=="ESP") %>% filter (EsloraTotal<10)  # menores de 10m.todas las lineas vienen de NV
  CodigoOrigen %>% filter (EsloraTotal<10) %>% filter(is.na(NV))  # 
  

# CRITERIOS y CORRECCIÓN
######################## #
#   - Eslora >10 & Sin Id Marea oficial  & PesoConsumo== 0 -> eliminar
  
check <- MareasTotales %>% filter (substr(IdMarea,1,3)!="ESP") %>% filter (EsloraTotal>=10)
check
sum(check$CapturasCalculadas)
length(unique(check$IdMarea))

Dori <- subset(Dori, !IdMarea %in% check$IdMarea)

    sum(Dori_ini$PesoConsumoTotal, na.rm=TRUE)
    sum(Dori$PesoConsumoTotal, na.rm=TRUE)
    sum(Dori_ini$PesoDescarte, na.rm=TRUE)
    sum(Dori$PesoDescarte, na.rm=TRUE)

MareasTotales <- funMareasTotales (Dori)
CodigoOrigen  <- funCodigoOrigen (Dori)
Desembarcado  <- funDesembarcado (Dori)  


# # ####################################################################################### #
# # Paso 3 - Mareas en las que se ha hecho el link entre Nv y logbooks                  #####
# #          y en las que se asigna Peso Consumo =0 a las líneas que vienen de NV       ### #
# # ####################################################################################### #

head(CodigoOrigen)

# identifico las mareas que tienen PesoConsumo de CA/ DE/ Descartes
check <- CodigoOrigen %>% filter ((!is.na(CA)) | (!is.na(DE) ) | (!is.na(Descartes) )) # mareas que  tienen pesos con origen CA/ DE/ Descartes
dim(check)
head(check)
tail(check)

  check2 <- CodigoOrigen %>% filter ((is.na(CA)) & (is.na(DE) ) & (is.na(Descartes) & !is.na(NV))) # mareas que solo tienen pesos con origen NV
  check2 <- CodigoOrigen %>% filter ((is.na(CA)) & (is.na(DE) ) & (is.na(Descartes) & !is.na(X0))) # mareas que solo tienen pesos con origen 0


# identifico las lineas que en esas mareas tienen un origen difernete a CA y DE. Todas tienen peso =0
temp <- Dori[,namevar] %>% filter(IdMarea %in% check$IdMarea & !CodigoOrigen %in% c("CA", "DE", "Descartes"))
dim(temp)

  sum(temp$PesoConsumo)
  sum(temp$PesoCapturado)
  sum(temp$PesoDesembarcado)
  sum(temp$PesoDescarte)


# reviso algunos ejemplos
head(unique(temp$IdMarea))

i <- "ESP-TRP-00594820190612113122"
i <- "42399_2019-01-02"
i <- "ESP-13702689"

subset(CodigoOrigen, IdMarea==i)
subset(Dori[,namevar], IdMarea==i)
subset(Dori[,namevar], IdMarea==i & IdDori %in% temp$IdDori)
subset(Dori[,namevar], IdMarea==i & !IdDori %in% temp$IdDori)



# CRITERIOS y CORRECCIÓN
######################## #
#   - eliminamos de la BD original. son lineas ficticias readas al combinar diferentes fuentes de información

check <- CodigoOrigen %>% filter ((!is.na(CA)) | (!is.na(DE) ) | (!is.na(Descartes) )) # mareas que  tienen pesos con origen CA/ DE/ Descartes
temp <- Dori[,namevar] %>% filter(IdMarea %in% check$IdMarea & !CodigoOrigen %in% c("CA", "DE", "Descartes"))  # dentro de las enteriores, lineas con origen NV/ 0

  length(unique(Dori$IdMarea))
Dori <- subset(Dori, !IdDori %in% temp$IdDori)
  length(unique(Dori$IdMarea))

  MareasTotales <- funMareasTotales (Dori)
  CodigoOrigen  <- funCodigoOrigen (Dori)
  Desembarcado  <- funDesembarcado (Dori)  
  


# # ######################################################## #
# # Paso 4 - Peso Consumo = 0 en toda la marea           #####
# # ######################################################## #

subset(Dori, PesoRetenido>0 | PesoRetenidoBajoTalla>0)
subset(Dori, PesoTransferido>0 | PesoTransferidoBajoTalla>0)
subset(Dori, PesoDesembarcado>0 | PesoDesembarcadoBajoTalla>0)
subset(Dori, PesoCapturado>0 | PesoCapturadoBajoTalla>0)


# con descarte
################## #
MareasTotales %>% filter(!is.na(Descartes))
MareasTotales %>% filter(CapturasCalculadas==0 & !is.na(Descartes))
  subset(Dori[,namevar], IdMarea=="ESP-TRP-02522920190902154842") 
  subset(Dori[,namevar], IdMarea=="ESP-TRP-02522920190906213918") 
  subset(MareasTotales, Nombre=="BETI PIEDAD" & month(C_FcRegresoFloor)%in% c(8,9,10))
  # 1200 kg de HOM capturados pero no desembarcados.  la marea siguiente(s) se utilizan en como cebo vivo y se declara como descarte (carnada)

  subset(Dori[,namevar], IdMarea=="ESP-TRP-01086320190625225213") 
  subset(Dori[,namevar], IdMarea=="ESP-TRP-01086320190702212727") 
  subset(MareasTotales, Nombre=="BETI SAN LUIS" & month(C_FcRegresoFloor)%in% c(6,7))
  # @@ en este ejemplo no se ve tan claro. 
  
  ## pero cuando hay descarte, mantenemos la marea
  
# sin descarte
################## #
MareasTotales %>% filter(CapturasCalculadas==0 & is.na(Descartes))    
check <-  MareasTotales %>% filter(CapturasCalculadas==0 & is.na(Descartes))   
dim(check)
head(check)

temp <- subset (Dori, IdMarea %in% check$IdMarea )
length(unique(temp$IdMarea))
table(temp$Desembarcado)
table(temp$CodigoOrigen)

    
    # ejemplos
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02306220191118005306") 
    subset(MareasTotales, Nombre=="AKETXE" & month(C_FcRegresoFloor)%in% c(11))
    
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02594520190929231610") 
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02594520190930163323") 
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02594520190930231819") 
    subset(MareasTotales, Nombre=="GURE AMETXA" & month(C_FcRegresoFloor)%in% c(9,10)) 
    # tiene codigo de marea pero peso consumo =0. tiene info de ventas. Parece que ocurre en mareas que se dan el mismo día
    # puede ser que una marea sea para largar el aparejo y otra para virarlo
    # @@ pensamos que en estos casos es mas correcto contabilizar solo una marea en vez de dos
    
    
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02433220190404103821")
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02433220190405053237") 
    subset(MareasTotales, Nombre=="MADRE CONSUELO" & month(C_FcRegresoFloor)%in% c(4))
    # tiene codigo de marea pero peso consumo =0. tiene dos mareas el mismo día
    # la priera marea desembarca en PAS y la segunda en OND. es un barco cantabro. puede que haya parado en OND para vender, o de camino de vuelta a casa, 
    # @@ pensamos que en estos casos es mas correcto es eliminar la marea con PesoConsumo 0
    
    
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02774320190401045446") 
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02774320190402074415") 
    subset(MareasTotales, Nombre=="KANTAURI" & month(C_FcRegresoFloor)%in% c(4)) 
    # la primera marea (peso>0) sale de lardo y desembarca en gijon. La segunda marea vuelve a gijón
    # @@ Pensamos que en estos casos es mas correcto es eliminar la marea con PesoConsumo 0
    
    subset(Dori[,namevar], IdMarea=="ESP-TRP-01086320190828155306") 
    subset(Dori[,namevar], IdMarea=="ESP-TRP-01086320190902222301") 
    subset(MareasTotales, Nombre=="BETI SAN LUIS" & month(C_FcRegresoFloor)%in% c(8,9))
    # la primera marea tiene PesoConsumo = 0, pero Peso Cpturado 700 (JAX). La segunda marea utiliza 400 kg de JAX y las declara como descarte
    # en estos casos sí que habría que contar el esfuerzo de la primera marea
    # cuantos casos como este tenemos? 



# sin descarte pero con peso capturado/desembarcado 
########################################################### #

check <-  MareasTotales %>% filter(CapturasCalculadas==0 & is.na(Descartes))   
temp <- subset (Dori, IdMarea %in% check$IdMarea & (PesoCapturado>0 | PesoDesembarcado>0))
length(unique(temp$IdMarea))
table(temp$Desembarcado, useNA = "ifany")
table(temp$CodigoOrigen, useNA = "ifany")

subset(temp[,namevar], Desembarcado==0)

temp%>% group_by(CensoPorModalidad) %>% summarise(Nmareas = length(unique(IdMarea)))
temp%>% group_by(CensoPorModalidad, Nombre) %>% summarise(Nmareas = length(unique(IdMarea))) %>% data.frame()

unique(temp$IdMarea[temp$CensoPorModalidad=="ARTES MENORES EN CANTABRICO NW"])
unique(temp$IdMarea[temp$CensoPorModalidad=="BACALADEROS"])
unique(temp$IdMarea[temp$CensoPorModalidad=="CERCO EN CANTABRICO NW"])
unique(temp$IdMarea[temp$CensoPorModalidad=="RASCO EN CANTABRICO NW"])

      
      # artes menores
      subset(Dori[,namevar], IdMarea=="ESP-TRP-02498820190429033952") 
      subset(MareasTotales, Nombre=="ESTELA DEL CARMEN" & month(C_FcRegresoFloor)%in% c(4,5))
      
      subset(Dori[,namevar], IdMarea=="ESP-TRP-02498820190815113601") 
      subset(Dori[,namevar], IdMarea=="ESP-TRP-02498820190819085534") 
      subset(Dori[,namevar], IdMarea=="ESP-TRP-02498820190823203604") 
      subset(MareasTotales, Nombre=="ESTELA DEL CARMEN" & month(C_FcRegresoFloor)%in% c(8))
      
      # bacaladeros
      subset(Dori[,namevar], IdMarea=="ESP-TRP-02685920190418084553") 
      subset(MareasTotales, Nombre=="EGUNABAR" )
      
      # cerco
      subset(Dori[,namevar], IdMarea=="ESP-TRP-01086320190731122450")
      subset(Dori[,namevar], IdMarea=="ESP-TRP-01086320190809053233") 
      subset(Dori[,namevar], IdMarea=="ESP-TRP-01086320190810193518") 
      subset(Dori[,namevar], IdMarea=="ESP-TRP-01086320190828155306") 
      subset(Dori[,namevar], IdMarea=="ESP-TRP-01086320190902222301") 
      subset(Dori[,namevar], IdMarea=="ESP-TRP-01086320190912174041") 
      subset(MareasTotales, Nombre=="BETI SAN LUIS" & month(C_FcRegresoFloor)%in% c(8,9))
      
      subset(Dori[,namevar], IdMarea=="ESP-TRP-02556820190726171148") 
      subset(Dori[,namevar], IdMarea=="ESP-TRP-02556820190730185024") 
      subset(MareasTotales, Nombre=="BERRIZ KUKUARRI" & month(C_FcRegresoFloor)%in% c(7,8))
      
      # rasco
      subset(Dori[,namevar], IdMarea=="ESP-TRP-02301520190102062857") 
      subset(Dori[,namevar], IdMarea=="ESP-TRP-02301520190110072502") 
      subset(MareasTotales, Nombre=="CARABA" & month(C_FcRegresoFloor)%in% c(1,2))
      
 # creo que estas habría que mantener
      

# sin descarte y sin peso capturado/desembarcado
########################################################### #
check <-  MareasTotales %>% filter(CapturasCalculadas==0 & is.na(Descartes))   

temp <- subset (Dori, IdMarea %in% check$IdMarea & !(PesoCapturado>0 | PesoDesembarcado>0))
  length(unique(temp$IdMarea))
  table(temp$Desembarcado)
  table(temp$CodigoOrigen)
temp <- subset (Dori, IdMarea %in% check$IdMarea & !(PesoCapturado>0 | PesoDesembarcado>0) & Desembarcado==0)


head(unique(temp$IdMarea))

subset(Dori[,namevar], IdMarea=="ESP-TRP-00796620190813173811") 



# CRITERIOS y CORRECCIÓN
######################## #
#   - si tienen PesoConsumo= 0 pero tienen descarte, mantenemos
#   - si tienen PesoConsumo= 0 pero peso desembarcado/Capturado > 0 & censo= CERCO, mantenemos
#   - el resto, las marcamso para que no se incorporen a la BD. Las mantenemso en la BD original porque son mareas reales, pero no las contabilizamos
#


check <-  MareasTotales %>% filter(CapturasCalculadas==0 & is.na(Descartes))   
temp <- subset (Dori[,namevar], IdMarea %in% check$IdMarea & !(PesoCapturado>0 | PesoDesembarcado>0) & Desembarcado==0)

unique(substr(temp$IdMarea,1,3))  # todas las mareas tienen codigo oficial. las guardamos en una tabla a parte y luego las juntamos (Como InfoCapturaLance0)


  length(unique(Dori$IdMarea))
Dori_captura0 <- subset(Dori, IdDori %in% temp$IdDori)
Dori <- subset(Dori, !IdDori %in% temp$IdDori)
  length(unique(Dori$IdMarea))

  
  MareasTotales <- funMareasTotales (Dori)
  CodigoOrigen  <- funCodigoOrigen (Dori)
  Desembarcado  <- funDesembarcado (Dori)
      
      

# # ######################################################## #
# # Paso 5 - Peso Desembarcado                           #####
# # ######################################################## #
  
  # identificamos mareas con desembarco False y peso consumo >0

  head(Desembarcado)
  
  Desembarcado %>% filter(Descartes==0)
  Desembarcado %>% filter(Descartes>0)
  subset(Dori[, namevar], IdMarea =="ESP-TRP-02306220190418141631")
  
  head(Desembarcado %>% filter(is.na(Descartes)))
  subset(Dori[, namevar], IdMarea =="ESP-TRP-02306220190101230813")
  subset(Dori[, namevar], IdMarea =="ESP-TRP-02306220190211012843")
  subset(Dori[, namevar], IdMarea =="ESP-TRP-02306220190213024800")
  
  head(Desembarcado %>% filter(is.na(Descartes) & True==0))
  subset(Dori[, namevar], IdMarea =="ESP-13703714")
  subset(Dori[, namevar], IdMarea =="ESP-TRP-02498820190429033952")
  subset(Dori[, namevar], IdMarea =="ESP-TRP-02498820191116083738")
  
  
  head(Desembarcado %>% filter(is.na(Descartes) & False==0))
  subset(Dori[, namevar], IdMarea =="ESP-TRP-02458020190908214451")
  subset(Dori[, namevar], IdMarea =="ESP-TRP-02280020190828024021")
  
  
  Desembarcado %>% filter(False>0)

  Desembarcado %>% filter(True==0)
  check <- Desembarcado %>% filter(True==0)
  temp <- subset (Dori[,namevar], IdMarea %in% check$IdMarea & !(PesoCapturado>0 | PesoDesembarcado>0))
  temp
  
  
  # CRITERIOS y CORRECCIÓN
  ######################## #
  #   - mantenemos en la BD original. son lineas reales
  
  

# # ######################################################## #
# # Paso 6 - Lineas con factor de conversion ==0         #####
# # ######################################################## #

  # GUH:    eviscerado y descabezado
  # GUL:    eviscerado con el higado
  # GUT:    eviscerado
  # LVR:    higado
  # OTH:    otros
  # ROE:    hueva
  # ROE-C:  huevas
  # TAL:    cola
  # WHL:    entero
  # WNG:    alas
  
  
  check <- CodigoOrigen %>% filter(EsloraTotal<10)
  dim(check)
  head(check)
  
  #barcos <10m eslora
  Dori %>% filter(EsloraTotal<10 & PesoConsumoTotal ==0) # no hay mareas con peso = 0 entre los barcos con eslora <10m
  
  #barcos >10m eslora
  Dori[, namevar] %>% filter(EsloraTotal >= 10 & FactorConversion ==0 & CodigoOrigen=="NV") # no haya lineas que vienen de NV 
  Dori[, namevar] %>% filter(EsloraTotal >= 10 & FactorConversion ==0 & CodigoOrigen=="CA")  # más de 1400 que vienen de CA
  Dori[, namevar] %>% filter(EsloraTotal >= 10 & FactorConversion ==0 & CodigoOrigen=="DE")  # cero lineas que vienen de DE

  subset(Dori[, namevar], IdMarea =="ESP-TRP-02282920190107101933")
  subset(Dori[, namevar], IdMarea =="ESP-TRP-01086320190709024810")
  
  
  temp <-  Dori[, namevar] %>% filter(EsloraTotal >= 10 & FactorConversion ==0 & CodigoOrigen=="CA")  
  table(temp$Presentacion_AL3)

  
  # CRITERIOS y CORRECCIÓN
  ######################## #
  #   - mantenemos en la BD original. son lineas reales
  
  
  
  
# # ####################################### #
# # base de datos Final                  ####
# # ####################################### #


   Dori$AZTI_BD <- "Incorporar"
   Dori_captura0$AZTI_BD <- "Captura0"
   Dori_lance0$AZTI_BD <- "Lance0"

   Dori_FIN <- rbind(Dori, Dori_captura0, Dori_lance0)

   
   save(Dori_FIN, file="Datos/Dori2019_v3.RData")
   
   
   
   # duda: peso desembarcado, peso capturado: lo mantengo en la BD??
   
   
   
   
   
   
   
   
  