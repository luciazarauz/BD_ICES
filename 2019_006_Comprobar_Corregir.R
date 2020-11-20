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


# # ################## #
# # Load               #
# # ################## #

load(file="Datos/Dori2019_v2.Rdata"   )
load(file="Datos/Infobase2019_Unique_20200724.Rdata"   )

Dori_ini <- Dori
namevar <- c("IdDiario", "Nombre", "EsloraTotal", "CensoPorModalidad", "PuertoBase", "IdMarea", 
              "FcSalida","CodigoPuertoSalida_AL5", "C_FcRegresoFloor","C_FcRegreso",  "C_CodigoPuertoDesembarque_AL5", "C_FcVentaMax", "C_CodigoPuertoVenta_AL5",
              "CodigoOrigen", "OrigenIdentificador", "CatchCategory", "Desembarcado",
              "FcCaptura", "Posicion", "CodigoDivision", "RectanguloEstadistico", "CodigoArte_FaoAL3", "TiempoPescaMin", "NumOperaciones",
              "Especie_AL3", "Presentacion_AL3", "PesoConsumo", "PesoConsumoBajoTalla", "FactorConversion",  "PesoDesembarcado","PesoCapturado", "PesoRetenido" ,"PesoNotaVenta",  "PesoDescarte", "MotivoDescarte")


# # ################## #
# # Listado Mareas     #
# # ################## #

MareasTotales <- Dori %>% group_by( CensoPorModalidad, Nombre, EsloraTotal, IdMarea, C_FcRegresoFloor , CatchCategory) %>% 
                          summarise(PesoConsumo = sum(PesoConsumoTotal),PesoDescarte = sum(PesoDescarte)) 
MareasTotales$PesoConsumo[!is.na(MareasTotales$PesoDescarte) & MareasTotales$PesoDescarte>0 ] <- MareasTotales$PesoDescarte[!is.na(MareasTotales$PesoDescarte) & MareasTotales$PesoDescarte>0] 
MareasTotales <-MareasTotales %>% dcast(CensoPorModalidad + Nombre + EsloraTotal + C_FcRegresoFloor + IdMarea ~ CatchCategory,  value.var = "PesoConsumo")
MareasTotales <-MareasTotales %>% arrange(Nombre, C_FcRegresoFloor, CensoPorModalidad, EsloraTotal, IdMarea)

  colSums(MareasTotales[,c("CapturasCalculadas", "CapturasLance0", "Descartes")], na.rm = TRUE)
  sum(Dori$PesoConsumoTotal[Dori$CatchCategory=="CapturasCalculadas"], na.rm = TRUE )
  sum(Dori$PesoConsumoTotal[Dori$CatchCategory=="CapturasLance0"], na.rm = TRUE )
  sum(Dori$PesoDescarte[Dori$CatchCategory=="Descartes"], na.rm = TRUE )
  

  
  # # ################## #
  # 1. Mareas Huérfanas  #
  # # ################## #
  
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
  CodigoOrigen<- Dori %>% group_by( CensoPorModalidad, Nombre, EsloraTotal, IdMarea, C_FcRegresoFloor , CodigoOrigen) %>% summarise(PesoConsumo = sum(PesoConsumoTotal)) %>%
    dcast(CensoPorModalidad + Nombre + EsloraTotal + C_FcRegresoFloor + IdMarea ~ CodigoOrigen,  value.var = "PesoConsumo")
  names(CodigoOrigen) [names(CodigoOrigen) == 0] <- "X0"
  head(CodigoOrigen)
  
  CodigoOrigen %>% filter (substr(IdMarea,1,3)!="ESP") %>% filter (EsloraTotal>=10)  # huerfanas
  
  CodigoOrigen %>% filter (substr(IdMarea,1,3)!="ESP") %>% filter (EsloraTotal<10) %>% filter(!is.na(NV)) # menores de 10m.
  CodigoOrigen %>% filter (substr(IdMarea,1,3)!="ESP") %>% filter (EsloraTotal<10) %>% filter(is.na(NV))  # todas las lineas vienen de NV
  CodigoOrigen  %>% filter (EsloraTotal<10) %>% filter(is.na(NV)) # menores de 10m. todas las lineas vienen de NV
  
  
  # CRITERIOS:
  #   - Eslora >10 & Sin Id Marea oficial  & PesoConsumo== 0 -> eliminar
  
  # CORRECCIÓN:
  #   
      check <- MareasTotales %>% filter (substr(IdMarea,1,3)!="ESP") %>% filter (EsloraTotal>=10)
      check
      sum(check$CapturasCalculadas)
      length(unique(check$IdMarea))
      
      Dori <- subset(Dori, !IdMarea %in% check$IdMarea)
      
      sum(Dori_ini$PesoConsumoTotal, na.rm=TRUE)
      sum(Dori$PesoConsumoTotal, na.rm=TRUE)
      sum(Dori_ini$PesoDescarte, na.rm=TRUE)
      sum(Dori$PesoDescarte, na.rm=TRUE)
  
  
  # # ################## #
  # 2 Peso Desembarcado  #
  # # ################## #
  # identificamos mareas con desembarco False y peso consumo >0
  
  Desembarcado<- Dori %>% group_by( CensoPorModalidad, Nombre, EsloraTotal, IdMarea, C_FcRegresoFloor , Desembarcado) %>% summarise(PesoConsumo = sum(PesoConsumoTotal)) %>%
    dcast(CensoPorModalidad + Nombre + EsloraTotal + C_FcRegresoFloor + IdMarea ~ Desembarcado,  value.var = "PesoConsumo")
  names(Desembarcado) [names(Desembarcado)==0] <- "False"
  names(Desembarcado) [names(Desembarcado)==1] <- "True"
  names(Desembarcado) [names(Desembarcado)=="NA"] <- "XNA"
  
  head(Desembarcado)
  
  Desembarcado %>% filter(XNA==0)
  subset(Dori[,namevar], IdMarea=="ESP-TRP-02306220190103004849") # son líneas que vienen de descarte o capturalance0
  
  Desembarcado %>% filter(False>0)
  subset(Desembarcado, Nombre=="GUK" & month(C_FcRegresoFloor)%in% c(5))
  subset(Dori[namevar], IdMarea=="ESP-13699181") 
  subset(Dori[namevar], IdMarea=="ESP-TRP-02511520191127164649") 
  
  Desembarcado %>% filter(False==0)
  subset(Dori[namevar], IdMarea=="ESP-TRP-00594820190519224932") 
  
  Desembarcado %>% filter(False == 0 & True ==0)
  subset(Dori[namevar], IdMarea=="ESP-TRP-02369020190604110108") 
  
  a <- subset(Dori[namevar], Desembarcado== 0 & PesoCapturado>0)
  subset(Dori[namevar], Desembarcado== 0 & PesoDesembarcado>0)
  
  table(a$CensoPorModalidad)  ## revisar criterio
  
  # CRITERIOS:
  #   - Desembarco False y PesoConsumo >0 -> mantener
  #   - Desembarco False y PesoConsumo==0 -> eliminar
  
  # CORRECCIÓN:
  #   de momento no corrijo nada. podemos volver al final para ver cómo ha quedado
  
  
  
  # # ############################################################################ 
  # 3 Mareas en las que se ha hecho el link entre Nv y logbooks                  #
  # # y en las que se asigna Peso Consumo =0 a las líneas que vienen de NV       #
  # # ############################################################################ 
  
  head(CodigoOrigen)
  
  # identifico mareas que tienen peso en origen Capturas o de Desembarques
  CodigoOrigen %>% filter (X0>0) 
  check <- CodigoOrigen %>% filter (CA>0 | DE>0) 
  check
  
  # identifico las lineas qe en esas mareas tienen un origen difernete (NV). Todas tienen peso =0
  temp <- Dori %>% filter(IdMarea %in% check$IdMarea & !CodigoOrigen %in% c("CA", "DE")) %>% filter(PesoConsumoTotal ==0)
  dim(temp)
  head(temp)
  temp <- Dori %>% filter(IdMarea %in% check$IdMarea & !CodigoOrigen %in% c("CA", "DE"))
  dim(temp)
  temp <- temp %>% filter(IdMarea %in% check$IdMarea) %>% filter(PesoConsumoTotal ==0)
  dim(temp)
  
  # reviso algunos ejemplos
  head(unique(temp$IdMarea))
  subset(Dori[,namevar], IdMarea=="ESP-TRP-00594820190612113122")
  subset(Dori[,namevar], IdMarea=="ESP-TRP-00594820190519224932") 
  
  # CRITERIOS:
  #   - eliminamos de la BD original. son lineas ficticias readas al combinar diferentes fuentes de información
  
  # CORRECCIÓN:
  #   d
  
  
  
  # # ################################################################# 
  # 4 Lineas con factor de conversion ==0                            #
  # # ################################################################# 
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
  Dori %>% filter(IdMarea %in% check$IdMarea) %>% filter(PesoConsumoTotal ==0) # no hay mareas con peso = 0 entre los barcos con eslora <10m
  
  #barcos >10m eslora
  Dori %>% filter(!IdMarea %in% check$IdMarea) %>% filter(FactorConversion ==0 & CodigoOrigen=="NV") # solo hay tres lineas que vienen de NV 
  Dori %>% filter(!IdMarea %in% check$IdMarea) %>% filter(FactorConversion ==0 & CodigoOrigen=="CA")  # más de 1400 que vienen de CA
  Dori %>% filter(!IdMarea %in% check$IdMarea) %>% filter(FactorConversion ==0 & CodigoOrigen=="DE")  # cero lineas que vienen de DE
  Dori %>% filter(!IdMarea %in% check$IdMarea) %>% filter(FactorConversion ==0 & CodigoOrigen==0)     # más de 500 que vienen de NV, pero no se tienen en cuenta en el consumo
  
  temp <- Dori %>% filter(!IdMarea %in% check$IdMarea) %>% filter(FactorConversion ==0 & CodigoOrigen=="NV")
  head(temp[, namevar])
  table(temp$Presentacion_AL3)
  subset(Dori[, namevar], IdMarea=="41963_2019-03-29" & CodigoOrigen=="NV") 
  
  
  temp <- Dori %>% filter(!IdMarea %in% check$IdMarea) %>% filter(FactorConversion ==0 & CodigoOrigen=="CA")
  head(temp[, namevar])
  table(temp$Presentacion_AL3)
  subset(Dori[, namevar], IdMarea=="ESP-TRP-00871520190218191824" & CodigoOrigen=="CA") 
  subset(Dori[, namevar], IdMarea=="ESP-TRP-01086320190709024810" & CodigoOrigen=="CA") 
  
  
  
  # CRITERIOS:
  #   - mantenemos en la BD original. son lineas reales
  
  
  
  
  
# # ################## #
# 5 InfoCapturasLance0 #
# # ################## #

# Todas las mareas con Capturalance0 tienen un IdMarea oficial. 
# Si tienen codigo es porque el barco ha salido de puerto aunque no haya capturado nada. 
# Intentamso entender el por qué
# son muchas mareas (3518)
  
  # Todas las mareas con Capturalance0 tienen un IdMarea oficial (3518).
  subset(MareasTotales, substr(IdMarea,1,3) != "ESP" & ( is.na(CapturasCalculadas) & is.na(Descartes))) 
  subset(MareasTotales, substr(IdMarea,1,3) != "ESP" & ( is.na(CapturasCalculadas) & is.na(Descartes))) %>% summarise(CapturasLance0 = sum(CapturasLance0))

  # Hay 82 mareas con CapturaCalculada = 0 con un IdMarea oficial, 
  # Hay 148 mareas con CapturaCalculada = 0 sin un IdMarea oficial  
  dim(subset(MareasTotales, substr(IdMarea,1,3) != "ESP" & (CapturasCalculadas == 0))) 
  head(subset(MareasTotales, substr(IdMarea,1,3) != "ESP" & (CapturasCalculadas == 0))) 
  dim(subset(MareasTotales, substr(IdMarea,1,3) == "ESP" & (CapturasCalculadas == 0))) 
  head(subset(MareasTotales, substr(IdMarea,1,3) == "ESP" & (CapturasCalculadas == 0))) 
 
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
   head(check[check$CensoPorModalidad=="CERCO EN CANTABRICO NW",])
   head(check[check$CensoPorModalidad=="ARTES MENORES EN CANTABRICO NW",])
   head(check[check$CensoPorModalidad=="PALANGRE DE FONDO EN CANTABRICO NW",])
   table(check$CensoPorModalidad)
   dim(check)
   
   1293+2045+80
   
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
   
   
   13702710
   
   
   # CRITERIOS:
   #   - las marcamos para que no se incorporen a la BD. Las mantenemso en la BD original porque son mareas reales, pero no las contabilizamos
   #
   

# # ################################### 
# 6 Peso Consumo =0 en toda la marea  #
# # ################################### 

MareasTotales %>% filter(CapturasCalculadas==0)
MareasTotales %>% filter(CapturasCalculadas==0 & !is.na(Descartes))
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02522920190902154842") 
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02522920190906213918") 
    subset(MareasTotales, Nombre=="BETI PIEDAD" & month(C_FcRegresoFloor)%in% c(8,9,10))
    # 1200 kg de HOM capturados pero no desembarcados.  la marea siguiente(s) se utilizan en como cebo vivo y se declara como descarte (carnada)

    subset(Dori[,namevar], IdMarea=="ESP-TRP-01086320190625225213") 
    subset(Dori[,namevar], IdMarea=="ESP-TRP-01086320190702212727") 
    subset(MareasTotales, Nombre=="BETI SAN LUIS" & month(C_FcRegresoFloor)%in% c(6,7))
    # @@ en este ejemplo no se ve tan claro. pero cuando hay descarte, matenemos la marea
    
    
    
MareasTotales %>% filter(CapturasCalculadas==0 & is.na(Descartes))    
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02306220191118005306") 
    subset(MareasTotales, Nombre=="AKETXE" & month(C_FcRegresoFloor)%in% c(11))
    
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02594520190930163323") 
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02594520190926000705") 
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02594520190926061537") 
    subset(MareasTotales, Nombre=="GURE AMETXA" & month(C_FcRegresoFloor)%in% c(9)) 
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
    # cuantos casos como este tenemos? (21)
      check <-  MareasTotales %>% filter(CapturasCalculadas==0 )    
      temp <- subset (Dori, IdMarea %in% check$IdMarea & (PesoCapturado>0 | PesoDesembarcado>0))
      check_PS<- subset(check, IdMarea %in% temp$IdMarea & CensoPorModalidad=="CERCO EN CANTABRICO NW")
      table(month(check_PS$C_FcRegresoFloor))
      dim(check_PS)




# CRITERIOS:
#   - si tienen PesoConsumo= 0 pero tienen descarte, mantenemos
#   - si tienen PesoConsumo= 0 pero peso desembarcado/Capturado > 0 & censo= CERCO, mantenemos
#   - el resto, las marcamso para que no se incorporen a la BD. Las mantenemso en la BD original porque son mareas reales, pero no las contabilizamos
#



# # ###############################
# # Correciones                   #
# # ###############################




# # #####################################
# # Comparamos con NV                   #
# # #####################################

# pendiente

