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

Fun_unique <- function(x){length(unique(x))}


# # ################## #
# # Load               #
# # ################## #

load(file="Datos/Dori2019_v2.Rdata"   )
load(file="Datos/Infobase2019_Unique_20200724.Rdata"   )

 namevar <- c("IdDiario", "Nombre", "EsloraTotal", "CensoPorModalidad", "IdMarea", "FcSalida","C_FcRegresoFloor","C_FcRegreso",  "C_CodigoPuertoDesembarque_AL5", "C_FcVentaMax", "C_CodigoPuertoVenta_AL5",
              "CodigoOrigen", "OrigenIdentificador", "CatchCategory", "Desembarcado",
              "FcCaptura", "Posicion", "CodigoDivision", "RectanguloEstadistico", "CodigoArte_FaoAL3", "TiempoPescaMin", "NumOperaciones",
              "Especie_AL3", "PesoConsumo", "PesoConsumoBajoTalla", "FactorConversion",  "PesoDesembarcado","PesoCapturado", "PesoRetenido" ,"PesoNotaVenta",  "PesoDescarte", "MotivoDescarte")


# # ################## #
# # Listado Mareas     #
# # ################## #

MareasTotales <- Dori %>% group_by( CensoPorModalidad, Nombre, EsloraTotal, IdMarea, C_FcRegresoFloor , CatchCategory) %>% summarise(PesoConsumo = sum(PesoConsumoTotal)) %>%
  dcast(CensoPorModalidad + Nombre + EsloraTotal + C_FcRegresoFloor + IdMarea ~ CatchCategory,  value.var = "PesoConsumo")
MareasTotales <- MareasTotales %>% arrange(CensoPorModalidad, Nombre, EsloraTotal, C_FcRegresoFloor, IdMarea)
  head(MareasTotales)

  
# # ################## #
# # InfoCapturasLance0 #
# # ################## #

# Hay registros en esta tabla con esfuerzo igual a cero (NumOperaciones == 0 & TiempoPescaMin == 0)
# de 3934 idDiario únicos, 2391 tienen esfuerzo = 0
# @@ preguntar a SGP

dim(InfoCapturaLance0)
length(unique(InfoCapturaLance0$IdDiario))
temp <- subset(InfoCapturaLance0, (is.na(NumOperaciones) | NumOperaciones == 0) & 
                                  (is.na(TiempoPescaMin) | TiempoPescaMin == 0))
dim(temp)
unique(temp$NumOperaciones)
unique(temp$TiempoPescaMin)
length(unique(temp$IdDiario))

# los registros con esfuerzo = 0, tienen datos en infoCapturasCalculadas?
# 1179 idDiarios, sí lo tienen
# 
length(unique(temp$IdDiario[temp$IdDiario %in% Dori$IdDiario[Dori$CatchCategory=="CapturasCalculadas"] ]))
head(unique(temp$IdDiario[temp$IdDiario %in% Dori$IdDiario[Dori$CatchCategory=="CapturasCalculadas"] ]))

    subset(InfoCapturaLance0, IdDiario == 436392)
    subset(InfoCapturasCalculadas, IdDiario == 436392)
    subset(Dori[,namevar], IdDiario == 436392)
    subset(InfoCapturaLance0, IdDiario == 439559 )
    subset(InfoCapturasCalculadas, IdDiario == 439559 )
    subset(Dori[,namevar], IdDiario == 439559 )


# los registros con esfuerzo > 0, tienen datos en infoCapturasCalculadas?
# 1811 idDiarios, sí lo tienen

temp <- subset(InfoCapturaLance0, (!is.na(NumOperaciones) & NumOperaciones > 0) | (!is.na(TiempoPescaMin) & TiempoPescaMin > 0))
  unique(temp$NumOperaciones)
  unique(temp$TiempoPescaMin)
  unique(temp$TiempoPescaMin[temp$NumOperaciones==0])
  length(unique(temp$IdDiario))

length(unique(temp$IdDiario[temp$IdDiario %in% Dori$IdDiario[Dori$CatchCategory=="CapturasCalculadas"] ]))
head(unique(temp$IdDiario[temp$IdDiario %in% Dori$IdDiario[Dori$CatchCategory=="CapturasCalculadas"] ]))

  subset(InfoCapturaLance0, IdDiario == 436781)
  subset(InfoCapturasCalculadas, IdDiario == 436781)
  subset(Dori, IdDiario == 436781)
  subset(InfoCapturaLance0, IdDiario == 436414 )
  subset(InfoCapturasCalculadas, IdDiario == 436414 )
  subset(InfoCapturas, IdDiario == 436414 )
  subset(Dori, IdDiario == 436414 )


temp$IdMarea <- Dori$IdMarea[match(temp$IdDiario, Dori$IdDiario)]
check <- MareasTotales %>% filter(IdMarea %in% temp$IdMarea) 
check

head(subset(check, is.na(CapturasCalculadas) & is.na(Descartes)))
dim(subset(check, is.na(CapturasCalculadas) & is.na(Descartes)))
table(check$CensoPorModalidad[is.na(check$CapturasCalculadas) & is.na(check$Descartes)])

# revisamos mareas que solo tienen información de captura lance cero (con esfuerzo > 0)

subset(Dori[,namevar], IdMarea  == "ESP-11954963" )
subset(Dori, IdMarea  == "ESP-TRP-02135320191007200003" )

subset(Dori, IdMarea  == "ESP-TRP-02306120191122172744" )
subset(MareasTotales, Nombre=="GAZTELUGATXE" & month(C_FcRegresoFloor)==11)

subset(Dori, IdMarea  == "ESP-TRP-02306220190218011858" )
subset(MareasTotales, Nombre=="AKETXE" & month(C_FcRegresoFloor)==2)
subset(Dori, IdMarea  == "ESP-TRP-02306220190216022826" )


subset(Dori, substr(IdMarea,1,3)  == "ESP" )
a <- (subset(MareasTotales, substr(IdMarea,1,3)  == "ESP" & ( is.na(CapturasCalculadas) & is.na(Descartes))))
subset(MareasTotales, substr(IdMarea,1,3)  != "ESP" & ( is.na(CapturasCalculadas) & is.na(Descartes)))

# tienen idmarea y esfuerzo = 0
temp <- subset(Dori, IdMarea %in% a$IdMarea & (is.na(NumOperaciones) | NumOperaciones == 0) & (is.na(TiempoPescaMin) | TiempoPescaMin == 0) )
head(temp[,namevar])
dim(temp)
length(temp$IdMarea)
table(temp$CensoPorModalidad)

# tienen idmarea y esfuerzo > 0
temp <- subset(Dori, IdMarea %in% a$IdMarea & ((!is.na(NumOperaciones) & NumOperaciones > 0) | (!is.na(TiempoPescaMin) & TiempoPescaMin > 0)) )
head(temp[,namevar])
dim(temp)
length(temp$IdMarea)
table(temp$CensoPorModalidad)


table(a$CensoPorModalidad)
table(MareasTotales$CensoPorModalidad)

# CRITERIOS:
#   - lineas en Capturalance0 sin esfuerzo ->
#   - lineas en Capturalance0 con esfuerzo -> 
# @@Pendiente de aclarar con SGP.
# todos tiene nun codigo de marea

# Dori <- subset(Dori, !(CatchCategory =="CapturasLance0" & (is.na(NumOperaciones) | NumOperaciones == 0) & 
#                  (is.na(TiempoPescaMin) | TiempoPescaMin == 0)))


# # ################## #
# # Mareas Huerfanas   #
# # ################## #

unique(substr(MareasTotales$IdMarea,1,3))

check <- MareasTotales %>% filter (substr(IdMarea,1,3)!="ESP") %>% filter (EsloraTotal>=10)
check

subset(MareasTotales, Nombre=="KALAMENDI" & month(C_FcRegresoFloor)==1)
subset(Dori[,namevar], IdMarea=="16288817_2019-01-07")

subset(MareasTotales, Nombre=="JON KURTZIO" & month(C_FcRegresoFloor)%in% c(9,10))
subset(Dori, IdMarea=="12467136_2019-10-08")


unique(Dori$CodigoOrigen[Dori$IdMarea %in% check$IdMarea])  # vienen todas de notas de venta. (Huerfanas)


# CRITERIOS:
#   - Mareas huerfanas (eslora >10 m y creadas a partir de NV) -> las quitamos
# @@Pendiente de aclarar con SGP.

Dori <- subset(Dori, !IdMarea %in% check$IdMarea)



# revisamos origen del resto de mareas menores de 10 m.
# todos los origenes son NV: correcto

CodigoOrigen<- Dori %>% group_by( CensoPorModalidad, Nombre, EsloraTotal, IdMarea, C_FcRegresoFloor , CodigoOrigen) %>% summarise(PesoConsumo = sum(PesoConsumoTotal)) %>%
  dcast(CensoPorModalidad + Nombre + EsloraTotal + C_FcRegresoFloor + IdMarea ~ CodigoOrigen,  value.var = "PesoConsumo")
names(CodigoOrigen) [names(CodigoOrigen) == 0] <- "X0"
head(CodigoOrigen)

CodigoOrigen %>% filter (substr(IdMarea,1,3)!="ESP") %>% filter (EsloraTotal>=10)  # huerfanas

CodigoOrigen %>% filter (substr(IdMarea,1,3)!="ESP") %>% filter (EsloraTotal<10) %>% filter(!is.na(NV)) # menores de 10m.
CodigoOrigen %>% filter (substr(IdMarea,1,3)!="ESP") %>% filter (EsloraTotal<10) %>% filter(is.na(NV))  # todas las lineas vienen de NV

CodigoOrigen  %>% filter (EsloraTotal<10) %>% filter(is.na(NV)) # menores de 10m. todas las lineas vienen de NV



# # ################## #
# # Peso Desembarcado  #
# # ################## #
# identificamos mareas con desembarco False y peso consumo >0

Desembarcado<- Dori %>% group_by( CensoPorModalidad, Nombre, EsloraTotal, IdMarea, C_FcRegresoFloor , Desembarcado) %>% summarise(PesoConsumo = sum(PesoConsumoTotal)) %>%
  dcast(CensoPorModalidad + Nombre + EsloraTotal + C_FcRegresoFloor + IdMarea ~ Desembarcado,  value.var = "PesoConsumo")
names(Desembarcado) [names(Desembarcado)==0] <- "False"
names(Desembarcado) [names(Desembarcado)==1] <- "True"
names(Desembarcado) [names(Desembarcado)=="NA"] <- "XNA"

head(Desembarcado)

Desembarcado %>% filter(XNA==0)
subset(Dori[,namevar], IdMarea=="ESP-TRP-02306220190103004849") # vienen de descarte o capturalance0

Desembarcado %>% filter(False>0)
subset(Desembarcado, Nombre=="GUK" & month(C_FcRegresoFloor)%in% c(5))
subset(Dori[namevar], IdMarea=="ESP-13699181") 
subset(Dori[namevar], IdMarea=="ESP-TRP-02511520191127164649") 

Desembarcado %>% filter(False==0)

subset(Dori[namevar], IdMarea=="ESP-TRP-00594820190519224932") 


# CRITERIOS:
#   - Desembarco False y PesoConsumo>0 -> mantener
#



# # ################################### 
# # Peso Consumo =0 en toda la marea  #
# # ################################### 

MareasTotales <- subset(MareasTotales, IdMarea %in% unique(Dori$IdMarea))

MareasTotales %>% filter(CapturasCalculadas==0)
MareasTotales %>% filter(CapturasCalculadas==0 & !is.na(Descartes))
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02522920190902154842") 
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02522920190906213918") 
    subset(MareasTotales, Nombre=="BETI PIEDAD" & month(C_FcRegresoFloor)%in% c(8,9,10))
    # 1200 kg de HOM capturados pero no desembarcados.  la marea siguiente(s) se utilizan en como cebo vivo y se declara como descarte (carnada)

MareasTotales %>% filter(CapturasCalculadas==0 & is.na(Descartes))    
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02306220191118005306") 
    subset(MareasTotales, Nombre=="AKETXE" & month(C_FcRegresoFloor)%in% c(11))
    
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02594520190930163323") 
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02594520190926000705") 
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02594520190926061537") 
    subset(MareasTotales, Nombre=="GURE AMETXA" & month(C_FcRegresoFloor)%in% c(9)) 
      # caso curioso. tiene codigo de marea pero peso consumo =0. tiene info de ventas
      # parece que ocurre en mareas que se dan el mismo día

    
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02433220190404103821")
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02433220190405053237") 
    subset(MareasTotales, Nombre=="MADRE CONSUELO" & month(C_FcRegresoFloor)%in% c(4))
    # caso parecido en cerco
    
    
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02774320190401045446") 
    subset(Dori[,namevar], IdMarea=="ESP-TRP-02774320190402074415") 
    subset(MareasTotales, Nombre=="KANTAURI" & month(C_FcRegresoFloor)%in% c(4)) 
    
    
check <-  MareasTotales %>% filter(CapturasCalculadas==0 )    
dim(check)
table(month(check$C_FcRegresoFloor))

a <- subset (Dori, IdMarea %in% check$IdMarea & (PesoCapturado>0 | PesoDesembarcado>0))
length(unique(a$IdMarea))
aa<- subset(check, IdMarea %in% a$IdMarea & CensoPorModalidad=="CERCO EN CANTABRICO NW")
table(month(aa$C_FcRegresoFloor))

subset(Dori[,namevar], IdMarea=="ESP-TRP-01086320190828155306") 
subset(MareasTotales, Nombre=="KANTAURI" & month(C_FcRegresoFloor)%in% c(4)) 



table(check$CensoPorModalidad)
subset(Dori, IdMarea=="ESP-TRP-02498820190429033952") 
subset(MareasTotales, Nombre=="BETI SAN LUIS" & month(C_FcRegresoFloor)%in% c(8,9))


MareasTotales %>% filter(CapturasCalculadas==0) %>% filter(substr(IdMarea,1,3)!="ESP")
MareasTotales %>% filter(EsloraTotal==10) %>% filter(substr(IdMarea,1,3)!="ESP")


# CRITERIOS:
#   - tienen codigo de marea y pesoconsumo ==0
#  @@ aclarar con SGP, n oentiendo qué hace aquí el algoritmo
#



# # ############################################################################ 
# # Mareas en las que se ha hecho el link entre Nv y logbooks                  #
# # y en las que se asigna Peso Consumo =0 a las líneas que vienen de NV       #
# # ############################################################################ 

head(CodigoOrigen)

CodigoOrigen %>% filter (X0>0) 
check <- CodigoOrigen %>% filter (CA>0 | DE>0) 
check

lineId <- Dori %>% filter(IdMarea %in% check$IdMarea) %>% filter(PesoConsumoTotal ==0)

lineId <- Dori %>% filter(IdMarea %in% check$IdMarea & !CodigoOrigen %in% c("CA", "DE")) %>% filter(PesoConsumoTotal ==0)
head(unique(lineId$IdMarea))
head(subset(Dori[,namevar], IdMarea=="ESP-TRP-00594820190612113122")) 

subset(Dori[,namevar], IdMarea=="ESP-TRP-00594820190519224932") 




lineId <- Dori %>% filter(IdMarea %in% check$IdMarea & CodigoOrigen %in% c("CA", "DE")) %>% filter(PesoConsumoTotal ==0)

table(id$FactorConversion)




# # ############################################################################ 
# # Mareas de barcos menores de 10m de eslora (vienen de NV)                   #
# # con peso consumo = 0 y factor de conversion >0                             #
# # ############################################################################ 
# no tienen. todos los factores de conversion = 0 vienen de origen CA y DE. no es raro esto?  @@ consultar?

check <- CodigoOrigen %>% filter(EsloraTotal<10)
check

check %>% filter(IdMarea %in% check$IdMarea) %>% filter(NV ==0)
Dori %>% filter(IdMarea %in% check$IdMarea) %>% filter(PesoConsumo ==0)
Dori %>% filter(!IdMarea %in% check$IdMarea) %>% filter(FactorConversion ==0 & CodigoOrigen=="NV")
Dori %>% filter(!IdMarea %in% check$IdMarea) %>% filter(FactorConversion ==0 & CodigoOrigen==0)

subset(Dori, IdMarea=="ESP-TRP-00796620190704064751") 


head(InfoVentas)
table(InfoVentas$Nombre_Presentacion)

a <- InfoVentas %>% filter(Nombre_Presentacion %in% c("Higado",  "Huevas", "Hevas (colectivo)" ))  
Dori %>% filter(IdMarea %in% a$IdMarea) 



# # #####################################
# # Comparamos con NV                   #
# # #####################################

# pendiente

