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

 namevar <- c("IdDiario", "Nombre", "EsloraTotal", "CensoPorModalidad", "IdMarea", "FcSalida","C_FcRegresoFloor","C_FcRegreso",  "C_CodigoPuertoDesembarque_AL5", "C_FcVentaMax", "C_CodigoPuertoVenta_AL5",
              "CodigoOrigen", "OrigenIdentificador", "CatchCategory", "Desembarcado",
              "FcCaptura", "Posicion", "CodigoDivision", "RectanguloEstadistico", "CodigoArte_FaoAL3", "TiempoPescaMin", "NumOperaciones",
              "Especie_AL3", "PesoConsumo", "PesoConsumoBajoTalla", "FactorConversion",  "PesoDesembarcado","PesoCapturado", "PesoRetenido" ,"PesoNotaVenta",  "PesoDescarte", "MotivoDescarte")


# # ################## #
# # Listado Mareas     #
# # ################## #

MareasTotales <- Dori %>% group_by( CensoPorModalidad, Nombre, EsloraTotal, IdMarea, C_FcRegresoFloor , CatchCategory) %>% 
                          summarise(PesoConsumo = sum(PesoConsumoTotal),PesoDescarte = sum(PesoDescarte)) 
MareasTotales$PesoConsumo[!is.na(MareasTotales$PesoDescarte) & MareasTotales$PesoDescarte>0 ] <- MareasTotales$PesoDescarte[!is.na(MareasTotales$PesoDescarte) & MareasTotales$PesoDescarte>0] 
MareasTotales <-MareasTotales %>% dcast(CensoPorModalidad + Nombre + EsloraTotal + C_FcRegresoFloor + IdMarea ~ CatchCategory,  value.var = "PesoConsumo")
  
  colSums(MareasTotales[,c("CapturasCalculadas", "CapturasLance0", "Descartes")], na.rm = TRUE)
  sum(Dori$PesoConsumoTotal[Dori$CatchCategory=="CapturasCalculadas"], na.rm = TRUE )
  sum(Dori$PesoConsumoTotal[Dori$CatchCategory=="CapturasLance0"], na.rm = TRUE )
  sum(Dori$PesoDescarte[Dori$CatchCategory=="Descartes"], na.rm = TRUE )
  


# # ################## #
# # InfoCapturasLance0 #
# # ################## #

# Todas las mareas con Capturalance0 tienen un IdMarea oficial. Esto nos genera dudas a la hora de eliminar las mareas. 
# Si tienen codigo es porque el barco ha salido de puerto aunque no haya capturado nada
# y son muchas mareas (3518)
  
  head(  subset(MareasTotales, substr(IdMarea,1,3) != "ESP"))
  subset(MareasTotales, substr(IdMarea,1,3) != "ESP" & ( is.na(CapturasCalculadas) & is.na(Descartes))) 
  subset(MareasTotales, substr(IdMarea,1,3) != "ESP" & ( is.na(CapturasCalculadas) & is.na(Descartes))) %>% summarise(CapturasLance0 = sum(CapturasLance0))
  # Todas las mareas con Capturalance0 tienen un IdMarea oficial.
  
  dim(subset(MareasTotales, substr(IdMarea,1,3) != "ESP" & (CapturasCalculadas == 0))) 
  head(subset(MareasTotales, substr(IdMarea,1,3) != "ESP" & (CapturasCalculadas == 0))) 
  dim(subset(MareasTotales, substr(IdMarea,1,3) == "ESP" & (CapturasCalculadas == 0))) 
  head(subset(MareasTotales, substr(IdMarea,1,3) == "ESP" & (CapturasCalculadas == 0))) 
  # Hay 82 mareas con CapturaCalculada = 0 con un IdMarea oficial, 
  # Hay 148 mareas con CapturaCalculada = 0 sin un IdMarea oficial, 
  
   # mareas con captura lance 0
   check <- subset(MareasTotales, !is.na(CapturasLance0) )
   head(check)
   dim(check)
   # Comparativa en N de mareas
   res <- as.data.frame(table(MareasTotales$CensoPorModalidad)) %>% 
              left_join(as.data.frame(table(check$CensoPorModalidad)), by="Var1")
   names(res) <- c("CensoPorModalidad", "MareasTotales", "MareasLance0")
   res$Percent <- percent(res$MareasLance0/res$MareasTotales)
   
   subset(MareasTotales, CensoPorModalidad=="BACALADEROS")
   subset(Dori[,namevar], IdMarea == "ESP-TRP-01554020190403143914")
   
  

  
# Hay registros en esta tabla con esfuerzo igual a cero (NumOperaciones == 0 & TiempoPescaMin == 0)
# de 3518 idDiario únicos, 2006 tienen esfuerzo = 0

   
  dim(InfoCapturaLance0)
  length(unique(InfoCapturaLance0$IdDiario))
  temp <- subset(Dori, CatchCategory=="CapturasLance0" & (is.na(NumOperaciones) | NumOperaciones == 0) & 
                                                         (is.na(TiempoPescaMin) | TiempoPescaMin == 0))
  unique(temp$NumOperaciones)
  unique(temp$TiempoPescaMin)
  length(unique(temp$IdDiario))


# algunas mareas tienen datso en infoCapturas calculadas
# otras no. en estas (1393 mareas), incluir la inforamción de InfoCapturaLance0 supone crear nuevas mareas
  length(unique(check$IdMarea[check$IdMarea %in% Dori$IdMarea[Dori$CatchCategory=="CapturasCalculadas"] ]))
  length(unique(check$IdMarea[!check$IdMarea %in% Dori$IdMarea[Dori$CatchCategory=="CapturasCalculadas"] ]))  
  
# Revisamos ejemplos
    # tienen datos en CapturaCalculada
    head(check[check$IdMarea %in% Dori$IdMarea[Dori$CatchCategory=="CapturasCalculadas"], ])
    table(check$CensoPorModalidad[check$IdMarea %in% Dori$IdMarea[Dori$CatchCategory=="CapturasCalculadas"] ])
    head(check[check$IdMarea %in% Dori$IdMarea[Dori$CatchCategory=="CapturasCalculadas" & Dori$CensoPorModalidad=="CERCO EN CANTABRICO NW"], ])
    
    subset(Dori[,namevar], IdMarea == "ESP-TRP-02306220190103004849")
    subset(Dori[,namevar], IdMarea == "ESP-TRP-02306220190106230906" )
    subset(Dori[,namevar], IdMarea == "ESP-TRP-02674120190408043837" )
        # lienas de captura lance0 con datos de captura en esa misma marea y esfeurzo >0. Quitamos o contabilizamos el esfeurzo?
    
    # No tienen datos en CapturaCalculada
    head(check[!check$IdMarea %in% Dori$IdMarea[Dori$CatchCategory=="CapturasCalculadas"], ])
    table(check$CensoPorModalidad[!check$IdMarea %in% Dori$IdMarea[Dori$CatchCategory=="CapturasCalculadas"] ])
    head(check[!check$IdMarea %in% Dori$IdMarea[Dori$CatchCategory=="CapturasCalculadas"] & check$CensoPorModalidad=="ARTES MENORES EN CANTABRICO NW", ])
    head(check[!check$IdMarea %in% Dori$IdMarea[Dori$CatchCategory=="CapturasCalculadas"] & check$CensoPorModalidad=="CERCO EN CANTABRICO NW", ])
     
    subset(Dori[,namevar], IdMarea == "ESP-TRP-02306220190218011858")
    subset(MareasTotales, Nombre=="GAZTELUGATXE" & month(C_FcRegresoFloor)==11)
    
    subset(Dori[,namevar], IdMarea == "ESP-99007273")
    subset(Dori[,namevar], IdMarea == "ESP-99007274")
    subset(Dori[,namevar], IdMarea == "ESP-99007275")
    subset(MareasTotales, Nombre=="ANTIGUOTARRAK" & month(C_FcRegresoFloor)==8)
    
    subset(Dori[,namevar], IdMarea == "ESP-13702707")
    subset(Dori[,namevar], IdMarea == "ESP-13702708")
    subset(Dori[,namevar], IdMarea == "ESP-13702709")
    subset(MareasTotales, Nombre=="ATXURRA ANAIAK" & month(C_FcRegresoFloor)==4)
    
    subset(Dori[,namevar], IdMarea == "ESP-TRP-02531520190304172125")
    subset(Dori[,namevar], IdMarea == "ESP-TRP-02531520190305090122")
    subset(MareasTotales, Nombre=="AGUSTIN DEUNA" & month(C_FcRegresoFloor)==3)
    
    

    
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
      # tiene codigo de marea pero peso consumo =0. tiene info de ventas. Parece que ocurre en mareas que se dan el mismo día
      # puede ser que una marea sea para largar el aparejo y otra para virarlo

    
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
subset(Dori[,namevar], IdMarea=="ESP-TRP-01086320190902222301") 
subset(MareasTotales, Nombre=="BETI SAN LUIS" & month(C_FcRegresoFloor)%in% c(8,9))



table(check$CensoPorModalidad)
subset(Dori[,namevar], IdMarea=="ESP-TRP-02498820190429033952") 


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

