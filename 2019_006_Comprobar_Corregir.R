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



namevar <- c("IdDiario", "Nombre", "EsloraTotal", "CensoPorModalidad", "IdMarea", "C_FcRegreso", "C_PuertoDesembarque_AL5", "C_FcVenta", "C_CodigoPuertoVenta_AL5",
             "CodigoOrigen", "NCodigoOrigen", "OrigenIdentificador", "CatchCategory", 
             "FcCaptura", "Posicion", "CodigoDivision", "RectanguloEstadistico", "CodigoArte_FaoAL3", "TiempoPescaMin", "NumOperaciones",
             "Especie_AL3", "PesoConsumo", "PesoConsumoBajoTalla", "PesoDescarte", "PesoNotaVenta" )


# # ################## #
# # Listado Mareas     #
# # ################## #

MareasTotales <- Dori %>% group_by( CensoPorModalidad, Nombre, EsloraTotal, IdMarea, C_FcRegreso , CatchCategory) %>% summarise(PesoConsumo = sum(PesoConsumo)) %>%
  dcast(CensoPorModalidad + Nombre + EsloraTotal + C_FcRegreso + IdMarea ~ CatchCategory,  value.var = "PesoConsumo")
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
    subset(Dori, IdDiario == 436392)
    subset(InfoCapturaLance0, IdDiario == 439559 )
    subset(InfoCapturasCalculadas, IdDiario == 439559 )
    subset(Dori, IdDiario == 439559 )


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

subset(Dori, IdMarea  == "ESP-11954963" )
subset(Dori, IdMarea  == "ESP-TRP-02135320191007200003" )

subset(Dori, IdMarea  == "ESP-TRP-02306120191122172744" )
subset(MareasTotales, Nombre=="GAZTELUGATXE" & month(C_FcRegreso)==11)

subset(Dori, IdMarea  == "ESP-TRP-02306220190218011858" )
subset(MareasTotales, Nombre=="AKETXE" & month(C_FcRegreso)==2)
subset(Dori, IdMarea  == "ESP-TRP-02306220190216022826" )



# CRITERIOS:
#   - lineas en Capturalance0 sin esfuerzo ->
#   - lineas en Capturalance0 con esfuerzo -> 
# @@Pendiente de aclarar con SGP.




# # ################## #
# # Mareas Huerfanas   #
# # ################## #

unique(substr(MareasTotales$IdMarea,1,3))

check <- MareasTotales %>% filter (substr(IdMarea,1,3)!="ESP") %>% filter (EsloraTotal>10)
check

subset(MareasTotales, Nombre=="KALAMENDI" & month(C_FcRegreso)==1)
subset(Dori, IdMarea=="16288817_2019-01-07")

subset(MareasTotales, Nombre=="JON KURTZIO" & month(C_FcRegreso)%in% c(9,10))
subset(Dori, IdMarea=="12467136_2019-10-08")


unique(Dori$CodigoOrigen[Dori$IdMarea %in% check$IdMarea])  # vienen todas de notas de venta. (Huerfanas)


# CRITERIOS:
#   - Mareas huerfanas (eslora >10 m y creadas a partir de NV) ->
# @@Pendiente de aclarar con SGP.


# revisamos origen del resto de mareas menores de 10 m.
# todos los origenes son NV: correcto

CodigoOrigen<- Dori %>% group_by( CensoPorModalidad, Nombre, EsloraTotal, IdMarea, C_FcRegreso , CodigoOrigen) %>% summarise(PesoConsumo = sum(PesoConsumo)) %>%
  dcast(CensoPorModalidad + Nombre + EsloraTotal + C_FcRegreso + IdMarea ~ CodigoOrigen,  value.var = "PesoConsumo")
head(CodigoOrigen)

CodigoOrigen %>% filter (substr(IdMarea,1,3)!="ESP") %>% filter (EsloraTotal>10)  # huerfanas

CodigoOrigen %>% filter (substr(IdMarea,1,3)!="ESP") %>% filter (EsloraTotal<10) %>% filter(!is.na(NV)) # menores de 10m.
CodigoOrigen %>% filter (substr(IdMarea,1,3)!="ESP") %>% filter (EsloraTotal<10) %>% filter(is.na(NV))  # todas las lineas vienen de NV

CodigoOrigen  %>% filter (EsloraTotal<10) %>% filter(is.na(NV)) # menores de 10m. todas las lineas vienen de NV



# # ################## #
# # Peso Desembarcado  #
# # ################## #

Desembarcado<- Dori %>% group_by( CensoPorModalidad, Nombre, EsloraTotal, IdMarea, C_FcRegreso , Desembarcado) %>% summarise(PesoConsumo = sum(PesoConsumo)) %>%
  dcast(CensoPorModalidad + Nombre + EsloraTotal + C_FcRegreso + IdMarea ~ Desembarcado,  value.var = "PesoConsumo")
names(Desembarcado) <- make.names(Desembarcado)
head(Desembarcado)

Desembarcado %>% filter()
# 1_Quitamos las mareas creadas a partir de NV, y que en la misma fecha simple tienen ya una marea que viene de logbooks  ##
####################################################################


Dori %>% group_by(Nombre, C_FcRegreso) %>% summarize(n=Fun_unique(IdMarea )) %>% filter(n > 1)

test <- Dori %>% group_by(Nombre, C_FcRegreso, IdMarea, IdMareaIEO, CodigoOrigen, CatchCategory ) %>% summarize(Peso=sum(PesoConsumo ))
test$C_FcRegresoSimple <- format(test$C_FcRegreso, "%m/%d/%Y")

# look up table para marea, barco y fecha simple
lookup <- unique(test[ ,c("Nombre", "C_FcRegresoSimple", "IdMarea", "IdMareaIEO") ])

# preparamso tabla para identificar mareas srepetidas
test_wide <- test %>% filter(CatchCategory=="CapturasCalculadas") %>% group_by(Nombre, C_FcRegresoSimple, CodigoOrigen) %>% summarize(Peso=sum(Peso )) %>% dcast( Nombre + C_FcRegresoSimple ~ CodigoOrigen, value.var = "Peso")
test_wide$n <- (apply(subset(test_wide, select=-c(Nombre, C_FcRegresoSimple)), 1, function(x) length(x[!is.na(x)])))

check <- subset(test_wide, n==2 & NV==0)

#tabla con mareasque hay que eliminar
check_remove <- left_join(check, lookup, by= c("Nombre","C_FcRegresoSimple" ))
dim(check)
dim(check_remove)
length(unique(check_remove$IdMarea))
length(unique(paste(check_remove$Nombre, check_remove$C_FcRegresoSimple)))

check_remove <- subset(check_remove, !grepl("ESP", IdMarea))


#write.table(test, file="checktrips.csv", sep=";", dec=",", row.names = F)
write.table(check_remove, file="checktrips_remove.csv", sep=";", dec=",", row.names = F)


dim(Dori)
sum(Dori$PesoConsumo)


Dori_v2 <- subset(Dori, !IdMareaIEO %in% unique(check_remove$IdMareaIEO))
dim(Dori_v2)
sum(Dori_v2$PesoConsumo)


ntrips <- Dori_v2 %>% group_by(Nombre, CodigoCFR) %>% summarise(ntrips=length(unique(IdMareaIEO)))

subset(ntrips, Nombre=="GUADALUPEKO SAINDUTEGIA BERRIA")


## tras habalr con IEO quitamos
# 1.	Lineas que vienen de la tabla CapturasLanceCero
# 2.	Lineas con Desembarcado= False
# 3.	Mareas que tienen PesoConsumo = 0 en el total en la marea


# 2_Comparamos areas en NV ####
########################## #

setwd(path.data)
NV <-read.table("2019_NV_all.csv", sep=";",dec=",",header=T, stringsAsFactors = FALSE)


NV$CodigoMarea<- NV$CMarea
NV$CodigoMarea[is.na(NV$CodigoMarea)] <- NV$CHoja_Desem[is.na(NV$CodigoMarea)]
NV$Division_N1V[is.na(NV$Division_N1V)] <- NV$Sub_Zona_N1V[is.na(NV$Division_N1V)]
NV$Fecha_venta <- ISOdate(NV$Año,NV$Mes_Venta,NV$Dia_Venta)
NV$Division_N1V <- tolower(NV$Division_N1V)
NV$Zona <- tolower(NV$Zona)
NV$Zona[NV$Zona=="27.8.c.e"] <- "27.8.c"


Dori$C_Area <- tolower(Dori$C_Area )
NV$C_Area_DO <- Dori$C_Area [match(NV$CodigoMarea, Dori$IdMareaOrigen)]


areaNV <- NV %>% filter(!is.na(CodigoMarea)) %>% group_by(IdVenta, Nombre_Buque, Fecha_venta , Zona, C_Area_DO ) %>% summarize(Peso_Neto = sum(Peso_Neto , na.rm=T))

areaNV <- areaNV %>% dcast(IdVenta + Nombre_Buque + Fecha_venta + Zona ~ C_Area_DO , value.var="Peso_Neto")

dropnames <- c("IdVenta", "Nombre_Buque", "Fecha_venta", "Zona")
areaNV<-as.data.table(areaNV)
areaNV$C_Area <- colnames(areaNV[,-dropnames, with =F])[apply(areaNV[,-dropnames, with =F],1,which.max)]
areaNV<-as.data.frame(areaNV)

areaNV_fin <- subset(areaNV, !is.na(C_Area) & C_Area!="NA", select=c(IdVenta, Nombre_Buque, Fecha_venta, Zona, C_Area)) 

area_change <- subset(areaNV_fin, Zona != C_Area ) %>% arrange (Nombre_Buque, Fecha_venta)

setwd(path.res)
write.table(area_change, file="area_change_Ventas.csv", sep=";", dec=",", row.names = F)
##


subset(Dori, Nombre=="KALAMENDI" & C_Area=="27.8.c")


## tabla manolo ##
#################

head(DoriBD)
head(check_remove)

ieo <- DoriBD %>% group_by(C_IDMAREA, IDDIARIO, CODIGOMAREA, C_CODIGO_UE, LABORATORIO) %>% summarise(PesoConsumo=sum(PesoConsumo, na.rm=T)) %>% filter(LABORATORIO =="AZTI")
ieo <- subset(ieo, !ieo$C_IDMAREA %in% check_remove$IdMareaIEO)

write.table(ieo, "mareas azti para ieo.csv")

a <- subset(DoriBD, Desembarcado=="False")



mareasMM <- read.table("azti-mareas_enviado manuelmarin 20200327.txt", sep=";", header=TRUE)

mareas_all<- merge(ieo, mareasMM, by.x="IDDIARIO", by.y="IdDiario",  all.x = T, all.y = T)

mareas_all$ORIGEN_TABLA <- DoriBD$ORIGEN_TABLA[match(mareas_all$IDDIARIO, DoriBD$IDDIARIO)]
mareas_all$CensoPorModalidad <- DoriBD$CensoPorModalidad[match(mareas_all$IDDIARIO, DoriBD$IDDIARIO)]

write.table(mareas_all, "checking mareas.csv", sep=";", dec=",", row.names = F)



ESP-TRP-02688720190203235529


a<- mareas_all %>% filter(is.na(LABORATORIO.y))
mareas_all %>% filter(is.na(LABORATORIO.y & )
                      
                      
subset(DoriBD, IDDIARIO==483943) # error  io hay que cambiar a laboratorio azti
subset(DoriBD, IDDIARIO==630402) # no la encuentro
subset(DoriBD, CODIGOMAREA=="ESP-TRP-02282920190624215103")

azti_labazti <- c(564958,  614473,  614486,  660983,  737306,  778425,  782756,  793269,  800947)
azti_labazti <-c(475543, 475651, 516785, 528943, 552541, 552555, 692555, 767855)


subset(DoriBD, IDDIARIO %in% azti_labazti)[,1:10] # todos tienen pueto desembarque euskadi. peso consumo >0

subset(DoriBD, IDDIARIO==614473)
subset(DoriBD, IDDIARIO==614473)
subset(DoriBD, IDDIARIO==614473)
subset(DoriBD, IDDIARIO==614473)

subset(Dori, IdDiario==678062)
subset(InfoCapturasCalculadas, IdDiario==630402)

subset(InfoCapturasCalculadas, IdMareaOrigen=="ESP-TRP-02282920190624215103")



subset(DoriBD, IDDIARIO==528943)
subset(DoriBD, IDDIARIO==703693)

