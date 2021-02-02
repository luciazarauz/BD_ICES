# ----------------------------------------------------------------- #
# Readme:                                                           #
# ----------------------------------------------------------------- # 
# En este script: comparamos los datos con las Nv de Hazi
# salida BD: Notas de venta + detalle. Puerto base españa. Agregar por tamaños
#
# INDICE:
#
# Functions
# Libraries.
# Cargar la tabla maestra (dori) y las NV.
# Comprobaciones
#

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


# Funciones  ####
################ #

Fun_unique <- function(x){length(unique(x))}

mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}



# # ####################################### #
# # Load                                 ####
# # ####################################### #

load(file="Datos/Dori2019_v3.Rdata"   )
load(file="Datos/Dori2019_v2.Rdata"   )

NV <- read.csv("Datos/NV2019.csv", sep=",", dec=".", stringsAsFactors = F, skip=11)
names(NV) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(NV))
head(NV)

Ventas <- read.csv("Datos/Ventas2019.csv", sep=",", dec=".", stringsAsFactors = F, skip=8)
names(Ventas) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(Ventas))
head(Ventas)

MaestrosBuque <- read.csv("Auxtables/MaestroBuquesAZTI_2019.csv", sep=",", dec=".", stringsAsFactors = F)
names(MaestrosBuque) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(MaestrosBuque))
head(MaestrosBuque)


# # ####################################### #
# # Añadir Variables                     ####
# # ####################################### #

# Añadir variable CodigoCFR ####
NV$CodigoCFR <- MaestrosBuque$Codigo.UE[match(NV$Nombre_Buque, MaestrosBuque$Buque)]
sort(unique(NV$Nombre_Buque[is.na(NV$CodigoCFR)]))

names(Ventas)[names(Ventas)=="Cod_UE"] <- "CodigoCFR"


# Añadir variable Censo Por Modalidad ####
NV$CensoPorModalidad <- MaestrosBuque$Caladero.principal[match(NV$CodigoCFR, MaestrosBuque$Codigo.UE)]
sort(unique(NV$Nombre_Buque[is.na(NV$CensoPorModalidad)]))
NV$CensoPorModalidad[NV$CensoPorModalidad == "ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII y VIIIa"] <- "ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII y VIIIabde."

Ventas$CensoPorModalidad <- MaestrosBuque$Caladero.principal[match(Ventas$CodigoCFR, MaestrosBuque$Codigo.UE)]
sort(unique(Ventas$Nombre_Buque[is.na(Ventas$CensoPorModalidad)]))
Ventas$CensoPorModalidad[Ventas$CensoPorModalidad == "ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII y VIIIa"] <- "ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII y VIIIabde."



# Añadir variable Laboratorio ####
PuertoVasco <- c("ALGORTA","ARMINTZA", "BILBAO", "BERMEO", "CIERVANA", "DONOSTIA", "ELANCHOVE",
                 "FUENTERRABIA","GETARIA", "GUETARIA", "GETARIA", "HONDARRIBIA", "LEKEITIO", "LEQUEITIO","MOTRICO",
                 "MUNDAKA","MUTRIKU", "ONDARROA", "ORIO","PASAIA", "PASAJES","PLENTZIA", "PLENCIA","SAN SEBASTIAN",
                 "SANTURTZI", "SANTURCE","ZIERBENA", "ZUMAYA")

# NV
NV$Puerto_Base <- toupper(NV$Puerto_Base )
NV$Puerto_Venta <- toupper(NV$Puerto_Venta )

sort(unique(NV$Puerto_Venta))
  NV$LAB_PuertoDesembarco <- NA
  NV$LAB_PuertoDesembarco [NV$Puerto_Venta %in% PuertoVasco] <- "EUS"
  NV$LAB_PuertoDesembarco [is.na(NV$LAB_PuertoDesembarco)] <- "ESP"
  NV$LAB_PuertoDesembarco [NV$Puerto_Venta %in% c("TROMSØ")] <- "OTH"
  table(NV$LAB_PuertoDesembarco)

sort(unique(NV$Puerto_Base))
  NV$LAB_PuertoBase <- NA
  NV$LAB_PuertoBase [NV$Puerto_Base %in% PuertoVasco] <- "EUS"
  NV$LAB_PuertoBase [is.na(NV$LAB_PuertoBase)] <- "ESP"
  table(NV$LAB_PuertoBase)

NV$LABORATORIO<- NA
NV$LABORATORIO [is.na(NV$LABORATORIO) & NV$LAB_PuertoDesembarco == "EUS"] <-  "AZTI"
NV$LABORATORIO [is.na(NV$LABORATORIO) & NV$LAB_PuertoDesembarco == "OTH" & NV$LAB_PuertoBase=="EUS"] <-  "AZTI"
NV$LABORATORIO [is.na(NV$LABORATORIO) ]<- "IEO"

NV %>% group_by(IdVenta) %>% summarize(n=Fun_unique(LABORATORIO )) %>% filter(n > 1)
NV %>% group_by(IdVenta) %>% summarize(n=Fun_unique(LABORATORIO )) %>% filter(n > 1)

tapply(NV$Peso_Neto, list(NV$LAB_PuertoBase, NV$LAB_PuertoDesembarco,NV$LABORATORIO), sum, na.rm=TRUE)
unique(NV$Puerto_Venta[NV$LABORATORIO=="AZTI"])
unique(NV$Puerto_Venta[NV$LABORATORIO=="IEO"])


# Venta
Ventas$Puerto_Base <- toupper(Ventas$Puerto_Base )
Ventas$Puerto_Venta <- toupper(Ventas$Puerto_Venta )

sort(unique(Ventas$Puerto_Venta))
Ventas$LAB_PuertoDesembarco <- NA
Ventas$LAB_PuertoDesembarco [Ventas$Puerto_Venta %in% PuertoVasco] <- "EUS"
Ventas$LAB_PuertoDesembarco [is.na(Ventas$LAB_PuertoDesembarco)] <- "ESP"
Ventas$LAB_PuertoDesembarco [Ventas$Puerto_Venta %in% c("TROMSØ")] <- "OTH"
table(Ventas$LAB_PuertoDesembarco)

sort(unique(Ventas$Puerto_Base))
Ventas$LAB_PuertoBase <- NA
Ventas$LAB_PuertoBase [Ventas$Puerto_Base %in% PuertoVasco] <- "EUS"
Ventas$LAB_PuertoBase [is.na(Ventas$LAB_PuertoBase)] <- "ESP"
table(Ventas$LAB_PuertoBase)

Ventas$LABORATORIO<- NA
Ventas$LABORATORIO [is.na(Ventas$LABORATORIO) & Ventas$LAB_PuertoDesembarco == "EUS"] <-  "AZTI"
Ventas$LABORATORIO [is.na(Ventas$LABORATORIO) & Ventas$LAB_PuertoDesembarco == "OTH" & Ventas$LAB_PuertoBase=="EUS"] <-  "AZTI"
Ventas$LABORATORIO [is.na(Ventas$LABORATORIO) ]<- "IEO"

Ventas %>% group_by(IdVenta) %>% summarize(n=Fun_unique(LABORATORIO )) %>% filter(n > 1)
Ventas %>% group_by(IdVenta) %>% summarize(n=Fun_unique(LABORATORIO )) %>% filter(n > 1)

tapply(Ventas$Kg_Desemb_Peso_Vivo, list(Ventas$LAB_PuertoBase, Ventas$LAB_PuertoDesembarco,Ventas$LABORATORIO), sum, na.rm=TRUE)
unique(Ventas$Puerto_Venta[Ventas$LABORATORIO=="AZTI"])
unique(Ventas$Puerto_Venta[Ventas$LABORATORIO=="IEO"])



# # ####################################### #
# # Comparar Dori, NV, Ventas            ####
# # ####################################### #

## comparacion por censo    ####
Dori <- Dori_FIN[! Dori_FIN$AZTI_BD %in% c("Captura0",   "Lance0"),]

comp_dori <- Dori %>% group_by(CensoPorModalidad) %>% summarise(Ntrip = length(unique(IdMarea)),
                                                                    Peso  = sum(PesoConsumoTotal, na.rm=T))
comp_nv <- NV %>% group_by(CensoPorModalidad)         %>% summarise(Ntrip = length(unique(IdVenta)),
                                                                    Peso  = sum(Peso_Neto, na.rm=T))
comp_venta <- Ventas %>% group_by(CensoPorModalidad)  %>% summarise(Ntrip = length(unique(IdVenta)),
                                                                    Peso  = sum(Kg_Desemb_Peso_Vivo, na.rm=T))

comp <- comp_dori  %>% full_join(comp_nv, comp_venta,by=c("CensoPorModalidad"), suffix = c( "_DO", "_NV")) 
comp <- comp  %>% full_join(comp_venta,by=c("CensoPorModalidad")) %>% data.frame()
comp <- comp %>% arrange(CensoPorModalidad)
names(comp)[names(comp)=="Ntrip"] <- "Ntrip_Venta"
names(comp)[names(comp)=="Peso"] <- "Peso_Venta"
comp


## comparacion por censo y barco  ####
comp_dori  <- Dori %>% group_by(CensoPorModalidad, CodigoCFR) %>% summarise(Ntrip = length(unique(IdMarea)),
                                                                            Peso = sum(PesoConsumoTotal, na.rm=T))
comp_nv    <- NV       %>% group_by(CensoPorModalidad, CodigoCFR) %>% summarise(Ntrip = length(unique(IdVenta)),
                                                                                Peso = sum(Peso_Neto, na.rm=T))
comp_venta <- Ventas   %>% group_by(CensoPorModalidad, CodigoCFR) %>% summarise(Ntrip = length(unique(IdVenta)),
                                                                                Peso  = sum(Kg_Desemb_Peso_Vivo, na.rm=T))

comp <- comp_dori  %>% full_join(comp_nv, by=c("CensoPorModalidad", "CodigoCFR"), suffix = c("_DO", "_NV")) 
comp <- comp  %>% full_join(comp_venta,by=c("CensoPorModalidad", "CodigoCFR")) %>% data.frame()
names(comp)[names(comp)=="Ntrip"] <- "Ntrip_Venta"
names(comp)[names(comp)=="Peso"] <- "Peso_Venta"

comp$Nombre     <- MaestrosBuque$Buque[match(comp$CodigoCFR, MaestrosBuque$Codigo.UE)]
comp$PuertoBase <- MaestrosBuque$Puerto.base[match(comp$CodigoCFR, MaestrosBuque$Codigo.UE)]
comp$Eslora     <- MaestrosBuque$Eslora[match(comp$CodigoCFR, MaestrosBuque$Codigo.UE)]
comp <- comp[, c( "CensoPorModalidad",  "CodigoCFR", "Nombre", "PuertoBase", "Eslora", "Peso_DO", "Peso_NV")]

comp <- comp %>% arrange(CensoPorModalidad, CodigoCFR)
head(comp)

write.table(comp, "Comp_Barco.csv", sep=",", dec=".", row.names = F)



## comparacion por censo y barco y laboratorio   ####
comp_dori  <- Dori %>% group_by(CensoPorModalidad, LABORATORIO,  CodigoCFR)    %>% summarise(Ntrip = length(unique(IdMarea)),
                                                                                             Peso = sum(PesoConsumoTotal, na.rm=T))
comp_nv    <- NV       %>% group_by(CensoPorModalidad, LABORATORIO, CodigoCFR) %>% summarise(Ntrip = length(unique(IdVenta)),
                                                                                             Peso = sum(Peso_Neto, na.rm=T))
comp_venta <- Ventas   %>% group_by(CensoPorModalidad, LABORATORIO, CodigoCFR) %>% summarise(Ntrip = length(unique(IdVenta)),
                                                                                             Peso  = sum(Kg_Desemb_Peso_Vivo, na.rm=T))

comp <- comp_dori  %>% full_join(comp_nv,    by = c("CensoPorModalidad", "LABORATORIO", "CodigoCFR"), suffix = c("_DO", "_NV")) 
comp <- comp       %>% full_join(comp_venta, by = c("CensoPorModalidad", "LABORATORIO", "CodigoCFR")) %>% data.frame()
names(comp)[names(comp)=="Ntrip"] <- "Ntrip_Venta"
names(comp)[names(comp)=="Peso"] <- "Peso_Venta"

comp$Nombre     <- MaestrosBuque$Buque[match(comp$CodigoCFR, MaestrosBuque$Codigo.UE)]
comp$PuertoBase <- MaestrosBuque$Puerto.base[match(comp$CodigoCFR, MaestrosBuque$Codigo.UE)]
comp$Eslora     <- MaestrosBuque$Eslora[match(comp$CodigoCFR, MaestrosBuque$Codigo.UE)]
comp <- comp[, c( "CensoPorModalidad",  "CodigoCFR", "Nombre", "LABORATORIO", "PuertoBase", "Eslora", "Peso_DO", "Peso_NV")]
comp <- comp %>% arrange(CensoPorModalidad, CodigoCFR, LABORATORIO)
head(comp)

write.table(comp, "Comp_BarcoLaboratorio.csv", sep=",", dec=".", row.names = F)



## comparacion por codigo de marea ####
NV[!is.na(NV$CMarea) & !is.na(NV$CHoja_Desem),]
NV$IdMarea <- NV$CMarea
NV$IdMarea[is.na(NV$CMarea)] <- NV$CHoja_Desem[is.na(NV$CMarea)]

comp_dori  <- Dori   %>% group_by(CensoPorModalidad, CodigoCFR, IdMarea)  %>%  summarise(Peso = sum(PesoConsumoTotal, na.rm=T))
comp_nv    <- NV     %>% group_by(CensoPorModalidad, CodigoCFR, IdMarea)  %>%  summarise( Peso = sum(Peso_Neto, na.rm=T))                                                                                            


comp <- comp_dori  %>% full_join(comp_nv,    by = c("CensoPorModalidad", "CodigoCFR", "IdMarea"), suffix = c("_DO", "_NV")) 
comp$Nombre     <- MaestrosBuque$Buque[match(comp$CodigoCFR, MaestrosBuque$Codigo.UE)]
comp$PuertoBase <- MaestrosBuque$Puerto.base[match(comp$CodigoCFR, MaestrosBuque$Codigo.UE)]
comp$Eslora     <- MaestrosBuque$Eslora[match(comp$CodigoCFR, MaestrosBuque$Codigo.UE)]
comp <- comp[, c( "CensoPorModalidad", "IdMarea", "CodigoCFR", "Nombre", "PuertoBase", "Eslora", "Peso_DO", "Peso_NV")]
comp <- comp %>% arrange(CensoPorModalidad, CodigoCFR, IdMarea)

head(comp)

write.table(comp, "Comp_Marea.csv", sep=",", dec=".", row.names = F)


# revisar     ####
################ #

# mareas con codigo duplicado 
##
comp_dori[duplicated(comp_dori$IdMarea),]                     # en dori
comp_nv[duplicated(comp_nv$IdMarea), ] %>% arrange(IdMarea)   # en NV  -> aclarar con HAZI. comprobar 2020
unique(comp_nv[duplicated(comp_nv$IdMarea), "IdMarea"])

# Numero de codigos de marea linkados
##
dim(comp)
dim(comp[!is.na(comp$Peso_DO) & !is.na(comp$Peso_NV),]) # codigos de marea linkados
dim(comp[!is.na(comp$Peso_DO) & is.na(comp$Peso_NV),])  # codigos de marea en NV pero no en dori
dim(comp[is.na(comp$Peso_DO) & !is.na(comp$Peso_NV),])  # codigos de marea en dori pero no en NV


# Codigo de marea que están en NV y DO pero con otro nombre
##
id <- comp_dori$IdMarea[which(comp_dori$IdMarea %in% comp_nv$IdMarea)]
  length(id); length(unique(id))
idlink <- id[which(id %in% comp$IdMarea[is.na(comp$Peso_DO)])]
  idlink                                    # mareas que estan en NV y no se linkan porque tienen otro nombre
  subset(comp, IdMarea %in% idlink) %>% arrange(IdMarea)


id <- comp_nv$IdMarea[which(comp_nv$IdMarea %in% comp_dori$IdMarea)]
  length(id); length(unique(id))
idlink <- id[which(id %in% comp$IdMarea[is.na(comp$Peso_NV)])]
  idlink                                    # en DO no ocurre esto



# mareas con peso = 0 en NV. Y sin codigo marea
##
check <- subset(comp, Peso_NV==0)
head(check)
dim(check)
unique(check$IdMarea)                    # no tienen codigo de marea. de donde salen?
subset(NV, is.na(IdMarea) & (Peso_Neto == 0 | is.na(Peso_Neto)))
         
                            # Aketxe, Andrekala Berria: parejas con Nv en solo un barco, 
                            # Kalamendi: nota oppao sin NV
                            # Bide Ona Bi: tallas sin NV que se añaden a ventas (16/01, 18/01, 13/02..)
                            # Otros: tallas sin NV 


# mareas con peso = 0 en dori
check <- subset(comp, Peso_DO==0)
head(check)
dim(check)
unique(check$IdMarea)                    # no tienen codigo de marea. de donde salen?
a <- subset(Dori, (PesoConsumoTotal == 0 | is.na(PesoConsumoTotal)) & 
                  (PesoDescarte == 0    | is.na(PesoDescarte)) )

subset(a, (PesoDesembarcado == 0 | is.na(PesoDesembarcado))) %>% group_by(CensoPorModalidad) %>% summarize(Ntrips = length(unique(IdMarea)))
subset(a, (PesoCapturado == 0 | is.na(PesoCapturado))) %>% group_by(CensoPorModalidad) %>% summarize(Ntrips = length(unique(IdMarea)))

temp <- subset(a, (PesoCapturado == 0 | is.na(PesoCapturado)))
unique(temp$IdMarea)
table(temp$Presentacion_AL3)
table(temp$Presentacion_AL3)

subset(a, IdMarea == "ESP-TRP-02694820190122072607")

table(a$AZTI_BD)

length(unique(a$IdMarea))

