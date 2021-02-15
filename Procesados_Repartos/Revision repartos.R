############################################################################### #
#                    Estimas
############################################################################### #
############################################################################### #

#  Desembarcos (Ventas) 
#  Todos los buques; Todas las especies; Agrupar por tamaños; Todas las zonas; Todos los metiers
#  Puertos de Venta:	Euskadi; Puertos Base:	España;  Unidad Temporal:	Dia; 
#  csv. configuración ESP
#
#  Notas de Primera Venta 
#  Todos los buques; Todas las especies; Agrupar por tamaños; Todas las zonas; Todos los metiers
#  Puertos de Venta:	Euskadi; Puertos Base:	España;  Unidad Temporal:	Dia;    
#  csv. configuración ESP
#
#  Tallas (Informe IEO)
#  Todos los metiers GNS/GTR/LLS/FPO/LHM/LTL/LHP/OTB/PTB/PS/MIS
#  Convertir a txt
#


############################################################################# #
# En este codigo vamos a comparar los datos de ventas y NV con el objetivo de ver
# la magnitud de los desembarcos que se llevan a POSA y si están incluidos en las
# hojas de venta de la OPPAO
############################################################################# #

#      Librerias y datos      #####
################################### #

rm(list=(ls()))
options(scipen=999)

#Librerias

library(lubridate)
library (stringr)
#library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(data.table)
library(scales)

# library(fishPiCodes)
# data("ASFIS_WoRMS")


#Funciones
Fun_CountUnique <- function (x) { length(unique(x))}   

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

# Datos
path <- "C:/use/0_Lucia/1_Proyectos/AA_SegPes/2021/1_Depuracion datos/2_POSA"  
dat.path <- paste(path,"/Datos/",sep="")
setwd(dat.path)

#      Leer los ficheros       ####
################################## #
DBfile      <- "Ventas_2020.csv"  # Está toda la flota que desembarca en euskadi. OPPAO con sus hojas de venta, el resto con las NV
# Me dice Carmen que ha metido también los datos de POSA hasta junio. Estan identificados como tamaño POSA
NVfile      <- "N1V_2020.csv"     # está toda la flota que desembarca en euskadi.
Tallasfile  <- "Tallas_2020.txt"  # mareas muestradas. toda la flota
#
# # 
# DBfile      <- "Ventas_2019.csv"  #
# NVfile      <- "N1V_2019.csv"     #
# Tallasfile  <- "Tallas_2019.txt"  #



#Maestro especies
conv_sp<- read.csv("Especies_2020.txt",header=T,sep="\t", stringsAsFactors = FALSE); head(conv_sp)
head(conv_sp)
names(conv_sp) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(conv_sp))
conv_sp$Nombre.Oficial  <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), conv_sp$Nombre.Oficial )

#Maestro buques
conv_censo<- read.csv("Buques_2020.txt",header=T,sep="\t", stringsAsFactors = FALSE); head(conv_censo)
names(conv_censo) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(conv_censo))
conv_censo$Buque <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), conv_censo$Buque)
conv_censo$Puerto.base  <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), conv_censo$Puerto.base )
conv_censo$Caladero.principal <- conv_censo$CensoPorModalidad
conv_censo$Caladero.principal <- toupper(conv_censo$Caladero.principal)


#Desembarcos
DB0 <-read.table(DBfile, sep=",",dec=".",header=T, stringsAsFactors = FALSE)#, skip=10)# comment.char = "#")
DB<- DB0
head(DB); dim(DB)
names(DB) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(DB))
DB$Nombre_Buque <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), DB$Nombre_Buque)
DB$Puerto_Base  <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), DB$Puerto_Base )
DB$Puerto_Base  <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), DB$Puerto_Base )
DB$Puerto_Venta  <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), DB$Puerto_Venta )

names(DB)[names(DB)=="Kg_Desemb_Peso_Vivo"] <- "Peso"


#Notas de Primera venta
NV0 <-read.table(NVfile, sep=";",dec=",",header=T, stringsAsFactors = FALSE)
NV<- NV0
head(NV); dim(NV)
names(NV) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(NV))
NV$Nombre_Buque <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), NV$Nombre_Buque)
NV$Puerto_Venta  <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), NV$Puerto_Venta )

names(NV)[names(NV)=="Peso_Neto"] <- "Peso"


#Tallas
LN0 <-read.table(Tallasfile, sep="\t",dec=",",header=T, stringsAsFactors = FALSE)
LN<- LN0
head(LN); dim(LN)
names(LN) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(LN))
LN$Barco <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), LN$Barco)
LN$Puerto.venta  <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), LN$Puerto.venta )

names(LN)[names(LN)=="Barco.real"]          <- "Nombre_Buque"
names(LN)[names(LN)=="Id.Venta"]            <- "IdVenta"
names(LN)[names(LN)=="Especie.comercial"]   <- "Especie_ALFA3"
names(LN)[names(LN)=="Arte"]                <- "Metier"
names(LN)[names(LN)=="Nº.Individuos"]       <- "N.Individuos"
names(LN)[names(LN)=="PesMueCat.Kg."]       <- "PesoMuestreado"
names(LN)[names(LN)=="Especie.muestreada"]  <- "Especie.muestreada.ALFA3"
names(LN)[names(LN)=="Especie_ALFA3"]       <- "Especie.comercial.ALFA3"


LN$Fecha_Venta <- as_date(as.Date(LN$Fecha.venta, "%d/%m/%Y"))
LN$Mes <- month(LN$Fecha_Venta)
LN$Trimestre <- quarter(LN$Fecha_Venta)
LN$Especie.comercial <- conv_sp$Nombre.Oficial[match(LN$Especie.comercial.ALFA3, conv_sp$Cod..ALFA.3)]
LN$Especie.muestreada <- conv_sp$Nombre.Oficial[match(LN$Especie.muestreada.ALFA3, conv_sp$Cod..ALFA.3)]


LN<- LN %>%  group_by_at(vars(- N.Individuos, -Talla.cm.)) %>% summarize(N.Individuos=sum(N.Individuos = sum(N.Individuos))) %>% data.frame()




#Año
año <- unique(c(DB$Año, NV$Año))
año 


# Hacemos tabla resumen de mareas con la información de cabecera (barco, puerto venta, fecha venta...). 
#############
Cabeceras <- DB %>% group_by(IdVenta, Nombre_Buque, Cod_UE, Dia, Mes, Año, Metier, Puerto_Venta, Puerto_Base) %>% summarize(Peso=sum(Peso))
dim(Cabeceras)


# Corrección especies
# DB$Especie_Oficial_ini <- DB$Especie_Oficial
# DB$Especie_Oficial <- make.names(DB$Especie_Oficial )
# DB$Especie_Oficial <- gsub("...", ".", DB$Especie_Oficial, fixed = TRUE)
# DB$Especie_Oficial <- gsub("..", ".", DB$Especie_Oficial, fixed = TRUE)




# 
# Definir grupos de reparto  ###
########### #

tab_repartos <- NULL

rapes     <-  c( "Rapes Lophius", "Rape blanco", "Rape negro")
gallos    <-  c( "Gallos - ollarra", "Gallo boscii", "Gallo whiffiagonis")
calamares <-  c( "Calamares Loligo spp.", "Calamar comun", "Calamares Loliginidae", "Calamar veteado" )
sepias    <-  c( "Sepias y chocos", "Sepia comun", "Choco/sepia con punta" )
potas     <-  c( "Potas Ommastrephidae nep", "Pota Norteña",  "Pota costera",  "Pota voladora" )
triglidos <-  c( "Triglidos", "Perlon", "Bejel", "Rubio",  "Garneos nep", "Garneo", "Cuco - Peona", "Arete oscuro " )
rayas     <-  c( "Rayas spp",  "Raya de clavos", "Raya santiaguesa", "Raya pintada", "Raya mosaica (undulata)", "Raya cardadora", "Raya batis", "Raya estrellada", "Raya picuda", "Raya mosaica")
soleidos  <-  c( "Soleidos", "Soldadito",  "Acedia")
fanecas   <-  c( "Fanecas spp", "Faneca comun", "Faneca menor - Fodon - Capellan")
cabrachos <-  c( "Rascacios, Cabrachos (Scorpaena spp.)",  "Cabracho", "Rascacio", "Escorpora")


tab_repartos<- data.frame(Especie_Oficial = c(rapes, gallos, calamares, sepias, potas, triglidos, rayas, soleidos, fanecas, cabrachos),
                          Especie_Oficial_Gen = c(rep(rapes[1], length(rapes)),
                                                  rep(gallos[1], length(gallos)),
                                                  rep(calamares[1], length(calamares)),
                                                  rep(sepias[1], length(sepias)),
                                                  rep(potas[1], length(potas)),
                                                  rep(triglidos[1], length(triglidos)),
                                                  rep(rayas[1], length(rayas)),
                                                  rep(soleidos[1], length(soleidos)),
                                                  rep(fanecas[1], length(fanecas)),
                                                  rep(cabrachos[1], length(cabrachos))),
                          stringsAsFactors = F)



# Correción de especies: solo un nombre genérico & sin especies DEA  ###
########### #

temp <- c(DB$Especie_Oficial, LN$Especie.comercial, LN$Especie.muestreada)

unique(temp[grepl("rape", temp, ignore.case=TRUE)])
DB$Especie_Oficial[DB$Especie_Oficial %in% c("Rapes Lophiidae", "Rapes Lophius")] <- "Rapes Lophius"
LN$Especie.comercial[LN$Especie.comercial %in% c("Rapes Lophiidae", "Rapes Lophius")] <- "Rapes Lophius"

unique(temp[grepl("Gallo", temp, ignore.case=TRUE)])

unique(temp[grepl("Calam", temp, ignore.case=TRUE)])
DB$Especie_Oficial[DB$Especie_Oficial %in% c("Calamares Loligo spp.")] <- "Calamares Loliginidae"
LN$Especie.comercial[LN$Especie.comercial %in% c("Calamares Loligo spp.")] <- "Calamares Loliginidae"

unique(temp[grepl("Sepia|choco", temp, ignore.case=TRUE)])
DB$Especie_Oficial[DB$Especie_Oficial %in% c("Sepias nep")] <- "Sepias y chocos"
LN$Especie.comercial[LN$Especie.comercial %in% c("Sepias nep")] <- "Sepias y chocos"

unique(temp[grepl("Pota|Vola", temp, ignore.case=TRUE)])
DB$Especie_Oficial[DB$Especie_Oficial %in% c("Potas voladoras nep (N1V)")] <- "Potas Ommastrephidae nep"
LN$Especie.comercial[LN$Especie.comercial %in% c("Potas voladoras nep (N1V)")] <- "Potas Ommastrephidae nep"

unique(temp[grepl("Perlon|Arete|Bejel|Rubio|Cuco|Garneo|Trigli", temp, ignore.case=TRUE)])  
DB$Especie_Oficial[DB$Especie_Oficial %in% c("Garneos nep")] <- "Triglidos"
LN$Especie.comercial[LN$Especie.comercial %in% c("Garneos nep")] <- "Triglidos"

unique(temp[grepl("Raya", temp, ignore.case=TRUE)])  
DB$Especie_Oficial[DB$Especie_Oficial %in% c("Rayas (Rajidae)", "Raya de Murray (DEA)", "Raya boca de rosa (DEA)", "Raya de espejos (N1V)", "Raya bramante (DEA)", "Raya leopardus (N1V)")] <- "Rayas spp"
LN$Especie.comercial[LN$Especie.comercial %in% c("Rayas (Rajidae)", "Raya de Murray (DEA)", "Raya boca de rosa (DEA)", "Raya de espejos (N1V)", "Raya bramante (DEA)", "Raya leopardus (N1V)")] <- "Rayas spp"

unique(temp[grepl("Solei|acedi|soldadi", temp, ignore.case=TRUE)])  

unique(temp[grepl("fanec|Fodon", temp, ignore.case=TRUE)])  
DB$Especie_Oficial[DB$Especie_Oficial %in% c("Faneca noruega (DEA)")] <- "Fanecas spp"
LN$Especie.comercial[LN$Especie.comercial %in% c("Faneca noruega (DEA)")] <- "Fanecas spp"

unique(temp[grepl("rasca|cabra", temp, ignore.case=TRUE)])  


# Correción de especies: asignación de especies datos OPPAO  ###
########### #

DBidOPPAO <- unique(subset(DB, Metier %in% c("OTB_DEF_>=70_0_0","PTB_DEF_>=70_0_0", "PTB_MPD_>=55_0_0") &  Puerto_Venta=="Ondarroa", select="IdVenta"))
LNidOPPAO <- unique(subset(LN, Metier %in% c("OTB_DEF_>=70_0_0","PTB_DEF_>=70_0_0", "PTB_MPD_>=55_0_0") &  Puerto.venta=="Ondarroa", select="IdVenta"))

# fanecas
LN$Especie.comercial[LN$IdVenta %in% LNidOPPAO & LN$Categoria.comercial %in% c("Faneca Fanequita Ab", "Fogon Cd")] <- "Fanecas spp"
LN$Especie.comercial[LN$IdVenta %in% LNidOPPAO & !LN$Categoria.comercial %in% c("Faneca Fanequita Ab", "Fogon Cd")] <- "Faneca comun"
DB$Especie_Oficial[DB$IdVenta %in% DBidOPPAO & DB$Tamaño %in% c("Faneca Fanequita Ab", "Fogon Cd")] <- "Fanecas spp"
DB$Especie_Oficial[DB$IdVenta %in% DBidOPPAO & DB$Tamaño %in% c("Faneca Fanequita Ab", "Fogon Cd")] <- "Faneca comun"


# triglidos
LN$Especie.comercial[LN$IdVenta %in% LNidOPPAO & LN$Especie.comercial %in% triglidos] <- "Triglidos"
LN$Especie.comercial[LN$IdVenta %in% LNidOPPAO & LN$Especie.comercial %in% triglidos] <- "Triglidos"
DB$Especie_Oficial[DB$IdVenta %in% DBidOPPAO & DB$Especie_Oficial %in% triglidos] <- "Triglidos"
DB$Especie_Oficial[DB$IdVenta %in% DBidOPPAO & DB$Especie_Oficial %in% triglidos] <- "Triglidos"

                                  
# Nueva variable: especie genérico ###
########### #

DB$Especie_Oficial_Gen <- tab_repartos$Especie_Oficial_Gen[match(DB$Especie_Oficial, tab_repartos$Especie_Oficial)]
DB$Especie_Oficial_Gen[is.na(DB$Especie_Oficial_Gen)] <- DB$Especie_Oficial[is.na(DB$Especie_Oficial_Gen)]

LN$Especie.comercial.Gen <- tab_repartos$Especie_Oficial_Gen[match(LN$Especie.comercial, tab_repartos$Especie_Oficial)]
LN$Especie.comercial.Gen[is.na(LN$Especie.comercial.Gen)] <- LN$Especie.comercial[is.na(LN$Especie.comercial.Gen)]


## Gallos

i <- gallos [1]; i
i <- triglidos [1]; i

  
# General
## % en desembarcos

DB_summary <- DB %>% filter(Especie_Oficial_Gen %in% i) %>% 
  group_by (Metier, Especie_Oficial) %>% 
  summarise(Peso = round(sum(Peso, na.rm=T),0)) %>%
  dcast(Metier ~ Especie_Oficial, value.var= "Peso") %>%
  data.frame()
DB_summary[is.na(DB_summary)] <- 0

prop <- prop.table(as.matrix(DB_summary[,2:4]), margin = 1)
prop <- round(prop,2 )
prop <- data.frame(cbind(DB_summary[,"Metier"], prop  ))


# Opcion 1
## convertimos todo al generico antes de repartir

LN_TripSamp <- LN %>% group_by(Metier, Trimestre, Especie.comercial.Gen) %>% 
  summarise(TripSamp = length(unique(IdVenta)),
            PesoSamp = round(sum(PesoMuestreado),2))

temp1 <- LN %>% filter(Especie.comercial.Gen %in% i) %>% 
  group_by (Metier, Trimestre, Especie.comercial.Gen, Especie.muestreada) %>% 
  summarise(Peso = sum(PesoMuestreado, na.rm=T)) %>%
  left_join(LN_TripSamp, by=c("Metier", "Trimestre", "Especie.comercial.Gen")) %>%
  dcast(Metier + Trimestre + TripSamp + PesoSamp + Especie.comercial.Gen ~ Especie.muestreada, value.var= "Peso")
temp1[is.na(temp1)] <- 0

prop1 <- prop.table(as.matrix(temp1[,6:7]), margin = 1)
prop1 <- round(prop1,2 )

tablefin1 <- data.frame(cbind(temp1[,1:5], prop1  ))

DB_reparto1 <- DB %>% filter(Especie_Oficial_Gen %in% i) %>% 
  group_by (Metier, Trimestre, Especie_Oficial_Gen) %>% 
  summarise(Peso = round(sum(Peso, na.rm=T),0)) %>%
  left_join(tablefin1, by=c("Metier" = "Metier", "Trimestre" = "Trimestre", "Especie_Oficial_Gen" = "Especie.comercial.Gen")) %>%
  data.frame()



## opción 2
## mantenemos asignacion de desembarcos y solo repartimos lo que esta mezclado


LN_TripSamp <- LN %>% group_by(Metier, Trimestre, Especie.comercial) %>% 
  summarise(TripSamp = length(unique(IdVenta)),
            PesoSamp = round(sum(PesoMuestreado),2))

temp1 <- LN %>% filter(Especie.comercial %in% i) %>% 
  group_by (Metier, Trimestre, Especie.comercial, Especie.muestreada) %>% 
  summarise(Peso = sum(PesoMuestreado, na.rm=T)) %>%
  left_join(LN_TripSamp, by=c("Metier", "Trimestre", "Especie.comercial")) %>%
  dcast(Metier + Trimestre + TripSamp + PesoSamp + Especie.comercial ~ Especie.muestreada, value.var= "Peso")
temp1[is.na(temp1)] <- 0

prop1 <- prop.table(as.matrix(temp1[,6:7]), margin = 1)
prop1 <- round(prop1,2 )

tablefin1 <- data.frame(cbind(temp1[,1:5], prop1  ))

DB_reparto2 <- DB %>% filter(Especie_Oficial %in% i) %>% 
  group_by (Metier, Trimestre, Especie_Oficial) %>% 
  summarise(Peso = round(sum(Peso, na.rm=T),0)) %>%
  left_join(tablefin1, by=c("Metier" = "Metier", "Trimestre" = "Trimestre", "Especie_Oficial" = "Especie.comercial")) %>%
  data.frame()




