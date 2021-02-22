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
# En este codigo vamos a explorar los muestreos de tallas que tenemos para ver la mejor forma de 
# hacer lso repartos
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

#      Leer los ficheros       ####
################################## #
DBfile      <- "0_Datos/Ventas_2020.csv"  # Está toda la flota que desembarca en euskadi. OPPAO con sus hojas de venta, más POSA, el resto con las NV
NVfile      <- "0_Datos/N1V_2020.csv"     # está toda la flota que desembarca en euskadi.
Tallasfile  <- "0_Datos/Tallas_2020.txt"  # mareas muestradas. toda la flota
#
# # 
# DBfile      <- "Ventas_2019.csv"  #
# NVfile      <- "N1V_2019.csv"     #
# Tallasfile  <- "Tallas_2019.txt"  #



#Maestro especies
conv_sp<- read.csv("0_Maestros/Especies_2020.txt",header=T,sep="\t", stringsAsFactors = FALSE); head(conv_sp)
head(conv_sp)
names(conv_sp) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(conv_sp))
conv_sp$Nombre.Oficial  <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), conv_sp$Nombre.Oficial )

#Maestro buques
conv_censo<- read.csv("0_Maestros/Buques_2020.txt",header=T,sep="\t", stringsAsFactors = FALSE); head(conv_censo)
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
names(LN)[names(LN)=="PesTotCat.Kg."]       <- "Peso"
names(LN)[names(LN)=="Especie.muestreada"]  <- "Especie.muestreada.ALFA3"

names(LN)[names(LN)=="Especie_ALFA3"]       <- "Especie.comercial.ALFA3"

LN$Fecha_Venta <- as_date(as.Date(LN$Fecha.venta, "%d/%m/%Y"))
LN$Mes <- month(LN$Fecha_Venta)
LN$Trimestre <- quarter(LN$Fecha_Venta)
LN$Especie.comercial <- conv_sp$Nombre.Oficial[match(LN$Especie.comercial.ALFA3, conv_sp$Cod..ALFA.3)]
LN$Especie.muestreada <- conv_sp$Nombre.Oficial[match(LN$Especie.muestreada.ALFA3, conv_sp$Cod..ALFA.3)]

# agregamos el fichero de tallas por muestra (quitamos el nº de individuos por cm)
LN<- LN %>%  group_by_at(vars(- N.Individuos, -Talla.cm.)) %>% summarize(N.Individuos=sum(N.Individuos = sum(N.Individuos))) %>% data.frame()



#Año
año <- unique(c(DB$Año, NV$Año))
año 


# 
# Definir grupos de reparto  ###
########### #

tab_repartos <- NULL

rapes     <-  c( "Rapes Lophius", "Rape blanco", "Rape negro")
gallos    <-  c( "Gallos - ollarra", "Gallo boscii", "Gallo whiffiagonis")
calamares <-  c( "Calamares Loliginidae", "Calamar comun", "Calamar veteado" )
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

sp <- unique(tab_repartos$Especie_Oficial)[!unique(tab_repartos$Especie_Oficial) %in% unique(tab_repartos$Especie_Oficial_Gen)]



# Correción de especies: 
# unificamos los nombres genéricos de forma que solo se llamen de una forma
# y quitamos las sespecies marcadas scomo "(DEA)" o "(NV)" porque se supone que son erroneas (la spasamso al genérico)
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


# Correción de especies: 
# Corregimos asignación de especies datos OPPAO  (triglidos y fanecas) ###
# se supone que los triglidos tienen que ir como triglidos
# se supone que las fanecas pequeñas (fanequita, fodon) tienen que ir como fanecas spp
# aquí tengo un poco de dudas porque cuando hay tallas cambiamso las ventas
########### #

DBidOPPAO <- unique(subset(DB, Metier %in% c("OTB_DEF_>=70_0_0","PTB_DEF_>=70_0_0", "PTB_MPD_>=55_0_0") &  Puerto_Venta=="Ondarroa", select="IdVenta"))
LNidOPPAO <- unique(subset(LN, Metier %in% c("OTB_DEF_>=70_0_0","PTB_DEF_>=70_0_0", "PTB_MPD_>=55_0_0") &  Puerto.venta=="Ondarroa", select="IdVenta"))

# fanecas
unique(LN$Categoria.comercial[LN$Especie.comercial %in% fanecas])
unique(DB$Tamaño[DB$Especie_Oficial %in% fanecas])

LN$Especie.comercial[LN$IdVenta %in% LNidOPPAO$IdVenta & LN$Especie.comercial %in% fanecas & LN$Categoria.comercial %in% c("Faneca Fanequita Ab", "Fogon Cd")] <- "Fanecas spp"
LN$Especie.comercial[LN$IdVenta %in% LNidOPPAO$IdVenta & LN$Especie.comercial %in% fanecas & !LN$Categoria.comercial %in% c("Faneca Fanequita Ab", "Fogon Cd")] <- "Faneca comun"
DB$Especie_Oficial[DB$IdVenta %in% DBidOPPAO$IdVenta & DB$Especie_Oficial %in% fanecas & DB$Tamaño %in% c("Faneca Fanequita Ab", "Fogon Cd")] <- "Fanecas spp"
DB$Especie_Oficial[DB$IdVenta %in% DBidOPPAO$IdVenta & DB$Especie_Oficial %in% fanecas & !DB$Tamaño %in% c("Faneca Fanequita Ab", "Fogon Cd")] <- "Faneca comun"


# triglidos
LN$Especie.comercial[LN$IdVenta %in% LNidOPPAO$IdVenta & LN$Especie.comercial %in% triglidos] <- "Triglidos"
DB$Especie_Oficial[DB$IdVenta %in% DBidOPPAO$IdVenta & DB$Especie_Oficial %in% triglidos] <- "Triglidos"


# Creamos una nueva variable con la especie genérica para las especies de reparto ###
########### #

DB$Especie_Oficial_Gen <- tab_repartos$Especie_Oficial_Gen[match(DB$Especie_Oficial, tab_repartos$Especie_Oficial)]
DB$Especie_Oficial_Gen[is.na(DB$Especie_Oficial_Gen)] <- DB$Especie_Oficial[is.na(DB$Especie_Oficial_Gen)]

LN$Especie.comercial.Gen <- tab_repartos$Especie_Oficial_Gen[match(LN$Especie.comercial, tab_repartos$Especie_Oficial)]
LN$Especie.comercial.Gen[is.na(LN$Especie.comercial.Gen)] <- LN$Especie.comercial[is.na(LN$Especie.comercial.Gen)]

subset(LN, IdVenta %in% c(538377, 538838, 539178, 539379, 552483, 552797, 553043, 553112 ) &
         Especie.muestreada.ALFA3 %in% c("MEG", "LDB")) %>% group_by(Trimestre) %>% summarise(Ntrips=length(unique(IdVenta)))

subset(LN, IdVenta %in% c(553112) & Especie.muestreada.ALFA3 %in% c("MEG", "LDB"))


## tabla resumen de muestreos
####
LN_TripSamp <- LN %>% group_by(IdVenta, Metier,  Puerto.venta, Trimestre, Especie.comercial) %>% 
  summarise(TripSamp = length(unique(IdVenta)))
temp <- LN %>% group_by (IdVenta, Metier, Puerto.venta, Trimestre, Especie.comercial.Gen, Especie.comercial, Especie.comercial.ALFA3, Especie.muestreada, Especie.muestreada.ALFA3) %>% 
  summarise(Peso = sum(Peso, na.rm=T)) %>%
  left_join(LN_TripSamp, by=c("IdVenta",  "Metier","Puerto.venta", "Trimestre", "Especie.comercial"))

write.table(temp, "Procesados_Repartos/infoRepartos.csv", sep=";", dec=",", row.names = F)


## tabla resumen para DESEMBARCOS
####
unique(DB$Tamaño[DB$Especie_Oficial_Gen=="Rapes Lophius"])
unique(DB$Tamaño[DB$Especie_Oficial_Gen=="Gallos - ollarra"])
DB$Tamaño2<- DB$Tamaño
DB$Tamaño2<- gsub(" N| BL", "", DB$Tamaño2)
DB$Tamaño2<- gsub("Gallo |whiffi |boscii", "", DB$Tamaño2)

DB_summary <- DB %>% group_by(Metier, Trimestre, Tamaño2, Especie_Oficial_Gen, Especie_Oficial) %>% 
                           summarise(Peso=sum(Peso, na.rm = T)) 
            
write.table(DB_summary, "Procesados_Repartos/InfoDesembarcos.csv", sep=";", dec=",", row.names = F)


## seleccionar especies
# rapes, gallos,  calamares, sepias, potas, triglidos, rayas, soleidos, fanecas, cabrachos

i <- gallos [1]; i
i <- triglidos [1]; i
i <- calamares [1]; i


## General
##  % en desembarcos

DB_summary <- DB %>% filter(Especie_Oficial_Gen %in% i) %>% 
  group_by (Metier, Especie_Oficial) %>% 
  summarise(Peso = round(sum(Peso, na.rm=T),0)) %>%
  dcast(Metier  ~ Especie_Oficial, value.var= "Peso") %>%
  data.frame()
DB_summary[is.na(DB_summary)] <- 0

prop <- prop.table(as.matrix(DB_summary[,2:ncol(DB_summary)]), margin = 1)
prop <- round(prop,3)
prop <- data.frame(cbind(DB_summary[,"Metier"], prop  ))
names(prop)[1] <- "Metier"


## Opcion 1
## convertimos todo al generico antes de repartir
    
      # calculamos el nº de muestreos
LN_TripSamp <- LN %>% group_by(Metier, Trimestre, Especie.comercial.Gen) %>% 
  summarise(TripSamp = length(unique(IdVenta)),
            PesoSamp = round(sum(PesoMuestreado),2))
    
      # calculamos porcentajes
temp1 <- LN %>% filter(Especie.comercial.Gen %in% i) %>% 
  group_by (Metier, Trimestre, Especie.comercial.Gen, Especie.muestreada) %>% 
  summarise(Peso = sum(Peso, na.rm=T)) %>%
  left_join(LN_TripSamp, by=c("Metier", "Trimestre", "Especie.comercial.Gen")) %>%
  dcast(Metier + Trimestre + TripSamp + PesoSamp + Especie.comercial.Gen ~ Especie.muestreada, value.var= "Peso")
temp1[is.na(temp1)] <- 0

prop1 <- prop.table(as.matrix(temp1[,6:ncol(temp1)]), margin = 1)
prop1 <- round(prop1,3 )

tablefin1 <- data.frame(cbind(temp1[,1:5], prop1  ))


DB_reparto1 <- DB %>% filter(Especie_Oficial_Gen %in% i) %>% 
  group_by (Metier, Trimestre, Especie_Oficial_Gen) %>% 
  summarise(Peso = round(sum(Peso, na.rm=T),0)) %>%
  left_join(tablefin1, by=c("Metier" = "Metier", "Trimestre" = "Trimestre", "Especie_Oficial_Gen" = "Especie.comercial.Gen")) %>%
  data.frame()

      # calculamos pesos
DB_reparto1_KG <- DB_reparto1 %>% mutate_at(.funs = funs(.* Peso ), .vars = vars(7:ncol(DB_reparto1)) )
DB_reparto1_KG[,7:ncol(DB_reparto1_KG)] <- round(DB_reparto1_KG[,7:ncol(DB_reparto1_KG)],0)



## opción 2
## mantenemos asignacion de desembarcos y solo repartimos lo que esta mezclado

      # calculamos el nº de muestreos
LN_TripSamp <- LN %>% group_by(Metier, Trimestre, Especie.comercial) %>% 
  summarise(TripSamp = length(unique(IdVenta)),
            PesoSamp = round(sum(PesoMuestreado),2))

      # calculamos porcentajes
temp1 <- LN %>% filter(Especie.comercial %in% i) %>% 
  group_by (Metier, Trimestre, Especie.comercial, Especie.muestreada) %>% 
  summarise(Peso = sum(Peso, na.rm=T)) %>%
  left_join(LN_TripSamp, by=c("Metier", "Trimestre", "Especie.comercial")) %>%
  dcast(Metier + Trimestre + TripSamp + PesoSamp + Especie.comercial ~ Especie.muestreada, value.var= "Peso")
temp1[is.na(temp1)] <- 0

prop1 <- prop.table(as.matrix(temp1[,6:ncol(temp1)]), margin = 1)
prop1 <- round(prop1,2 )

tablefin1 <- data.frame(cbind(temp1[,1:5], prop1  ))


DB_reparto2 <- DB %>% filter(Especie_Oficial %in% i) %>% 
  group_by (Metier, Trimestre, Especie_Oficial) %>% 
  summarise(Peso = round(sum(Peso, na.rm=T),0)) %>%
  left_join(tablefin1, by=c("Metier" = "Metier", "Trimestre" = "Trimestre", "Especie_Oficial" = "Especie.comercial")) %>%
  data.frame()

        # calculamos pesos
DB_reparto2_KG <- DB_reparto2 %>% mutate_at(.funs = funs(. * Peso ), .vars = vars(7:ncol(DB_reparto2))) 
DB_reparto2_KG[,7:ncol(DB_reparto2_KG)] <- round(DB_reparto2_KG[,7:ncol(DB_reparto2_KG)],0)

