
# ----------------------------------------------------------------- #
# Readme:                                                           #
# ----------------------------------------------------------------- #
# Este codigo sive para asignar metiers a las ventas/mareas de la BD
#
# Fuentes de datos:
  # * Informe Ventas
  # * Informe Notas de venta +  detalle 
      #  Unidad Temporal:	Dia
      #  Agrupar por tamaño
      # salida a csv
  # Para la asignación de metiers utilizamos los datos de Ventas, pero añadiendoles el area y el metier principal que nos viene en las NV

################################################################### #
#   Cargar librerias y funciones                                #####
################################################################### #

rm(list=(ls()))
options(digits=2, scipen = 999)

library (stringr)
library(doBy)
library(dplyr)
library(data.table)
library(tidyverse)

# count unique
Fun_CountUnique <- function (x) { length(unique(x))} 

# mgsub: function for multiple replacement of accents
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



################################################################### #
#   Leer los ficheros                                           #####
################################################################### #

#Año
Ano <- 2020

# Ventas
DB <-read.table("0_Datos/Ventas_2020.csv", sep=",",dec=".",header=T, stringsAsFactors = FALSE)
  head(DB); dim(DB)
  names(DB)       <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), names(DB))
  DB$Nombre_Buque <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), DB$Nombre_Buque)
  DB$Puerto_Base  <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), DB$Puerto_Base)

# Notas de Venta
NV <-read.table("0_Datos/N1V_2020.csv", sep=";",dec=",",header=T, stringsAsFactors = FALSE)

head(NV); dim(NV)
  names(NV)       <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), names(NV))
  names(NV)       <- mgsub(c("\\."), c("\\_"), names(NV))
  NV$Nombre_Buque <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), NV$Nombre_Buque)
  NV$Puerto_Base  <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), NV$Puerto_Base)
  
# Notas de Venta
DO <-read.table("0_Datos/DO_2020.csv", sep=";",dec=",",header=T, stringsAsFactors = FALSE)

head(DO); dim(DO)
names(DO)       <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), names(DO))
names(DO)       <- mgsub(c("\\."), c("\\_"), names(DO))
DO$Nombre_Buque <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), DO$Nombre_Buque)
DO$Puerto_Base  <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), DO$Puerto_Base)

names(DB)[!names(DB) %in%  names(DO)]

DO <- DO %>% rename( Cod_UE                = Cod__UE_Buque,
                     IdVenta               = IdMarea,
                     Dia_Desembarco        = Dia_desemb,
                     Mes_Desembarco        = Mes_desemb,
                     Trimestre_Desembarco  = Trimestre_desemb,
                     Dia                   = Dia_venta,
                     Mes                   = Mes_venta,
                     Trimestre             = Trimestre_venta,
                     Kg_Desemb_Peso_Vivo   = Peso,
                     MetierDO              = Nombre_Arte
                     )


#Conversiones
#Censo buque
buques      <- read.csv("0_Maestros/Buques_2020.txt",header=T,sep="\t",dec=",", stringsAsFactors = FALSE); head(buques)
names(buques)       <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), names(buques))
buques$Buque        <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), buques$Buque)
buques$Puerto.base  <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), buques$Puerto.base)
buques$Caladero.principal  <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), buques$Caladero.principal)
buques$CensoPorModalidad  <- mgsub(c("ñ","á","é","í","ó","ú"), c("n","a","e","i","o","u"), buques$CensoPorModalidad)
buques$Buque <- toupper(buques$Buque)
buques$Caladero.principal <- toupper(buques$Caladero.principal)
buques$CensoPorModalidad <- toupper(buques$CensoPorModalidad)

sort(unique(buques$Caladero.principal))

#Puerto Base
conv_base   <- read.csv("Depuracion_Metiers/Conv_puerto_base.txt",header=T,sep=",",dec=".", stringsAsFactors = FALSE); head(conv_base)
#Listas de especies objetivo para la asignación de metier
spAM         <- read.csv("Depuracion_Metiers/spAM.csv",header=T,sep=";",dec=",", stringsAsFactors = FALSE); head(spAM)
spOTB        <- read.csv("Depuracion_Metiers/spOTB.csv",header=T,sep=";",stringsAsFactors = FALSE); head(spOTB)
#Arte principal
#buquesLLSGNS <- read.csv("Depuracion_Metiers/2019_buques_LLS_GNS.csv",header=T,sep=";",dec=",", stringsAsFactors = FALSE); head(buquesLLSGNS)


  # #Ventas
  # DB <- DB
  # Fuente <- "Ventas"
  
  #Datos Oficiales
  DB <- DO
  Fuente <- "DO"
  
  ##################################### #
  #   NV: Zona y Arte               #####
  ##################################### #
  # El problema es qu een las NV, dentro de la misma marea, podemso tener varias zonas y artes
  # Y necesitamos asignar la mara a n solo arte y una sola area
  
  dim(NV[is.na(NV$Sub_Zona_N1V),])
  dim(NV[is.na(NV$Division_N1V),])
  
  table(NV$Sub_Zona_N1V[!is.na(NV$Sub_Zona_N1V)])
  table(NV$Division_N1V[!is.na(NV$Division_N1V)])
  
  #Variable Zona NV
  NV$ZonaNV<- NV$Division_N1V
  NV$ZonaNV[is.na(NV$ZonaNV)]<- NV$Sub_Zona_N1V[is.na(NV$ZonaNV)]
  length(NV$ZonaNV[is.na(NV$ZonaNV)])
  
  NV$ZonaNV[is.na(NV$ZonaNV)]<- toupper(NV$Zona[is.na(NV$ZonaNV)])
  
  # Variable Arte NV
  NV$MetierNV<- NV$Arte_Cod_N1V
  NV$MetierNV[NV$MetierNV %in% c("GN")] <- "GNS"
  NV$MetierNV[NV$MetierNV %in% c("LL", "LX")] <- "LLS"
  NV$MetierNV[NV$MetierNV %in% c("TB")] <- "OTB"
  NV$MetierNV[is.na(NV$MetierNV)]<- substr(NV$Metier[is.na(NV$MetierNV)],1,3)
  
  
  # ZonaSelect
  ########## #
  ZonaSelect <- NV %>% group_by(IdVenta, ZonaNV) %>% summarise(Peso_Neto = sum(Peso_Neto, na.rm=T)) %>%
    dcast(IdVenta ~ ZonaNV, value.var="Peso_Neto")   
  names(ZonaSelect) <- make.names(names(ZonaSelect))
   # Crear C_Zona
  dropnames <- c("IdVenta")
  ZonaSelect<-as.data.table(ZonaSelect)
  ZonaSelect$C_Zona <- colnames(ZonaSelect[,-dropnames, with =F])[apply(ZonaSelect[,-dropnames, with =F],1,which.max)]
  ZonaSelect<-as.data.frame(ZonaSelect)
  ZonaSelect$C_Zona <- gsub("X", "",ZonaSelect$C_Zona)
  # Algun corrección
  ZonaSelect$C_Zona[ZonaSelect$C_Zona=="27.8.C.E"] <- "27.8.C"
  ZonaSelect$C_Zona[ZonaSelect$C_Zona=="27.8.ABD"] <- "27.8.B"  #porque es la más común
  ZonaSelect$C_Zona[ZonaSelect$C_Zona=="27.6"] <- "27.6.A"  #porque es la más común
  ZonaSelect$C_Zona[ZonaSelect$C_Zona=="27.9"] <- "27.9.A"  #porque es la más común
  
  ZonaSelect$C_Zona[ZonaSelect$C_Zona=="27.8" & !is.na(ZonaSelect$X27.8.C)] <- "27.8.C"
  ZonaSelect$C_Zona[ZonaSelect$C_Zona=="27.8" & !is.na(ZonaSelect$X27.8.B)] <- "27.8.B"
  ZonaSelect$C_Zona[ZonaSelect$C_Zona=="27.8" & !is.na(ZonaSelect$X27.8.A)] <- "27.8.A"
  ZonaSelect$C_Zona[ZonaSelect$C_Zona=="27.8" & !is.na(ZonaSelect$X27.8.D)] <- "27.8.D"
  head(ZonaSelect[ZonaSelect$C_Zona=="27.8",])
  
  idx <- ZonaSelect$IdVenta[ZonaSelect$C_Zona %in% c("27.4", "27.4.A","27.4.C","27.5")]
  head(subset(NV, IdVenta %in% idx)) %>% group_by(Nombre_Buque) %>% summarise (nTrips = Fun_CountUnique(IdVenta))
  # corrijo zonas imposibles y la zona 8 porque no permite distinguir entre 8c y 8abd
  ZonaSelect$C_Zona[ZonaSelect$C_Zona %in% c("27.4", "27.4.A","27.4.C", "27.5", "27.8")] <- NA

  
  # MetierSelect
  ########## #
  MetierSelect <- NV %>% group_by(IdVenta, MetierNV) %>% summarise(Peso_Neto = sum(Peso_Neto, na.rm=T)) %>%
    dcast(IdVenta ~ MetierNV, value.var="Peso_Neto")   
  names(MetierSelect) <- make.names(names(MetierSelect))
   # Crear C_Metier
  dropnames <- c("IdVenta")
  MetierSelect<-as.data.table(MetierSelect)
  MetierSelect$C_Metier <- colnames(MetierSelect[,-dropnames, with =F])[apply(MetierSelect[,-dropnames, with =F],1,which.max)]
  MetierSelect<-as.data.frame(MetierSelect)
  

  
  ############################### #
  #   DO:  Arte               #####
  ############################### #
  # El problema es que en los DO, dentro de la misma marea, podemos tener varias artes
  # Y necesitamos asignar la mara a un solo arte

  
  # MetierSelect
  ########## #
  table(DO$MetierDO)
  
  DO$MetierDO[DO$MetierDO %in% c("Arrastre de fondo (sin especificar)")] <- "TB"
  DO$MetierDO[DO$MetierDO %in% c("Arrastre de fondo a la pareja" )] <- "PTB"
  DO$MetierDO[DO$MetierDO %in% c("Arrastre de fondo con puertas")] <- "OTB"
  DO$MetierDO[DO$MetierDO %in% c("Cacea al currican")] <- "LTL"
  DO$MetierDO[DO$MetierDO %in% c("Cerco con jareta", "Red de cerco con jareta desde dos embarcaciones")] <- "PS"  
  DO$MetierDO[DO$MetierDO %in% c("Líneas de mano y líneas de caña (manuales)")] <- "LHM"
  DO$MetierDO[DO$MetierDO %in% c("Líneas de mano y líneas de caña (mecanizadas)")] <- "LHP"
  DO$MetierDO[DO$MetierDO %in% c("Nasas")] <- "FPO"  
  DO$MetierDO[DO$MetierDO %in% c("Palangre de superficie","Palangres (sin especificar)", "Palangres calados")] <- "LLS"  
  DO$MetierDO[DO$MetierDO %in% c("Redes de enmalle (sin especificar)", "Redes de enmalle de cerco", "Redes de enmalle de fondo (caladas)")] <- "GNS"  
  DO$MetierDO[DO$MetierDO %in% c("Redes de trasmallo (Miños, etc.)")] <- "GTR"  
  
  
  MetierSelectDO <- DO %>% group_by(IdVenta, MetierDO) %>% summarise(Kg_Desemb_Peso_Vivo = sum(Kg_Desemb_Peso_Vivo, na.rm=T)) %>%
    dcast(IdVenta ~ MetierDO, value.var="Kg_Desemb_Peso_Vivo")   
  names(MetierSelectDO) <- make.names(names(MetierSelectDO))
  # Crear C_Metier
  dropnames <- c("IdVenta")
  MetierSelectDO<-as.data.table(MetierSelectDO)
  MetierSelectDO$C_Metier <- colnames(MetierSelectDO[,-dropnames, with =F])[apply(MetierSelectDO[,-dropnames, with =F],1,which.max)]
  MetierSelectDO<-as.data.frame(MetierSelectDO)

  head(MetierSelectDO)
    
################################################################### #
#   Preparar Datos: Nuevas variables                            #####
################################################################### #

#    .Crear variables                           ####
################################################## #

#Censo
DB$Nombre_Buque <- toupper(DB$Nombre_Buque)
  
DB$Censo <- buques$CensoPorModalidad[match(DB$Cod_UE, buques$Codigo.UE)]
  unique(DB$Nombre_Buque[is.na(DB$Censo)])
  unique(DB$Nombre_Buque[DB$Censo==""])
  unique(DB$Censo)
  
DB$Vessel_length <- buques$Eslora [match(DB$Cod_UE, buques$Codigo.UE)]
  unique(DB$Nombre_Buque[is.na(DB$Vessel_length)])
  unique(DB$Pais_Base[is.na(DB$Vessel_length)])
  
#Puertos  
DB$Puerto_Base_CA <- conv_base$origen[match(DB$Puerto_Base, conv_base$puerto)]
  unique(DB$Nombre_Buque[is.na(DB$Puerto_Base_CA)])
  unique(DB$Puerto_Base[is.na(DB$Puerto_Base_CA)])
  unique(DB$Puerto_Base_CA)
  
DB$Pais_Base <- conv_base$origen_cod[match(DB$Puerto_Base, conv_base$puerto)]
  unique(DB$Nombre_Buque[is.na(DB$Pais_Base)])
  unique(DB$Puerto_Base[is.na(DB$Pais_Base)])
  unique(DB$Pais_Base)


#Fecha desembarco
DB$Dia_Desembarco[is.na(DB$Dia_Desembarco)] <- DB$Dia[is.na(DB$Dia_Desembarco)]
DB$Mes_Desembarco[is.na(DB$Mes_Desembarco)] <- DB$Mes[is.na(DB$Mes_Desembarco)]
DB$Trimestre_Desembarco[is.na(DB$Trimestre_Desembarco)] <- DB$Trimestre[is.na(DB$Trimestre_Desembarco)]
DB$Fecha_Desembarco <- as.Date(format(ISOdate(DB$Ano,DB$Mes_Desembarco,DB$Dia_Desembarco),"%d/%m/%Y"),"%d/%m/%Y")

#Trip
DB$Trip <- DB$IdVenta

#Metier codigo
DB$Metier_cod  <- substr(DB$Metier,1,3)

# #Metier (lista LLS y GNS)
# DB$Metier_Principal <- buquesLLSGNS$Metier.principal.2018[match(DB$Cod_UE, buquesLLSGNS$Codigo.UE)] ## necesitamos datos actualizados!
# DB$Metier_Principal[is.na(DB$Metier_Principal)]<- "Otros"

#Zona  DB
DB$ZonaDB <- DB$Zona
DB$Zona [grep("27.8.a|27.8.b|27.8.d|27.8.e",DB$Zona)]<- "27.8.abde"
DB$Zona [grep("27.8.c",DB$Zona)]<- "27.8.c"
DB$Zona [grep("27.7",DB$Zona)]<- "27.7"
DB$Zona [grep("27.6",DB$Zona)]<- "27.6"
DB$Zona [grep("27.9",DB$Zona)]<- "27.9.a"
DB$Zona [grep("27.4",DB$Zona)]<- "27.4"
DB$Zona [grep("27.2",DB$Zona)]<- "27.2"
DB$Zona [grep("27.1",DB$Zona)]<- "27.1"

unique(DB$Zona[is.na(DB$Zona)])
unique(DB$Nombre_Buque[is.na(DB$Zona)])
table(DB$Zona)


#Area Rev y Area NV
DB$Zona_Rev   <- tolower(ZonaSelect$C_Zona[match(DB$IdVenta, ZonaSelect$IdVenta)])
DB$Zona_Rev   <- tolower(ZonaSelect$C_Zona[match(DB$IdVenta, ZonaSelect$IdVenta)])

DB$Zona_Rev[DB$Zona_Rev =="27.8.c"] <- "27.8.c.e"
DB$Zona_Rev[DB$Censo %in% c("ARRASTRE DE FONDO EN CANTABRICO NW") & !is.na(DB$Zona_Rev) & DB$Zona_Rev!= "27.8.c.e"] <- "27.8.c.e" 
DB$Zona_Rev[DB$Censo %in% c("ARTES MENORES EN CANTABRICO NW") & !is.na(DB$Zona_Rev) & DB$Zona_Rev %in% c("27.6.a", "27.7")] <- NA

DB$ZonaNV   <- tolower(ZonaSelect$C_Zona[match(DB$IdVenta, ZonaSelect$IdVenta)])
DB$ZonaNV [grep("27.8.a|27.8.b|27.8.d|27.8.e",DB$ZonaNV)]<- "27.8.abde"
DB$ZonaNV [grep("27.8.c",DB$ZonaNV)]<- "27.8.c"
DB$ZonaNV [grep("27.7",DB$ZonaNV)]<- "27.7"
DB$ZonaNV [grep("27.6",DB$ZonaNV)]<- "27.6"
DB$ZonaNV [grep("27.9",DB$ZonaNV)]<- "27.9.a"
DB$ZonaNV [grep("27.2",DB$ZonaNV)]<- "27.2"
DB$ZonaNV [grep("27.1",DB$ZonaNV)]<- "27.1"
unique(DB$ZonaNV) 
  # corrijo NA
  DB$ZonaNV[is.na(DB$ZonaNV)] <- DB$Zona[is.na(DB$ZonaNV)]


# Metier NV
DB$MetierNV <- MetierSelect$C_Metier[match(DB$IdVenta, MetierSelect$IdVenta)]
#

# Metier DO
DB$MetierDO <- MetierSelectDO$C_Metier[match(DB$IdVenta, MetierSelectDO$IdVenta)]
#DB$MetierDO <- NA # Igualar a NA si todavia no tenemos los DO


# Metier Principal y secundario
DB$MetierPrincipal <- buques$Metier.principal[match(DB$Cod_UE, buques$Codigo.UE)]
DB$MetierSecundario <- buques$Metier.secundario[match(DB$Cod_UE, buques$Codigo.UE)]

subset(DB, is.na(DB$MetierPrincipal))



#   
sum(DB$Kg_Desemb_Peso_Vivo, na.rm=TRUE)       # 45590178


#    .Depuración especies                       ####
################################################## #

# rapes lophiidae (ANF) a rapes lophius (MNZ)
# rayas spp (SKA) a rajidae (RAJ)
# calamares loligo (SQc) a calamares loliginidae (SQZ)
# varios/variado a peces marinos (MZZ)
# camarones a camarones palaemon
# escorpaenidae (SCO) a rascacies (SCS)


unique(DB$Especie_Oficial[grep("rape",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("calama",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("raya",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("scor",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("scy",DB$Especie_Cientifico, ignore.case = TRUE)])
unique(DB$Especie_Cientifico[grep("scy",DB$Especie_Cientifico, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("vari",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("camaro",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("cabra",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_ALFA3[grep("cabra",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("alg",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("gelidi",DB$Especie_Oficial, ignore.case = TRUE)])

# alguna corrección (pero hay que corregirlo en la BD)
DB$Especie_Oficial[DB$Especie_Oficial %in% c("Rapes Lophiidae")] <- "Rapes Lophius"
DB$Especie_ALFA3[DB$Especie_Oficial %in% c("Rapes Lophius")] <- "MNZ"
DB$Especie_Oficial[DB$Especie_Oficial %in% c("Calamares Loligo spp.")] <- "Calamares Loliginidae"
DB$Especie_ALFA3[DB$Especie_Oficial %in% c("Calamares Loliginidae")] <- "SQZ"
DB$Especie_Oficial[DB$Especie_Oficial %in% c("Rayas spp", "Raya de Murray (DEA)", "Raya leopardus (N1V)",
                                             "Raya boca de rosa (DEA)", "Raya de espejos (N1V)", 
                                             "Raya bramante (DEA)")] <- "Rayas (Rajidae)"
DB$Especie_ALFA3[DB$Especie_Oficial %in% c("Rayas (Rajidae)")] <- "RAJ"
DB$Especie_Oficial[DB$Especie_Oficial %in% c("Algas nep (DEA-N1V)", "Algas rojas (DEA)")] <- "Alga Imperial (Gelidium)"
DB$Especie_ALFA3[DB$Especie_Oficial %in% c("Alga Imperial (Gelidium)")] <- "GEQ"



#   ...........................................................  #### 
################################################################### #
#   * BUQUES EXTRANJEROS                                        #####
################################################################### #


DB_extr<- subset(DB, !DB$Pais_Base %in% c("ARM", "BER", "BIO", "DON", "ESP", "GET", "HON", "LEK", "MUN",
                                    "MUT", "OND", "ORI", "PAS", "PLE", "SAN", "ZIE") )                                    
                                    
head(DB_extr)
table(DB_extr$MetierPrincipal)
sort(unique(DB_extr$Puerto_Base))
sort(unique(DB_extr$Pais_Base))
#   ...........................................................  #### 
################################################################### #
#   * ALTURA (PTB, OTB) *                                       #####
################################################################### #

DB_alt<- subset(DB, substr(DB$MetierPrincipal, 1, 3) %in% c("OTB", "PTB") )
DB_alt<- subset(DB_alt, DB_alt$Pais_Base %in% c("ARM", "BER", "BIO", "DON", "ESP", "GET", "HON", "LEK", "MUN",
                                          "MUT", "OND", "ORI", "PAS", "PLE", "SAN", "ZIE") )                                    
#@esta separacion de buques españoles se podria mejorar accediendo directamente  a maestros en la BD
head(DB_alt)
table(DB_alt$MetierPrincipal)
table(DB_alt$Censo)
table(DB_alt$Censo,DB_alt$MetierPrincipal)

#separo OTB de PTB
otb   <- subset(DB_alt, substr(DB_alt$MetierPrincipal, 1, 3) %in% c("OTB") )
ptb   <- subset(DB_alt, substr(DB_alt$MetierPrincipal, 1, 3) %in% c("PTB"))

#    .Exploración de datos                      ####
################################################## #

tapply(otb$Trip, list(otb$Zona, otb$Metier), Fun_CountUnique)
tapply(otb$Trip, list(otb$ZonaNV, otb$Metier), Fun_CountUnique)
tapply(otb$Trip, list(otb$Puerto_Base, otb$Metier), Fun_CountUnique)

tapply(ptb$Trip, list(ptb$Zona, ptb$Metier), Fun_CountUnique)
tapply(ptb$Trip, list(ptb$Puerto_Base, ptb$Metier), Fun_CountUnique)
#tapply(ptb$Trip, list(ptb$Censo, ptb$Metier), Fun_CountUnique)

temp<- subset(DB_alt, Puerto_Base=="Ondarroa" & Metier_cod %in% c("OTB", "PTB"))
tapply(temp$Trip, list(temp$Nombre_Buque, temp$Metier_cod), Fun_CountUnique)

temp<- subset(temp, Nombre_Buque %in% c("GURE GASKUNA", "GURE KANTABRIKO"))
tapply(temp$Trip, list(temp$Fecha_Desembarco, temp$Metier_cod), Fun_CountUnique)
tapply(temp$Trip, list(temp$Fecha_Desembarco, temp$Metier_cod, temp$Nombre_Buque), Fun_CountUnique)

temp<- subset(DB_alt, Puerto_Base=="Ondarroa" )
tapply(temp$Trip, list(temp$Nombre_Buque, temp$MetierPrincipal), Fun_CountUnique)

#   ..........................................  ####
#    PTB                                       ####
################################################## #

#    .Asignar especies                          ####
################################################## #
ptb$SpGroup <- NA

#    .Agrupar por marea                         ####
################################################## #
ptb_met     <- ptb %>% group_by(Nombre_Buque, Puerto_Base, Censo, Trip, Fecha_Desembarco, Dia, Mes, Ano, ZonaDB, Zona, Zona_Rev, ZonaNV, Metier, MetierPrincipal, MetierSecundario) %>% 
                        summarise(Peso = sum(Kg_Desemb_Peso_Vivo, na.rm=T)) %>%
                        data.frame()


#    .Asignar metiers                           ####
################################################## #

ptb_met$Metier_Rev <- NA

#    ..VIIIc     ####
ptb_met$Metier_Rev[ptb_met$Censo %in% c("ARRASTRE DE FONDO EN CANTABRICO NW")] <- "PTB_MPD_>=55_0_0"

  sort(unique(ptb_met$Puerto_Base [ptb_met$Metier %in% c("PTB_MPD_>=55_0_0")]))
  sort(unique(ptb_met$Nombre_Buque[ptb_met$Metier %in% c("PTB_MPD_>=55_0_0")]))

#    ..VIIIabd   ####    
ptb_met$Metier_Rev[ptb_met$Censo %in% c("ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII Y VIIIABDE")] <- "PTB_DEF_>=70_0_0"
  
  sort(unique(ptb_met$Puerto_Base [ptb_met$Metier %in% c("PTB_DEF_>=70_0_0")]))
  sort(unique(ptb_met$Nombre_Buque[ptb_met$Metier %in% c("PTB_DEF_>=70_0_0")]))

#    .Variable Check                            ####
################################################## #
#     Crear variable Metier check  
ptb_met$Metier_Check <- ptb_met$Metier==ptb_met$Metier_Rev


#    .Chequeos                                  ####
################################################## #
head(ptb_met[ptb_met$Metier_Check==FALSE,])

# NAs
ptb_met[is.na(ptb_met$Metier_Rev),]
  

#    .Guardar ficheros                          ####
################################################## #
write.table(ptb_met, paste("Depuracion_Metiers\\Output\\", Ano, Fuente,"_porMarea_PTB_detail.csv", sep=""), row.names = FALSE, sep=";", dec=",")


    
#   ..........................................  ####  
#    OTB                                       ####
################################################## #


#    .Asignar especies                          ####
################################################## #

otb$SpGroup<- spOTB$Grupo[match(otb$Especie_ALFA3,spOTB$Cod.ALFA.3)]
otb$SpGroup[is.na(otb$SpGroup)] <- "Otras"

    # alguna comprobacion de que no nos dejamos ningun sp importante fuera
    sp   <- summaryBy(Kg_Desemb_Peso_Vivo ~ Especie_Oficial + SpGroup,
                       data=otb, FUN=sum, na.rm=TRUE)
    temp <- subset(sp, SpGroup=="Otras") %>% arrange(-Kg_Desemb_Peso_Vivo.sum)
    sp   <- sp %>% arrange(SpGroup, -Kg_Desemb_Peso_Vivo.sum)
    sp

    
#    .Agrupar por marea                         ####
################################################## #
otb_met     <- otb %>% group_by(Nombre_Buque, Puerto_Base, Censo, Trip, Fecha_Desembarco, Dia, Mes, Ano, ZonaDB, Zona, Zona_Rev, ZonaNV, 
                                Metier, MetierPrincipal, MetierSecundario, SpGroup) %>% 
                          summarise(Peso = sum(Kg_Desemb_Peso_Vivo, na.rm=T)) %>%
                          pivot_wider(names_from = SpGroup, values_from = Peso) %>%
                          data.frame()    
otb_met$Demersales [is.na(otb_met$Demersales)] <- 0
otb_met$Mixto [is.na(otb_met$Mixto)] <- 0
otb_met$Otras [is.na(otb_met$Otras)] <- 0
otb_met$Pelagico [is.na(otb_met$Pelagico)] <- 0

otb_met$Total <- otb_met$Demersales + otb_met$Pelagico + otb_met$Mixto +  otb_met$Otras
otb_met$P_dem <- otb_met$Demersales /  otb_met$Total 
otb_met$P_pel <- otb_met$Pelagico /  otb_met$Total 
otb_met$P_mix <- otb_met$Mixto /  otb_met$Total 
otb_met$P_otr <- otb_met$Otras /  otb_met$Total 

head(otb_met)

otb_met   <- otb_met %>% mutate_if(is.numeric, round, digits = 2)


#    .Asignar metiers                           ####
################################################## #

otb_met$Metier_Rev <- NA

#    ..VIIIc     ####
  otb_met$Metier_Rev[otb_met$Censo %in% c("ARRASTRE DE FONDO EN CANTABRICO NW")] <- "OTB_DEF_>=55_0_0"
  otb_met$Metier_Rev[otb_met$Metier_Rev %in% c("OTB_DEF_>=55_0_0") & otb_met$P_pel>=0.9 ] <- "OTB_SPF_>=55_0_0"
    
    sort(unique(otb_met$Puerto_Base [otb_met$Metier %in% c("OTB_DEF_>=55_0_0","OTB_SPF_>=55_0_0")]))
    sort(unique(otb_met$Nombre_Buque[otb_met$Metier %in% c("OTB_DEF_>=55_0_0","OTB_SPF_>=55_0_0")]))
   
    subset(otb_met, otb_met$Metier %in% c("OTB_DEF_>=55_0_0","OTB_SPF_>=55_0_0") & Puerto_Base=="Pasaia")
    
#    ..VII       ####
  otb_met$Metier_Rev[otb_met$Censo %in% c("ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII Y VIIIABDE") & otb_met$ZonaNV %in% c("27.7" )] <- "OTB_DEF_70-99_0_0"
    sort(unique(otb_met$Puerto_Base[otb_met$Metier_Rev=="OTB_DEF_70-99_0_0"]))
  
#    ..VI        ####
  otb_met$Metier_Rev[otb_met$Censo %in% c("ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII Y VIIIABDE") & otb_met$Censo %in% c("ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII Y VIIIABDE") & otb_met$ZonaNV %in% c("27.6")] <- "OTB_DEF_100-119_0_0"
    sort(unique(otb_met$Puerto_Base[otb_met$Metier_Rev=="OTB_DEF_100-119_0_0"]))

#    ..VIIIabd   ####
  otb_met$Metier_Rev[otb_met$Censo %in% c("ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII Y VIIIABDE") & otb_met$ZonaNV %in% c("27.8.abde")] <- "OTB_DEF_>=70_0_0"
  otb_met$Metier_Rev[otb_met$Metier_Rev =="OTB_DEF_>=70_0_0" & otb_met$P_pel>0.8] <- "OTB_SPF_>=70_0_0"
  otb_met$Metier_Rev[otb_met$Metier_Rev =="OTB_DEF_>=70_0_0" & otb_met$P_pel<=0.8 & otb_met$P_pel>=0.5 ] <- "OTB_MPD_>=70_0_0"
  otb_met$Metier_Rev[otb_met$Metier_Rev =="OTB_DEF_>=70_0_0" & otb_met$P_mix>=0.25] <- "OTB_MCF_>=70_0_0"
  otb_met$Metier_Rev[otb_met$Metier_Rev =="OTB_DEF_>=70_0_0" & otb_met$P_mix<0.25 & otb_met$P_dem>=0.5 ] <- "OTB_DEF_>=70_0_0"
  otb_met$Metier_Rev[otb_met$Metier_Rev =="OTB_DEF_>=70_0_0" & otb_met$P_mix<0.2] <- "OTB_DEF_>=70_0_0"
  


#    ..II        ####
otb_met$Metier_Rev[otb_met$Censo %in% c("BACALADEROS") ] <- "OTB_DEF_>=120_0_0"
sort(unique(otb_met$Puerto_Base[otb_met$Metier_Rev=="OTB_DEF_>=120_0_0"]))


#    .Variable Check                            ####
################################################## #

  #     Crear variable Metier check  
  otb_met$Metier_Check <- otb_met$Metier==otb_met$Metier_Rev


#    .Chequeos                                  ####
################################################## #
# NAs
otb_met[is.na(otb_met$Metier_Rev),]
head(otb_met[otb_met$Metier_Check==FALSE,])


subset(otb_met, Nombre_Buque=="GURE GASKUNA")
subset(otb_met, Nombre_Buque=="INTXORTAMENDI")


# .Guardar ficheros                          ####
write.table(otb_met, paste("Depuracion_Metiers\\Output\\", Ano, Fuente,"_porMarea_OTB_detail.csv", sep=""), row.names = FALSE, sep=";", dec=",")


#   ...........................................................  #### 
################################################################### #
#   * BAJURA Y ARTESANAL *                                      #####
################################################################### #

DBba<- subset(DB, !Censo %in% c("ARRASTRE DE FONDO EN CANTABRICO NW", "ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII Y VIIIABDE") )
DBba<- subset(DBba, Pais_Base %in% c("ARM", "BER", "BIO", "DON", "GET", "HON", "LEK", "MUN",
                                      "MUT", "OND", "ORI", "PAS", "PLE", "SAN", "ZIE",
                                      "ESP") )



unique(DB$Puerto_Base)[!unique(DB$Puerto_Base) %in% unique(DBba$Puerto_Base)]

#    .Asignar especies                          ####
################################################## #

sort(unique(DBba$Censo))
sort(unique(DBba$Zona))

DBba$SpGroup <- NA
DBba$SpGroup <- spAM$Grupo[match(DBba$Especie_ALFA3 , spAM$Cod.ALFA.3)]
DBba$SpGroup[is.na(DBba$SpGroup)] <- "Otras"

  
  # alguna comprobacion de que no nos dejamos ningun sp importante fuera
  sp   <- summaryBy(Kg_Desemb_Peso_Vivo ~ Especie_Oficial + SpGroup,
                    data=DBba, FUN=sum, na.rm=TRUE)
  temp <- subset(sp, SpGroup=="Otras") %>% arrange(-Kg_Desemb_Peso_Vivo.sum)
  sp   <- sp %>% arrange(SpGroup, -Kg_Desemb_Peso_Vivo.sum)
  sp



#    .Agrupar por marea                         ####
################################################## #

# calcular el número de especies por marea
db_sp<- DBba %>% arrange(Nombre_Buque, Fecha_Desembarco,Puerto_Venta) %>%
                group_by(Trip) %>%
                mutate(Nsp=length(unique(Especie_ALFA3))) %>%
                ungroup() 

# agregar por marea
db_sp<-   db_sp %>% group_by(Nombre_Buque , Cod_UE , Eslora_total , Censo , Puerto_Base , Puerto_Base_CA  , Fecha_Desembarco , 
                         Trip ,  ZonaDB, Zona, Zona_Rev, ZonaNV, Metier , MetierNV, MetierDO, MetierPrincipal, MetierSecundario, Nsp , SpGroup) %>%
                    summarise(Peso=sum(Kg_Desemb_Peso_Vivo, na.rm=TRUE))

db_met <- dcast(db_sp,  Nombre_Buque  + Eslora_total + Censo + Puerto_Base + Puerto_Base_CA  + Fecha_Desembarco + 
                  Trip +  Metier + MetierNV + MetierDO +MetierPrincipal + MetierSecundario + ZonaDB + Zona + Zona_Rev + ZonaNV + Nsp  ~ SpGroup,
                fill=0, value.var = "Peso") #el fill=0 hace que en vez de poner NA a las celdas vac?as las rellene como = 0 que es lo real.

vars <- unique(spAM$Grupo)
db_met[setdiff(vars,names(db_met))] <- 0

db_met <- as.data.frame(db_met)


db_met$Total <- db_met$CEP + db_met$CRU + db_met$GNS  + db_met$GTR + db_met$HKE + db_met$LHM + 
                db_met$LLS + db_met$MIS +  db_met$MOL + db_met$TUN + db_met$LPF + db_met$DWS + db_met$Otras 
db_met$P_CEP <-  db_met$CEP /  db_met$Total 
db_met$P_CRU <-  db_met$CRU /  db_met$Total 
db_met$P_GNS <-  db_met$GNS /  db_met$Total 
db_met$P_GTR <-  db_met$GTR /  db_met$Total
db_met$P_HKE <-  db_met$HKE /  db_met$Total 
db_met$P_LHM <-  db_met$LHM /  db_met$Total 
db_met$P_LLS <-  db_met$LLS /  db_met$Total 
db_met$P_MIS <-  db_met$MIS /  db_met$Total 
db_met$P_MOL <-  db_met$MOL /  db_met$Total 
db_met$P_TUN <-  db_met$TUN /  db_met$Total 
db_met$P_LPF <-  db_met$LPF /  db_met$Total 
db_met$P_DWS <-  db_met$DWS /  db_met$Total 
db_met$P_otr <-  db_met$Otras /  db_met$Total

db_met$P_Total <- db_met$P_CEP + db_met$P_CRU + db_met$P_GNS + db_met$P_GTR + db_met$P_HKE + db_met$P_LHM + 
                  db_met$P_LLS + db_met$P_MIS + db_met$P_MOL + db_met$P_TUN + db_met$P_LPF+ db_met$P_DWS  +db_met$P_otr

db_met$P2_GNS <-  (db_met$P_HKE + db_met$P_LHM + db_met$P_GNS) 
db_met$P2_LLS <-  (db_met$P_HKE + db_met$P_LLS) 

#db_met<- db_met %>% mutate_if( is.numeric, function(x){round(x,digits=2)})
db_met   <- db_met %>% mutate_if(is.numeric, round, digits = 2)

unique(db_met$P_Total)

# corregimos NaN
subset(db_met, is.nan(P_Total))
db_met <- db_met %>% mutate_at(.vars = vars(contains("P_")), replace_na, 0)
db_met <- db_met %>% mutate_at(.vars = vars(contains("P2_")), replace_na, 0)

#   ..........................................  ####  
#    ARTES FIJAS ALTURA                         ####
################################################## #

#    .Crear bd                                  ####
################################################## #
af_met<- subset(db_met, db_met$Censo %in% c("ARTES FIJAS EN ZONAS CIEM VB, VI,VII Y VIIIABDE"))

sort(unique(af_met$Censo))
sort(unique(af_met$Puerto_Base_CA))
sort(unique(af_met$Metier))
sort(unique(af_met$ZonaNV))
sort(unique(af_met$MetierNV))
table(unique(af_met$MetierDO))

#    .Asignar metier                            ####
################################################## #
sort(unique(af_met$Metier))
af_met$Metier_Rev<-NA

af_met$Metier_Rev  <- "LLS_DEF_0_0_0"
#af_met$Metier_Rev[ af_met$MetierNV == "GNS"]  <- "GNS_DEF_>=100_0_0"
af_met$Metier_Rev[ af_met$MetierDO == "GNS"]  <- "GNS_DEF_>=100_0_0"
af_met$Metier_Rev[ af_met$MetierPrincipal == "GNS_DEF_>=100_0_0"]  <- "GNS_DEF_>=100_0_0"

#    .Variable Check                            ####
################################################## #
#Metier_Check
af_met$Metier_Check <- af_met$Metier==af_met$Metier_Rev 

#rescatar marea anterior y posterior
af_met <- af_met %>% group_by(Nombre_Buque) %>% mutate(PrevMet=lag(Metier_Rev), NextMet=lead(Metier_Rev)) %>%  
  ungroup() %>% data.frame
subset(af_met, Metier_Rev!=PrevMet & Metier_Rev!=NextMet )
subset(af_met, Metier_Rev!=PrevMet & Metier_Rev!=NextMet )


#    .Chequeos                                  ####
################################################## #
# NA
af_met[is.na(af_met$Metier_Rev),]
head(subset(af_met, Metier_Check==FALSE))

# Other checkings
temp <- DBba %>% filter(Trip %in% af_met$Trip)
temp$Metier_Rev <- af_met$Metier_Rev[match(temp$Trip,af_met$Trip)]
temp$Metier_Check <- temp$Metier==temp$Metier_Rev 
head(subset(temp, Metier!=Metier_Rev))

SpSum <- summaryBy(Kg_Desemb_Peso_Vivo~Metier_Rev +Especie_Oficial, data=temp, FUN=sum, na.rm=TRUE)
SpSum <- arrange(SpSum, Metier_Rev, desc( Kg_Desemb_Peso_Vivo.sum))
SpSum

# CheckTripId<- unique(temp$Trip[temp$Especie_Oficial!="Verdel - Caballa" & temp$Metier_Rev=="LHM_SPF_0_0_0"])
# subset(temp, Trip %in% CheckTripId)
# subset(temp, Metier!=Metier_Rev)


#   ..........................................  ####  
#    RASCO Y VOLANTA                           ####
################################################## #

#    .Crear bd                                  ####
################################################## #
rv_met<- subset(db_met, db_met$Censo %in% c("RASCO EN CANTABRICO NW","VOLANTA EN CANTABRICO NW"))

sort(unique(rv_met$Censo))
sort(unique(rv_met$Puerto_Base_CA))
sort(unique(rv_met$Metier))
sort(unique(rv_met$ZonaNV))
sort(unique(rv_met$MetierNV))
sort(unique(rv_met$MetierDO))

#    .Asignar metier                            ####
################################################## #
sort(unique(rv_met$Metier))
rv_met$Metier_Rev<-NA
rv_met$Metier_Rev[rv_met$P_LHM>0.80]  <- "LHM_SPF_0_0_0"
rv_met$Metier_Rev[is.na(rv_met$Metier_Rev) & rv_met$P_TUN>0.80]  <- "LTL_LPF_0_0_0"
rv_met$Metier_Rev[is.na(rv_met$Metier_Rev) & rv_met$Zona %in% c("27.8.abde", "27.8.c" )]  <- "GNS_DEF_>=100_0_0"
rv_met$Metier_Rev[is.na(rv_met$Metier_Rev) & rv_met$Puerto_Base_CA %in% c("Euskadi") &
                    rv_met$Zona %in% c("27.7","27.6" )]  <- "GNS_DEF_100-119_0_0"
rv_met[is.na(rv_met$Metier_Rev) & !rv_met$Puerto_Base_CA %in% c("Euskadi") &
                    rv_met$Zona %in% c("27.7","27.6" ),]

#    .Variable Check                            ####
################################################## #
#Metier_Check
rv_met$Metier_Check <- rv_met$Metier==rv_met$Metier_Rev 

# rescatar marea anterior y posterior
rv_met <- rv_met %>% group_by(Nombre_Buque) %>% mutate(PrevMet=lag(Metier_Rev), NextMet=lead(Metier_Rev)) %>%  ungroup() %>% data.frame()
subset(rv_met, Metier_Rev!=PrevMet & Metier_Rev!=NextMet)
# marcar para chequear
rv_met$Metier_Check[rv_met$Metier_Rev != rv_met$PrevMet & rv_met$Metier_Rev != rv_met$NextMet & rv_met$Metier_Rev=="LHM_SPF_0_0_0"] <- "Check"
rv_met$Metier_Check[rv_met$PrevMet == rv_met$NextMet & rv_met$Metier_Rev!= rv_met$NextMet & rv_met$NextMet == "LHM_SPF_0_0_0"] <- "Check"
rv_met$Metier_Check[rv_met$Metier_Rev != rv_met$PrevMet & rv_met$Metier_Rev != rv_met$NextMet & rv_met$Metier_Rev=="LTL_LPF_0_0_0"] <- "Check"
rv_met$Metier_Check[rv_met$PrevMet == rv_met$NextMet & rv_met$Metier_Rev!= rv_met$NextMet & rv_met$NextMet == "LTL_LPF_0_0_0"] <- "Check"



#    .Chequeos                                  ####
################################################## #
# NA
rv_met[is.na(rv_met$Metier_Rev),]
head(subset(rv_met, Metier_Check==FALSE))

# Other checkings
temp <- DBba %>% filter(Trip %in% rv_met$Trip)
temp$Metier_Rev <- rv_met$Metier_Rev[match(temp$Trip,rv_met$Trip)]
temp$Metier_Check <- temp$Metier==temp$Metier_Rev 
head(subset(temp, Metier!=Metier_Rev))

SpSum <- summaryBy(Kg_Desemb_Peso_Vivo~Metier_Rev +Especie_Oficial, data=temp, FUN=sum, na.rm=TRUE)
SpSum <- arrange(SpSum, Metier_Rev, desc( Kg_Desemb_Peso_Vivo.sum))
SpSum

CheckTripId<- unique(temp$Trip[temp$Especie_Oficial!="Verdel - Caballa" & temp$Metier_Rev=="LHM_SPF_0_0_0"])
subset(temp, Trip %in% CheckTripId)
subset(temp, Metier!=Metier_Rev)


#   ..........................................  ####  
#    PALANGRE BAJURA                           ####
################################################## #

#    .Crear bd                                  ####
################################################## #
pa_met<- subset(db_met, db_met$Censo %in% c("PALANGRE DE FONDO EN CANTABRICO NW", 
                                            "PALANGRE DE FONDO MENORES 100 TRB EN VIIIABDE",
                                            "PALANGRE DE SUPERFICIE CALADERO NACIONAL"))

sort(unique(pa_met$Censo))
sort(unique(pa_met$Puerto_Base_CA))
sort(unique(pa_met$Metier))
sort(unique(pa_met$ZonaNV))

#    .Asignar metier                            ####
################################################## #
sort(unique(pa_met$Metier))
pa_met$Metier_Rev<-NA
pa_met$Metier_Rev[pa_met$P_LHM>0.80]  <- "LHM_SPF_0_0_0"
pa_met$Metier_Rev[is.na(pa_met$Metier_Rev) & pa_met$P_TUN>0.80]  <- "LTL_LPF_0_0_0"
pa_met$Metier_Rev[is.na(pa_met$Metier_Rev) & pa_met$P_MIS>0.80]  <- "MIS_MIS_0_0_0_HC"
pa_met$Metier_Rev[is.na(pa_met$Metier_Rev) & pa_met$P_LPF>0.85]  <- "LLS_LPF_0_0_0"
pa_met$Metier_Rev[is.na(pa_met$Metier_Rev) & pa_met$P_DWS>0.85]  <- "LLS_DWS_0_0_0"
pa_met$Metier_Rev[is.na(pa_met$Metier_Rev)]  <- "LLS_DEF_0_0_0"

# excepciones: no están censados en artes menores pero son barcos pequeños
# pa_met$Metier_Rev[pa_met$Nombre_Buque %in% c("KALA BERRI", "EL DAVID", "MAR DE PEDRO") &
#                     pa_met$Metier_Rev == "LLS_DEF_<24LOA"] <- "LLS_DEF_<=1000"  

#    .Variable Check                            ####
################################################## #
# Metier_Check
pa_met$Metier_Check <- pa_met$Metier==pa_met$Metier_Rev 


# rescatar marea anterior y posterior
pa_met <- pa_met %>% group_by(Nombre_Buque) %>% mutate(PrevMet=lag(Metier_Rev), NextMet=lead(Metier_Rev)) %>%  ungroup() %>% data.frame()
subset(pa_met, Metier_Rev!=PrevMet & Metier_Rev!=NextMet & pa_met$Metier_Rev=="LHM_SPF_0_0_0")
subset(pa_met, Metier_Rev!=PrevMet & Metier_Rev!=NextMet & pa_met$Metier_Rev=="LTL_LPF_0_0_0")
# marcar para chequear
pa_met$Metier_Check[pa_met$Metier_Rev != pa_met$PrevMet & pa_met$Metier_Rev != pa_met$NextMet & pa_met$Metier_Rev=="LHM_SPF_0_0_0"] <- "Check"
pa_met$Metier_Check[pa_met$PrevMet == pa_met$NextMet & pa_met$Metier_Rev!= pa_met$NextMet & pa_met$NextMet == "LHM_SPF_0_0_0"] <- "Check"
pa_met$Metier_Check[pa_met$Metier_Rev != pa_met$PrevMet & pa_met$Metier_Rev != pa_met$NextMet & pa_met$Metier_Rev=="LTL_LPF_0_0_0"] <- "Check"
pa_met$Metier_Check[pa_met$PrevMet == pa_met$NextMet & pa_met$Metier_Rev!= pa_met$NextMet & pa_met$NextMet == "LTL_LPF_0_0_0"] <- "Check"



#    .Chequeos                                  ####
################################################## #
#NA
pa_met[is.na(pa_met$Metier_Rev),]

# Other checkings
temp <- DBba %>% filter(Trip %in% pa_met$Trip)
temp$Metier_Rev <- pa_met$Metier_Rev[match(temp$Trip,pa_met$Trip)]
temp$Metier_Check <- temp$Metier==temp$Metier_Rev 
head(subset(temp, Metier!=Metier_Rev))

SpSum <- summaryBy(Kg_Desemb_Peso_Vivo~Metier_Rev +Especie_Oficial, data=temp, FUN=sum, na.rm=TRUE)
SpSum <- arrange(SpSum, Metier_Rev, desc( Kg_Desemb_Peso_Vivo.sum))
SpSum

subset(temp, Metier!=Metier_Rev)

CheckTripId<- unique(temp$Trip[temp$Especie_Oficial!="Verdel - Caballa" & temp$Metier_Rev=="LHM_SPF_0_0_0"])
subset(temp, Trip %in% CheckTripId)

CheckTripId<- unique(temp$Trip[temp$Especie_Oficial %in% c("Percebe") & temp$Metier_Rev=="LLS_DEF_0_0_0"])
subset(temp, Trip %in% CheckTripId)

CheckTripId<- unique(temp$Trip[temp$MetierNV %in% c("LHM") & temp$Metier_Rev=="LLS_DEF_0_0_0"])
subset(temp, Trip %in% CheckTripId)


#   ..........................................  ####  
#    ARTES MENORES                             ####
################################################## #

#    .Crear bd                                  ####
################################################## #
am_met<- subset(db_met, db_met$Censo %in% c("ARTES MENORES EN CANTABRICO NW"))

sort(unique(am_met$Censo))
sort(unique(am_met$Puerto_Base_CA))
sort(unique(am_met$Metier))
sort(unique(am_met$ZonaNV))
sort(unique(am_met$MetierNV))


#excepciones: están censados en artes menores pero son barcos grandes
# id <- c("CANALECHEBARRIA", "IZURDIA MAITEA", "OSTARTE (EX KREXAL)", "OSTARTE (EX KREXAL)", "GURE AMA MARTINA",
#         "BETI BEGONAKO AMA (EX BETI BARRENETXEA)")

#    .Asignar metier                            ####
################################################## #
sort(unique(am_met$Metier))
am_met$Metier_Rev<-NA

#lineas de mano
am_met$Metier_Rev[am_met$P_LHM>0.99 ]  <- "LHM_SPF_0_0_0"
am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$P_LHM>0.90 & am_met$LHM>1000]  <- "LHM_SPF_0_0_0"
am_met[is.na(am_met$Metier_Rev) & am_met$P_LHM>0.80 & am_met$LHM>1000, ] 
am_met[is.na(am_met$Metier_Rev) & am_met$P_LHM>0.80 & am_met$LHM>300, ] 

#cacea
am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$P_TUN>0.80]  <- "LTL_LPF_0_0_0"

#algas/anemonas
am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$P_MIS >0.9]  <- "MIS_MIS_0_0_0_HC"

#nasas
am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$P_CRU>0.75 ] <- "FPO_CRU_0_0_0"
am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$P_MOL>0.75 ] <- "FPO_MOL_0_0_0"

am_met$Metier_Rev[is.na(am_met$Metier_Rev) & (am_met$P_MOL+ am_met$P_CRU)>0.75  & am_met$P_MOL>am_met$P_CRU] <- "FPO_MOL_0_0_0"
am_met$Metier_Rev[is.na(am_met$Metier_Rev) & (am_met$P_MOL+ am_met$P_CRU)>0.75  & am_met$P_MOL<am_met$P_CRU] <- "FPO_CRU_0_0_0"
am_met$Metier_Rev[is.na(am_met$Metier_Rev) & (am_met$P_MOL+ am_met$P_CRU)>0.75  & am_met$P_MOL==am_met$P_CRU] <- "FPO_CRU_0_0_0"

#lineas de mano a calamares
am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$P_CEP>0.5]  <- "LHM_CEP_0_0_0"

#redes y palangres

  # 
  # # basado en el metier de NV
  # # am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$MetierNV=="LLS"  ] <- "LLS_DEF_0_0_0"

  # # basado en la composición específica y en el metier de Maestros
  # temp <- am_met[is.na(am_met$Metier_Rev) & am_met$P2_LLS>0.5 & (am_met$MetierPrincipal=="LLS_DEF_0_0_0" | am_met$MetierSecundario=="LLS_DEF_0_0_0") , ]
  # subset(temp, Metier %in% c("GNS_DEF_80-99_0_0", "GNS_DEF_60-79_0_0", "GTR_DEF_60-79_0_0"))
  # temp <- am_met[is.na(am_met$Metier_Rev) & !( am_met$P2_LLS>0.5 & (am_met$MetierPrincipal=="LLS_DEF_0_0_0" | am_met$MetierSecundario=="LLS_DEF_0_0_0") ), ]
  # subset(temp, Metier=="LLS_DEF_0_0_0")
  # #am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$P2_LLS>0.5 & (am_met$MetierPrincipal=="LLS_DEF_0_0_0" | am_met$MetierSecundario=="LLS_DEF_0_0_0")  ] <- "LLS_DEF_0_0_0"
  
  # basado en el Metier de Datos Oficiales (1)
  #explorar
    temp <- (am_met[is.na(am_met$Metier_Rev) & am_met$MetierDO %in% c("LLS"),])  
    subset(temp, Metier %in% c("GNS_DEF_80-99_0_0", "GNS_DEF_60-79_0_0", "GTR_DEF_60-79_0_0"))
    temp <- (am_met[is.na(am_met$Metier_Rev) & am_met$MetierDO %in% c("GNS"),])  
    subset(temp, Metier %in% c("LLS_DEF_0_0_0"))
  #aplicar
    am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$MetierDO %in% c("LLS")]  <- "LLS_DEF_0_0_0"
    am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$MetierDO %in% c("GNS")]  <- "GNS_DEF_60-79_0_0"
  

  # basado en la composición específica y en el metier de Maestros (2)
  #explorar
    temp <- am_met[is.na(am_met$Metier_Rev) & am_met$P2_LLS>0.75,]  
      subset(temp, Metier %in% c("GNS_DEF_80-99_0_0", "GNS_DEF_60-79_0_0", "GTR_DEF_60-79_0_0"))
    temp <- (am_met[is.na(am_met$Metier_Rev) & am_met$P_LLS>(am_met$P_GNS + am_met$P_GTR) & (am_met$MetierPrincipal=="LLS_DEF_0_0_0" | am_met$MetierSecundario=="LLS_DEF_0_0_0"),])  
      subset(temp, Metier %in% c("GNS_DEF_80-99_0_0", "GNS_DEF_60-79_0_0", "GTR_DEF_60-79_0_0"))
    temp <- (am_met[is.na(am_met$Metier_Rev) & am_met$P_LLS<(am_met$P_GNS + am_met$P_GTR) & (am_met$MetierPrincipal=="LLS_DEF_0_0_0" | am_met$MetierSecundario=="LLS_DEF_0_0_0"),])  
      subset(temp, Metier=="LLS_DEF_0_0_0")
  #aplicar
    am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$P2_LLS>0.75]  <- "LLS_DEF_0_0_0"
    am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$P_LLS>(am_met$P_GNS + am_met$P_GTR) & (am_met$MetierPrincipal=="LLS_DEF_0_0_0" | am_met$MetierSecundario=="LLS_DEF_0_0_0")  ] <- "LLS_DEF_0_0_0"
    
  
am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$Puerto_Base_CA!="Euskadi"  ] <- "GNS_DEF_80-99_0_0"
am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$Puerto_Base_CA=="Euskadi"  ] <- "GNS_DEF_60-79_0_0"

am_met$Metier_Rev[am_met$Metier_Rev=="GNS_DEF_60-79_0_0" & am_met$P2_GNS>=0.5 ] <- "GNS_DEF_60-79_0_0"
am_met$Metier_Rev[am_met$Metier_Rev=="GNS_DEF_60-79_0_0" &  am_met$P_GTR>=0.5 ] <- "GTR_DEF_60-79_0_0"
am_met$Metier_Rev[am_met$Metier_Rev=="GNS_DEF_60-79_0_0" &  am_met$P_GTR>=0.4 & am_met$P2_GNS<0.2] <- "GTR_DEF_60-79_0_0"


#    .Variable Check                            ####
################################################## #
# Metier_Check
am_met$Metier_Check <- am_met$Metier==am_met$Metier_Rev 
subset(am_met, Metier_Check==FALSE)

# rescatar marea anterior y posterior
am_met <- am_met %>% group_by(Nombre_Buque) %>% mutate(PrevMet=lag(Metier_Rev), NextMet=lead(Metier_Rev)) %>%  
  ungroup() %>% data.frame
subset(am_met, Metier_Rev!=PrevMet & Metier_Rev!=NextMet & Metier_Rev=="LHM_SPF_0_0_0")
subset(am_met, Metier_Rev!=PrevMet & Metier_Rev!=NextMet & Metier_Rev=="LTL_LPF_0_0_0")
subset(am_met, Metier_Rev!=PrevMet & Metier_Rev!=NextMet & Metier_Rev=="LLS_DEF_0_0_0")
# marcar para chequear
am_met$Metier_Check[am_met$Metier_Rev != am_met$PrevMet & am_met$Metier_Rev != am_met$NextMet & am_met$Metier_Rev == "LHM_SPF_0_0_0"] <- "Check"
am_met$Metier_Check[am_met$Metier_Rev != am_met$PrevMet & am_met$Metier_Rev != am_met$NextMet & am_met$Metier_Rev == "LHM_CEP_0_0_0"] <- "Check"
am_met$Metier_Check[am_met$Metier_Rev != am_met$PrevMet & am_met$Metier_Rev != am_met$NextMet & am_met$Metier_Rev == "LTL_LPF_0_0_0"] <- "Check"
am_met$Metier_Check[am_met$Metier_Rev != am_met$PrevMet & am_met$Metier_Rev != am_met$NextMet & am_met$Metier_Rev == "LLS_DEF_0_0_0"] <- "Check"
am_met$Metier_Check[am_met$Metier_Rev != am_met$PrevMet & am_met$Metier_Rev != am_met$NextMet & am_met$NextMet != "GTR_DEF_60-79_0_0" & am_met$PrevMet != "GTR_DEF_60-79_0_0" & am_met$Metier_Rev == "GNS_DEF_60-79_0_0"] <- "Check"
am_met$Metier_Check[am_met$Metier_Rev != am_met$PrevMet & am_met$Metier_Rev != am_met$NextMet & am_met$NextMet != "GNS_DEF_60-79_0_0" & am_met$PrevMet != "GNS_DEF_60-79_0_0" & am_met$Metier_Rev == "GTR_DEF_60-79_0_0"] <- "Check"




#    .Chequeos                                  ####
################################################## #
#NA
am_met[is.na(am_met$Metier_Rev),]

# Other checkings
temp <- DBba %>% filter(Trip %in% am_met$Trip)
temp$Metier_Rev <- am_met$Metier_Rev[match(temp$Trip,am_met$Trip)]
temp$Metier_Check <- temp$Metier==temp$Metier_Rev 
head(subset(temp, Metier!=Metier_Rev))

SpSum <- summaryBy(Kg_Desemb_Peso_Vivo~Metier_Rev +Especie_Oficial, data=temp, FUN=sum, na.rm=TRUE)
SpSum <- arrange(SpSum, Metier_Rev, desc( Kg_Desemb_Peso_Vivo.sum))
SpSum

subset(temp, Metier!=Metier_Rev)

CheckTripId<- unique(temp$Trip[temp$Especie_Oficial!="Verdel - Caballa" & temp$Metier_Rev=="LHM_SPF_0_0_0"])
subset(temp, Trip %in% CheckTripId)


#   ..........................................  ####  
#    CERCO                                     ####
################################################## #

#    .Crear bd                                  ####
################################################## #
ps_met<- subset(db_met, db_met$Censo %in% c("CERCO EN CANTABRICO NW"))

sort(unique(ps_met$Censo))
sort(unique(ps_met$Puerto_Base_CA))
sort(unique(ps_met$Metier))
sort(unique(ps_met$ZonaNV))
sort(unique(ps_met$MetierNV))


#    .Asignar metier                            ####
################################################## #
ps_met$Metier_Rev<-NA

# utilizando lista de buques de referencia
# ps_met$Metier_Rev[ps_met$P_TUN>0.80]  <- "LHP_LPF_0_0_0"
# ps_met$Metier_Rev[ps_met$Metier_Rev %in% c("LHP_LPF_0_0_0") & ps_met$Nombre_Buque %in% Buque_cercoLTL] <- "LTL_LPF_0_0_0"
# 
# ps_met$Metier_Rev[ is.na(ps_met$Metier_Rev) & ps_met$P_LHM>0.90 &  ps_met$Nombre_Buque %in% Buque_cercoLHM] <- "LHM_SPF_0_0_0"
# ps_met[is.na(ps_met$Metier_Rev) & ps_met$P_LHM>0.80 &  ps_met$Nombre_Buque %in% Buque_cercoLHM & ps_met$LHM>1000,]
#ps_met$Metier_Rev[is.na(ps_met$Metier_Rev) & ps_met$P_LHM>0.90 &  ps_met$Nombre_Buque %in% Buque_cercoLHM & ps_met$LHM>1000]  <- "LHM_SPF_0_0_0"ps_met$Metier_Rev[ is.na(ps_met$Metier_Rev)] <- "PS_SPF_0_0_0" 

# utilizando arte NV
#ps_met$Metier_Rev[ is.na(ps_met$Metier_Rev) & ps_met$P_TUN>0.90 &  ps_met$MetierNV %in% c("LHM", "LTL")] <- "LTL_LPF_0_0_0"
#ps_met$Metier_Rev[ is.na(ps_met$Metier_Rev) & ps_met$P_LHM>0.90 &  ps_met$MetierNV %in% c("LHM", "LTL")] <- "LHM_SPF_0_0_0"

# utilizando datos oficiales y maestros
ps_met$Metier_Rev[ is.na(ps_met$Metier_Rev) & ps_met$P_TUN>0.90 &  ps_met$MetierDO %in% c("LTL")] <- "LTL_LPF_0_0_0"

ps_met$Metier_Rev[ is.na(ps_met$Metier_Rev) & ps_met$P_TUN>0.90 &  ps_met$MetierPrincipal %in% c("LTL_LPF_0_0_0")] <- "LTL_LPF_0_0_0"
ps_met$Metier_Rev[ is.na(ps_met$Metier_Rev) & ps_met$P_TUN>0.90 &  ps_met$MetierSecundario %in% c("LTL_LPF_0_0_0")] <- "LTL_LPF_0_0_0"

ps_met$Metier_Rev[ is.na(ps_met$Metier_Rev) & ps_met$P_LHM>0.90 &  ps_met$MetierDO %in% c("LHM")] <- "LHM_SPF_0_0_0"

ps_met$Metier_Rev[ is.na(ps_met$Metier_Rev) & ps_met$P_LHM>0.90 &  ps_met$MetierPrincipal %in% c("LHM_SPF_0_0_0")] <- "LHM_SPF_0_0_0"
ps_met$Metier_Rev[ is.na(ps_met$Metier_Rev) & ps_met$P_LHM>0.90 &  ps_met$MetierSecundario %in% c("LHM_SPF_0_0_0")] <- "LHM_SPF_0_0_0"

ps_met$Metier_Rev[ is.na(ps_met$Metier_Rev) & ps_met$P_TUN>0.90 ] <- "LHP_LPF_0_0_0"

ps_met$Metier_Rev[ is.na(ps_met$Metier_Rev) ] <- "PS_SPF_0_0_0"
                   

#    .Variable Check                            ####
################################################## #

# Metier_Check
ps_met$Metier_Check <- ps_met$Metier==ps_met$Metier_Rev 

#rescatar marea anterior y posterior
ps_met <- ps_met %>% group_by(Nombre_Buque) %>% mutate(PrevMet=lag(Metier_Rev), NextMet=lead(Metier_Rev)) %>%  
  ungroup() %>% data.frame
subset(ps_met, Metier_Rev!=PrevMet & Metier_Rev!=NextMet & Metier_Rev=="LHM_SPF_0_0_0")
subset(ps_met, Metier_Rev!=PrevMet & Metier_Rev!=NextMet & Metier_Rev=="LTL_LPF_0_0_0")
#marcar para chequear
ps_met$Metier_Check[ps_met$Metier_Rev != ps_met$PrevMet & ps_met$Metier_Rev != ps_met$NextMet & ps_met$Metier_Rev =="LHM_SPF_0_0_0"] <- "Check"
ps_met$Metier_Check[ps_met$Metier_Rev != ps_met$PrevMet & ps_met$Metier_Rev != ps_met$NextMet & ps_met$Metier_Rev =="LHP_LPF_0_0_0"] <- "Check"
ps_met$Metier_Check[ps_met$Metier_Rev != ps_met$PrevMet & ps_met$Metier_Rev != ps_met$NextMet & ps_met$Metier_Rev =="LTL_LPF_0_0_0"] <- "Check"



#    .Chequeos              ####
############################## #
#NA
ps_met[is.na(ps_met$Metier_Rev),]

# Other checkings
temp <- DBba %>% filter(Trip %in% ps_met$Trip)
temp$Metier_Rev <- ps_met$Metier_Rev[match(temp$Trip,ps_met$Trip)]
temp$Metier_Check <- temp$Metier==temp$Metier_Rev 
head(subset(temp, Metier!=Metier_Rev))

SpSum <- summaryBy(Kg_Desemb_Peso_Vivo~Metier_Rev +Especie_Oficial, data=temp, FUN=sum, na.rm=TRUE)
SpSum <- arrange(SpSum, Metier_Rev, desc( Kg_Desemb_Peso_Vivo.sum))
SpSum

subset(temp, Metier!=Metier_Rev)
subset(ps_met, Metier!=Metier_Rev)

CheckTripId<- unique(temp$Trip[temp$Especie_Oficial!="Verdel - Caballa" & temp$Metier_Rev=="LHM_SPF_0_0_0"])
subset(temp, Trip %in% CheckTripId)


#   ..........................................  ####  
########################################################## #
#   * JUNTAR BAJURA Y ARTESANAL *                      #####
########################################################## #
dim(af_met)
dim(rv_met)
dim(pa_met)
dim(am_met)
dim(ps_met)

# por marea total (juntar todo bajura y artesanal)
##
met_ba<- rbind(af_met, rv_met,pa_met,am_met,ps_met)
met_ba <- arrange(met_ba, Nombre_Buque, Fecha_Desembarco, desc(Total))

#   Guardar ficheros                            ####
################################################## #
write.table(met_ba, paste("Depuracion_Metiers\\Output\\", Ano, Fuente,"_porMarea_BajuraAr_detail.csv", sep=""), row.names = FALSE, sep=";", dec=",")

#   ..........................................  ####
############################################ #
#   * JUNTAR TODO *                      #####
############################################ #

# BD por metier
MET_all <- rbind( otb_met[,c("Trip", "Nombre_Buque", "Censo", "Fecha_Desembarco", "Metier", "MetierPrincipal", "MetierSecundario", "Metier_Rev", "Metier_Check", "ZonaDB", "Zona_Rev")], 
                  ptb_met[,c("Trip", "Nombre_Buque", "Censo", "Fecha_Desembarco", "Metier", "MetierPrincipal", "MetierSecundario", "Metier_Rev", "Metier_Check", "ZonaDB", "Zona_Rev")], 
                  met_ba[ ,c("Trip", "Nombre_Buque", "Censo", "Fecha_Desembarco", "Metier", "MetierPrincipal", "MetierSecundario", "Metier_Rev", "Metier_Check", "ZonaDB", "Zona_Rev")])
# BD por meitier para importar en la BD ICES
  MET_all_imp <- MET_all[,c("Trip", "Metier_Rev", "Zona_Rev")]

head(MET_all)
subset(MET_all, )

# BD total (detalle por especie)
DB_all<- rbind(otb, ptb, DBba)
DB_all$Metier_Rev <- MET_all$Metier_Rev[match(DB_all$Trip, MET_all$Trip)]
DB_all$Metier_Check <- MET_all$Metier_Check[match(DB_all$Trip, MET_all$Trip)]
DB_all$Zona_Rev <- MET_all$Zona_Rev[match(DB_all$Trip, MET_all$Trip)]

DB_all <- DB_all[,c("Trip", "Nombre_Buque", "Cod_UE","Puerto_Base", "Eslora_total", "Censo", 
                    "Fecha_Desembarco", "Puerto_Venta",  
                    "Zona","ZonaNV",
                    "MetierNV","MetierDO",
                    "Especie_Oficial","SpGroup",
                    "Kg_Desemb_Peso_Vivo",  
                    "Metier", "Metier_Rev",  "Metier_Check", "Zona_Rev")]


#   Guardar ficheros                            ####
################################################## #
write.table(MET_all, paste("Depuracion_Metiers\\Output\\", Ano, Fuente, "_porMarea_all_Resumen.csv", sep=""), row.names = FALSE, sep=";", dec=",")
write.table(MET_all_imp, paste("Depuracion_Metiers\\Output\\", Ano, Fuente, "_porMarea_all_Importar.csv", sep=""), row.names = FALSE, sep=";", dec=",")
write.table(DB_all, paste("Depuracion_Metiers\\Output\\", Ano, Fuente, "_all.csv", sep=""), row.names = FALSE, sep=";", dec=",")




#   ..........................................  ####  
########################################################## #
#   * Barcos dudosos: expert knowledge *               #####
########################################################## #

# Check metier principal
check <- subset(met_ba, Metier_Check!="Check"|is.na(Metier_Check))
check$Metier_Rev <- substr(check$Metier_Rev,1,3)
check$Metier_Rev [check$Metier_Rev == "GTR"] <- "GNS"

check_fin <- check %>%
  group_by(Nombre_Buque) %>%
  mutate (Nmetier = Fun_CountUnique(Metier_Rev)) %>% 
  ungroup() %>%
  group_by(Nombre_Buque, Censo, MetierPrincipal, MetierSecundario, Metier_Rev, Nmetier) %>% 
  summarise(Ntrip = Fun_CountUnique(Trip)) %>%
  pivot_wider(names_from = Metier_Rev, values_from = Ntrip)
check_fin[is.na(check_fin)] <- 0

# explorar los datos
subset(check_fin, Censo %in% "RASCO EN CANTABRICO NW")
(subset(check_fin, Censo =="RASCO EN CANTABRICO NW" & GNS>0 & LLS>0))

subset(check_fin, Censo =="VOLANTA EN CANTABRICO NW")
(subset(check_fin, Censo =="VOLANTA EN CANTABRICO NW" & GNS>0 & LLS>0))

subset(check_fin, Censo %in% c("PALANGRE DE FONDO EN CANTABRICO NW", 
                               "PALANGRE DE FONDO MENORES 100 TRB EN VIIIABDE",
                               "PALANGRE DE SUPERFICIE CALADERO NACIONAL"))
(subset(check_fin, Censo %in% c("PALANGRE DE FONDO EN CANTABRICO NW", 
                                "PALANGRE DE FONDO MENORES 100 TRB EN VIIIABDE",
                                "PALANGRE DE SUPERFICIE CALADERO NACIONAL")
                  & GNS>0 & LLS>0))

head(subset(check_fin, Censo =="ARTES MENORES EN CANTABRICO NW"))
subset(check_fin, Censo =="ARTES MENORES EN CANTABRICO NW" & GNS>0 & LLS>0)

head(subset(check_fin, Censo =="CERCO EN CANTABRICO NW"))
(subset(check_fin, Censo =="CERCO EN CANTABRICO NW" & Nmetier==3))
(subset(check_fin, Censo =="CERCO EN CANTABRICO NW" & PS_>0 & LHM>0))
(subset(check_fin, Censo =="CERCO EN CANTABRICO NW" & LHP>0 & LTL>0))
(subset(check_fin, Censo =="CERCO EN CANTABRICO NW" & PS_>0 & LTL>0))
(subset(check_fin, Censo =="CERCO EN CANTABRICO NW" & LHM>0 & LHP>0))



#marcar para chequear
tCheck <- subset(check_fin, (Censo =="ARTES MENORES EN CANTABRICO NW" & GNS>0 & LLS>0) |
                            (Censo =="CERCO EN CANTABRICO NW" & PS_>0 & LHM>0) |
                            (Censo =="CERCO EN CANTABRICO NW" & LHP>0 & LTL>0) |
                            (Censo =="CERCO EN CANTABRICO NW" & PS_>0 & LTL>0) |
                            (Censo =="CERCO EN CANTABRICO NW" & LHM>0 & LHP>0))

                      
tCheck <- tCheck %>% arrange(Censo, Nombre_Buque) %>% data.frame()                      
                      
#grabar tabla
write.table(tCheck, paste("Depuracion_Metiers\\Output\\", Ano, Fuente,"_BuquesCheck.csv", sep=""), row.names = FALSE, sep=";", dec=",")


#   ..........................................  ####  
########################################################## #
#   * Metier DO: expert knowledge *               #####
########################################################## #

# Check metier principal
checkDO <- subset(met_ba, Metier_Check!="Check"|is.na(Metier_Check))
table(checkDO$MetierDO)
checkDO$MetierDO[checkDO$MetierDO %in% c("GN","GNS", "GTN", "GTR")] <- "GNS"
checkDO$MetierDO[checkDO$MetierDO %in% c("LL", "LLD", "LLS", "LX")] <- "LLS"
checkDO$MetierDO[checkDO$MetierDO %in% c("PS", "PS1", "PS2")] <- "PS_"

check_finDO <- checkDO %>%
  group_by(Nombre_Buque) %>%
  mutate (Nmetier = Fun_CountUnique(MetierDO)) %>% 
  ungroup() %>%
  group_by(Nombre_Buque, Censo, MetierPrincipal, MetierSecundario, MetierDO, Nmetier) %>% 
  summarise(Ntrip = Fun_CountUnique(Trip)) %>%
  pivot_wider(names_from = MetierDO, values_from = Ntrip)
check_finDO[is.na(check_finDO)] <- 0


#marcar para chequear
tCheckDO <- check_finDO %>% filter(Nombre_Buque %in% tCheck$Nombre_Buque)


tCheckDO <- tCheckDO %>% arrange(Censo, Nombre_Buque) %>% data.frame()                      

#grabar tabla
write.table(tCheckDO, paste("Depuracion_Metiers\\Output\\", Ano, Fuente,"_BuquesCheckDO.csv", sep=""), row.names = FALSE, sep=";", dec=",")



