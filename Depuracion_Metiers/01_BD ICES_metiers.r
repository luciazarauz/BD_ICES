
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
  buquesLLSGNS <- read.csv("Depuracion_Metiers/2019_buques_LLS_GNS.csv",header=T,sep=";",dec=",", stringsAsFactors = FALSE); head(buquesLLSGNS)


#DB <- NV

  
  ################################################################### #
  #   Sacamos información de Zona y Arte de las NV                #####
  ################################################################### #
  # El problema es qu een las NV, dentro de la misma marea, podemso tener varias zonas y artes
  # Y necesitamos asignar la mara a n solo arte y una sola area
  
  dim(NV[is.na(NV$Sub_Zona_N1V),])
  dim(NV[is.na(NV$Division_N1V),])
  
  table(NV$Sub_Zona_N1V[!is.na(NV$Sub_Zona_N1V)])
  table(NV$Division_N1V[!is.na(NV$Division_N1V)])
  
  #Variable Zona NV
  NV$ZonaNV<- NV$Division_N1V
  NV$ZonaNV[is.na(NV$ZonaNV)]<- NV$Sub_Zona_N1V[is.na(NV$ZonaNV)]
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
  # Crear NZona
  ZonaSelect$NZona <- (apply(ZonaSelect, 1, function(x) length(x[!is.na(x)])))
  ZonaSelect$NZona <- ZonaSelect$NZona -1
  # Crear C_Zona
  dropnames <- c("IdVenta", "NZona")
  ZonaSelect<-as.data.table(ZonaSelect)
  ZonaSelect$C_Zona <- colnames(ZonaSelect[,-dropnames, with =F])[apply(ZonaSelect[,-dropnames, with =F],1,which.max)]
  ZonaSelect<-as.data.frame(ZonaSelect)
  ZonaSelect$C_Zona <- gsub("X", "",ZonaSelect$C_Zona)
  # Algun corrección
  ZonaSelect$C_Zona[ZonaSelect$C_Zona=="27.8" & !is.na(ZonaSelect$X27.8.A)] <- "27.8.A"
  ZonaSelect$C_Zona[ZonaSelect$C_Zona=="27.8" & !is.na(ZonaSelect$X27.8.B)] <- "27.8.B"
  ZonaSelect$C_Zona[ZonaSelect$C_Zona=="27.8" & !is.na(ZonaSelect$X27.8.D)] <- "27.8.D"
  ZonaSelect$C_Zona[ZonaSelect$C_Zona=="27.8" & !is.na(ZonaSelect$X27.8.C)] <- "27.8.C"
  ZonaSelect[ZonaSelect$C_Zona=="27.8" & ZonaSelect$NZona>1,]
  
  
  # MetierSelect
  ########## #
  MetierSelect <- NV %>% group_by(IdVenta, MetierNV) %>% summarise(Peso_Neto = sum(Peso_Neto, na.rm=T)) %>%
    dcast(IdVenta ~ MetierNV, value.var="Peso_Neto")   
  names(MetierSelect) <- make.names(names(MetierSelect))
  # Crear NMetier
  MetierSelect$NMetier <- (apply(MetierSelect, 1, function(x) length(x[!is.na(x)])))
  MetierSelect$NMetier <- MetierSelect$NMetier -1
  # Crear C_Metier
  dropnames <- c("IdVenta", "NMetier")
  MetierSelect<-as.data.table(MetierSelect)
  MetierSelect$C_Metier <- colnames(MetierSelect[,-dropnames, with =F])[apply(MetierSelect[,-dropnames, with =F],1,which.max)]
  MetierSelect<-as.data.frame(MetierSelect)
  
  
  ################################################################### #
#   Preparar Datos Ventas                                       #####
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
  unique(DB$Puerto_Base_CA)
  
DB$Pais_Base <- conv_base$origen_cod[match(DB$Puerto_Base, conv_base$puerto)]
  unique(DB$Nombre_Buque[is.na(DB$Pais_Base)])
  unique(DB$Puerto_Base[is.na(DB$Pais_Base)])
  unique(DB$Pais_Base)


#Trip
DB$Dia_Desembarco[is.na(DB$Dia_Desembarco)] <- DB$Dia[is.na(DB$Dia_Desembarco)]
DB$Mes_Desembarco[is.na(DB$Mes_Desembarco)] <- DB$Mes[is.na(DB$Mes_Desembarco)]
DB$Trimestre_Desembarco[is.na(DB$Trimestre_Desembarco)] <- DB$Trimestre[is.na(DB$Trimestre_Desembarco)]
DB$Fecha_Desembarco <- as.Date(format(ISOdate(DB$Ano,DB$Mes_Desembarco,DB$Dia_Desembarco),"%d/%m/%Y"),"%d/%m/%Y")
#DB$Trip_id_V_desembarco <- paste(DB$Nombre_Buque, as.character(DB$Fecha_Desembarco), DB$Puerto_Venta, sep="_")
DB$Trip <- DB$IdVenta

#Metier codigo
DB$Metier_cod  <- substr(DB$Metier,1,3)

#Metier (LLS y GNS)
DB$Metier_Principal <- buquesLLSGNS$Metier.principal.2018[match(DB$Cod_UE, buquesLLSGNS$Codigo.UE)] ## necesitamos datos actualizados!
DB$Metier_Principal[is.na(DB$Metier_Principal)]<- "Otros"

#Zona  DB
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


#Area NV
DB$ZonaNV   <- tolower(ZonaSelect$C_Zona[match(DB$IdVenta, ZonaSelect$IdVenta)])
DB$ZonaNV [grep("27.8.a|27.8.b|27.8.d|27.8.e",DB$ZonaNV)]<- "27.8.abde"
DB$ZonaNV [grep("27.8.c",DB$ZonaNV)]<- "27.8.c"
DB$ZonaNV [grep("27.7",DB$ZonaNV)]<- "27.7"
DB$ZonaNV [grep("27.6",DB$ZonaNV)]<- "27.6"
DB$ZonaNV [grep("27.9",DB$ZonaNV)]<- "27.9.a"
DB$ZonaNV [grep("27.4",DB$ZonaNV)]<- "27.4"
DB$ZonaNV [grep("27.2",DB$ZonaNV)]<- "27.2"
DB$ZonaNV [grep("27.1",DB$ZonaNV)]<- "27.1"

tapply(DB$Kg_Desemb_Peso_Vivo, list(DB$ZonaNV, DB$Zona), sum, na.rm=TRUE)
head(subset(DB, ZonaNV=="27.4")) %>% group_by(Nombre_Buque, Censo) %>% summarise (nTrips = Fun_CountUnique(Trip))

DB$Zona2 <- DB$ZonaNV
DB$Zona2[DB$Zona2 %in% c("27.4", "27.5","27.8" )] <- DB$Zona[DB$Zona2 %in% c("27.4", "27.5","27.8" )]



# Metier NV
DB$MetierNV <- MetierSelect$C_Metier[match(DB$IdVenta, MetierSelect$IdVenta)]

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
table(DB_extr$Metier)
sort(unique(DB_extr$Puerto_Base))

#   ...........................................................  #### 
################################################################### #
#   * ALTURA (PTB, OTB) *                                       #####
################################################################### #

DB_alt<- subset(DB, substr(DB$Metier, 1, 3) %in% c("OTB", "PTB") )
DB_alt<- subset(DB_alt, DB_alt$Pais_Base %in% c("ARM", "BER", "BIO", "DON", "ESP", "GET", "HON", "LEK", "MUN",
                                          "MUT", "OND", "ORI", "PAS", "PLE", "SAN", "ZIE") )                                    

head(DB_alt)
table(DB_alt$Metier)
table(DB_alt$Censo)
table(DB_alt$Censo,DB_alt$Metier)

#separo OTB de PTB
otb   <- subset(DB_alt, Metier_cod %in% c("OTB") )
ptb   <- subset(DB_alt, Metier_cod %in% c("PTB"))

#    .Exploración de datos                      ####
################################################## #

tapply(otb$Trip, list(otb$Zona, otb$Metier), Fun_CountUnique)
tapply(otb$Trip, list(otb$ZonaNV, otb$Metier), Fun_CountUnique)
tapply(otb$Trip, list(otb$Puerto_Base, otb$Metier), Fun_CountUnique)

tapply(ptb$Trip, list(ptb$Zona, ptb$Metier), Fun_CountUnique)
tapply(ptb$Trip, list(ptb$Puerto_Base, ptb$Metier), Fun_CountUnique)
tapply(ptb$Trip, list(ptb$Censo, ptb$Metier), Fun_CountUnique)

temp<- subset(DB_alt, Puerto_Base=="Ondarroa" & Metier_cod %in% c("OTB", "PTB"))
tapply(temp$Trip, list(temp$Nombre_Buque, temp$Metier_cod), Fun_CountUnique)

temp<- subset(temp, Nombre_Buque %in% c("GURE GASKUNA", "GURE KANTABRIKO"))
tapply(temp$Trip, list(temp$Fecha_Desembarco, temp$Metier_cod), Fun_CountUnique)
tapply(temp$Trip, list(temp$Fecha_Desembarco, temp$Metier_cod, temp$Nombre_Buque), Fun_CountUnique)


#   ..........................................  ####
#    PTB                                       ####
################################################## #

#    .Agrupar por marea                         ####
################################################## #
ptb_met     <- ptb %>% group_by(Nombre_Buque, Puerto_Base, Censo, Trip, Fecha_Desembarco, Dia, Mes, Ano, Zona2, Metier) %>% 
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

  
#    .Chequeos                                  ####
################################################## #
#     Crear variable check  
ptb_met$check <- ptb_met$Metier==ptb_met$Metier_Rev
head(ptb_met[ptb_met$check==FALSE,])

# NAs
ptb_met[is.na(ptb_met$Metier_Rev),]
  

#    .Guardar ficheros                          ####
################################################## #
write.table(ptb_met, paste("Depuracion_Metiers\\Output\\", Ano,"_DB_metierizada_PTB_porMarea.csv", sep=""), row.names = FALSE, sep=",", dec=".")


    
#   ..........................................  ####  
#    OTB                                       ####
################################################## #


#    .Asignar especies                          ####
################################################## #

otb$Grupo<- spOTB$Grupo[match(otb$Especie_ALFA3,spOTB$Cod.ALFA.3)]
otb$Grupo[is.na(otb$Grupo)] <- "Otras"

    # alguna comprobacion de que no nos dejamos ningun sp importante fuera
    sp   <- summaryBy(Kg_Desembarcados ~ Especie_Oficial + Grupo,
                       data=otb, FUN=sum, na.rm=TRUE)
    temp <- subset(sp, Grupo=="Otras") %>% arrange(-Kg_Desembarcados.sum)
    sp   <- sp %>% arrange(Grupo, -Kg_Desembarcados.sum)
    sp

    
#    .Agrupar por marea                         ####
################################################## #
otb_met     <- otb %>% group_by(Nombre_Buque, Puerto_Base, Censo, Trip, Fecha_Desembarco, Dia, Mes, Ano, Zona2, Metier, Grupo) %>% 
                          summarise(Peso = sum(Kg_Desemb_Peso_Vivo, na.rm=T)) %>%
                          pivot_wider(names_from = Grupo, values_from = Peso) %>%
                          data.frame()    
otb_met [is.na(otb_met)] <- 0
  
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
  
#    ..VII       ####
  otb_met$Metier_Rev[otb_met$Zona2 %in% c("27.7" )] <- "OTB_DEF_70-99_0_0"
    sort(unique(otb_met$Puerto_Base[otb_met$Metier_Rev=="OTB_DEF_70-99_0_0"]))
  
#    ..VI        ####
  otb_met$Metier_Rev[otb_met$Zona2 %in% c("27.6")] <- "OTB_DEF_100-119_0_0"
    sort(unique(otb_met$Puerto_Base[otb_met$Metier_Rev=="OTB_DEF_100-119_0_0"]))
  
#    ..II        ####
  otb_met$Metier_Rev[otb_met$Zona2 %in% c("27.1", "27.2")] <- "OTB_DEF_>=120_0_0"
    sort(unique(otb_met$Puerto_Base[otb_met$Metier_Rev=="OTB_DEF_>=120_0_0"]))
  

#    ..VIIIabd   ####
  otb_met$Metier_Rev[otb_met$Zona2 %in% c("27.8.abde")] <- "OTB_DEF_>=70_0_0"
  otb_met$Metier_Rev[otb_met$Metier_Rev =="OTB_DEF_>=70_0_0" & otb_met$P_pel>0.8] <- "OTB_SPF_>=70_0_0"
  otb_met$Metier_Rev[otb_met$Metier_Rev =="OTB_DEF_>=70_0_0" & otb_met$P_pel<=0.8 & otb_met$P_pel>=0.5 ] <- "OTB_MPD_>=70_0_0"
  otb_met$Metier_Rev[otb_met$Metier_Rev =="OTB_DEF_>=70_0_0" & otb_met$P_mix>=0.25] <- "OTB_MCF_>=70_0_0"
  otb_met$Metier_Rev[otb_met$Metier_Rev =="OTB_DEF_>=70_0_0" & otb_met$P_mix<0.25 & otb_met$P_dem>=0.5 ] <- "OTB_DEF_>=70_0_0"
  otb_met$Metier_Rev[otb_met$Metier_Rev =="OTB_DEF_>=70_0_0" & otb_met$P_mix<0.2] <- "OTB_DEF_>=70_0_0"
  

#    .Chequeos                                  ####
################################################## #
#     Crear variable check
otb_met$check <- otb_met$Metier==otb_met$Metier_Rev

# NAs
otb_met[is.na(otb_met$Metier_Rev),]
head(otb_met[otb_met$check==FALSE,])


subset(otb_met, Nombre_Buque=="GURE GASKUNA")
subset(otb_met, Nombre_Buque=="INTXORTAMENDI")


# .Guardar ficheros                          ####
setwd(res.path)

write.table(otb_met, paste("Depuracion_Metiers\\Output\\", Ano,"_DB_metierizada_OTB_porMarea.csv", sep=""), row.names = FALSE, sep=",", dec=".")


#   ...........................................................  #### 
################################################################### #
#   * BAJURA Y ARTESANAL *                                      #####
################################################################### #

DB<- subset(DB, !DB$Censo %in% c("ARRASTRE DE FONDO EN CANTABRICO NW", "ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII Y VIIIABDE") )
DB<- subset(DB, DB$Pais_Base %in% c("ARM", "BER", "BIO", "DON", "GET", "HON", "LEK", "MUN",
                                    "MUT", "OND", "ORI", "PAS", "PLE", "SAN", "ZIE",
                                    "ESP") )



#    .Asignar especies                          ####
################################################## #

sort(unique(DB$Censo))
sort(unique(DB$Zona))

DB$SpGroup <- NA
DB$SpGroup <- spAM$Grupo[match(DB$Especie_ALFA3 , spAM$Cod.ALFA.3)]
DB$SpGroup[is.na(DB$SpGroup)] <- "Otras"

  
  # alguna comprobacion de que no nos dejamos ningun sp importante fuera
  sp   <- summaryBy(Kg_Desembarcados ~ Especie_Oficial + SpGroup,
                    data=DB, FUN=sum, na.rm=TRUE)
  temp <- subset(sp, SpGroup=="Otras") %>% arrange(-Kg_Desembarcados.sum)
  sp   <- sp %>% arrange(SpGroup, -Kg_Desembarcados.sum)
  sp



#    .Agrupar por marea                         ####
################################################## #

# calcular el número de especies por marea
db_sp<- DB %>% arrange(Nombre_Buque, Fecha_Desembarco,Puerto_Venta) %>%
                group_by(Trip) %>%
                mutate(Nsp=length(unique(Especie_ALFA3))) %>%
                ungroup() 

# agregar por marea
db_sp<-   db_sp %>% group_by(Nombre_Buque , Cod_UE , Eslora_total , Censo , Puerto_Base , Puerto_Base_CA  , Fecha_Desembarco , 
                         Trip ,  Metier , Zona2 , MetierNV, Metier_Principal, Nsp , SpGroup) %>%
                    summarise(Peso=sum(Kg_Desemb_Peso_Vivo, na.rm=TRUE))

db_met <- dcast(db_sp,  Nombre_Buque  + Eslora_total + Censo + Puerto_Base + Puerto_Base_CA  + Fecha_Desembarco + 
                  Trip +  Metier + MetierNV + Zona2 + Metier_Principal + Nsp  ~ SpGroup,
                fill=0, value.var = "Peso") #el fill=0 hace que en vez de poner NA a las celdas vac?as las rellene como = 0 que es lo real.

vars <- unique(spAM$Grupo)
db_met[setdiff(vars,names(db_met))] <- 0

db_met <- as.data.frame(db_met)



#selvar <- c("Nombre_Buque", "C?d_UE","Eslora_total", "Puerto_Base","Censo", "Fecha_Desembarco", "Puerto_Venta", "Trip_id_V_desembarco",
#            "Metier", "Zona2", "Especie_Oficial", "Kg_Desembarcados", "SpGroup","Metier_Principal", "Nsp", "PrevMet",  "NextMet" ) 


db_met$Total <- db_met$ALG + db_met$CRU + db_met$GNS + db_met$GTR + db_met$LHM + 
                db_met$MIS + db_met$MOL + db_met$TUN + db_met$CEP + db_met$Otras 
db_met$P_ALG <-  db_met$ALG /  db_met$Total 
db_met$P_CRU <-  db_met$CRU /  db_met$Total 
db_met$P_GNS <-  db_met$GNS /  db_met$Total 
db_met$P_GTR <-  db_met$GTR /  db_met$Total 
db_met$P_LHM <-  db_met$LHM /  db_met$Total 
db_met$P_MIS <-  db_met$MIS /  db_met$Total 
db_met$P_MOL <-  db_met$MOL /  db_met$Total 
db_met$P_TUN <-  db_met$TUN /  db_met$Total 
db_met$P_CEP <-  db_met$CEP /  db_met$Total 
db_met$P_otr <-  db_met$Otras /  db_met$Total
db_met$P_Total <- db_met$P_ALG + db_met$P_CRU + db_met$P_GNS + db_met$P_GTR + db_met$P_LHM + 
                  db_met$P_MIS + db_met$P_MOL + db_met$P_TUN + db_met$P_CEP +db_met$P_otr
db_met$P2_GNS <-  (db_met$LHM + db_met$GNS) /  db_met$Total 

#db_met<- db_met %>% mutate_if( is.numeric, function(x){round(x,digits=2)})
db_met   <- db_met %>% mutate_if(is.numeric, round, digits = 2)



#   ..........................................  ####  
#    RASCO Y VOLANTA                           ####
################################################## #

#    .Crear bd                                  ####
################################################## #
DBrv<- subset(DB, DB$Censo %in% c("RASCO EN CANTABRICO NW","VOLANTA EN CANTABRICO NW"))
rv_met<- subset(db_met, db_met$Censo %in% c("RASCO EN CANTABRICO NW","VOLANTA EN CANTABRICO NW"))

sort(unique(DBrv$Censo))
sort(unique(DBrv$Puerto_Base_CA))
sort(unique(DBrv$Metier))
sort(unique(DBrv$Zona2))
sort(unique(DBrv$Metier_Principal))
sort(unique(DBrv$MetierNV))
table(DBrv$Metier_Principal)


#    .Asignar metier                            ####
################################################## #
sort(unique(DBrv$Metier))
rv_met$Metier_Rev<-NA
rv_met$Metier_Rev[rv_met$P_LHM>0.80]  <- "LHM_SPF_0_0_0"
rv_met$Metier_Rev[is.na(rv_met$Metier_Rev) & rv_met$P_TUN>0.80]  <- "LTL_LPF_0_0_0"
rv_met$Metier_Rev[is.na(rv_met$Metier_Rev) & rv_met$Zona2 %in% c("27.8.abde", "27.8.c" )]  <- "GNS_DEF_>=100_0_0"
rv_met$Metier_Rev[is.na(rv_met$Metier_Rev) & rv_met$Puerto_Base_CA %in% c("Euskadi") &
                    rv_met$Zona %in% c("27.7","27.6" )]  <- "GNS_DEF_100-119_0_0"
rv_met[is.na(rv_met$Metier_Rev) & !rv_met$Puerto_Base_CA %in% c("Euskadi") &
                    rv_met$Zona %in% c("27.7","27.6" ),]

rv_met$Metier_Check <- rv_met$Metier==rv_met$Metier_Rev 


#    .Chequeos                                  ####
################################################## #
#NA
rv_met[is.na(rv_met$Metier_Rev),]

#rescatar marea anterior y posterior
rv_met <- rv_met %>% group_by(Nombre_Buque) %>% mutate(PrevMet=lag(Metier_Rev), NextMet=lead(Metier_Rev)) %>%  ungroup() %>% data.frame()
subset(rv_met, Metier_Rev!=PrevMet & Metier_Rev!=NextMet)

#Variable en DB
DBrv$Metier_Rev <- rv_met$Metier_Rev[match(DBrv$Trip, rv_met$Trip )]
DBrv$Metier_Check <- DBrv$Metier==DBrv$Metier_Rev 

#other checkings
subset(DBrv, is.na(Metier_Rev))
sort(unique(DBrv$Metier_Rev))
head(subset(DBrv, Metier!=Metier_Rev))

SpSum_rv <- summaryBy(Kg_Desemb_Peso_Vivo~Metier_Rev +Especie_Oficial, data=DBrv, FUN=sum, na.rm=TRUE)
SpSum_rv <- arrange(SpSum_rv, Metier_Rev, desc( Kg_Desemb_Peso_Vivo.sum))
SpSum_rv

CheckTripId<- unique(DBrv$Trip[DBrv$Especie_Oficial!="Verdel - Caballa" & DBrv$Metier_Rev=="LHM_SPF_0_0_0"])
subset(DBrv, Trip %in% CheckTripId)

subset(DBrv, Metier!=Metier_Rev)


#   ..........................................  ####  
#    PALANGRE BAJURA                           ####
################################################## #

#    .Crear bd                                  ####
################################################## #
DBpa<- subset(DB, DB$Censo %in% c("PALANGRE DE FONDO EN CANTABRICO NW", "PALANGRE DE FONDO MENORES 100 TRB EN VIIIABDE."))
pa_met<- subset(db_met, db_met$Censo %in% c("PALANGRE DE FONDO EN CANTABRICO NW", "PALANGRE DE FONDO MENORES 100 TRB EN VIIIABDE."))

sort(unique(DBpa$Censo))
sort(unique(DBpa$Puerto_Base_CA))
sort(unique(DBpa$Metier))
sort(unique(DBpa$Zona2))
sort(unique(DBpa$Metier_Principal))

#    .Asignar metier                            ####
################################################## #
sort(unique(DBpa$Metier))
pa_met$Metier_Rev<-NA
pa_met$Metier_Rev[pa_met$P_LHM>0.80]  <- "LHM_SPF_0_0_0"
pa_met$Metier_Rev[is.na(pa_met$Metier_Rev) & pa_met$P_TUN>0.80]  <- "LTL_LPF_0_0_0"
pa_met$Metier_Rev[is.na(pa_met$Metier_Rev) & pa_met$P_ALG>0.80]  <- "MIS_ALG_0"
pa_met$Metier_Rev[is.na(pa_met$Metier_Rev)]  <- "LLS_DEF_<24LOA"
pa_met$Metier_Check <- pa_met$Metier==pa_met$Metier_Rev 


#    .Chequeos                                  ####
################################################## #
#NA
pa_met[is.na(pa_met$Metier_Rev),]

#rescatar marea anterior y posterior
pa_met <- pa_met %>% group_by(Nombre_Buque) %>% mutate(PrevMet=lag(Metier_Rev), NextMet=lead(Metier_Rev)) %>%  ungroup() %>% data.frame()
subset(pa_met, Metier_Rev!=PrevMet & Metier_Rev!=NextMet & pa_met$Metier_Rev=="LHM_SPF_0_0_0")
subset(pa_met, Metier_Rev!=PrevMet & Metier_Rev!=NextMet & pa_met$Metier_Rev=="LTL_LPF_0_0_0")

#dejar vacias
pa_met$Metier_Rev[pa_met$Metier_Rev != pa_met$PrevMet & pa_met$Metier_Rev != pa_met$NextMet & pa_met$Metier_Rev=="LHM_SPF_0_0_0"] <- NA
pa_met$Metier_Rev[pa_met$Metier_Rev != pa_met$PrevMet & pa_met$Metier_Rev != pa_met$NextMet & pa_met$Metier_Rev=="LTL_LPF_0_0_0"] <- NA


#Create variable in DB
DBpa$Metier_Rev <- pa_met$Metier_Rev[match(DBpa$Trip, pa_met$Trip )]
DBpa$Metier_Check <- DBpa$Metier==DBpa$Metier_Rev 

#Other checkings
SpSum_pa <- summaryBy(Kg_Desemb_Peso_Vivo ~ Metier_Rev +Especie_Oficial, data=DBpa, FUN=sum, na.rm=TRUE)
SpSum_pa <- arrange(SpSum_pa, Metier_Rev, desc( Kg_Desemb_Peso_Vivo.sum))
SpSum_pa

tapply(pa_met$Trip, list(pa_met$Metier_Rev, pa_met$MetierNV), Fun_CountUnique)

CheckTripId<- unique(DBpa$Trip[DBpa$Especie_Oficial!="Verdel - Caballa" & DBpa$Metier_Rev=="LHM_SPF_0_0_0"])
subset(DBpa, Trip %in% CheckTripId)

CheckTripId<- unique(DBpa$Trip[DBpa$Especie_Oficial %in% c("Percebe") & DBpa$Metier_Rev=="LLS_DEF_<24LOA"])
subset(DBpa, Trip %in% CheckTripId)

CheckTripId<- unique(DBpa$Trip[DBpa$MetierNV %in% c("LHM") & DBpa$Metier_Rev=="LLS_DEF_<24LOA"])
subset(DBpa, Trip %in% CheckTripId)

#   ..........................................  ####  
#    ARTES MENORES                             ####
################################################## #

#    .Crear bd                                  ####
################################################## #
DBam<- subset(DB, DB$Censo %in% c("ARTES MENORES EN CANTABRICO NW"))
am_met<- subset(db_met, db_met$Censo %in% c("ARTES MENORES EN CANTABRICO NW"))

sort(unique(DBam$Censo))
sort(unique(DBam$Puerto_Base_CA))
sort(unique(DBam$Metier))
sort(unique(DBam$Zona2))
sort(unique(DBam$Metier_Principal))
sort(unique(DBam$MetierNV))

#    .Asignar metier                            ####
################################################## #
sort(unique(DBam$Metier))
am_met$Metier_Rev<-NA

#lineas de mano
am_met$Metier_Rev[am_met$P_LHM>0.99 ]  <- "LHM_SPF_0_0_0"
am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$P_LHM>0.90 & am_met$LHM>1000]  <- "LHM_SPF_0_0_0"
am_met[is.na(am_met$Metier_Rev) & am_met$P_LHM>0.80 & am_met$P_LHM>1000, ] 

#cacea
am_met$Metier_Rev[is.na(am_met$Metier_Rev) & pa_met$P_TUN>0.80]  <- "LTL_LPF_0_0_0"

#algas
am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$P_ALG >0.9]  <- "MIS_ALG_0"

#anemonas
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
head(buquesLLSGNS)
sort(unique(buquesLLSGNS$Metier.principal.2018))

am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$Metier_Principal=="GNS_DEF_60-79_0_0"  ] <- "GNS_DEF_60-79_0_0"
am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$Metier_Principal=="LLS_DEF_<=1000"  ] <- "LLS_DEF_<=1000"

am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$Metier=="LLS_DEF_<=1000" ] <- "LLS_DEF_<=1000"
am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$Puerto_Base_CA!="Euskadi"  ] <- "GNS_DEF_80-99_0_0"
am_met$Metier_Rev[is.na(am_met$Metier_Rev) & am_met$Puerto_Base_CA=="Euskadi"  ] <- "GNS_DEF_60-79_0_0"

am_met$Metier_Rev[am_met$Metier_Rev=="GNS_DEF_60-79_0_0" & am_met$P2_GNS>=0.5 ] <- "GNS_DEF_60-79_0_0"
am_met$Metier_Rev[am_met$Metier_Rev=="GNS_DEF_60-79_0_0" &  am_met$P_GTR>=0.5 ] <- "GTR_DEF_60-79_0_0"
am_met$Metier_Rev[am_met$Metier_Rev=="GNS_DEF_60-79_0_0" &  am_met$P_GTR>=0.4 & am_met$P2_GNS<0.2] <- "GTR_DEF_60-79_0_0"

#    .Chequeos                                  ####
################################################## #
#NA
am_met[is.na(am_met$Metier_Rev),]

#rescatar marea anterior y posterior
am_met <- am_met %>% group_by(Nombre_Buque) %>% mutate(PrevMet=lag(Metier_Rev), NextMet=lead(Metier_Rev)) %>%  
                      ungroup() %>% data.frame
subset(am_met, Metier_Rev!=PrevMet & Metier_Rev!=NextMet & Metier_Rev=="LHM_SPF_0_0_0")
subset(am_met, Metier_Rev!=PrevMet & Metier_Rev!=NextMet & Metier_Rev=="LTL_LPF_0_0_0")
subset(am_met, Metier_Rev!=PrevMet & Metier_Rev!=NextMet & Metier_Rev=="LLS_DEF_<=1000")

#dejar vacias
am_met$Metier_Rev[am_met$Metier_Rev != am_met$PrevMet & am_met$Metier_Rev != am_met$NextMet & am_met$Metier_Rev=="LHM_SPF_0_0_0"] <- NA
am_met$Metier_Rev[am_met$Metier_Rev != am_met$PrevMet & am_met$Metier_Rev != am_met$NextMet & am_met$Metier_Rev=="LTL_LPF_0_0_0"] <- NA
am_met$Metier_Rev[am_met$Metier_Rev != am_met$PrevMet & am_met$Metier_Rev != am_met$NextMet & am_met$Metier_Rev=="LLS_DEF_<=1000"] <- NA

#Check
am_met$Metier_Check <- am_met$Metier==am_met$Metier_Rev 
head(subset(am_met, Metier_Check ==FALSE))

#Variable en DB
DBam$Metier_Rev <- am_met$Metier_Rev[match(DBam$Trip, am_met$Trip )]
DBam$Metier_Check <- DBam$Metier==DBam$Metier_Rev 

#Other checking
SpSum_am <- summaryBy(Kg_Desemb_Peso_Vivo ~ Metier_Rev +Especie_Oficial, data=DBam, FUN=sum, na.rm=TRUE)
SpSum_am <- arrange(SpSum_am, Metier_Rev, desc( Kg_Desemb_Peso_Vivo.sum))
SpSum_am

CheckTripId<- unique(DBam$Trip[DBam$Especie_Oficial!="Verdel - Caballa" & DBam$Metier_Rev=="LHM_SPF_0_0_0"])
subset(DBam, Trip %in% CheckTripId)


#   ..........................................  ####  
#    CERCO                                     ####
################################################## #

#    .Crear bd                                  ####
################################################## #
DBps<- subset(DB, DB$Censo %in% c("CERCO EN CANTABRICO NW"))
ps_met<- subset(db_met, db_met$Censo %in% c("CERCO EN CANTABRICO NW"))

Buque_cercoLHM <- c("AITA RAMON", "ANDUIZA ANAIAK", "BETI ITXAS ARGI", "MARIA DIGNA DOS")
Buque_cercoLTL <- c("AITA RAMON", "AMATXO (3BI21-96)", "ANDUIZA ANAIAK", "BETI EUSKAL HERRIA", "BETI ITXAS ARGI", "DEMAR",
                    "LEKANDA",   "OSKARBI", "MARIA DIGNA DOS", "NUEVO ROBER")

sort(unique(DBps$Censo))
sort(unique(DBps$Puerto_Base_CA))
sort(unique(DBps$Metier))
sort(unique(DBps$Zona2))
sort(unique(DBps$Metier_Principal))


#    .Comprobar costeras                        ####
################################################## #
ps_met[ ps_met$Metier %in% c("LHP_LPF_0_0_0", "LTL_LPF_0_0_0") & ps_met$P_TUN<0.8,]
ps_met[ !ps_met$Metier %in% c("LHP_LPF_0_0_0", "LTL_LPF_0_0_0") & ps_met$P_TUN>0.75,]

ps_met[ ps_met$Metier %in% c("LHM_SPF_0_0_0") & !ps_met$Nombre_Buque %in% Buque_cercoLHM,]
ps_met[ ps_met$Metier %in% c("PS_SPF_0_0_0") & ps_met$Nombre_Buque %in% Buque_cercoLHM,]

ps_met[ ps_met$Metier %in% c("LTL_SPF_0_0_0") & !ps_met$Nombre_Buque %in% Buque_cercoLTL,]
ps_met[ ps_met$Metier %in% c("LHP_SPF_0_0_0") & ps_met$Nombre_Buque %in% Buque_cercoLTL,]


#    .Asignar metier                            ####
################################################## #
ps_met$Metier_Rev<-NA
ps_met$Metier_Rev<-ps_met$Metier

ps_met$Metier_Rev[ !ps_met$Metier %in% c("LHP_LPF_0_0_0","LTL_LPF_0_0_0") & ps_met$P_TUN>0.75] <- "LHP_LPF_0_0_0"
ps_met$Metier_Rev[  ps_met$Metier %in% c("LHP_LPF_0_0_0") & ps_met$Nombre_Buque %in% Buque_cercoLTL] <- "LTL_LPF_0_0_0"
ps_met$Metier_Rev[  ps_met$Metier %in% c("LTL_LPF_0_0_0") & !ps_met$Nombre_Buque %in% Buque_cercoLTL] <- "LHP_LPF_0_0_0"

ps_met$Metier_Rev[ ps_met$Metier %in% c("PS_SPF_0_0_0") &  ps_met$Nombre_Buque %in% Buque_cercoLHM] <- "LHM_SPF_0_0_0"
ps_met$Metier_Rev[ ps_met$Metier %in% c("LHM_SPF_0_0_0") &  !ps_met$Nombre_Buque %in% Buque_cercoLHM] <- "PS_SPF_0_0_0"


#    .Chequeos                                  ####
################################################## #
ps_met$Metier_Check <- ps_met$Metier==ps_met$Metier_Rev 

DBps$Metier_Rev <- ps_met$Metier_Rev[match(DBps$Trip_id_V_desembarco, ps_met$Trip_id_V_desembarco )]
DBps$Metier_Check <- DBps$Metier==DBps$Metier_Rev 


SpSum_ps <- summaryBy(Kg_Desemb_Peso_Vivo~Metier_Rev +Especie_Oficial, data=DBps, FUN=sum, na.rm=TRUE)
SpSum_ps <- arrange(SpSum_ps, Metier_Rev, desc( Kg_Desemb_Peso_Vivo.sum))

CheckTripId<- unique(DBps$Trip_id_V_desembarco[DBps$Especie_Oficial!="Verdel, Caballa" & DBps$Metier_Rev=="LHM_SPF_0_0_0"])
subset(DBps, Trip_id_V_desembarco %in% CheckTripId)



#   ..........................................  ####  
#   Juntar todo                                 ####
################################################## #

dim(DBrv)
dim(DBpa)
dim(DBam)
dim(DBps)

DBall<-rbind(DBrv,DBpa,DBam,DBps)
DBall <- arrange(DBall, Nombre_Buque, Fecha_Desembarco, Fecha_venta, desc(Kg_Desemb_Peso_Vivo))
DBall <- DBall[,c("Nombre_Buque", "C?d_UE","Puerto_Base", "Eslora_total", "Censo", "Zona2", 
                  "Fecha_Desembarco",  "Fecha_venta", "Puerto_Venta", "Trip_id_V_desembarco", 
                  "Metier_col", "Puerto_col", "Metier_Principal", "Especie_Oficial","SpGroup",
                  "Kg_Desemb_Peso_Vivo", "Metier", "Metier_Rev",  "Metier_Check" )]

dim(rv_met)
dim(pa_met)
dim(am_met)
dim(ps_met)
met_all<- rbind(rv_met,pa_met,am_met,ps_met)
met_all <- arrange(met_all, Nombre_Buque, Fecha_Desembarco, desc(Total))


SpSum <- summaryBy(Kg_Desemb_Peso_Vivo~Censo + Metier_Rev +Especie_Oficial, data=DBall, FUN=sum, na.rm=TRUE)
SpSum <- arrange(SpSum, Censo, Metier_Rev, desc( Kg_Desemb_Peso_Vivo.sum))

CheckTripId<- unique(DBall$Trip_id_V_desembarco[DBall$Especie_Oficial!="Verdel, Caballa" & DBall$Metier_Rev=="LHM_SPF_0_0_0"])
subset(DBall, Trip_id_V_desembarco %in% CheckTripId)


#   Guardar ficheros                            ####
################################################## #

write.table(DBall, paste(Ano, "DB_metierizada_BajuraArt.csv", sep="_"), row.names = FALSE,sep=";", dec=",")
write.table(met_all, paste(Ano, "DB_metierizada_BajuraArt_porMarea.csv", sep="_"), row.names = FALSE,sep=";", dec=",")
write.table(Check1V, paste(Ano, "DB_mareas con misma fecha de venta y dos registros.csv", sep="_"), row.names = FALSE,sep=";", dec=",")


db_met[db_met$Trip_id_V_desembarco=="ALAIN BI_2018-11-01",]
db_sp[db_sp$Trip_id_V_desembarco=="ALAIN BI_2018-11-01",]


