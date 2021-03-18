# ----------------------------------------------------------------- #
# Readme:                                                           #
# ----------------------------------------------------------------- #
#
# Aquí se lee los ficheros InfoBase que nos envía SGP a travñes del IEO.
#
# sacamos el fichero "NotasVenta2019_Originales.RData"
# que NO está dentro de la carpeta "Datos/".
#
# INDICE:
#
# Libraries.
# Path.
# Leer tablas BD.
# Leer Tablas NotasVenta.
#
# ----------------------------------------------------------------- #
#
# ----------------------------------------------------------------- #
# ## R version 3.6.0 (2019-04-26) -- "Planting of a Tree"
# ## Necesario abrir R en 32 Bits
# ## rStudio 1.1 (1.2 no funciona con 32 bits)
# ----------------------------------------------------------------- #

# ################## #
# Libraries          #
# ################## #
rm(list=(ls()))

library(RODBC)
library(here)
library (stringr)



# ##################################### #
# Leer tablas BD. (Solo la primera vez) #
# ##################################### #

# channel <- odbcConnectAccess("C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\2021\\7_InfoBase\\20210309_infobase\\InfoBase2020_Azti.mdb")
# sqlTables(channel)
# 
# BarcosAzti        <- sqlFetch(channel, "BarcosAzti",stringsAsFactors=FALSE)
# InfoBase          <- sqlFetch(channel, "InfoBase",stringsAsFactors=FALSE)
# InfoBuques        <- sqlFetch(channel, "InfoBuques",stringsAsFactors=FALSE)
# #InfoBuquesOk     <- sqlFetch(channel, "InfoBuquesOk",stringsAsFactors=FALSE)
# InfoCapturaLance0 <- sqlFetch(channel, "InfoCapturaLance0",stringsAsFactors=FALSE)
# InfoCapturas      <- sqlFetch(channel, "InfoCapturas",stringsAsFactors=FALSE)
# InfoCapturasCalculadas <- sqlFetch(channel, "InfoCapturasCalculadas",stringsAsFactors=FALSE)
# InfoDescartes     <- sqlFetch(channel, "InfoDescartes",stringsAsFactors=FALSE)
# InfoDiarios       <- sqlFetch(channel, "InfoDiarios",stringsAsFactors=FALSE)
# InfoOrigenLineas  <- sqlFetch(channel, "InfoOrigenLineas",stringsAsFactors=FALSE)
# InfoParametrosArteCapturas  <- sqlFetch(channel, "InfoParametrosArteCapturas",stringsAsFactors=FALSE)
# InfoVentas        <- sqlFetch(channel, "InfoVentas",stringsAsFactors=FALSE)
# #ParametrosArte   <- sqlFetch(channel, "ParametrosArte",stringsAsFactors=FALSE)
# Puertos           <- sqlFetch(channel, "Puertos",stringsAsFactors=FALSE)
# 
# close(channel)
# 
# 

InfoBase                    <- read.table("C:\\use\\0_GitHub\\BD_ICES\\0_Datos\\Infobase\\2020\\InfoBase.txt", sep=";", dec=",", header=TRUE, stringsAsFactors = FALSE)
InfoBuques                  <- read.table("C:\\use\\0_GitHub\\BD_ICES\\0_Datos\\Infobase\\2020\\InfoBuques.txt", sep=";", dec=",", header=TRUE, stringsAsFactors = FALSE)
InfoBuquesOk                <- read.table("C:\\use\\0_GitHub\\BD_ICES\\0_Datos\\Infobase\\2020\\InfoBuquesOk.txt", sep=";", dec=",", header=TRUE, stringsAsFactors = FALSE)
InfoCaptura0                <- read.table("C:\\use\\0_GitHub\\BD_ICES\\0_Datos\\Infobase\\2020\\InfoCaptura0.txt", sep=";", dec=",", header=TRUE, stringsAsFactors = FALSE)
InfoCapturasCalculadas      <- read.table("C:\\use\\0_GitHub\\BD_ICES\\0_Datos\\Infobase\\2020\\InfoCapturasCalculadas.txt", sep=";", dec=",", header=TRUE, stringsAsFactors = FALSE)
InfoDescartes               <- read.table("C:\\use\\0_GitHub\\BD_ICES\\0_Datos\\Infobase\\2020\\InfoDescartes.txt", sep=";", dec=",", header=TRUE, stringsAsFactors = FALSE)
InfoDiarios                 <- read.table("C:\\use\\0_GitHub\\BD_ICES\\0_Datos\\Infobase\\2020\\InfoDiarios.txt", sep=";", dec=",", header=TRUE, stringsAsFactors = FALSE)
InfoOrigenLineas            <- read.table("C:\\use\\0_GitHub\\BD_ICES\\0_Datos\\Infobase\\2020\\InfoOrigenLineas.txt", sep=";", dec=",", header=TRUE, stringsAsFactors = FALSE)
InfoParametrosArteCapturas  <- read.table("C:\\use\\0_GitHub\\BD_ICES\\0_Datos\\Infobase\\2020\\InfoParametrosArteCapturas.txt", sep=";", dec=",", header=TRUE, stringsAsFactors = FALSE)
InfoVentas                  <- read.table("C:\\use\\0_GitHub\\BD_ICES\\0_Datos\\Infobase\\2020\\InfoVentas.txt", sep=";", dec=",", header=TRUE, stringsAsFactors = FALSE)

head(InfoBase)
head(InfoBuques)
head(InfoBuquesOk)
head(InfoCaptura0)
head(InfoCapturasCalculadas)
head(InfoDescartes)
head(InfoDiarios)
head(InfoOrigenLineas)
head(InfoParametrosArteCapturas)
head(InfoVentas)


save( InfoBase,
      InfoBuques,
      InfoBuquesOk,
      InfoCaptura0,
      InfoCapturasCalculadas,
      InfoDescartes,
      InfoDiarios,
      InfoOrigenLineas,
      InfoParametrosArteCapturas,
      InfoVentas,
      file = "0_Datos/Infobase/2020/InfoBase2020_Originales_20210309.RData")


# ############################################# #
# Leer tablas Notasventa. (Solo la primera vez) #
############################################### #

# channel <- odbcConnectAccess("NotasVenta2019_Azti.mdb")
# sqlTables(channel)
# 
# NotasVenta2019        <- sqlFetch(channel, "NotasVenta2019_Azti",stringsAsFactors=FALSE)
# names(NotasVenta2019) <- make.names(names(NotasVenta2019))
# names(NotasVenta2019) <-gsub("\\.","",names(NotasVenta2019))

NotasVenta  <- read.table("C:\\use\\0_GitHub\\BD_ICES\\0_Datos\\Infobase\\2020\\NotasVenta2020_Azti.txt", sep=";", dec=",", header=TRUE, stringsAsFactors = FALSE)
save(NotasVenta,  file = "0_Datos/Infobase/2020/NotasVenta2020_Originales_20210224.RData")

# ----------------------------------------------------------------- #
