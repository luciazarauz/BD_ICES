# Readme: 
# Aquí se lee el fichero Infobase.mdb que nos envía el IEO y 
# sacamos el fichero DatosOficiales2019_Originales_ultimaversion.RData
# que está dentro de la carpeta Datos

# también se lee el fichero de NotasVenta.mdb que nos envía el IEO y 
# sacamos el fichero NotasVenta2019_Originale.RData
# que NO está dentro de la carpeta Datos


# #  R version 3.6.0 (2019-04-26) -- "Planting of a Tree"
# ## Necesario abrir R en 32 Bits
# ## rStudio 1.1 (1.2 no funciona con 32 bits)
# 
# # Libraries                 ####
# ############################## #
# rm(list=(ls()))
# 
# 
# library(RODBC)
# library(here)
# library (stringr)
# 
# 
# 
# # Definir el path           ####
# ############################## #
# 
# path.data <- file.path("Datos")
# 
# setwd(path.data)
# 
# 
# # Leer tablas BD. (Solo la primera vez) ####
# 
# channel <- odbcConnectAccess("InfoBase2019_Final_Azti.mdb")
# sqlTables(channel)
# 
# BarcosAzti <- sqlFetch(channel, "BarcosAzti",stringsAsFactors=FALSE)
# InfoBase <- sqlFetch(channel, "InfoBase",stringsAsFactors=FALSE)
# InfoBuques <- sqlFetch(channel, "InfoBuques",stringsAsFactors=FALSE)
# #InfoBuquesOk <- sqlFetch(channel, "InfoBuquesOk",stringsAsFactors=FALSE)
# InfoCapturaLance0 <- sqlFetch(channel, "InfoCapturaLance0",stringsAsFactors=FALSE)
# InfoCapturas <- sqlFetch(channel, "InfoCapturas",stringsAsFactors=FALSE)
# InfoCapturasCalculadas <- sqlFetch(channel, "InfoCapturasCalculadas",stringsAsFactors=FALSE)
# InfoDescartes <- sqlFetch(channel, "InfoDescartes",stringsAsFactors=FALSE)
# InfoDiarios <- sqlFetch(channel, "InfoDiarios",stringsAsFactors=FALSE)
# InfoOrigenLineas <- sqlFetch(channel, "InfoOrigenLineas",stringsAsFactors=FALSE)
# InfoParametrosArteCapturas <- sqlFetch(channel, "InfoParametrosArteCapturas",stringsAsFactors=FALSE)
# InfoVentas  <- sqlFetch(channel, "InfoVentas",stringsAsFactors=FALSE)
# #ParametrosArte <- sqlFetch(channel, "ParametrosArte",stringsAsFactors=FALSE)
# Puertos <- sqlFetch(channel, "Puertos",stringsAsFactors=FALSE)
# 
# close(channel)
# 
# 
# save(BarcosAzti, InfoBase, InfoBuques, InfoCapturaLance0, InfoCapturas, InfoCapturasCalculadas, InfoDescartes, InfoDiarios,
#      InfoOrigenLineas, InfoParametrosArteCapturas, InfoVentas, Puertos,  file = "DatosOficiales2019_Originales_ultimaversion.RData")
# 
# 
# 
# # Leer tablas Notasventa. (Solo la primera vez) ####
# setwd(path.venta)
# 
# 
# channel <- odbcConnectAccess("NotasVenta2019_Azti.mdb")
# sqlTables(channel)
# 
# NotasVenta2019 <- sqlFetch(channel, "NotasVenta2019_Azti",stringsAsFactors=FALSE)
# names(NotasVenta2019) <- make.names(names(NotasVenta2019))
# names(NotasVenta2019) <-gsub("\\.","",names(NotasVenta2019))
# save(NotasVenta2019,  file = "NotasVenta2019_Originales.RData")
# 
# 
