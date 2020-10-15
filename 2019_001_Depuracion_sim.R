# 
# R version 3.6.0 (2019-04-26) -- "Planting of a Tree"
# rStudio 1.1 

# Librerias                 ####
############################## #
rm(list=(ls()))

library(fishPiCodes)
data("UNLOCODE")
library (stringr)
library(doBy)
library(dplyr)
library(lubridate)
library(data.table)
library(reshape2)


# Funciones                 ####
############################## #
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


# Definir el path           ####
############################## #

path.data <-  file.path("C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\2020\\15_Simulacro\\20200421_InfoBaseFinal")
path.aux  <-  file.path("C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\2020\\15_Simulacro\\Auxtables")
path.res  <-  file.path("C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\2020\\15_Simulacro\\Results")
# 


# Cargar datos             ####
############################# #

# Tablas auxiliares #
setwd(path.aux)

# Puertos <- read.csv("Puertos.csv", sep=",", stringsAsFactors = FALSE)
MaestroBuques <- read.csv("MaestroBuquesAZTI_2019.txt", sep="\t", dec=",", stringsAsFactors = FALSE)
MaestroBuques <- MaestroBuques [, !names(MaestroBuques) %in% c("Observaciones","Tipo.pesca.principal", "Segmento", "Varios")]
names(MaestroBuques) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(MaestroBuques))
MaestroBuques$Caladero.principal <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), MaestroBuques$Caladero.principal)
CensoBuques <- read.csv("Censo Flota Operativa_ene 2019.txt", sep="\t", dec=",", stringsAsFactors = FALSE)

# Infobase      #
setwd(path.data)
load(file = "DatosOficiales2019_Originales_ultimaversion.RData")

# NotasVentas   #
# setwd(path.venta)
# load(file = "NotasVenta2019_Originales.RData")

setwd(path.res)


# Overview of the tables ########################
############################################### #
  
head(Puertos)
head(BarcosAzti)
head(InfoBuques)
#head(InfoBuquesOk)

head(InfoBase)
head(InfoDiarios)
head(InfoOrigenLineas)
head(InfoCapturas)
head(InfoCapturasCalculadas)

head(InfoCapturaLance0)
head(InfoVentas)
head(InfoDescartes)

head(InfoParametrosArteCapturas)
#head(ParametrosArte)


# Comprobar tablas- Primary Key    #######################
########################################################## #
# revisar cada tabla y ver cual es su primary key


# BarcosAZTI
############## #
  # hay barcos que no tienen el IdBuque, porque no aparecen en InfoBuques
  # son barcos que estan en el censo español. Pero he comprobado que no tienen desembarcos en 2019. 
  # algunos son buques auxiliares de atuneros congeladores
head(BarcosAzti)
check <- subset(BarcosAzti, is.na(IdBuque))
check

subset(InfoBuques, CodigoCFR %in% c(check$CodigoCFR))
subset(CensoBuques, CODIGO_BUQUE %in% as.integer(substr(check$CodigoCFR,4,12)))[,c("CODIGO_BUQUE", "NOMBRE_BUQUE", "PUERTO_BASE",
                                                                                   "CENSOPORMODALIDAD")]


# InfoBuques
############### #
  # Hay barcos con varios registros por cambios datos tecnicos/motor
  # NUEVA TABLA: InfoBuquesUnique
  #              primary Key: IdBuque o CodigoCFR

  # Identificamos los barcos que han cambiado de Puertobase o caracteristicas tecnicas en 2019 y estan en nuestra BD
CheckId <- InfoBuques %>%  count(CodigoCFR) %>% filter(n > 1) 
CheckMaestroBuques <- InfoBuques [InfoBuques$CodigoCFR %in% CheckId$CodigoCFR & 
                                    InfoBuques$CodigoCFR %in% unique(MaestroBuques$Codigo.UE),]
CheckMaestroBuques <- CheckMaestroBuques %>% arrange(CodigoCFR)
write.table(CheckMaestroBuques, "CheckMaestroBuques.csv", sep=",", dec=".", row.names = FALSE)
write.table(InfoBuques, "MaestroBuques.csv", sep=",", dec=".", row.names = FALSE)

  # Creamos nueva tabla con un registro para cada barco
  # !> lo mas correcto sería cruzarlo con InfoDiarios en función de la fecha 
names(InfoBuques) <- iconv(names(InfoBuques),to="ASCII//TRANSLIT")
InfoBuques$fcCambio2 <- ymd(InfoBuques$fcCambio)

InfoBuquesUnique <- InfoBuques %>% group_by( IdBuque, CodigoCFR) %>%
summarise(fcCambio2=max(fcCambio2)) %>%
left_join(InfoBuques, by=c("IdBuque", "CodigoCFR", "fcCambio2")) %>%
ungroup()%>% as.data.frame() 

InfoBuquesUnique %>%  count(CodigoCFR) %>% filter(n > 1)  
subset(InfoBuquesUnique, is.na(CodigoCFR)) ## 
subset(InfoBuquesUnique, is.na(CodigoCFR), select = c("IdBuque", "CodigoCFR", "Nombre", "PuertoBase",
                                                      "CensoPorModalidad")) 
subset(InfoDiarios, IdBuque %in% c(51346396, 79454692))
  ##  quedan 2 barcos sin codigo CFR. Pero no tienen ningun registro en InfoDiarios. Se pueden eliminar
InfoBuquesUnique <- subset(InfoBuquesUnique, !IdBuque %in% c(51346396, 79454692))

    #Por si queda algún duplicado por algun cambio que no tenga fecha 
    #(se supone que ya no va apasar pero por si acaso)
        #CheckId <-  duplicated(InfoBuquesUnique[, names(InfoBuquesUnique)!="CensoPorModalidad"])
        #InfoBuquesUnique[CheckId,] 
        #InfoBuquesUnique<- InfoBuquesUnique[!CheckId,]  


# InfoDiarios
################ #
  #  Primary key = IdDiarios
head(InfoDiarios)
dim(InfoDiarios)
length(unique(InfoDiarios$IdDiario))

temp <- InfoDiarios %>%  count(IdDiario) %>% filter(n > 1)
temp


# InfoVentas
################ #
  #  Primary key = IdVenta

head (InfoVentas)
dim (InfoVentas)
length(unique(InfoVentas$IdVenta))  
length(unique(InfoVentas$idDiario))

temp <- InfoVentas %>%  count(IdVenta) %>% filter(n > 1)
temp


# InfoOrigenLineas
################ #
  #  Primary key = IdInfoOrigenLineas 
dim (InfoOrigenLineas)
length(unique(InfoOrigenLineas$IdInfoOrigenLineas))
dim(subset(InfoOrigenLineas, is.na(IdInfoOrigenLineas)))


# Depurar InfoCapturas
###################### #  
  #  Primary key = IdCaptura 
head(InfoCapturas)
dim (InfoCapturas)
length(unique(InfoCapturas$IdCaptura))



# Depurar InfoCapturasCalculadas
################################ #  
  # Primary key: IdCapturaCalculada
head(InfoCapturasCalculadas)
dim (InfoCapturasCalculadas)
length(unique(InfoCapturasCalculadas$c ))


# InfoCapturaLance0
################### #  
  # Primary key: IdCaptura
head(InfoCapturaLance0)
dim (InfoCapturaLance0)
length(unique(InfoCapturaLance0$IdCaptura))

    
    # hay registros en esta tabla con esfuerzo igual a cero -> preguntar a SGP
    length(unique(InfoCapturaLance0$IdDiario))
    dim(InfoCapturaLance0[InfoCapturaLance0$NumOperaciones>0,])
    length(unique(InfoCapturaLance0$IdDiario[InfoCapturaLance0$NumOperaciones>0]))
    dim(InfoCapturaLance0[InfoCapturaLance0$TiempoPescaMin>0,])
    length(unique(InfoCapturaLance0$IdDiario[InfoCapturaLance0$TiempoPescaMin>0]))
    
    dim(InfoCapturaLance0[InfoCapturaLance0$NumOperaciones>0 | InfoCapturaLance0$TiempoPescaMin>0,])
    length(unique(InfoCapturaLance0$IdDiario[InfoCapturaLance0$NumOperaciones>0 | InfoCapturaLance0$TiempoPescaMin>0]))
    


# InfoDescartes
################ #  
  # Primary key: IdDescarte
head(InfoDescartes)
dim (InfoDescartes)
length(unique(InfoDescartes$IdDescarte))


# InfoParametrosArteCapturas
############################ #  
  # new table:  InfoParametrosArteCapturas_wide
  # Primary Key:   IdCaptura

head(InfoParametrosArteCapturas)

InfoParametrosArteCapturas %>% count( IdCaptura, CodigoParametroArte) %>% filter(n > 1)

InfoParametrosArteCapturas_wide <- dcast(InfoParametrosArteCapturas, IdCaptura + CodigoArte_FaoAL3 + Arte ~ CodigoParametroArte + ParametroArte, value.var= "ValorParametro", fun.aggregate = sum)
dim (InfoParametrosArteCapturas_wide)
length(unique(InfoParametrosArteCapturas_wide$IdCaptura))




######################################################## #
# revisar consistencia en los nombres          ###########
######################################################## #
  
# InfoBuques 
############ #
names(InfoBuquesUnique)  

  
# InfoDiarios   
############# #
names(InfoDiarios)

  
# InfoVentas  
############# #
head (InfoVentas)
names(InfoVentas)
names(InfoVentas)[names(InfoVentas)=="idDiario"] <- "IdDiario"


# InfoOrigenLineas 
################## #
head(InfoOrigenLineas)
names(InfoOrigenLineas)
names(InfoOrigenLineas)[names(InfoOrigenLineas)=="IdInfoOrigenLineas"] <- "IdInfoOrigenLinea"
names(InfoOrigenLineas)[names(InfoOrigenLineas)=="PuertoRegreso_AL5"] <- "CodigoPuertoDesembarque_AL5"
names(InfoOrigenLineas)[names(InfoOrigenLineas)=="idDivision"] <- "CodigoDivision"
names(InfoOrigenLineas)[names(InfoOrigenLineas)=="rectanguloEstadistico"] <- "RectanguloEstadistico"


# Depurar InfoCapturas 
###################### #
head(InfoCapturas)
names(InfoCapturas)


# Depurar InfoCapturasCalculadas
################################ #
head(InfoCapturasCalculadas)
names(InfoCapturasCalculadas)


# InfoCapturaLance0
#################### #
# Posibiliad de filtrar las mareas con Numero de Operaciones/Tiempo de Pesca > 0 
head(InfoCapturaLance0)
dim(subset(InfoCapturaLance0, NumOperaciones>0))


names(InfoCapturaLance0)
names(InfoCapturaLance0)[names(InfoCapturaLance0)=="PuertoSalida_AL5"] <- "CodigoPuertoSalida_AL5"
names(InfoCapturaLance0)[names(InfoCapturaLance0)=="PuertoRegreso_AL5"] <- "CodigoPuertoRegreso_AL5"
names(InfoCapturaLance0)[names(InfoCapturaLance0)=="PuertoDesembarque_AL5"] <- "CodigoPuertoDesembarque_AL5"


# InfoDescartes
############### #
head(InfoDescartes)
names(InfoDescartes)
names(InfoDescartes)[names(InfoDescartes) == "Peso"] <- "PesoDescarte"
names(InfoDescartes)[names(InfoDescartes) == "NumPiezas"] <- "NumPiezasDescartadas"
names(InfoDescartes)[names(InfoDescartes) == "PesoBajoTalla"] <- "PesoDescarteBajoTalla"
names(InfoDescartes)[names(InfoDescartes) == "NumPiezasBajoTalla"] <- "NumPiezasDescartadasBajoTalla"
names(InfoDescartes)[names(InfoDescartes) == "CodigoRectanguloEstadistico"] <- "RectanguloEstadistico"
names(InfoDescartes)[names(InfoDescartes) == "Pais_AL3"] <- "PaisBandera_AL3"


# InfoParametrosArteCapturas
############################# #
head(InfoParametrosArteCapturas_wide)
names(InfoParametrosArteCapturas_wide)

names(InfoParametrosArteCapturas_wide) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(InfoParametrosArteCapturas_wide))
names(InfoParametrosArteCapturas_wide) <- make.names(names(InfoParametrosArteCapturas_wide))
InfoParametrosArteCapturas_wide <- rename(InfoParametrosArteCapturas_wide, 
                                          Profundidad_m                  = FD_Profundidad..metros.,
                                          TamañoMedioAnzuelos_mm         = GC_Tamaño.medio.de.los.anzuelos..mm.,       
                                          Altura_m                       = GD_Altura..metros.,                        
                                          AlturaMediaRedes_m             = GD_Altura.media.de.las.redes..m. ,          
                                          LongitudMediaRedes_m           = GL_Logitud.media.de.las.redes..metros.  ,  
                                          Longitud_m                     = GL_Longitud..metros.  ,                     
                                          TamanoMalla_m                  = ME_Tamaño.malla..mm.  ,                    
                                          NumeroTotalAnzuelos            = NH_Numero.total.de.anzuelos  ,              
                                          NumeroNasas                    = NN_Numero.de.nasas  ,                      
                                          NumeroPalangresLineasLanzadas  = NN_Numero.de.palangres.o.lineas.lanzadas  ,
                                          NumeroRedesLanzadas            = NN_Numero.de.redes.lanzadas ,              
                                          NumeroTotalRedes               = QG_Numero.total.de.redes.a.bordo  ,         
                                          LomgitudTotalRedes             = TL_Longitud.total.de.las.redes.a.bordo..m.)



# Save data                 ####
############################# #
setwd(path.data)
save(InfoBase, InfoBuquesUnique, InfoDiarios, InfoVentas, InfoOrigenLineas, 
     InfoCapturaLance0, InfoCapturas, InfoCapturasCalculadas, InfoDescartes, 
     InfoParametrosArteCapturas_wide,  file="Infobase2019_Unique_20200724.Rdata"  )

    