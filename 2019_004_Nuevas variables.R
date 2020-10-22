# ----------------------------------------------------------------- #
# Readme:                                                           #
# ----------------------------------------------------------------- # 
# En este script: quitamos tunidos tropicales, creamos las variables de cabecera de nuestra BD y asegurar que tienen dato, y creamos nuevas variables
#
# INDICE:
#
# Functions
# Libraries.
# Cargar la tabla maestra (dori).
# Crear variables
#
# ----------------------------------------------------------------- # 
# R version 3.6.0 (2019-04-26) -- "Planting of a Tree"
# ----------------------------------------------------------------- # 

rm(list=ls())

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





# Libraries             #####
############################ #

library(fishPiCodes)
library(lubridate)
library(data.table)
library(reshape2)
library(dplyr)


# Load                     ####
############################# #


load(file="Datos/Dori2019_v1.Rdata"   )


### Previo
data("UNLOCODE")
data("ASFIS")
head(UNLOCODE)
UNLOCODE$locName <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), UNLOCODE$locName)
UNLOCODE$locName <- mgsub(c("à", "è"), c("a", "e"), UNLOCODE$locName)
UNLOCODE$locName <- toupper(UNLOCODE$locName)



# Identificar y eliminar Atuneros Tropicales #####
################################################ #

# buques censados en cerco y operando con atuneros tropicales
table(Dori$Nombre [Dori$CensoPorModalidad %in% c("CERCO EN CANTABRICO NW") & substr(Dori$CodigoDivision, 1,2)!="27"])
table(Dori$CodigoPuertoDesembarque_AL5 [Dori$CensoPorModalidad %in% c("CERCO EN CANTABRICO NW") & substr(Dori$CodigoDivision, 1,2)!="27"])


# buques censados en arrastre y operando como atuneros tropicales
table(Dori$CensoPorModalidad[ Dori$Nombre %in% c("TXORI BI", "TXORI HIRU" ) ])
table(Dori$CodigoDivision[ Dori$Nombre %in% c("TXORI BI", "TXORI HIRU" ) ])
table(Dori$CodigoPuertoDesembarque_AL5[ Dori$Nombre %in% c("TXORI BI", "TXORI HIRU" ) ])
table(Dori$CodigoPuertoRegreso_AL5[ Dori$Nombre %in% c("TXORI BI", "TXORI HIRU" ) ])
table(Dori$CodigoPuertoSalida_AL5[ Dori$Nombre %in% c("TXORI BI", "TXORI HIRU" ) ])

Dori$CensoPorModalidad[ Dori$Nombre %in% c("AITA FRAXKU", "BERRIZ SAN FRANCISCO", "GAZTELUGAITZ", "IRIBAR ZULAIKA", "KERMANTXO", "NUEVO SAN LUIS" )] <- "ATUNEROS CERQUEROS CONGELADORES cerco en censo"
Dori$CensoPorModalidad[ Dori$Nombre %in% c("TXORI BI", "TXORI HIRU" )] <- "ATUNEROS CERQUEROS CONGELADORES arrastre en censo"

# Filtramos
Dori <- subset(Dori, !CensoPorModalidad %in% c("ATUNEROS CERQUEROS CONGELADORES EN OCEANO INDICO Y PACIFICO",
                                               "ATUNEROS CERQUEROS CONGELADORES EN OCEANO ATLANTICO, INDICO Y PACIFICO",
                                               "ATUNEROS CERQUEROS CONGELADORES arrastre en censo",
                                               "ATUNEROS CERQUEROS CONGELADORES cerco en censo"))



  # estos son errores de asignación del área
  table(Dori$Nombre [ substr(Dori$CodigoDivision, 1,2)!="27"] )
  table(Dori$CodigoDivision [ substr(Dori$CodigoDivision, 1,2)!="27"] )
  table(Dori$CensoPorModalidad [ substr(Dori$CodigoDivision, 1,2)!="27"] )
  table(Dori$CodigoPuertoDesembarque_AL5 [ substr(Dori$CodigoDivision, 1,2)!="27"] )
  table(Dori$CodigoPuertoRegreso_AL5 [ substr(Dori$CodigoDivision, 1,2)!="27"] )
  table(Dori$CodigoPuertoSalida_AL5 [ substr(Dori$CodigoDivision, 1,2)!="27"] )
  





#    Identificador de linea     ####
################################### #
Dori$IdDori <- 1:dim(Dori)[1]

#    Variables Cabecera     ####
############################## #
  # requieren completar NAs (no puede haber registros vacíos en la fecha de desembarque, puerto de desembarque...)
  # Identificamos las variables de cabecera con una "C_" al comienzo del nombre


# NAs en FechaRegreso/FechaDesembarco  ####
##### #
  # Priorizamos la fecha de regreso para la cabecera porque hay menos NAs. Ademas, la fecha de desembarque no tiene informacion datetime 
  # !> no se si la informacion de la hora es fiable.. pero en teoria nos puede servir para identificar varias mareas en el mismo día
  # hay un amarea con NA en FcRegreso. Tampoco tiene informacionde FcDesembarque. Utilizamos la ultima fecha de captura

sapply(Dori[grepl("Fc", names(Dori))], function(x) sum(is.na(x)))
temp <- Dori %>% group_by(IdDiario,FcRegreso,FcDesembarque) %>% summarize (dif_t = as.numeric((unique(FcRegreso) - unique(FcDesembarque)))) %>%
  filter(dif_t>0)
temp

Dori %>% group_by(IdDiario) %>% summarize (n = Fun_unique(FcRegreso[is.na(FcRegreso )])) %>% filter(n == 1)  # 1 mareas sin Fecha de Regreso
subset(Dori, IdDiario==574851)
unique(Dori$FcDesembarque[is.na(Dori$FcRegreso)] )
unique(Dori$FcSalida[is.na(Dori$FcRegreso)] )
unique(Dori$FcCaptura[is.na(Dori$FcRegreso)] )


Dori$C_FcRegreso <- Dori$FcRegreso  
FcMaxCap <- Dori %>% group_by(IdDiario) %>% summarise(C_Maxcap= max(FcCaptura, rm.na=T))
Dori$C_FcRegreso[is.na(Dori$C_FcRegreso)] <- FcMaxCap$C_Maxcap[match(Dori$IdDiario[is.na(Dori$C_FcRegreso)], FcMaxCap$IdDiario)]

head(Dori[grepl("Fc", names(Dori))])


# Crear IdMarea ####
##### #
    # CA: captura
    # DE: desembarcos
    # NV: nota de venta
    # Puede haber lineas que vienen de CA/DE con CodigoMarea, y otras lineas que vienen de NV sin CodigoMarea.
    # En la misma marea puede haber FcRegreso que vienen de CA/DE con las que vienen de NV. Cambian las horas. Por eso utilizamos la feha sin hora (floor)
    # criterio: Agrupamos por barco + fecha sin hora ->
    #           IdMarea: Codigomarea de la agrupación. Si la agrupación tiene varios CodigoMarea, los concatenamos. Concatenamos también NAs
    #           Si IdMarea == "NA" (no tienen ningun Codigomarea en la agrupación), creamos IdMarea = barco + fecha sin hora
    #           En aquellos IdMarea con varios CodigoMarea, IdMarea = CodigoMarea

Dori$C_FcRegresoFloor <- floor_date(ymd_hms(Dori$C_FcRegreso, tz = "Europe/Madrid"), "day")
Dori$Buque_FcRegresoFloor <- paste(Dori$IdBuque, Dori$C_FcRegresoFloor, sep="_")

Dori <- Dori %>% group_by(C_FcRegresoFloor, IdBuque) %>% mutate(IdMarea = paste(unique(CodigoMarea), collapse = "//")) %>% data.frame()
Dori$IdMarea <- gsub("NA//", "" ,Dori$IdMarea)
Dori$IdMarea <- gsub("//NA", "" ,Dori$IdMarea)

Dori$IdMarea[grepl("//", Dori$IdMarea)] <- Dori$CodigoMarea[grepl("//", Dori$IdMarea)] 
Dori$IdMarea[Dori$IdMarea=="NA"]   <- Dori$Buque_FcRegresoFloor[Dori$IdMarea=="NA"] 

Dori$IdMarea[is.na(Dori$IdMarea)]

# some checkings
  head(unique(Dori$IdMarea[grepl("//NA", Dori$IdMarea)]))
  head(unique(Dori$IdMarea[grepl("NA//", Dori$IdMarea)]))
  head(unique(Dori$IdMarea[grepl("//", Dori$IdMarea)]))
  head(unique(Dori$IdMarea[grepl("NA", Dori$IdMarea)]))
  head(unique(Dori$IdMarea[Dori$IdMarea=="NA"]))
  length(Dori$IdMarea[Dori$IdMarea=="NA"]) 
  dim(Dori[Dori$IdMarea=="NA",] )
  dim(Dori[is.na(Dori$IdMarea),] )
  
  Dori[Dori$IdMarea=="ESP-13703399//NA",]
  Dori[Dori$IdMarea=="ESP-TRP-00594820191219012808//ESP-TRP-00594820191220043440",]
  Dori[Dori$IdMarea=="NA",]
  
  length(unique(Dori$IdMarea))
  length(unique(Dori$IdMarea[Dori$PesoConsumo!=0]))
## @@ si hubiesemos quitado todas las lineas con pesoconsumo=0 (como hace el IEO), el numero de mareas totales es bastante menor
##    hay que comprobar si estas mareas que hemos creado, son reales o no  

# some checkings
  Dori %>%  group_by(C_FcRegresoFloor, IdBuque) %>% summarize (n = Fun_unique(IdMarea)) %>% filter(n > 1) 
  a<- subset(Dori, C_FcRegresoFloor=="2019-01-22 00:00:00" & IdBuque==43260); 
  a<- subset(Dori, C_FcRegresoFloor=="2019-01-15 00:00:00" & IdBuque==796082393); 
  a<- subset(Dori, C_FcRegresoFloor=="2019-01-08 00:00:00" & IdBuque==16622030); 
  a<- subset(Dori, C_FcRegresoFloor=="2019-01-19 00:00:00" & IdBuque==19856793); 
  a<- subset(Dori, C_FcRegresoFloor=="2019-10-16 00:00:00" & IdBuque==47152658 ); 
  
  select(a, c(Nombre, CensoPorModalidad, C_FcRegreso, CodigoMarea, IdMarea, C_FcRegresoFloor, OrigenIdentificador, PesoConsumo)) %>% data.frame
  
  # en la msima marea, mas de una fecha corta?
  Dori  %>% group_by(IdMarea) %>% summarize (n = Fun_unique(C_FcRegresoFloor)) %>% filter(n > 1)  # 
  Dori  %>% group_by(IdMarea) %>% summarize (n = Fun_unique(IdBuque)) %>% filter(n > 1)  # 
  Dori  %>% group_by(Buque_FcRegresoFloor) %>% summarize (n = Fun_unique(IdMarea)) %>% filter(n > 1)  # 
  
  head(unique(Dori$IdMarea[is.na(Dori$CodigoMarea) & substr(Dori$IdMarea,1,3)=="ESP"]))
  a<- subset(Dori, IdMarea=="41058_2019-01-15" ); table(a$C_FcRegreso)
  a<- subset(Dori, IdMarea=="ESP-13690256" ); table(a$C_FcRegreso)
  a<- subset(Dori, IdMarea=="ESP-11985178" ); table(a$C_FcRegreso)
  

# Crear IdMarea - codigo IEO ####
##### #
CodigoMareaAZTI <- data.frame(IdMarea = sort(unique(Dori$IdMarea)),
                              IdMareaIEO = 19900001: (19900000+length(unique(Dori$IdMarea))))
Dori$IdMareaIEO <- CodigoMareaAZTI$IdMareaIEO[match(Dori$IdMarea, CodigoMareaAZTI$IdMarea)]
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(IdMareaIEO )) %>% filter(n > 1)



# Puerto Regreso/ PuertoDesembarco ####
##### #
sapply(Dori[grepl("Puerto", names(Dori))], function(x) sum(is.na(x))) # Priorizamos el Puerto de Desembarque porque hay menos NAs

  # relleno NAs con info de la marea
Dori <- Dori %>% group_by(IdMarea) %>% mutate(C_CodigoPuertoDesembarque_AL5 = paste(unique(CodigoPuertoDesembarque_AL5), collapse = "//")) %>% data.frame()
Dori$C_CodigoPuertoDesembarque_AL5 <- gsub("NA//", "" ,Dori$C_CodigoPuertoDesembarque_AL5)
Dori$C_CodigoPuertoDesembarque_AL5 <- gsub("//NA", "" ,Dori$C_CodigoPuertoDesembarque_AL5)
Dori$C_CodigoPuertoDesembarque_AL5[grepl("//", Dori$C_CodigoPuertoDesembarque_AL5)] <- Dori$CodigoPuertoDesembarque_AL5[grepl("//", Dori$C_CodigoPuertoDesembarque_AL5)] 
Dori$C_CodigoPuertoDesembarque_AL5[grepl("NA", Dori$C_CodigoPuertoDesembarque_AL5)] <- NA

  # Selecciono los puertos con mas desembarcos
PuertoDesembarqueSelect <- Dori %>% group_by(IdMarea, C_CodigoPuertoDesembarque_AL5) %>% summarise(Peso = sum(PesoConsumo, na.rm=T)) %>%
  dcast(IdMarea ~ C_CodigoPuertoDesembarque_AL5, value.var="Peso")   
PuertoDesembarqueSelect<-as.data.table(PuertoDesembarqueSelect)
PuertoDesembarqueSelect$C_CodigoPuertoDesembarque_AL5 <- colnames(PuertoDesembarqueSelect[,-c("IdMarea"), with =F])[apply(PuertoDesembarqueSelect[,-c("IdMarea"), with =F],1,which.max)]
PuertoDesembarqueSelect<-as.data.frame(PuertoDesembarqueSelect)
PuertoDesembarqueSelect$C_CodigoPuertoDesembarque_AL5[PuertoDesembarqueSelect$C_CodigoPuertoDesembarque_AL5=="NA"] <- NA

  # relleno NAs
Dori$C_CodigoPuertoDesembarque_AL5 <- PuertoDesembarqueSelect$C_CodigoPuertoDesembarque_AL5[match(Dori$IdMarea, PuertoDesembarqueSelect$IdMarea)]
Dori$C_CodigoPuertoDesembarque_AL5[is.na(Dori$C_CodigoPuertoDesembarque_AL5)] <- Dori$CodigoPuertoRegreso_AL5[is.na(Dori$C_CodigoPuertoDesembarque_AL5)] 
Dori$C_CodigoPuertoDesembarque_AL5[is.na(Dori$C_CodigoPuertoDesembarque_AL5)] <- Dori$CodigoPuertoSalida_AL5[is.na(Dori$C_CodigoPuertoDesembarque_AL5)] 

Dori  %>% group_by(IdMarea) %>% summarize (n = Fun_unique(C_CodigoPuertoDesembarque_AL5)) %>% filter(n > 1)  # 

  # Nombre puerto
Dori$C_PuertoDesembarque <- UNLOCODE$locName[match(Dori$C_CodigoPuertoDesembarque_AL5,UNLOCODE$loCode )]




# Puerto de Venta y Fecha de Venta  #### 
#################################################### #
# para la cabecera necesitamos un único Puerto venta y Fecha Venta, 
# pero en algunas mareas hay varios puertos/fechas de venta
# !> esto es algo que habría que simplificar en la BD: quitar la fecha/puerto venta de la cabecera y pasarlo al detalle
################################################################ #

# Fecha venta
###
InfoVentas$IdMarea <- Dori$IdMarea[match(InfoVentas$IdDiario, Dori$IdDiario)]
InfoVentas$FcVentaFloor <- floor_date(ymd_hms(InfoVentas$FcVenta, tz = "Europe/Madrid"), "day")
InfoVentas <- InfoVentas %>% group_by(IdMarea) %>% mutate(C_FcVentaMax= max(FcVentaFloor, rm.na=T)) %>% data.frame
Dori$C_FcVentaMax <- InfoVentas$C_FcVentaMax[match(Dori$IdMarea, InfoVentas$IdMarea)]

  InfoVentas %>% group_by(IdMarea) %>% summarize(n=Fun_unique(CodigoPuertoVenta_AL5 )) %>% filter(n > 1)
  InfoVentas %>% group_by(IdMarea) %>% summarize(n=Fun_unique(FcVenta )) %>% filter(n > 1)
  InfoVentas %>% group_by(IdMarea) %>% summarize(n=Fun_unique(FcVentaFloor )) %>% filter(n > 1)
  
  subset(InfoVentas, IdMarea=="ESP-11954982")
  subset(InfoVentas, IdMarea=="119389129_2019-04-01" )
  subset(InfoVentas, IdMarea=="126108924_2019-06-17" )
  
  InfoVentas %>% group_by(IdDiario) %>% summarize(n=Fun_unique(FcVentaFloor )) %>% filter(n > 1)
  subset(InfoVentas, IdDiario=="407374" )
  subset(Dori, IdDiario=="407374" )
  
  InfoVentas %>% group_by(IdDiario) %>% summarize(n=Fun_unique(C_FcVentaMax )) %>% filter(n > 1)


# Puerto venta #
###
  #  @@ nos han quedado mareas sin puerto de venta asignado. 
  #     valora si aplicar el puerto de desembarco
PuertoVentaSelect <- InfoVentas %>% group_by(IdMarea, CodigoPuertoVenta_AL5) %>% summarise(Peso = sum(Peso, na.rm=T)) %>%
  dcast(IdMarea ~ CodigoPuertoVenta_AL5, value.var="Peso")   
PuertoVentaSelect<-as.data.table(PuertoVentaSelect)
PuertoVentaSelect$C_CodigoPuertoVenta_AL5 <- colnames(PuertoVentaSelect[,-c("IdMarea"), with =F])[apply(PuertoVentaSelect[,-c("IdMarea"), with =F],1,which.max)]
PuertoVentaSelect<-as.data.frame(PuertoVentaSelect)
PuertoVentaSelect$C_CodigoPuertoVenta_AL5[PuertoVentaSelect$C_CodigoPuertoVenta_AL5 =="NA"] <- NA
unique(PuertoVentaSelect$IdDiario[is.na(PuertoVentaSelect$C_CodigoPuertoVenta_AL5)])

Dori$C_CodigoPuertoVenta_AL5 <- PuertoVentaSelect$C_CodigoPuertoVenta_AL5[match(Dori$IdMarea, PuertoVentaSelect$IdMarea)]
Dori[is.na(Dori$C_CodigoPuertoVenta_AL5)]
length(unique(Dori$IdMarea[is.na(Dori$C_CodigoPuertoVenta_AL5)]))


# Division - area ####
#################################### #
  # asignamos el area de mayores capturas. 
  # en caso de NA, asignamos por puerto censo y puerto de desembarco

sapply(Dori[grepl("Division|Rect", names(Dori))], function(x) sum(is.na(x))) # 

  # Creamos el objeto AreaSelect para asignar un área a cada marea. Saca el área con mayores capturas
AreaSelect <- Dori %>% group_by(IdMarea, Nombre, CensoPorModalidad,  C_FcRegreso, C_PuertoDesembarque, CodigoDivision) %>% summarise(PesoConsumo = sum(PesoConsumo, na.rm=T)) %>%
  dcast(IdMarea + Nombre + CensoPorModalidad + C_FcRegreso + C_PuertoDesembarque ~ CodigoDivision, value.var="PesoConsumo")   
names(AreaSelect) <- make.names(names(AreaSelect))

  # Crear NArea
AreaSelect$NArea <- (apply(subset(AreaSelect, select=-c(NA.)), 1, function(x) length(x[!is.na(x)])))
AreaSelect$NArea <- AreaSelect$NArea -5

  # Crear C_Area
dropnames <- c("IdMarea", "Nombre", "CensoPorModalidad",  "C_FcRegreso", "C_PuertoDesembarque", "NArea")
AreaSelect<-as.data.table(AreaSelect)
AreaSelect$C_Area <- colnames(AreaSelect[,-dropnames, with =F])[apply(AreaSelect[,-dropnames, with =F],1,which.max)]
AreaSelect<-as.data.frame(AreaSelect)
AreaSelect$C_Area <- gsub("X", "",AreaSelect$C_Area)
AreaSelect$C_Area[AreaSelect$C_Area=="NA."] <- NA


  # Existen mareas sin area asignada. Son todas de CapturaLance0 o de descartes
z <- AreaSelect %>% filter( NArea==0) ## Todos los NA corresponden a CapturasLance0
table(Dori$CatchCategory[Dori$IdMarea %in% z$IdMarea])
table(Dori$CensoPorModalidad[Dori$IdMarea %in% z$IdMarea])

head(Dori[Dori$IdMarea %in% z$IdMarea,])
 
  # Rellenar NA/27.8/27.6/27.9 segun puerto desembarque. Por censo
###
  
    # arrastre cantabrico nw
  table(AreaSelect$C_PuertoDesembarque[(is.na(AreaSelect$C_Area) | AreaSelect$C_Area=="27.8") & 
                                             AreaSelect$CensoPorModalidad=="ARRASTRE DE FONDO EN CANTABRICO NW"] )
  AreaSelect$C_Area[(is.na(AreaSelect$C_Area) | AreaSelect$C_Area=="27.8") & 
                      AreaSelect$CensoPorModalidad=="ARRASTRE DE FONDO EN CANTABRICO NW"] <- "27.8.c"
  
    # arrastre VIII y VI
  table(AreaSelect$C_PuertoDesembarque[(is.na(AreaSelect$C_Area) | AreaSelect$C_Area %in% c("27.6","27.8")) & 
                                             AreaSelect$CensoPorModalidad=="ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII y VIIIabde."] )
  AreaSelect$C_Area[  (is.na(AreaSelect$C_Area) | AreaSelect$C_Area %in% c("27.6"))  & 
                       AreaSelect$CensoPorModalidad=="ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII y VIIIabde." & 
                       AreaSelect$C_PuertoDesembarque==c("BELFAST")] <- "27.6.a"
  AreaSelect$C_Area[ (is.na(AreaSelect$C_Area) | AreaSelect$C_Area %in% c("27.8"))  & 
                       AreaSelect$CensoPorModalidad=="ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII y VIIIabde." & 
                       AreaSelect$C_PuertoDesembarque==c("ONDARROA")] <- "27.8.b"
  
    # bacalao
  table(AreaSelect$C_PuertoDesembarque[is.na(AreaSelect$C_Area) & AreaSelect$CensoPorModalidad=="BACALADEROS"] )
  AreaSelect$C_Area[is.na(AreaSelect$C_Area) & AreaSelect$CensoPorModalidad=="BACALADEROS"] <- "27.2.b.2"
  
    # artes fijas
  table(AreaSelect$C_PuertoDesembarque[ (is.na(AreaSelect$C_Area) | AreaSelect$C_Area %in% c("27.8", "27.7", "27.6")) & 
                                         AreaSelect$CensoPorModalidad=="ARTES FIJAS EN ZONAS CIEM VB, VI,VII y VIIIabde."] )
  AreaSelect$C_Area[(is.na(AreaSelect$C_Area) | AreaSelect$C_Area %in% c("27.8", "27.7", "27.6"))  & 
                      AreaSelect$CensoPorModalidad=="ARTES FIJAS EN ZONAS CIEM VB, VI,VII y VIIIabde." &
                      AreaSelect$C_PuertoDesembarque %in% c("ONDARROA", "PASAJES", "SANTANDER")] <- "27.8.b" 
  AreaSelect$C_Area[(is.na(AreaSelect$C_Area) | AreaSelect$C_Area %in% c("27.8", "27.7", "27.6"))  & 
                      AreaSelect$CensoPorModalidad=="ARTES FIJAS EN ZONAS CIEM VB, VI,VII y VIIIabde." &
                      AreaSelect$C_PuertoDesembarque %in% c("CASTLETOWN BEARHAVEN")] <- "27.6.a" 
  
    # resto: artes menores, cerco, rasco, volanta, palangre
  resto <- c("ARTES MENORES EN CANTABRICO NW", "CERCO EN CANTABRICO NW", "PALANGRE DE FONDO EN CANTABRICO NW",
             "PALANGRE DE SUPERFICIE CALADERO NACIONAL", "RASCO EN CANTABRICO NW", "VOLANTA EN CANTABRICO NW")
  
  table(AreaSelect$C_PuertoDesembarque[(is.na(AreaSelect$C_Area) | AreaSelect$C_Area=="27.8") & 
                                             AreaSelect$CensoPorModalidad %in% resto])
  AreaSelect$C_Area[(is.na(AreaSelect$C_Area) | AreaSelect$C_Area=="27.8") & 
                      AreaSelect$CensoPorModalidad %in% resto &
                      AreaSelect$C_PuertoDesembarque %in% c("FUENTERRABIA", "PASAJES", "SAN SEBASTIAN")] <- "27.8.b"
  AreaSelect$C_Area[(is.na(AreaSelect$C_Area) | AreaSelect$C_Area=="27.8") & 
                      AreaSelect$CensoPorModalidad %in% resto &
                      !AreaSelect$C_PuertoDesembarque %in% c("FUENTERRABIA", "PASAJES", "SAN SEBASTIAN")] <- "27.8.c"
  
  table(AreaSelect$C_PuertoDesembarque[(is.na(AreaSelect$C_Area) | AreaSelect$C_Area=="27.9") & 
                                         AreaSelect$CensoPorModalidad %in% resto])
  AreaSelect$C_Area[ (is.na(AreaSelect$C_Area) | AreaSelect$C_Area=="27.9") & 
                       AreaSelect$CensoPorModalidad %in% resto] <- "27.9.a"
  
  table(AreaSelect$C_PuertoDesembarque[ (AreaSelect$C_Area=="34.1.2" | AreaSelect$C_Area=="34.2") & 
                                         AreaSelect$CensoPorModalidad %in% resto])
  AreaSelect$C_Area[ (AreaSelect$C_Area=="34.1.2" | AreaSelect$C_Area=="34.2") &
                       AreaSelect$C_PuertoDesembarque %in% c("ELANCHOVE")] <- "27.8.c"
  AreaSelect$C_Area[ (AreaSelect$C_Area=="34.1.2" | AreaSelect$C_Area=="34.2") &
                       AreaSelect$C_PuertoDesembarque %in% c("LA CORUÑA")] <- "27.8.c"
  
    # checkings
  AreaSelect %>% filter( is.na(C_Area))
  table(AreaSelect$C_Area)
  
  # Asignar área a Dori
##
Dori$C_Area <- AreaSelect$C_Area[ match(Dori$IdMarea , AreaSelect$IdMarea)]             

Dori  %>% group_by(IdMarea) %>% summarize (n = Fun_unique(C_Area)) %>% filter(n > 1)  # 


# Nombre Puerto ####
#################################### #

Dori$C_PuertoVenta <- UNLOCODE$locName[match(Dori$C_CodigoPuertoVenta_AL5,UNLOCODE$loCode )]
Dori$PuertoSalida <- UNLOCODE$locName[match(Dori$CodigoPuertoSalida_AL5,UNLOCODE$loCode )]
Dori$PuertoDesembarque <- UNLOCODE$locName[match(Dori$CodigoPuertoDesembarque_AL5,UNLOCODE$loCode )]
Dori$PuertoRegreso <- UNLOCODE$locName[match(Dori$CodigoPuertoRegreso_AL5,UNLOCODE$loCode )]


Dori$PuertoBase <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), Dori$PuertoBase)
Dori$PuertoBase <- toupper(Dori$PuertoBase)
names(Dori)[names(Dori)=="AL5_PuertoBase"] <- "CodigoPuertoBase_AL5"

Dori$C_PuertoDesembarque <- toupper(Dori$C_PuertoDesembarque)
Dori$C_PuertoVenta <- toupper(Dori$C_PuertoVenta)


Dori$CodigoPuertoSalida_AL5[is.na(Dori$PuertoSalida) & !is.na(Dori$CodigoPuertoSalida_AL5)]
Dori$CodigoPuertoDesembarque_AL5[is.na(Dori$PuertoDesembarque) & !is.na(Dori$CodigoPuertoDesembarque_AL5)]
Dori$CodigoPuertoRegreso_AL5[is.na(Dori$PuertoRegreso) & !is.na(Dori$CodigoPuertoRegreso_AL5)]

subset(Dori, CodigoPuertoSalida_AL5=="OMFAH")
subset(UNLOCODE, loCode=="OMFAH" )

# Especie ####
#################################### #
# NAs en especie #
##
sapply(Dori[grepl("Esp", names(Dori))], function(x) sum(is.na(x))) # 
table(Dori$CatchCategory[is.na(Dori$Especie_AL3)])
  # son todas de captura lance cero

#Especie cientifico #
##
Dori$Especie_Sci <- ASFIS$Scientific_name[match(Dori$Especie_AL3, ASFIS$X3A_CODE)]
Dori$Especie_Sci[is.na(Dori$Especie_AL3)] <- NA

Dori$Especie_AL3[is.na(Dori$Especie_Sci) & !is.na(Dori$Especie_AL3)]


# PesoConsumo/ PesoConsumoBajoTalla/ PesoDescarte ####
#################################### #
sapply(Dori[grepl("Peso", names(Dori))], function(x) sum(is.na(x)))


#Posicion ####
#################################### #
# @@ funcion para hacer esto? se podría mover al codigo de depuracion para que las X e Y coincidieran con las de Capturas

geom <- sub(Dori$Posicion, pattern = "POINT", replacement = "")
geom <- sub(geom, pattern = "[( ]", replacement = "")
geom <- sub(geom, pattern = "[(]", replacement = "")
geom <- sub(geom, pattern = "[)]", replacement = "")
geom[is.na(geom)] <- "0 0"
geom[geom==""] <- "0 0"
latlon <- as.data.frame(matrix(as.numeric(unlist(strsplit(geom, split = " "))),ncol=2,byrow=TRUE), stringsAsFactors = FALSE)
names(latlon) <- c("Lat", "Lon")
latlon[latlon$Lat==0 & latlon$Lon==0,] <- NA
Dori$Lat <- latlon$Lat
Dori$Lon <- latlon$Lon

sum(latlon$Lat, na.rm=TRUE)
sum(Dori$Lat, na.rm=TRUE)
dim(subset(latlon, !is.na(Lat)))
dim(subset(latlon, !is.na(Lon)))
dim(subset(Dori, !is.na(Lat)))


# Crear CodigoOrigen y NCodigoOrigen ####
#################################### #
# Hay NAs- Los NAs tienen PesoConsumo = 0
# sustituimos NAs por cero
unique(substr(Dori$OrigenIdentificador,1,2))
head(Dori[is.na(Dori$OrigenIdentificador),])
head(Dori[is.na(Dori$OrigenIdentificador) & Dori$PesoConsumo>0,])

Dori$CodigoOrigen <- substr(Dori$OrigenIdentificador,1,2)
Dori$CodigoOrigen[is.na(Dori$CodigoOrigen)] <- 0

Dori<- Dori %>% group_by(IdMarea) %>%
  mutate(NCodigoOrigen=length(unique(CodigoOrigen[CatchCategory=="CapturasCalculadas"]))) %>%
  ungroup() %>% as.data.frame()



# Laboratorio ####
#################################### #

PuertoVasco <- c("ALGORTA","ARMINTZA", "BILBAO", "BERMEO", "CIERVANA","ELANCHOVE",
                 "FUENTERRABIA","GUETARIA", "GETARIA", "LEQUEITIO","MOTRICO",
                 "ONDARROA", "ORIO","PASAJES","PLENCIA","SAN SEBASTIAN",
                 "SANTURCE","ZUMAYA")

Dori$LAB_PuertoDesembarco <- NA
Dori$LAB_PuertoDesembarco [Dori$C_PuertoDesembarque %in% PuertoVasco] <- "EUS"
Dori$LAB_PuertoDesembarco [is.na(Dori$LAB_PuertoDesembarco) & substr(Dori$C_CodigoPuertoDesembarque_AL5,1,2)=="ES"] <- "ESP"
Dori$LAB_PuertoDesembarco [is.na(Dori$LAB_PuertoDesembarco) & substr(Dori$C_CodigoPuertoDesembarque_AL5,1,2)!="ES"] <- "OTH"
Dori[is.na(Dori$LAB_PuertoDesembarco),]
table(Dori$LAB_PuertoDesembarco)

Dori$LAB_PuertoBase <- NA
Dori$LAB_PuertoBase [Dori$PuertoBase %in% PuertoVasco] <- "EUS"
Dori$LAB_PuertoBase [is.na(Dori$LAB_PuertoBase) & substr(Dori$CodigoPuertoBase_AL5,1,2)=="ES"] <- "ESP"
Dori$LAB_PuertoBase [is.na(Dori$LAB_PuertoBase) & substr(Dori$CodigoPuertoBase_AL5,1,2)!="ES"] <- "OTH"
Dori[is.na(Dori$LAB_PuertoBase),]
table(Dori$LAB_PuertoBase)


Dori$LABORATORIO<- NA
Dori$LABORATORIO [is.na(Dori$LABORATORIO) & Dori$LAB_PuertoDesembarco == "EUS"] <-  "AZTI"
Dori$LABORATORIO [is.na(Dori$LABORATORIO) & Dori$LAB_PuertoDesembarco == "OTH" & Dori$LAB_PuertoBase=="EUS"] <-  "AZTI"
Dori$LABORATORIO [is.na(Dori$LABORATORIO) ]<- "IEO"
#Dori$LABORATORIO [ Dori$Nombre == "NUEVO CHAROLAIS" ]<- "IEO"



Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(LABORATORIO )) %>% filter(n > 1)
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(LABORATORIO )) %>% filter(n > 1)

tapply(Dori$PesoConsumo, list(Dori$LAB_PuertoBase, Dori$LAB_PuertoDesembarco,Dori$LABORATORIO), sum)




# # Cruce especies (revisión Iñaki) ####
# Dori$EspecieBD <- MaestroEspecies$Nombre.Oficial[match(Dori$Especie_AL3,MaestroEspecies$Cod..ALFA.3 )]
# Dori$EspecieSciBD <- MaestroEspecies$Nombre.Cient?fico[match(Dori$Especie_AL3,MaestroEspecies$Cod..ALFA.3 )]
# unique(Dori$Especie_AL3[is.na(Dori$EspecieBD)]  )
# 
# Dori$EspecieASFIS <- ASFIS$Spanish_name[match(Dori$Especie_AL3,ASFIS$X3A_CODE )]
# Dori$EspecieSciASFIS <- ASFIS$Scientific_name [match(Dori$Especie_AL3,ASFIS$X3A_CODE )]
# unique(Dori$Especie_AL3[is.na(Dori$EspecieASFIS)]  )



# 



# Check: Que cada Idmarea tenga un solo IdDiario, IdBuque, Fcregreso..  #####
############################################################################# #

sapply(Dori[grepl("C_", names(Dori))], function(x) sum(is.na(x))) # 


# diferentes IdDiario: OK
##
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(IdDiario )) %>% filter(n > 1 ) # son IdDiarios negativos que vienen de NV
Dori %>% group_by(IdMarea) %>% filter(IdDiario > 0) %>% summarize(n=Fun_unique(IdDiario )) %>% filter(n > 1 )

# diferentes IdBuque: OK
##
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(IdBuque )) %>% filter(n > 1)

# diferentes FechaRecreso/FechaDesembarco:OK
##
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(C_FcRegresoFloor )) %>% filter(n > 1)
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(C_FcRegreso )) %>% filter(n > 1)

# diferentes Puerto: OK
##
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(C_CodigoPuertoDesembarque_AL5 )) %>% filter(n > 1)
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(C_PuertoDesembarque )) %>% filter(n > 1)


# diferentes Areas
##
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(C_Area )) %>% filter(n > 1)

# diferentes FechaVenta y PuertoVenta
##
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique( C_FcVentaMax  )) %>% filter(n > 1) 
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(C_CodigoPuertoVenta_AL5 )) %>% filter(n > 1) 



# Areas por censo Check
for (i in sort(unique(Dori$CensoPorModalidad))){
  temp <- subset(Dori, CensoPorModalidad == i)
  print(tapply(temp$IdMarea, list(temp$C_Area,temp$CensoPorModalidad), Fun_unique))
  
}


# Grabar fichero                          ####
############################################### #

setwd(path.data)
save(Dori, file="Datos/Dori2019_v2.RData")


