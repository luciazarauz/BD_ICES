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
#data(package = "fishPiCodes")
library(fishPifct)
#data(package = "fishPifct")
library(lubridate)
library(data.table)
library(reshape2)
library(dplyr)
library(tidyr)
data("UNLOCODE")
data("ASFIS")

# Load                     ####
############################# #

### Fichero Dori
load(file="0_Datos/Infobase/2020/Dori2020_v1.Rdata"   )


### Data sets
data("UNLOCODE")
data("ASFIS")
str(UNLOCODE)
str(ASFIS)

head(UNLOCODE)
UNLOCODE$locName <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), UNLOCODE$locName)
UNLOCODE$locName <- mgsub(c("à", "è"), c("a", "e"), UNLOCODE$locName)
UNLOCODE$locName <- toupper(UNLOCODE$locName)

head(ASFIS)
ASFIS <- ASFIS %>% mutate_if(is.factor, as.character)


################################################### #   
#    DEPURACIONES                                ####
################################################### #  

# .Sustituir "" por NA                        #####
################################################ #

Dori %>%  summarise_all(funs(sum(is.na(.))))

table(Dori$CatchCategory[is.na(Dori$Especie_AL3)])

Dori %>%  summarise_all(funs(length(which(. =="" ))))
length(Dori$CodigoDivision[which(!is.na(Dori$CodigoDivision) & Dori$CodigoDivision!="")])

Dori <- Dori %>% mutate_all(funs(replace(., which(. ==""), NA)))

length(Dori$CodigoDivision[which(!is.na(Dori$CodigoDivision) & Dori$CodigoDivision!="")])


# .Identificar y eliminar Atuneros Tropicales #####
################################################ #

# buques censados en cerco y operando con atuneros tropicales
table(Dori$Nombre [Dori$CensoPorModalidad %in% c("CERCO EN CANTABRICO NW") & substr(Dori$CodigoDivision, 1,2)!="27"])
table(Dori$CodigoPuertoDesembarque_AL5 [Dori$CensoPorModalidad %in% c("CERCO EN CANTABRICO NW") & substr(Dori$CodigoDivision, 1,2)!="27"])

table(Dori$Nombre [Dori$CensoPorModalidad %in% c("CERCO EN CANTABRICO NW") & 
                     substr(Dori$CodigoDivision, 1,2)!="27" & 
                     substr(Dori$CodigoPuertoRegreso_AL5,1,2)!="ES"])


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
  
  # corrijo
  Dori$CodigoDivision [!is.na(Dori$CodigoDivision) & substr(Dori$CodigoDivision, 1,2)!="27"] <- "27.8.c"


# .Corregir nombre puerto erroneo             #####
################################################ #
  
  # identifico
    sapply(Dori[grepl("Puerto", names(Dori))], function(x) sum(is.na(x)))
    
    
    CheckPuerto1 <- Dori %>% select(IdDiario, CodigoPuertoSalida_AL5, CodigoPuertoRegreso_AL5, CodigoPuertoDesembarque_AL5) %>%
                            pivot_longer(!IdDiario, names_to = "Port", values_to = "Code") %>%
                            filter(!is.na(Code))
    CheckPuerto2 <- InfoVentas %>% filter(IdDiario %in% unique(Dori$IdDiario)) %>%
                            select(IdDiario, CodigoPuertoVenta_AL5) %>%
                            pivot_longer(!IdDiario, names_to = "Port", values_to = "Code") %>%
                            filter(!is.na(Code))
    
    CheckPuerto <- rbind( CheckPuerto1, CheckPuerto2) 
    CheckPuerto <- unique(CheckPuerto[,c("Port",   "Code" )])

    CheckPuerto_codes <- subset(CheckPuerto, !Code %in% MaestroPuertos$Codigo.RDB)
    CheckPuerto_codes
    
    # busco lineas erroneas
    Dori %>% filter( CodigoPuertoSalida_AL5=="ESADI") %>% select(c("Nombre", "FcRegreso", "CodigoMarea", names(Dori)[ grepl("Puerto", names(Dori))]))
    Dori %>% filter( CodigoPuertoDesembarque_AL5=="FRMAC") %>% select(c("Nombre", "FcRegreso", "CodigoMarea", names(Dori)[ grepl("Puerto", names(Dori))]))
    InfoVentas %>% filter( CodigoPuertoVenta_AL5=="ESZJY")
    InfoVentas %>% filter( CodigoPuertoVenta_AL5=="ESMAN")
    InfoVentas %>% filter( CodigoPuertoVenta_AL5=="FRMAC")
    
    
    # Identifico mareas sospechosas con ventas en Pasaia y en Huelva, Mallorca..
    check <- InfoVentas %>% filter( CodigoPuertoVenta_AL5 %in% c("ESZJY","ESMAN", "ESAGA")) %>% group_by(IdDiario) %>% summarise(Nrows = length(IdInfoBase))
    check1 <- Dori %>% filter(IdDiario %in% check$IdDiario) %>% group_by(Nombre, IdDiario) %>% summarise(PesoConsumo=sum(PesoConsumo))
    check2 <- InfoVentas %>% filter(IdDiario %in% check$IdDiario) %>% group_by(IdDiario, CodigoPuertoVenta_AL5) %>% summarise(Peso=sum(Peso))%>%
                    pivot_wider(names_from=CodigoPuertoVenta_AL5, values_from = Peso) %>%
                    as.data.frame()
    checkFin <- check1 %>% left_join(check2, by=c("IdDiario")) %>% data.frame()
    checkFin
    
    Dori%>% filter(IdDiario==-26741019) 
    Dori%>% filter(IdDiario==1174916) %>% group_by(CodigoPuertoDesembarque_AL5) %>% summarise(Peso=sum(PesoConsumo))
    InfoVentas%>% filter(IdDiario==1174916)
    
    # corrijo
    ##
    # CodigoPuertoSalida_AL5 =="ESADI" -> faltaba el código en maestros. La incluyo
    # CodigoPuertoVenta_AL5  =="FRMAC" -> es un error. Cambio a "ESBRM" (bermeo)
    # CodigoPuertoVenta_AL5  =="ESZJY" -> es un error. Parece que hay dos mareas en una
    # CodigoPuertoVenta_AL5  =="ESMAN" -> es un error. Parece que hay dos mareas en una
    # CodigoPuertoVenta_AL5  =="FRMAC" -> es un error. Cambio a "ESBRM" (bermeo)
    
    Dori$CodigoPuertoDesembarque_AL5[which(Dori$CodigoPuertoDesembarque_AL5=="FRMAC")] <- "ESBRM"
    InfoVentas$IdDiario[which(InfoVentas$IdDiario %in% checkFin$IdDiario & InfoVentas$CodigoPuertoVenta_AL5 %in% c("ESZJY","ESMAN","ESAGA"))] <- 0
    InfoVentas$CodigoPuertoVenta_AL5[which(InfoVentas$CodigoPuertoVenta_AL5 %in% c("FRMAC"))]  <- "ESBRM"
    

################################################### #   
#    NUEVAS VARIABLES                            ####
################################################### #  

#    .Identificador de linea     ####
################################### #
Dori$IdDori <- 1:dim(Dori)[1]

#    .Formato fechas y horas  ####
################################# #

Dori <- Dori %>% mutate_at(.vars = vars(names(Dori)[grepl("Fc", names(Dori))]), ~ dmy_hms(.))
Dori <- Dori %>% mutate_at(.vars = vars(names(Dori)[grepl("Hr", names(Dori))]), ~ hms(.))

head(Dori)
  

#    .Variables Cabecera     ####
############################## #
  # requieren completar NAs (no puede haber registros vacíos en la fecha de desembarque, puerto de desembarque...)
  # Identificamos las variables de cabecera con una "C_" al comienzo del nombre


# ..NAs en FechaRegreso/FechaDesembarco  ####
##### #
  # Priorizamos la fecha de regreso para la cabecera porque hay menos NAs. Ademas, la fecha de desembarque no tiene informacion datetime 
  # !> no se si la informacion de la hora es fiable.. pero en teoria nos puede servir para identificar varias mareas en el mismo día
  # hay un amarea con NA en FcRegreso. Tampoco tiene informacionde FcDesembarque. Utilizamos la ultima fecha de captura

sapply(Dori[grepl("Fc", names(Dori))], function(x) sum(is.na(x)))

  
  #fecha de regreso > fecha deembarque
  Dori %>% group_by(IdDiario,FcRegreso,FcDesembarque) %>% summarize (dif_t = (unique(FcRegreso) - unique(FcDesembarque))) %>%
  filter(dif_t>0)


Dori$C_FcRegreso <- Dori$FcRegreso  
# FcMaxCap <- Dori %>% group_by(IdDiario) %>% summarise(C_Maxcap= max(FcCaptura, rm.na=T))
# Dori$C_FcRegreso[is.na(Dori$C_FcRegreso)] <- FcMaxCap$C_Maxcap[match(Dori$IdDiario[is.na(Dori$C_FcRegreso)], FcMaxCap$IdDiario)]

head(Dori[grepl("Fc", names(Dori))])


# ..Crear IdMarea ####
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

#Dori$C_FcRegresoFloor <- floor_date(ymd_hms(Dori$C_FcRegreso, tz = "Europe/Madrid"), "day")
Dori$Buque_FcRegresoFloor <- paste(Dori$IdBuque, Dori$C_FcRegreso, sep="_")

# Identifico Mareas desembarcadas el mismo dia
Dori <- Dori %>% group_by(C_FcRegreso, IdBuque) %>% mutate(IdMarea = paste(unique(CodigoMarea), collapse = "//")) %>% data.frame()

#si para el mismo barco*dia, alguna linea tiene CodigMarea y otras no, le aplico a todas el mismo
Dori$IdMarea <- gsub("NA//", "" ,Dori$IdMarea)
Dori$IdMarea <- gsub("//NA", "" ,Dori$IdMarea)

#si para el mismo barco*dia,hay varios CodigMarea,dejo el original de cada linea
Dori$IdMarea[grepl("//", Dori$IdMarea)] <- Dori$CodigoMarea[grepl("//", Dori$IdMarea)] 
Dori$IdMarea[is.na(Dori$IdMarea)]   <- Dori$Buque_FcRegresoFloor[is.na(Dori$IdMarea)] 
Dori$IdMarea[Dori$IdMarea =="NA"]   <- Dori$Buque_FcRegresoFloor[Dori$IdMarea =="NA"] 

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
  
  # Dori[Dori$IdMarea=="ESP-13703399//NA",]
  # Dori[Dori$IdMarea=="ESP-TRP-00594820191219012808//ESP-TRP-00594820191220043440",]
  # Dori[Dori$IdMarea=="NA",]
  
  length(unique(Dori$IdMarea))
  length(unique(Dori$IdMarea[Dori$PesoConsumo!=0]))
##    hay que comprobar si estas mareas que hemos creado, son reales o no  

# some checkings
  Dori %>%  group_by(C_FcRegreso, IdBuque) %>% summarize (n = Fun_unique(IdMarea)) %>% filter(n > 1) 
  a<- subset(Dori, C_FcRegreso==ymd_hms("2020-01-02 00:00:00") & IdBuque==233133); a
  a<- subset(Dori, C_FcRegreso==ymd_hms("2020-01-07 00:00:00") & IdBuque==233133); a
  a<- subset(Dori, C_FcRegreso==ymd_hms("2020-01-07 00:00:00") & IdBuque==9966950); a
  a<- subset(Dori, C_FcRegreso==ymd_hms("2020-01-16 00:00:00") & IdBuque==28024005); a
  
  
  select(a, c(Nombre, CensoPorModalidad, C_FcRegreso, CodigoMarea, IdMarea, C_FcRegreso, OrigenIdentificador, CodigoArte_FaoAL3, CatchCategory, PesoConsumo)) %>% data.frame
  
  aa <- Dori %>%  group_by(C_FcRegreso, IdBuque, CodigoArte_FaoAL3) %>% summarize (n = Fun_unique(IdMarea)) %>% filter(n > 1) 
  table(aa$CodigoArte_FaoAL3)
  table(month(aa$C_FcRegreso[aa$CodigoArte_FaoAL3=="PS"]))
  
  
  # en la msima marea, mas de una fecha o mas de un buque?
  Dori  %>% group_by(IdMarea) %>% summarize (n = Fun_unique(C_FcRegreso)) %>% filter(n > 1)  # 
  Dori  %>% group_by(IdMarea) %>% summarize (n = Fun_unique(IdBuque)) %>% filter(n > 1)  # 

  
# ..Crear IdMarea - codigo IEO ####
##### #
CodigoMareaAZTI <- data.frame(IdMarea = sort(unique(Dori$IdMarea)),
                              IdMareaIEO = 20900001: (20900000+length(unique(Dori$IdMarea))))
Dori$IdMareaIEO <- CodigoMareaAZTI$IdMareaIEO[match(Dori$IdMarea, CodigoMareaAZTI$IdMarea)]
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(IdMareaIEO )) %>% filter(n > 1)


# ..Puerto Regreso/ PuertoDesembarco ####
##### #
sapply(Dori[grepl("Puerto", names(Dori))], function(x) sum(is.na(x))) # Priorizamos el Puerto de Desembarque porque hay menos NAs


  # relleno NAs con info de la marea
  #hay areas que tienen lineas que vienen de NV y de CA. Solo las de CA tienen info sobre la marea. Hay que rellenar todas las lineas con esa infromación
Dori$CodigoPuertoDesembarque_AL5temp <- Dori$CodigoPuertoRegreso_AL5
Dori$CodigoPuertoDesembarque_AL5temp[is.na(Dori$CodigoPuertoDesembarque_AL5temp)] <- Dori$CodigoPuertoDesembarque_AL5[is.na(Dori$CodigoPuertoDesembarque_AL5temp)]

Dori <- Dori %>% group_by(IdMarea) %>% mutate(C_CodigoPuertoDesembarque_AL5 = paste(unique(CodigoPuertoDesembarque_AL5temp), collapse = "//")) %>% data.frame()

length(unique(Dori$IdMarea[grepl("//NA", Dori$C_CodigoPuertoDesembarque_AL5)]))
length(unique(Dori$IdMarea[grepl("NA//", Dori$C_CodigoPuertoDesembarque_AL5)]))


Dori$C_CodigoPuertoDesembarque_AL5 <- gsub("NA//", "" ,Dori$C_CodigoPuertoDesembarque_AL5)
Dori$C_CodigoPuertoDesembarque_AL5 <- gsub("//NA", "" ,Dori$C_CodigoPuertoDesembarque_AL5)
Dori$C_CodigoPuertoDesembarque_AL5[grepl("//", Dori$C_CodigoPuertoDesembarque_AL5)] <- Dori$CodigoPuertoDesembarque_AL5temp[grepl("//", Dori$C_CodigoPuertoDesembarque_AL5)]
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

sort(unique(Dori$C_CodigoPuertoDesembarque_AL5))
length(unique(Dori$IdMarea[is.na(Dori$C_CodigoPuertoDesembarque_AL5)]))
# a<- subset(Dori, IdMarea=="ESP-13552398"); a
# subset(PuertoDesembarqueSelect, IdMarea=="ESP-13552398");

Dori  %>% group_by(IdMarea) %>% summarize (n = Fun_unique(C_CodigoPuertoDesembarque_AL5)) %>% filter(n > 1)  # 

  # Nombre puerto
#Dori$C_PuertoDesembarque <- UNLOCODE$locName[match(Dori$C_CodigoPuertoDesembarque_AL5,UNLOCODE$loCode )]
Dori$C_PuertoDesembarque <- MaestroPuertos$Nombre.Puerto[match(Dori$C_CodigoPuertoDesembarque_AL5,MaestroPuertos$Codigo.RDB )]
unique(Dori$C_CodigoPuertoDesembarque_AL5[is.na(Dori$C_PuertoDesembarque)])

sort(unique(Dori$C_PuertoDesembarque ))



# ..Puerto de Venta y Fecha de Venta  #### 
#################################################### #
# para la cabecera necesitamos un único Puerto venta y Fecha Venta, 
# pero en algunas mareas hay varios puertos/fechas de venta
# !> esto es algo que habría que simplificar en la BD: quitar la fecha/puerto venta de la cabecera y pasarlo al detalle
################################################################ #

# Fecha venta
###
InfoVentas$IdMarea <- Dori$IdMarea[match(InfoVentas$IdDiario, Dori$IdDiario)]
InfoVentas$FcVentaFloor <- dmy_hms(InfoVentas$FcVenta)
InfoVentas <- InfoVentas %>% group_by(IdMarea) %>% mutate(C_FcVentaMax= max(FcVentaFloor, rm.na=T)) %>% data.frame

Dori$C_FcVentaMax <- InfoVentas$C_FcVentaMax[match(Dori$IdMarea, InfoVentas$IdMarea)]
length(unique(Dori$IdMarea[is.na(Dori$C_FcVentaMax)]))
Dori$C_FcVentaMax [is.na(Dori$C_FcVentaMax)] <- Dori$C_FcRegreso [is.na(Dori$C_FcVentaMax)]

  InfoVentas %>% group_by(IdMarea) %>% summarize(n=Fun_unique(CodigoPuertoVenta_AL5 )) %>% filter(n > 1)
  InfoVentas %>% group_by(IdMarea) %>% summarize(n=Fun_unique(FcVenta )) %>% filter(n > 1)
  InfoVentas %>% group_by(IdMarea) %>% summarize(n=Fun_unique(FcVentaFloor )) %>% filter(n > 1)
  
  subset(InfoVentas, IdMarea=="ESP-11954982")
  subset(InfoVentas, IdMarea=="119389129_2019-04-01" )
  subset(InfoVentas, IdMarea=="126108924_2019-06-17" )
  
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
length(unique(Dori$IdMarea[is.na(Dori$C_CodigoPuertoVenta_AL5)]))

Dori$C_CodigoPuertoVenta_AL5[is.na(Dori$C_CodigoPuertoVenta_AL5)] <- Dori$C_CodigoPuertoDesembarque_AL5[is.na(Dori$C_CodigoPuertoVenta_AL5)] 


# ..Division - area ####
#################################### #
  # asignamos el area de mayores capturas. 
  # en caso de NA, asignamos por puerto censo y puerto de desembarco
  # se podria sacar la zona a partir del rectangulo estadistico . 

sapply(Dori[grepl("Division|Rect", names(Dori))], function(x) sum(is.na(x))) # 

length(unique(Dori$CodigoDivision[is.na(Dori$CodigoDivision) & !is.na(Dori$RectanguloEstadistico)]))  
length(unique(Dori$CodigoDivision[is.na(Dori$RectanguloEstadistico) & !is.na(Dori$CodigoDivision)]))  

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
  # les asignamos un area segun censo y puerto de desembarque
z <- AreaSelect %>% filter( NArea==0) ## Todos los NA corresponden a CapturasLance0
table(Dori$CatchCategory[Dori$IdMarea %in% z$IdMarea])
table(Dori$CensoPorModalidad[Dori$IdMarea %in% z$IdMarea])
table(Dori$IdMarea[Dori$IdMarea %in% z$IdMarea])

  # Rellenar NA/27.8/27.6/27.9 segun puerto desembarque. Por censo
###
  
    # arrastre cantabrico nw
  table(AreaSelect$C_PuertoDesembarque[(is.na(AreaSelect$C_Area) | AreaSelect$C_Area=="27.8") & 
                                             AreaSelect$CensoPorModalidad=="ARRASTRE DE FONDO EN CANTABRICO NW"] )
  table(AreaSelect$C_Area[ AreaSelect$C_Area=="27.8" & AreaSelect$CensoPorModalidad=="ARRASTRE DE FONDO EN CANTABRICO NW"] )
  
  AreaSelect$C_Area[(is.na(AreaSelect$C_Area) | AreaSelect$C_Area=="27.8") & 
                      AreaSelect$CensoPorModalidad=="ARRASTRE DE FONDO EN CANTABRICO NW"] <- "27.8.c"
  
    # arrastre VIII y VI
  table(AreaSelect$C_PuertoDesembarque[(is.na(AreaSelect$C_Area) | AreaSelect$C_Area %in% c("27.6","27.8")) & 
                                             AreaSelect$CensoPorModalidad=="ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII y VIIIabde."] )
  table(AreaSelect$C_Area[AreaSelect$C_Area %in% c("27.6","27.8") & AreaSelect$CensoPorModalidad=="ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII y VIIIabde."] )
  
  AreaSelect$C_Area[  (is.na(AreaSelect$C_Area) | AreaSelect$C_Area %in% c("27.6"))  & 
                       AreaSelect$CensoPorModalidad=="ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII y VIIIabde." & 
                       AreaSelect$C_PuertoDesembarque==c("BELFAST")] <- "27.6.a"
  AreaSelect$C_Area[ (is.na(AreaSelect$C_Area) | AreaSelect$C_Area %in% c("27.8"))  & 
                       AreaSelect$CensoPorModalidad=="ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII y VIIIabde." & 
                       AreaSelect$C_PuertoDesembarque==c("ONDARROA")] <- "27.8.b"
  
    # bacalao
  table(AreaSelect$C_PuertoDesembarque[is.na(AreaSelect$C_Area) & AreaSelect$CensoPorModalidad=="BACALADEROS"] )
  table(AreaSelect$C_Area[AreaSelect$CensoPorModalidad=="BACALADEROS"] )
  
  AreaSelect$C_Area[is.na(AreaSelect$C_Area) & AreaSelect$CensoPorModalidad=="BACALADEROS"] <- "27.2.b.2"
  
    # artes fijas
  table(AreaSelect$C_PuertoDesembarque[ (is.na(AreaSelect$C_Area) | AreaSelect$C_Area %in% c("27.8", "27.7", "27.6")) & 
                                         AreaSelect$CensoPorModalidad=="ARTES FIJAS EN ZONAS CIEM VB, VI,VII y VIIIabde."] )
  table(AreaSelect$C_Area[ AreaSelect$C_Area %in% c("27.8", "27.7", "27.6") & AreaSelect$CensoPorModalidad=="ARTES FIJAS EN ZONAS CIEM VB, VI,VII y VIIIabde."] )
  
  AreaSelect$C_Area[(is.na(AreaSelect$C_Area) | AreaSelect$C_Area %in% c("27.8", "27.7", "27.6"))  & 
                      AreaSelect$CensoPorModalidad=="ARTES FIJAS EN ZONAS CIEM VB, VI,VII y VIIIabde." &
                      AreaSelect$C_PuertoDesembarque %in% c("ONDARROA", "PASAJES", "SANTANDER")] <- "27.8.b" 
  AreaSelect$C_Area[(is.na(AreaSelect$C_Area) | AreaSelect$C_Area %in% c("27.8", "27.7", "27.6"))  & 
                      AreaSelect$CensoPorModalidad=="ARTES FIJAS EN ZONAS CIEM VB, VI,VII y VIIIabde." &
                      AreaSelect$C_PuertoDesembarque %in% c("CASTLETOWN BEARHAVEN")] <- "27.6.a" 
  
  subset(AreaSelect, Nombre=="O XAN", select=c("IdMarea", "Nombre", "CensoPorModalidad", "C_FcRegreso", "C_PuertoDesembarque", "C_Area"))
  subset(AreaSelect, Nombre=="BRIEIRO", select=c("IdMarea", "Nombre", "CensoPorModalidad", "C_FcRegreso", "C_PuertoDesembarque", "C_Area"))
  subset(AreaSelect, Nombre=="AKILLAMENDI BERRIA", select=c("IdMarea", "Nombre", "CensoPorModalidad", "C_FcRegreso", "C_PuertoDesembarque", "C_Area"))
  
  AreaSelect$C_Area[  AreaSelect$IdMarea == "ESP-TRP-01555720200228061547" ] <- "27.8.a"
  AreaSelect$C_Area[  AreaSelect$IdMarea == "ESP-TRP-02538420201021115348" ] <- "27.7.k.2"
  AreaSelect$C_Area[  AreaSelect$IdMarea == "ESP-TRP-02694820200705220928" ] <- "27.8.a"
  
    # resto: artes menores, cerco, rasco, volanta, palangre
  resto <- c("ARTES MENORES EN CANTABRICO NW", "CERCO EN CANTABRICO NW", "PALANGRE DE FONDO EN CANTABRICO NW",
             "PALANGRE DE SUPERFICIE CALADERO NACIONAL", "RASCO EN CANTABRICO NW", "VOLANTA EN CANTABRICO NW")
  
  table(AreaSelect$C_PuertoDesembarque[(is.na(AreaSelect$C_Area) | AreaSelect$C_Area=="27.8") & 
                                             AreaSelect$CensoPorModalidad %in% resto])
  table(AreaSelect$C_Area [AreaSelect$C_Area=="27.8" &  AreaSelect$CensoPorModalidad %in% resto])
  
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
  # AreaSelect$C_Area[ (AreaSelect$C_Area=="34.1.2" | AreaSelect$C_Area=="34.2") &
  #                      AreaSelect$C_PuertoDesembarque %in% c("ELANCHOVE")] <- "27.8.c"
  # AreaSelect$C_Area[ (AreaSelect$C_Area=="34.1.2" | AreaSelect$C_Area=="34.2") &
  #                      AreaSelect$C_PuertoDesembarque %in% c("LA CORUÑA")] <- "27.8.c"
  
  table(AreaSelect$C_Area [ substr(AreaSelect$C_Area, 1,2)!="27"] )
  
    # checkings
  AreaSelect %>% filter( is.na(C_Area))
  table(AreaSelect$C_Area)
  
  # Checkigs: Areas por censo Check
  for (i in sort(unique(AreaSelect$CensoPorModalidad))){
    temp <- subset(AreaSelect, CensoPorModalidad == i)
    print(tapply(temp$IdMarea, list(temp$C_Area,temp$CensoPorModalidad), Fun_unique))
    }
  
  # Corregir
  AreaSelect[AreaSelect$CensoPorModalidad=="ARRASTRE DE FONDO EN CANTABRICO NW" & AreaSelect$C_Area=="27.8.b",]
  AreaSelect$C_Area[AreaSelect$CensoPorModalidad=="ARRASTRE DE FONDO EN CANTABRICO NW" & AreaSelect$C_Area=="27.8.b"] <- "27.8.c"
  
  AreaSelect[AreaSelect$CensoPorModalidad=="ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII y VIIIabde." & AreaSelect$C_Area=="27.8.c",]
  AreaSelect$C_Area[AreaSelect$CensoPorModalidad=="ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII y VIIIabde." & AreaSelect$C_Area=="27.8.c"] <- "27.8.b"
 
  AreaSelect[AreaSelect$CensoPorModalidad=="ARTES FIJAS EN ZONAS CIEM VB, VI,VII y VIIIabde." & AreaSelect$C_Area=="27.8.c",]
  AreaSelect$C_Area[AreaSelect$CensoPorModalidad=="ARTES FIJAS EN ZONAS CIEM VB, VI,VII y VIIIabde." & AreaSelect$C_Area=="27.8.c"] <- "27.8.b"

  AreaSelect[AreaSelect$CensoPorModalidad=="ARTES MENORES EN CANTABRICO NW" & AreaSelect$C_Area=="27.7.j.2",]
  Dori[which(Dori$CensoPorModalidad=="ARTES MENORES EN CANTABRICO NW" & Dori$CodigoDivision=="27.7.j.2"),]

    
 
  # Asignar área a Dori
##
Dori$C_Area <- AreaSelect$C_Area[ match(Dori$IdMarea , AreaSelect$IdMarea)]             

Dori  %>% group_by(IdMarea) %>% summarize (n = Fun_unique(C_Area)) %>% filter(n > 1)  # 

unique(Dori$IdMarea [is.na(Dori$C_Area )])


   
    ## Corregir


# .Nombre Puerto ####
#################################### #

# Dori$C_PuertoVenta <- UNLOCODE$locName[match(Dori$C_CodigoPuertoVenta_AL5,UNLOCODE$loCode )]
# Dori$PuertoSalida <- UNLOCODE$locName[match(Dori$CodigoPuertoSalida_AL5,UNLOCODE$loCode )]
# Dori$PuertoDesembarque <- UNLOCODE$locName[match(Dori$CodigoPuertoDesembarque_AL5,UNLOCODE$loCode )]
# Dori$PuertoRegreso <- UNLOCODE$locName[match(Dori$CodigoPuertoRegreso_AL5,UNLOCODE$loCode )]

Dori$C_PuertoVenta     <- MaestroPuertos$Nombre.Puerto[match(Dori$C_CodigoPuertoVenta_AL5,MaestroPuertos$Codigo.RDB )]
Dori$PuertoSalida      <- MaestroPuertos$Nombre.Puerto[match(Dori$CodigoPuertoSalida_AL5,MaestroPuertos$Codigo.RDB )]
Dori$PuertoDesembarque <- MaestroPuertos$Nombre.Puerto[match(Dori$CodigoPuertoDesembarque_AL5,MaestroPuertos$Codigo.RDB )]
Dori$PuertoRegreso     <- MaestroPuertos$Nombre.Puerto[match(Dori$CodigoPuertoRegreso_AL5,MaestroPuertos$Codigo.RDB )]

Dori$C_CodigoPuertoVenta_AL5 [is.na(Dori$C_PuertoVenta) & !is.na(Dori$C_CodigoPuertoVenta_AL5)] 
Dori$CodigoPuertoSalida_AL5 [is.na(Dori$PuertoSalida) & !is.na(Dori$CodigoPuertoSalida_AL5)] 
Dori$CodigoPuertoDesembarque_AL5 [is.na(Dori$PuertoDesembarque) & !is.na(Dori$CodigoPuertoDesembarque_AL5)] 
Dori$CodigoPuertoRegreso_AL5 [is.na(Dori$PuertoRegreso) & !is.na(Dori$CodigoPuertoRegreso_AL5)] 


Dori$PuertoBase <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), Dori$PuertoBase)
# Dori$PuertoBase <- toupper(Dori$PuertoBase)


# Dori$C_PuertoDesembarque <- toupper(Dori$C_PuertoDesembarque)
# Dori$C_PuertoVenta <- toupper(Dori$C_PuertoVenta)



# .Especie ####
#################################### #
# NAs en especie #
##
sapply(Dori[grepl("Esp", names(Dori))], function(x) sum(is.na(x))) # 
table(Dori$CatchCategory[is.na(Dori$Especie_AL3)])
  # son todas de captura lance cero

#Especie cientifico #
##
head(MaestroEspecies)

Dori$Especie_Oficial <- MaestroEspecies$Nombre.Oficial[match(Dori$Especie_AL3, MaestroEspecies$Cod..ALFA.3)]

Dori$Especie_Sci <- MaestroEspecies$Nombre.Cientifico[match(Dori$Especie_AL3, MaestroEspecies$Cod..ALFA.3)]
Dori$Especie_Sci[is.na(Dori$Especie_Sci)] <- ASFIS$Scientific_name[match(Dori$Especie_AL3[is.na(Dori$Especie_Sci)], ASFIS$X3A_CODE)]
Dori$Especie_Sci[is.na(Dori$Especie_AL3)] <- NA

unique(Dori$Especie_AL3[is.na(Dori$Especie_Sci) & !is.na(Dori$Especie_AL3)])


        #@@ hay esecies que no están en la BD. cómo lo hacemos?
        # # Cruce especies (revisión Iñaki) ####
        
        check_especies <- Dori %>% select(c(Nombre, CensoPorModalidad, CodigoMarea, C_FcRegreso, C_PuertoDesembarque, C_Area, CodigoArte_FaoAL3, CatchCategory, 
                                            Especie_AL3, Especie_Sci, PesoConsumo, PesoCapturadoBajoTalla, PesoCapturado, PesoDesembarcado, PesoNotaVenta ))
        
        check_especies$EspecieBD <- MaestroEspecies$Nombre.Oficial[match(check_especies$Especie_AL3,MaestroEspecies$Cod..ALFA.3 )]
        check_especies$EspecieSciBD <- MaestroEspecies$Nombre.Cientifico[match(check_especies$Especie_AL3,MaestroEspecies$Cod..ALFA.3 )]
        unique(check_especies$Especie_AL3[is.na(check_especies$EspecieBD)]  )
        
        check_especies$EspecieASFIS <- ASFIS$Spanish_name[match(check_especies$Especie_AL3,ASFIS$X3A_CODE )]
        check_especies$EspecieSciASFIS <- ASFIS$Scientific_name [match(check_especies$Especie_AL3,ASFIS$X3A_CODE )]
        unique(check_especies$Especie_AL3[is.na(Dori$EspecieASFIS)]  )
        
        check_especies <- subset(check_especies, CatchCategory!="CapturasLance0" )
        check_especies <- subset(check_especies, !is.na(CodigoMarea) )
        
        head(check_especies)
        subset(check_especies,  is.na(EspecieBD))
        
        #guardo resultados
        write.table(check_especies, file="Importacion_InfoBase/CheckEspecies.csv", sep=";", dec=",", row.names = FALSE)
        

# .PesoConsumo/ PesoConsumoBajoTalla/ PesoDescarte ####
#################################### #
sapply(Dori[grepl("Peso", names(Dori))], function(x) sum(is.na(x)))


# .Posicion ####
#################################### #
# @@ funcion para hacer esto? se podría mover al codigo de depuracion para que las X e Y coincidieran con las de Capturas

# geom <- sub(Dori$Posicion, pattern = "POINT", replacement = "")
# geom <- sub(geom, pattern = "[( ]", replacement = "")
# geom <- sub(geom, pattern = "[(]", replacement = "")
# geom <- sub(geom, pattern = "[)]", replacement = "")
# geom[is.na(geom)] <- "0 0"
# geom[geom==""] <- "0 0"
# latlon <- as.data.frame(matrix(as.numeric(unlist(strsplit(geom, split = " "))),ncol=2,byrow=TRUE), stringsAsFactors = FALSE)
# names(latlon) <- c("Lat", "Lon")
# latlon[latlon$Lat==0 & latlon$Lon==0,] <- NA
# Dori$Lat <- latlon$Lat
# Dori$Lon <- latlon$Lon
# 
# Dori$Lat[is.na(Dori$Lat)] <- Dori$X[is.na(Dori$Lat)]
# Dori$Lon[is.na(Dori$Lon)] <- Dori$Y[is.na(Dori$Lon)]

subset(Dori, is.na(Posicion) & !is.na(X))
subset(Dori, !is.na(Posicion) & !is.na(X))
subset(Dori, is.na(Posicion) & !is.na(Y))
subset(Dori, !is.na(Posicion) & is.na(Y))
subset(Dori, is.na(Posicion) & !is.na(RectanguloEstadistico))
subset(Dori, !is.na(Posicion) & is.na(RectanguloEstadistico))

Dori$Lat <- Dori$Y
Dori$Lon <- Dori$X

range(Dori$Lat, na.rm=TRUE)
range(Dori$Lon, na.rm=TRUE)



# .Crear CodigoOrigen y NCodigoOrigen ####
#################################### #
# Hay NAs- Los NAs tienen PesoConsumo = 0
# sustituimos NAs por cero
unique(substr(Dori$OrigenIdentificador,1,2))
table(Dori$CatchCategory[is.na(Dori$OrigenIdentificador)])
head(Dori[is.na(Dori$OrigenIdentificador) & Dori$PesoConsumo>0,])

Dori$CodigoOrigen <- substr(Dori$OrigenIdentificador,1,2)
Dori$CodigoOrigen[Dori$CatchCategory=="Descartes"] <- "Descartes"
Dori$CodigoOrigen[Dori$CatchCategory=="CapturasLance0"] <- "CapturasLance0"

Dori$CodigoOrigen[is.na(Dori$CodigoOrigen)] <- 0


Dori<- Dori %>% group_by(IdMarea) %>%
  mutate(NCodigoOrigen=length(unique(CodigoOrigen[CatchCategory=="CapturasCalculadas"]))) %>%
  ungroup() %>% as.data.frame()



# .Laboratorio ####
#################################### #

PuertoVasco <- c("ALGORTA","ARMINTZA", "BILBAO", "BERMEO", "CIERVANA","ELANCHOVE",
                 "FUENTERRABIA","GUETARIA", "GETARIA", "LEQUEITIO","MOTRICO",
                 "ONDARROA", "ORIO","PASAJES","PLENCIA","SAN SEBASTIAN",
                 "SANTURCE","ZUMAYA")

PuertoVasco <- c("Armintza", "Bermeo","Bilbao", "Ciervana", "Donostia", "Elantxobe", "Fuenterrabia", "Getaria","Guetaria", "Hondarribia",
                 "Lekeitio","Lequeitio", "Motrico", "Mutriku", "Ondarroa", "Orio","Pasaia","Pasajes","Plencia",
                 "Plentzia", "San Sebastian", "Santurce", "Santurtzi","Zierbena")


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

sort(unique(Dori$C_PuertoDesembarque[Dori$LAB_PuertoDesembarco =="EUS"]))
sort(unique(Dori$C_PuertoDesembarque[Dori$LAB_PuertoDesembarco =="ESP"]))
sort(unique(Dori$PuertoBase[Dori$LAB_PuertoBase =="EUS"]))
sort(unique(Dori$PuertoBase[Dori$LAB_PuertoBase =="ESP"]))


Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(LABORATORIO )) %>% filter(n > 1)
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(LABORATORIO )) %>% filter(n > 1)

tapply(Dori$PesoConsumo, list(Dori$LAB_PuertoBase, Dori$LAB_PuertoDesembarco,Dori$LABORATORIO), sum)







################################################### #   
#    CHECK                                       ####
################################################### #
# Check: Que cada Idmarea tenga un solo IdDiario, IdBuque, Fcregreso..

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
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(C_FcRegreso )) %>% filter(n > 1)
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



# 
############################################### #
# Crear tabla completa de ventas             ####
############################################### #


# Cabecera <- unique(Dori[,c("IdDiario", "Nombre", "PuertoBase", "CensoPorModalidad", "CodigoCFR", "CodigoMarea", 
#                            "FcRegreso", "HrRegreso", "CodigoPuertoRegreso_AL5")])

Cabecera <- Dori %>% group_by (IdDiario, Nombre, PuertoBase, CensoPorModalidad, CodigoCFR, CodigoMarea, 
                                C_FcRegreso, C_PuertoDesembarque)  %>%
                    summarize (Peso = sum(PesoConsumo)) %>% select(-Peso)
    dim(Cabecera)
    length(unique(Cabecera$IdDiario))
InfoVentas_All <- InfoVentas %>%  left_join(Cabecera, by= c("IdDiario" = "IdDiario"))

# limpiar variables
names(InfoVentas_All)[grepl(".x",   names(InfoVentas_All))]

dim(subset(InfoVentas_All, is.na(IdMarea)))

#guardar
write.table(InfoVentas_All, file="Importacion_InfoBase/InfoVentas_All.csv", sep=";", dec=",", row.names = FALSE)



################################################### #   
#    GRABAR FICHERO                              ####
################################################### #


save(Dori, InfoVentas_all, file="0_Datos/Infobase/2020/Dori2020_v2.RData")


