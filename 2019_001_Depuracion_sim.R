# ----------------------------------------------------------------- #
# Readme:                                                           #
# ----------------------------------------------------------------- #
# Aquí se carga "DatosOficiales2019_Originales_ultimaversion.RData" 
# y las tablas auxiliares de la carpeta "Auxiliares/". Se chequeo de 
# ID único de tablas.Se renombran de forma uniforme las cabeceras de 
# las tablas. 
# 
# INDICE:
#
# Librerias.
# Funciones.
# Cargar datos: tablas auxiliares, tablas infobase y tablas Notasventa.
# Overview tables.
# Comprobar primary keys de tablas.
# Revisar consistencia en los nombres entre las tablas.
# Save data.
#
# ----------------------------------------------------------------- #
#
# ----------------------------------------------------------------- #
# R version 3.6.0 (2019-04-26) -- "Planting of a Tree"
# rStudio 1.1 
# ----------------------------------------------------------------- #
#
# # ################## #
# # Librerias          #
# # ################## #

rm(list=(ls()))

library(fishPiCodes) # Creada en el proyecto FishPi, contiene codigos de puertos, especies, etc.
data("UNLOCODE")     # Puertos LOCODE
library (stringr)
library(doBy)
library(dplyr)
library(lubridate)
library(data.table)
library(reshape2)

# # #################### #
# # Funciones            #
# # #################### #
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

# # #################### #
# # Cargar datos         #
# # #################### #

# # ------------------ #
# # Tablas auxiliares  #
# # ------------------ #

  # Censo Flota Operativa: censo flota española
  # MaestrosBuquesAZTI: tabla maestra de buques en BD AZTI
  # MaestrosEspeciesAZTI: tabla maestra de especies en BD AZTI

MaestroBuques <- read.csv("Auxtables/MaestroBuquesAZTI_2019.txt", sep="\t", dec=",", stringsAsFactors = FALSE)
MaestroBuques <- MaestroBuques [, !names(MaestroBuques) %in% c("Observaciones","Tipo.pesca.principal", "Segmento", "Varios")]
names(MaestroBuques) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(MaestroBuques))
MaestroBuques$Caladero.principal <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), MaestroBuques$Caladero.principal)

# # ------------------ #
# # Infobase           #
# # ------------------ #

load(file = "Datos/DatosOficiales2019_Originales_ultimaversion.RData")

  # BarcosAzti:        Lista de barcos que han desembarcado al menos un día en el PV. Nos la da HAZI. 
  #                    Nosotros se la mandamos al IEO para que la utilice en el filtro.  
  # Infobase:          Metadata.
  # InfoBuques:        Censo oficial de barcos con registro de cambios durante 2019. 
  #                    El mismo barco puede tener varios registro si ha tendio algun cambio en nombre, 
  #                    puerto base, etc
  # InfoDiarios:       Info de Fecha y puerto de salida, regreso y desembarco de la marea.                 
  # InfoOrigenLineas:  Info sobre el area, RectEst y Arte de cada linea de Captura  
  # InfoCapturasCalculadas: Captura por día y especie. Incluye PesoConsumo y PesoConsumoBajoTalla.
  #                         El PESOCONSUMO es lo que consideramos DATOS OFICIALES de captura para responder 
  #                         al DATACALL se saca a apartir del ALGORITMO de consumo de SGP, que combina 
  #                         Diario de Pesca, Declaración de desembarque y Notas de Venta.  
  #                         Hay otros pesos que también salen del algoritmo (Captura, trasbordo, venta, etc.),
  #                         pero que no son Datos Oficiales.
  #                             - Para buques con logbook, PesoConsumo, viene de logbook.
  #                             - Para buques sin logbook, PesoConsumo, viene de notas de venta.
  # InfoCapturas:      Info sobre tiempo pesca número operaciones y posición para cada línea de captura.
  # InfoParametrosArteCapturas: Información sobre la profundidad, altura de malla, numero de anzuelos, etc 
  # InfoCapturaLance0: Info sobre lances con captura cero? @@necesitamos aclarar con SGP el significado 
  #                    de esta tabla. Se relaciona directemente con InfoDiarios.
  # InfoDescartes:     Info sobre los descartes; fecha, motivo descarte, peso, posición... 
  #                    Se relaciona directemente con InfoDiarios.
  # InfoVentas         Info sobre las ventas.
  #                    - Para los buques sin logbook utiliza el peso de venta y crea un idDiario negativo. 
  #                    Pero este idDiario no es una marea sino una venta (buque, fecha venta, especie, 
  #                    comprador).
  #                    @@ pedir a SGP que incluya la fecha de regreso



# # ------------------ #
# # NotasVentas        #
# # ------------------ #
# load(file = "NotasVenta2019_Originales.RData")


# # ###################### #
# # Overview of the tables # # Vista general de cada tabla.
# # ###################### #
  
head(BarcosAzti)
head(InfoBuques)

head(InfoBase)
head(InfoDiarios)
head(InfoOrigenLineas)
head(InfoCapturasCalculadas)
head(InfoCapturas)
head(InfoParametrosArteCapturas)

head(InfoCapturaLance0)
head(InfoVentas)
head(InfoDescartes)


# # ################################ #
# # Comprobar tablas - Primary Key   # # Revisar cada tabla y ver cual es su id unico.
# # ################################ #

# # ------------------------------- #
# # BarcosAZTI                      #
# # ------------------------------- #

  # Hay barcos que no tienen el IdBuque porque no aparecen en InfoBuques.
  # Son barcos que están en el censo español. Pero he comprobado que no tienen desembarcos en 2019. 
  # Algunos son buques auxiliares de atuneros congeladores.

head(BarcosAzti)
check <- subset(BarcosAzti, is.na(IdBuque))
check

subset(InfoBuques, CodigoCFR %in% c(check$CodigoCFR)) # no hay buques sin codigo en InfoBuques en 2019

# # ------------------------------- #
# # InfoBuques                      #
# # ------------------------------- #
  # id unico = IdBuque o CodigoCFR.

  # Hay barcos con varios registros por cambios datos tecnicos/motor/nombre etc.
  # NUEVA TABLA: InfoBuquesUnique con id unico: IdBuque o CodigoCFR.
  
  # Creamos nueva tabla con un registro para cada barco.
  # !! lo mas correcto sería cruzarlo con InfoDiarios en función de la fecha.
  #     hay barcos que cambian de puerto a mitad de año y que pueden pasar de laboratorio AZTI a IEO
  #     (laboratorio AZTI: desembarcos en Euskadi. laboratorio IEO: el resto )
  #     en este fichero podríamos identificar que codigos CFR tienen algun cambio en puerto base o nombre y en que fecha se da, para luego hacer el link

names(InfoBuques) <- iconv(names(InfoBuques),to="ASCII//TRANSLIT") # cambio encoding nombres
InfoBuques$fcCambio2 <- ymd(InfoBuques$fcCambio)  # cambio de formato de fecha

InfoBuques %>%  count(CodigoCFR) %>% filter(n > 2) 
subset(InfoBuques, CodigoCFR=="ESP000025406")

# Nos quedamos con los registros de la ultima fecha de cambio par acada IdBuque.

InfoBuquesUnique <- InfoBuques %>% group_by( IdBuque, CodigoCFR) %>%
summarise(fcCambio2=max(fcCambio2)) %>%
left_join(InfoBuques, by=c("IdBuque", "CodigoCFR", "fcCambio2")) %>%
ungroup()%>% as.data.frame() 

  # Chequeos::

dim(InfoBuques)
dim(InfoBuquesUnique)

InfoBuquesUnique %>%  count(CodigoCFR) %>% filter(n > 1)  # comprobamos que IdBuque es unico
subset(InfoBuquesUnique, is.na(CodigoCFR))                ## hay dos buques sin CodigoCFR
subset(InfoBuquesUnique, is.na(CodigoCFR), select = c("IdBuque", "CodigoCFR", "Nombre", "PuertoBase",
                                                      "CensoPorModalidad")) 
  # Quedan 2 barcos sin codigo CFR. Pero no tienen ningun registro en InfoDiarios. Se pueden eliminar.
subset(InfoDiarios, IdBuque %in% c(51346396, 79454692))
 InfoBuquesUnique <- subset(InfoBuquesUnique, !IdBuque %in% c(51346396, 79454692))

# @ ---------------------------- @ #
# Para poder actualizar nuestra BD #
# @ ---------------------------- @ #
 
  # Identificamos los barcos que han cambiado de Puertobase o caracteristicas tecnicas en 2019 
  # y estan en nuestra BD.

CheckId <- InfoBuques %>%  count(CodigoCFR) %>% filter(n > 1) 
CheckMaestroBuques <- InfoBuques [InfoBuques$CodigoCFR %in% CheckId$CodigoCFR & 
                                  InfoBuques$CodigoCFR %in% unique(MaestroBuques$Codigo.UE),]
CheckMaestroBuques <- CheckMaestroBuques %>% arrange(CodigoCFR)

write.table(CheckMaestroBuques, "CheckMaestroBuques.csv", sep=",", dec=".", row.names = FALSE)

  # Censo completo español.

write.table(InfoBuques, "MaestroBuques.csv", sep=",", dec=".", row.names = FALSE)

# # ------------------------------- #
# # InfoDiarios                     #
# # ------------------------------- #
  #  id unico = IdDiarios

head(InfoDiarios)
dim(InfoDiarios)
length(unique(InfoDiarios$IdDiario))

temp <- InfoDiarios %>%  count(IdDiario) %>% filter(n > 1)
temp

# # ------------------------------- #
# # InfoVentas                      #
# # ------------------------------- #
  #  id unico = IdVenta

head (InfoVentas)
dim (InfoVentas)
length(unique(InfoVentas$IdVenta))  
length(unique(InfoVentas$idDiario))

temp <- InfoVentas %>%  count(IdVenta) %>% filter(n > 1)
temp

# # ------------------------------- #
# # InfoOrigenLineas                #
# # ------------------------------- #
  #  id unico = IdInfoOrigenLineas 

dim (InfoOrigenLineas)
length(unique(InfoOrigenLineas$IdInfoOrigenLineas))
dim(subset(InfoOrigenLineas, is.na(IdInfoOrigenLineas)))

# # ------------------------------- #
# # Depurar InfoCapturas            #
# # ------------------------------- #
  #  id unico = IdCaptura 

head(InfoCapturas)
dim (InfoCapturas)
length(unique(InfoCapturas$IdCaptura))

# # ------------------------------- #
# # Depurar InfoCapturasCalculadas  #
# # ------------------------------- #
  # id unico: IdInfoOrigenLinea
  # creamos la variable IdCaptura

head(InfoCapturasCalculadas)
dim (InfoCapturasCalculadas)
length(unique(InfoCapturasCalculadas$IdInfoOrigenLinea ))

InfoOrigenLineas$IdCaptura <- InfoCapturasCalculadas$IdCaptura [match(InfoOrigenLineas$IdInfoOrigenLinea, InfoCapturasCalculadas$IdInfoOrigenLinea)]


# # ------------------------------- #
# # InfoCapturaLance0               #
# # ------------------------------- #
  # id unica: IdCaptura

head(InfoCapturaLance0)
dim (InfoCapturaLance0)
length(unique(InfoCapturaLance0$IdCaptura))

    
    # Hay registros en esta tabla con esfuerzo igual a cero -> @@ preguntar a SGP
    
    length(unique(InfoCapturaLance0$IdDiario))
    dim(InfoCapturaLance0[InfoCapturaLance0$NumOperaciones>0,])
    length(unique(InfoCapturaLance0$IdDiario[InfoCapturaLance0$NumOperaciones>0]))
    dim(InfoCapturaLance0[InfoCapturaLance0$TiempoPescaMin>0,])
    length(unique(InfoCapturaLance0$IdDiario[InfoCapturaLance0$TiempoPescaMin>0]))
    
    dim(InfoCapturaLance0[InfoCapturaLance0$NumOperaciones>0 | InfoCapturaLance0$TiempoPescaMin>0,])
    length(unique(InfoCapturaLance0$IdDiario[InfoCapturaLance0$NumOperaciones>0 | InfoCapturaLance0$TiempoPescaMin>0]))
    
    # Mirar que el IdCaptura en InfoCapturaLance0 están en InfoCapturasCalculadas.
    
    InfoCapturaLance0[InfoCapturaLance0$NumOperaciones>0 | InfoCapturaLance0$TiempoPescaMin>0,]
    
    # Hemos revisado que hay registros dentro de InfoCapturaLance0 que estan tambien dentro 
    # de InfoCapturasCalculadas.
    # Sospechamos que en infocapturaLancecero se incluyen todas aquellas idCaptura 
    # con al menos algun lance cero.
    # @@Pendiente de aclarar con SGP.
    
    temp<- InfoCapturaLance0 %>% inner_join(InfoCapturasCalculadas, by="IdCaptura")  ; dim(temp)
    unique(temp$IdCaptura)
    head(subset(temp, PesoConsumo==0))
    
# # ------------------------------- #    
# # InfoDescartes                   #
# # ------------------------------- #
  # Id unico = IdDescarte

head(InfoDescartes)
dim (InfoDescartes)
length(unique(InfoDescartes$IdDescarte))

# # ------------------------------- #
# # InfoParametrosArteCapturas      #
# # ------------------------------- #  
  # Id unico = IdCaptura
  # New table:  InfoParametrosArteCapturas_wide

head(InfoParametrosArteCapturas)

InfoParametrosArteCapturas %>% count( IdCaptura, CodigoParametroArte) %>% filter(n > 1)

InfoParametrosArteCapturas_wide <- dcast(InfoParametrosArteCapturas, IdCaptura + CodigoArte_FaoAL3 + Arte ~ CodigoParametroArte + ParametroArte, value.var= "ValorParametro", fun.aggregate = sum)
dim (InfoParametrosArteCapturas_wide)
length(unique(InfoParametrosArteCapturas_wide$IdCaptura))


# # ###################################################### #
# # Revisar consistencia en los nombres entre las tablas   #
# # ###################################################### #

# ------------------------------- #  
# InfoBuques 
# ------------------------------- #  

names(InfoBuquesUnique)  

# ------------------------------- #   
# InfoDiarios   
# ------------------------------- #  

names(InfoDiarios)

# ------------------------------- #    
# InfoVentas  
# ------------------------------- #  

head (InfoVentas)
names(InfoVentas)
names(InfoVentas)[names(InfoVentas)=="idDiario"] <- "IdDiario"

# ------------------------------- #  
# InfoOrigenLineas 
# ------------------------------- #  
head(InfoOrigenLineas)
names(InfoOrigenLineas)
names(InfoOrigenLineas)[names(InfoOrigenLineas)=="IdInfoOrigenLineas"] <- "IdInfoOrigenLinea"
names(InfoOrigenLineas)[names(InfoOrigenLineas)=="PuertoRegreso_AL5"] <- "CodigoPuertoDesembarque_AL5"
names(InfoOrigenLineas)[names(InfoOrigenLineas)=="idDivision"] <- "CodigoDivision"
names(InfoOrigenLineas)[names(InfoOrigenLineas)=="rectanguloEstadistico"] <- "RectanguloEstadistico"

# ------------------------------- #  
# Depurar InfoCapturas 
# ------------------------------- #  
head(InfoCapturas)
names(InfoCapturas)

# ------------------------------- #  
# Depurar InfoCapturasCalculadas
# ------------------------------- #  #
head(InfoCapturasCalculadas)
names(InfoCapturasCalculadas)

# ------------------------------- #  
# InfoCapturaLance0
# ------------------------------- #  
# Posibiliad de filtrar las mareas con Numero de Operaciones/Tiempo de Pesca > 0. 

head(InfoCapturaLance0)
dim(subset(InfoCapturaLance0, NumOperaciones>0))

names(InfoCapturaLance0)
names(InfoCapturaLance0)[names(InfoCapturaLance0)=="PuertoSalida_AL5"] <- "CodigoPuertoSalida_AL5"
names(InfoCapturaLance0)[names(InfoCapturaLance0)=="PuertoRegreso_AL5"] <- "CodigoPuertoRegreso_AL5"
names(InfoCapturaLance0)[names(InfoCapturaLance0)=="PuertoDesembarque_AL5"] <- "CodigoPuertoDesembarque_AL5"

# ------------------------------- #  
# InfoDescartes
# ------------------------------- #  

head(InfoDescartes)
names(InfoDescartes)
names(InfoDescartes)[names(InfoDescartes) == "Peso"] <- "PesoDescarte"
names(InfoDescartes)[names(InfoDescartes) == "NumPiezas"] <- "NumPiezasDescartadas"
names(InfoDescartes)[names(InfoDescartes) == "PesoBajoTalla"] <- "PesoDescarteBajoTalla"
names(InfoDescartes)[names(InfoDescartes) == "NumPiezasBajoTalla"] <- "NumPiezasDescartadasBajoTalla"
names(InfoDescartes)[names(InfoDescartes) == "CodigoRectanguloEstadistico"] <- "RectanguloEstadistico"
names(InfoDescartes)[names(InfoDescartes) == "Pais_AL3"] <- "PaisBandera_AL3"

# ------------------------------- #  
# InfoParametrosArteCapturas
# ------------------------------- #  

head(InfoParametrosArteCapturas_wide)
names(InfoParametrosArteCapturas_wide)

names(InfoParametrosArteCapturas_wide) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(InfoParametrosArteCapturas_wide))
names(InfoParametrosArteCapturas_wide) <- make.names(names(InfoParametrosArteCapturas_wide))
InfoParametrosArteCapturas_wide <- rename(InfoParametrosArteCapturas_wide, 
                                          Profundidad_m                  = FD_Profundidad..metros.,
                                          TamanoMedioAnzuelos_mm         = GC_Tamaño.medio.de.los.anzuelos..mm.,       
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
                                          LongitudTotalRedes             = TL_Longitud.total.de.las.redes.a.bordo..m.)


# # ####################### #
# # Save data               #
# # ####################### #

save(InfoBase, InfoBuquesUnique, InfoDiarios, InfoVentas, InfoOrigenLineas, 
     InfoCapturaLance0, InfoCapturas, InfoCapturasCalculadas, InfoDescartes, 
     InfoParametrosArteCapturas_wide,  file="Datos/Infobase2019_Unique_20200724.Rdata"  )

# ----------------------------------------------------------------- #
    