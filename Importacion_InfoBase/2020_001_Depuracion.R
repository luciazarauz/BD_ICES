# ----------------------------------------------------------------- #
# Readme:                                                           #
# ----------------------------------------------------------------- #
# Aquí se carga "DatosOficiales2020_Originales_ultimaversion.RData" 
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
library(tidyverse)

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

MaestroBuques <- read.csv("0_Maestros/Buques_2020.txt", sep="\t", dec=",", stringsAsFactors = FALSE)
MaestroBuques <- MaestroBuques [, !names(MaestroBuques) %in% c("Observaciones","Tipo.pesca.principal", "Segmento", "Varios")]
names(MaestroBuques) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(MaestroBuques))
MaestroBuques$CensoPorModalidad <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), MaestroBuques$CensoPorModalidad)

MaestroPuertos <- read.csv("0_Maestros/Puertos_2020.csv", sep=";", dec=",", stringsAsFactors = FALSE)
MaestroPuertos$Nombre.Puerto <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), MaestroPuertos$Nombre.Puerto )

MaestroEspecies <- read.csv("0_Maestros/Especies_2020.txt", sep="\t", dec=",", stringsAsFactors = FALSE)
names(MaestroEspecies) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(MaestroEspecies))
MaestroEspecies$Nombre.Oficial <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), MaestroEspecies$Nombre.Oficial )

# # ------------------ #
# # Infobase           #
# # ------------------ #

load(file = "0_Datos/Infobase/2020/InfoBase2020_Originales_20210309.RData")

  # BarcosAzti:        Lista de barcos que han desembarcado al menos un día en el PV. Nos la da HAZI. 
  #                    Nosotros se la mandamos al IEO para que la utilice en el filtro.  
  # Infobase:          Metadata.
  # InfoBuques:        Censo oficial de barcos con registro de cambios durante 2019. 
  #                    El mismo barco puede tener varios registro si ha tendio algun cambio en nombre, 
  #                    puerto base, etc
  # InfoBuques:        InfoBuques corregido con un solo dato por buque
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
load(file = "0_Datos/Infobase/2020/NotasVenta2020_Originales_20210224.RData")


# # ###################### #
# # Overview of the tables # # Vista general de cada tabla.
# # ###################### #
  
head(InfoBuques); dim(InfoBuques)
head(InfoBuquesOk); dim(InfoBuquesOk)

head(InfoBase)
head(InfoDiarios)
head(InfoOrigenLineas)
head(InfoCapturasCalculadas)
head(InfoParametrosArteCapturas)

head(InfoCaptura0); dim(InfoCaptura0)
head(InfoVentas)
head(InfoDescartes)


# # ################################ #
# # Comprobar tablas - Primary Key   # # Revisar cada tabla y ver cual es su id unico.
# # ################################ #


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
  # en 2020 el IEO nos esta table ya con valore súnicos por barco: InfoBuquesOk. Utilizamos esta

# names(InfoBuques) <- iconv(names(InfoBuques),to="ASCII//TRANSLIT") # cambio encoding nombres
# InfoBuques$fcCambio2 <- dmy_hms(InfoBuques$fcCambio)  # cambio de formato de fecha
# 
# InfoBuques %>%  count(CodigoCFR) %>% filter(n > 2) 
# subset(InfoBuques, CodigoCFR=="ESP000021731")
# 
# # Nos quedamos con los registros de la ultima fecha de cambio par acada IdBuque.
# 
# InfoBuquesUnique <- InfoBuques %>% group_by( IdBuque, CodigoCFR) %>%
# summarise(fcCambio2=max(fcCambio2)) %>%
# left_join(InfoBuques, by=c("IdBuque", "CodigoCFR", "fcCambio2")) %>%
# ungroup()%>% as.data.frame() 
# 
#   # Chequeos::
# 
# dim(InfoBuques)
# dim(InfoBuquesUnique)
# dim(InfoBuquesOk)

names(InfoBuquesOk) <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), names(InfoBuquesOk) )

InfoBuquesOk %>%  count(CodigoCFR) %>% filter(n > 1)  # comprobamos que IdBuque es unico
subset(InfoBuquesOk, is.na(CodigoCFR))                ## hay dos buques sin CodigoCFR
subset(InfoBuquesOk, is.na(CodigoCFR), select = c("IdBuque", "CodigoCFR", "Nombre", "PuertoBase",
                                                      "CensoPorModalidad")) 
InfoBuquesUnique <- InfoBuquesOk
names(InfoBuquesUnique)[names(InfoBuquesUnique)=="AL5_PuertoBase"] <- "CodigoPuertoBase_AL5"



# @ ---------------------------- @ #
# Para poder actualizar nuestra BD #
# @ ---------------------------- @ #
 
  # Identificamos los barcos que han cambiado de Puertobase o caracteristicas tecnicas en 2019 
  # y estan en nuestra BD.

CheckId <- InfoBuques %>%  count(CodigoCFR) %>% filter(n > 1) 
CheckMaestroBuques <- InfoBuques [InfoBuques$CodigoCFR %in% CheckId$CodigoCFR & 
                                  InfoBuques$CodigoCFR %in% unique(MaestroBuques$Codigo.UE),]
CheckMaestroBuques <- CheckMaestroBuques %>% arrange(CodigoCFR)
CheckMaestroBuques[, c("fcCambio", "Nombre", "CodigoCFR", "EsloraTotal", "PuertoBase", "CensoPorModalidad")]

write.table(CheckMaestroBuques, "Importacion_InfoBase\CheckMaestroBuques.csv", sep=",", dec=".", row.names = FALSE)

  # Censo completo español.

write.table(InfoBuques, "Importacion_InfoBase\CensoFlotaEspaña_2020.csv", sep=",", dec=".", row.names = FALSE)

# # ------------------------------- #
# # InfoDiarios                     #
# # ------------------------------- #
  #  id unico = IdDiarios

head(InfoDiarios)
dim(InfoDiarios)
length(unique(InfoDiarios$IdDiario))

temp <- InfoDiarios %>%  count(IdDiario) %>% filter(n > 1)

# # ------------------------------- #
# # InfoVentas                      #
# # ------------------------------- #
# El IdVenta no es único. 
# De infoVentas, solo sacamos la FCVenta y el Puertoventa. 
# Creo que no nos va a influir para sacar esta información, pero es importante tenerlo en cuenta


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

head (InfoOrigenLineas)
dim (InfoOrigenLineas)
length(unique(InfoOrigenLineas$IdInfoOrigenLinea))
dim(subset(InfoOrigenLineas, is.na(IdInfoOrigenLinea)))


# # ------------------------------- #
# # Depurar InfoCapturasCalculadas  #
# # ------------------------------- #
  # id unico: IdInfoOrigenLinea
  # creamos la variable IdCaptura

head(InfoCapturasCalculadas)
dim (InfoCapturasCalculadas)
length(unique(InfoCapturasCalculadas$IdInfoOrigenLinea ))

#InfoOrigenLineas$IdCaptura <- InfoCapturasCalculadas$IdCaptura [match(InfoOrigenLineas$IdInfoOrigenLinea, InfoCapturasCalculadas$IdInfoOrigenLinea)]


# # ------------------------------- #
# # InfoCapturaLance0               #
# # ------------------------------- #
  # id unica: IdCaptura

head(InfoCaptura0)
dim (InfoCaptura0)
length(unique(InfoCaptura0$IdCaptura))

    

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
str(InfoParametrosArteCapturas)
InfoParametrosArteCapturas %>% count( IdCaptura, CodigoParametroArte) %>% filter(n > 1)

  #cambiamos Valor parametros a numerico
table(InfoParametrosArteCapturas$ValorParametro)
InfoParametrosArteCapturas$ValorParametro <- as.numeric(InfoParametrosArteCapturas$ValorParametro)
range(InfoParametrosArteCapturas$ValorParametro)
  #cambiamos nombre parametros arte
table(InfoParametrosArteCapturas$ParametroArte)
InfoParametrosArteCapturas$ParametroArte <- mgsub(c("á","é","í","ó","ú"), c("a","e","i","o","u"), InfoParametrosArteCapturas$ParametroArte )
InfoParametrosArteCapturas <- InfoParametrosArteCapturas %>% 
                                        mutate(ParametroArte = recode (ParametroArte, "Tamaño malla (mm)"                       = "TamanoMalla_m",
                                                                                      "Altura (metros)"                         = "Altura_m",
                                                                                      "Altura media de las redes (m)"           = "AlturaMediaRedes_m",
                                                                                      "Longitud (metros)"                       = "Longitud_m",
                                                                                      "Logitud media de las redes (metros)"     = "LongitudMediaRedes_m",
                                                                                      "Longitud total de las redes a bordo (m)" = "LongitudTotalRedes",
                                                                                      "Numero de nasas"                         = "NumeroNasas",
                                                                                      "Numero total de anzuelos"                = "NumeroTotalAnzuelos",
                                                                                      "Numero de palangres o lineas lanzadas"   = "NumeroPalangresLineasLanzadas",
                                                                                      "Numero total de redes a bordo"           = "NumeroTotalRedes",
                                                                                      "Numero de redes lanzadas"                = "NumeroRedesLanzadas",
                                                                                      "Tamaño medio de los anzuelos (mm)"       = "TamañoMedioAnzuelos_mm",
                                                                                      "Perimetro de apertura (metros)"          = "PerimetroApertura_m",
                                                                                      "Profundidad (metros)"                    = "Profundidad_m"))



InfoParametrosArteCapturas_wide <- InfoParametrosArteCapturas %>% pivot_wider(id_cols = -CodigoParametroArte, 
                                                                              names_from = ParametroArte, 
                                                                              values_from = ValorParametro, 
                                                                              values_fn = list(ValorParametro = sum) )


sum(InfoParametrosArteCapturas_wide[,5:18], na.rm=T  )
sum(InfoParametrosArteCapturas$ValorParametro, na.rm=T  )

dim (InfoParametrosArteCapturas_wide)
length(unique(InfoParametrosArteCapturas_wide$IdCaptura))

head (InfoParametrosArteCapturas_wide)
unique(InfoParametrosArteCapturas[,c("CodigoParametroArte", "ParametroArte")])%>% arrange(CodigoParametroArte)

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

head(InfoDiarios)
names(InfoDiarios)

# ------------------------------- #    
# InfoVentas  
# ------------------------------- #  

head (InfoVentas)
names(InfoVentas)
names(InfoVentas)[names(InfoVentas)=="idDiario"] <- "IdDiario"
InfoVentas$Peso <- as.numeric(InfoVentas$Peso)



# ------------------------------- #  
# InfoOrigenLineas 
# ------------------------------- #  
head(InfoOrigenLineas)
names(InfoOrigenLineas)
names(InfoOrigenLineas)[names(InfoOrigenLineas)=="rectanguloEstadistico"] <- "RectanguloEstadistico"
names(InfoOrigenLineas)[names(InfoOrigenLineas)=="Division"] <- "CodigoDivision"
names(InfoOrigenLineas)[names(InfoOrigenLineas) == "FcInicioCaptura"] <- "FcInicio"
names(InfoOrigenLineas)[names(InfoOrigenLineas) == "FcFinCaptura"] <- "FcFin"

# ------------------------------- #  
# Depurar InfoCapturasCalculadas
# ------------------------------- #  #
head(InfoCapturasCalculadas)
names(InfoCapturasCalculadas)

# ------------------------------- #  
# InfoCaptura0
# ------------------------------- #  
# Posibiliad de filtrar las mareas con Numero de Operaciones/Tiempo de Pesca > 0. 

head(InfoCaptura0)
dim(subset(InfoCaptura0, NumOperaciones>0))
names(InfoCaptura0)[names(InfoCaptura0) == "PuertoSalida_AL5"]      <- "CodigoPuertoSalida_AL5"
names(InfoCaptura0)[names(InfoCaptura0) == "PuertoRegreso_AL5"]     <- "CodigoPuertoRegreso_AL5"
names(InfoCaptura0)[names(InfoCaptura0) == "PuertoDesembarque_AL5"] <- "CodigoPuertoDesembarque_AL5"


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
names(InfoDescartes)[names(InfoDescartes) == "Pais_AL3"] <- "PaisCaptura_AL3"
names(InfoDescartes)[names(InfoDescartes) == "FcDescarte"] <- "FcCaptura"
names(InfoDescartes)[names(InfoDescartes) == "HrDescarte"] <- "HrCaptura"

# ------------------------------- #  
# InfoParametrosArteCapturas
# ------------------------------- #  

head(InfoParametrosArteCapturas_wide)
names(InfoParametrosArteCapturas_wide)


# # ####################### #
# # Save data               #
# # ####################### #

save(InfoBase, InfoBuquesUnique, InfoDiarios, InfoVentas, InfoOrigenLineas, 
     InfoCaptura0, InfoCapturasCalculadas, InfoDescartes, 
     InfoParametrosArteCapturas_wide,
     MaestroBuques, MaestroPuertos, MaestroEspecies, file="0_Datos/Infobase/2020/Infobase2020_Unique_20210312.Rdata"  )

# ----------------------------------------------------------------- #
    