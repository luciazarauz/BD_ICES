# ----------------------------------------------------------------- #
# Readme:                                                           #
# ----------------------------------------------------------------- # 
# En este script combinamos todas las tablas en una sola mediante sus id correspondientes.
#
# INDICE:
#
# Functions
# Libraries.
# Load data from depuracion.
# Link tables
#
# ----------------------------------------------------------------- # 
# R version 3.6.0 (2019-04-26) -- "Planting of a Tree"
# ----------------------------------------------------------------- # 



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
data("UNLOCODE")
library(lubridate)
library(data.table)
library(reshape2)
library(dplyr)
#library(plyr)

# # ################## #
# # Load               #
# # ################## #

rm(list=ls())
load(file="0_Datos/Infobase/2020/Infobase2020_Unique_20210312.Rdata"   )


# EXPLORE LINKS OF THE TABLES      ########################
########################################################### #

head(InfoBase)
head(InfoBuquesUnique)
head(InfoDiarios)
head(InfoVentas)
head(InfoOrigenLineas)
head(InfoParametrosArteCapturas_wide)
head(InfoCapturasCalculadas)
head(InfoCaptura0)
head(InfoDescartes)



# Crear tabla completa con los desembarques  ####
############################################### #

temp<- InfoCapturasCalculadas %>% anti_join(InfoOrigenLineas, by="IdInfoOrigenLinea")  ; dim(temp)
temp<- InfoCapturasCalculadas %>% anti_join(InfoDiarios, by="IdDiario")  ; dim(temp)
temp<- InfoDiarios %>% anti_join(InfoBuquesUnique, by="IdBuque")  ; dim(temp)


InfoCapturasCalculadas_join <- select(InfoCapturasCalculadas, -c(CodigoMarea))
InfoOrigenLineas_join <- select(InfoOrigenLineas, -c(IdInfoBase, IdDiario, IdNotaDeVenta, IdCaptura, IdDesembarqueEspecie, FcCaptura, HrCaptura ))
InfoParametrosArteCapturas_wide_join <- select(InfoParametrosArteCapturas_wide, -c(IdInfoBase, CodigoArte_FaoAL3))
InfoDiarios_join <- select(InfoDiarios, -c(IdInfoBase))
InfoBuquesUnique_join <- select(InfoBuquesUnique, -c( fcCambio, IdInfoBase, idcierrexea,
                                                      FechaCambioDatosTecnicos, FechaCambioDatosMotor,
                                                      FcCambioIdentificacion, FcCambioCensoxModalidad))

InfoCapturasCalculadas_All <- InfoCapturasCalculadas_join %>%  left_join(InfoOrigenLineas_join, by= c("IdInfoOrigenLinea" = "IdInfoOrigenLinea")) %>%
                                                               left_join(InfoParametrosArteCapturas_wide_join, by= c("IdCaptura" = "IdCaptura"))  %>% 
                                                               left_join(InfoDiarios_join, by= c("IdDiario" = "IdDiario")) %>%
                                                               left_join(InfoBuquesUnique_join, by= c("IdBuque" = "IdBuque"))
                                                          
# limpiar variables
names(InfoCapturasCalculadas_All)[grepl(".x",   names(InfoCapturasCalculadas_All))]
      # dim(subset(InfoCapturasCalculadas_All, CodigoMarea.x !=CodigoMarea.y)) 
      # dim(subset(InfoCapturasCalculadas_All, FcCaptura.x !=FcCaptura.y)) 
      # head(subset(InfoCapturasCalculadas_All, FcCaptura.x !=FcCaptura.y, 
      #             select=c("IdDiario", "IdInfoOrigenLinea","Nombre", "FcCaptura.x", "HrCaptura.x", "FcCaptura.y", "HrCaptura.y")))    
      # head(subset(InfoCapturasCalculadas_All, FcCaptura.x !=FcCaptura.y & FcCaptura.y!="", 
      #   select=c("IdDiario", "IdInfoOrigenLinea","Nombre", "FcCaptura.x", "HrCaptura.x", "FcCaptura.y", "HrCaptura.y")))    
      # head(subset(InfoCapturasCalculadas_All, FcCaptura.x=="", 
      #            select=c("IdDiario", "IdInfoOrigenLinea","Nombre", "OrigenIdentificador" , "FcCaptura.x", "HrCaptura.x", "FcCaptura.y", "HrCaptura.y"))) 
      # length(sort(unique(InfoCapturasCalculadas$FcCaptura)))
      # length(sort(unique(InfoOrigenLineas$FcCaptura)))
      # 
      # subset(InfoCapturasCalculadas_All, IdDiario==-29225023)
      # subset(InfoCapturasCalculadas_All, FcSalida=="17/6/2020 0:00:00" & CodigoCFR=="ESP000027037")[,c("IdDiario", "IdInfoOrigenLinea","Nombre","OrigenIdentificador",  
      #                                                                                                  "FcCaptura.x", "HrCaptura.x", "FcCaptura.y", "HrCaptura.y")]
      
      
# crear variables
InfoCapturasCalculadas_All$CatchCategory <- "CapturasCalculadas"


  # Some checkings
  dim(InfoCapturasCalculadas)
  dim(InfoCapturasCalculadas_All)
  sum(InfoCapturasCalculadas$PesoConsumo)
  sum(InfoCapturasCalculadas_All$PesoConsumo)
     


# Crear tabla completa Descartes     #####
######################################## #
  # @@ qué hacer con las mareas que tienen solo descarte? 
  #    12 mareas descarte y sin capturas calculadas
  #    6 mareas descarte y sin capturas calculadas ni captura0 (carnada)

    temp<- InfoCapturasCalculadas_All %>% anti_join(InfoDescartes, by="IdDiario")  ; dim(temp)
    temp<- InfoDescartes %>% anti_join(InfoCapturasCalculadas_All, by="IdDiario")  ; dim(temp)  # Hay mareas que no estan en capturas calculadas. 
    temp<- InfoDescartes %>% anti_join(InfoCaptura0, by="IdDiario")  ; dim(temp)
    temp<- InfoDescartes %>% anti_join(InfoDiarios, by="IdDiario")  ; dim(temp)
    temp<- InfoDescartes %>% anti_join(InfoCapturasCalculadas_All, by="IdDiario") %>% anti_join(InfoCaptura0, by="IdDiario") ; dim(temp)
    

InfoDescartes_join <- select(InfoDescartes, -c(PaisCaptura_AL3))
InfoDescartes_All <- InfoDescartes_join %>%  left_join(InfoDiarios_join, by= c("IdDiario" = "IdDiario"))  %>%
                                             left_join(InfoBuquesUnique_join, by= c("IdBuque" = "IdBuque")) 
# limpiar variables
names(InfoDescartes_All)[grepl(".x",   names(InfoDescartes_All))]

# crear variables
InfoDescartes_All$CatchCategory <- "Descartes"


names(InfoDescartes_All)[grepl("Peso",names(InfoDescartes_All))]
names(InfoCapturasCalculadas_All)[grepl("Peso",names(InfoCapturasCalculadas_All))]


  # Some checkings
  dim(InfoDescartes)
  dim(InfoDescartes_All)
  sum(InfoDescartes$PesoDescarte)
  sum(InfoDescartes_All$PesoDescarte)
  names(InfoDescartes_All)
 
  

# Crear tabla completa InfoCapturaLance0    #####
############################################### #
  dim(InfoCaptura0)
  length(unique(InfoCaptura0$IdDiario))
  temp<- InfoCaptura0 %>% anti_join(InfoCapturasCalculadas_All, by="IdDiario")  ; dim(temp); length(unique(temp$IdDiario))
  temp<- InfoCaptura0 %>% anti_join(InfoDiarios, by="IdDiario")  ; dim(temp)
  
  
  temp<- InfoCaptura0 %>% anti_join(InfoCapturasCalculadas_All, by="IdCaptura")  ; dim(temp)
  temp<- InfoCaptura0 %>% inner_join(InfoCapturasCalculadas_All, by="IdCaptura")  ; dim(temp); length(unique(temp$IdDiario.x)) 
      ## IdCaptura que estan en InfoCapturascalculadas y en Captura0 (101 mareas)
  unique(temp$IdCaptura)


# @@ que hacemos con la CapturaLance0, que tiene un idCaptura en el que también hay CapturasCalculadas?
  # tenemos
  subset(InfoCaptura0, IdCaptura==1233899 )
  (subset(InfoCapturasCalculadas_All, IdCaptura==1233899 ))
  (subset(InfoCaptura0, IdDiario ==814935  ))
  (subset(InfoCapturasCalculadas_All, IdDiario ==814935  ))
  (subset(InfoDescartes, IdDiario ==814935  ))
  subset(InfoCapturasCalculadas_All, IdCaptura==1233899 ) %>% group_by(FcCaptura) %>% summarise(Peso=sum(PesoConsumoTotal))
  
  #las 101 lineas de Capturas Calcualdas que stán también en infoCaptura0, tienen PesoConsumo = 0
  a <- subset(InfoCapturasCalculadas, IdCaptura %in% temp$IdCaptura)
  a %>% summarise(Peso=sum(PesoConsumoTotal))
  length(unique(a$IdDiario))
  
  
#creamos nueva tabla  
  
  InfoCaptura0_join <- select(InfoCaptura0, -c(FcSalida, HrSalida, FcRegreso, HrRegreso, FcDesembarque, HrDesembarque, PaisBandera_AL3,
                                                         CodigoPuertoSalida_AL5, CodigoPuertoRegreso_AL5, CodigoPuertoDesembarque_AL5, CodigoMarea))
  InfoCaptura0_All <- InfoCaptura0_join %>%  left_join(InfoDiarios_join, by= c("IdDiario" = "IdDiario", "IdBuque" = "IdBuque")) %>%
                                             left_join(InfoBuquesUnique_join, by= c("IdBuque" = "IdBuque")) 
# limpiar variables
  names(InfoCaptura0_All)[grepl(".x",   names(InfoCaptura0_All))]
  # dim(subset(InfoCaptura0_All, CodigoMarea.x != CodigoMarea.y))
  # InfoCapturaLance0_All <- InfoCapturaLance0_All %>% select(-c(FcSalida.y, FcRegreso.y, FcDesembarque.y, PaisBandera_AL3.y))
  # names(InfoCapturaLance0_All)[names(InfoCapturaLance0_All)=="PaisBandera_AL3.x"] <- "PaisBandera_AL3"
  
# crear variables
  InfoCaptura0_All$CatchCategory <- "CapturasLance0"

  
  
  # Some checkings
  dim(InfoCaptura0)
  dim(subset(InfoCaptura0_All, CatchCategory=="CapturasLance0"))
  sum(InfoCaptura0$NumOperaciones, na.rm=T)
  sum(InfoCaptura0_All$NumOperaciones[InfoCaptura0_All$CatchCategory =="CapturasLance0"], na.rm=T)
  

# Unir las tres tablas: PESO CONSUMO Y TODA LA INFO ASOCIADA ####
############################################################## #
  
  names(InfoDescartes_All)[!names(InfoDescartes_All) %in%  names(InfoCapturasCalculadas_All)]
  names(InfoCaptura0_All)[!names(InfoCaptura0_All) %in%  names(InfoCapturasCalculadas_All)]

  
  library(plyr)
  Dori <- rbind.fill(  InfoCapturasCalculadas_All, InfoDescartes_All, InfoCaptura0_All)
  detach(package:plyr)
  

  v1 <- c(names(Dori)[grepl("PesoConsumo|PesoCapturado|PesoDesembarcado|PesoNotaVenta|PesoRetenido|PesoTransferido", names(Dori))],
          names(Dori)[grepl("NumPiezasCapturadas|NumPiezasDesembarcadas|NumPiezasNotaVenta|NumPiezasRetenidas|NumPiezasTransferidas", names(Dori))])
  Dori[Dori$CatchCategory %in% c("Descartes", "CapturasLance0"), v1] <- 0 

  
  Dori[Dori$CatchCategory %in% c("Descartes"), "Desembarcado"] <- "Descartes" 
  Dori[Dori$CatchCategory %in% c("CapturasLance0"), "Desembarcado"] <- "CapturasLance0" 
  
  
  v2 <- c(names(Dori)[grepl("PesoDescarte", names(Dori))],
          names(Dori)[grepl("NumPiezasDescartadas", names(Dori))])
  Dori[Dori$CatchCategory %in% c("CapturasCalculadas", "CapturasLance0" ), v2] <- 0 
 
  Dori <- Dori%>% arrange(IdBuque, FcRegreso, FcCaptura, Especie_AL3) %>% data.frame()

  

# Algunas comprobaciones      ####
################################ #
  
  dim(Dori)
  dim(InfoCapturasCalculadas_All)[1] +   dim(InfoDescartes_All)[1] +   dim(InfoCaptura0_All)[1]
  
  sum(Dori$PesoConsumo, na.rm=T)
  sum(InfoCapturasCalculadas$PesoConsumo, na.rm=T)
  
  sum(Dori$PesoDescarte, na.rm=T)
  sum(InfoDescartes$PesoDescarte, na.rm=T)
  


 
# Grabar fichero                          ####
############################################### #
  
  save(Dori,InfoVentas, MaestroBuques, MaestroPuertos, MaestroEspecies, file="0_Datos/Infobase/2020/Dori2020_v1.RData")



  