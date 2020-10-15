# 
# R version 3.6.0 (2019-04-26) -- "Planting of a Tree"

# Load                     ####
############################# #
rm(list=ls())


path.data <- file.path("C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\2020\\15_Simulacro\\20200421_InfoBaseFinal")
path.aux <- file.path("C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\2020\\15_Simulacro\\Auxtables")
path.res <- file.path("C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\2020\\15_Simulacro\\Results")


setwd(path.data)
load(file="Infobase2019_Unique_20200724.Rdata"   )



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



# EXPLORE LINKS OF THE TABLES      ########################
########################################################### #

head(InfoBase)
head(InfoBuquesUnique)
head(InfoDiarios)
head(InfoVentas)
head(InfoOrigenLineas)
head(InfoCapturas)
head(InfoParametrosArteCapturas_wide)
head(InfoCapturasCalculadas)
head(InfoCapturaLance0)
head(InfoDescartes)





# Crear tabla completa con los desembarques  ####
############################################### #

temp<- InfoCapturasCalculadas %>% anti_join(InfoCapturas, by="IdCaptura")  ; dim(temp)
temp<- InfoCapturas %>% anti_join(InfoCapturasCalculadas, by="IdCaptura")  ; dim(temp)
temp<- InfoCapturasCalculadas %>% anti_join(InfoOrigenLineas, by="IdInfoOrigenLinea")  ; dim(temp)
temp<- InfoCapturasCalculadas %>% anti_join(InfoDiarios, by="IdDiario")  ; dim(temp)
temp<- InfoDiarios %>% anti_join(InfoBuquesUnique, by="IdBuque")  ; dim(temp)


InfoCapturas_join <- select(InfoCapturas, -c(FcCaptura))
InfoParametrosArteCapturas_wide_join <- select(InfoParametrosArteCapturas_wide, -c(CodigoArte_FaoAL3))
InfoOrigenLineas_join <- select(InfoOrigenLineas, -c(IdDiario, IdNotaDeVenta))
InfoDiariosUnique_join <- select(InfoDiarios, -c())
InfoBuquesUnique_join <- select(InfoBuquesUnique, -c( FechaCambioDatosTecnicos, FechaCambioDatosMotor,FcEfectoInicial,
                                                      fcCambio, FcCambioIdentificacion))

InfoCapturasCalculadas_All <- InfoCapturasCalculadas %>%  left_join(InfoCapturas_join, by= c("IdCaptura" = "IdCaptura")) %>%
                                                          left_join(InfoOrigenLineas_join, by= c("IdInfoOrigenLinea" = "IdInfoOrigenLinea")) %>%
                                                          left_join(InfoParametrosArteCapturas_wide_join, by= c("IdCaptura" = "IdCaptura"))  %>% 
                                                          left_join(InfoDiariosUnique_join, by= c("IdDiario" = "IdDiario")) %>%
                                                          left_join(InfoBuquesUnique_join, by= c("IdBuque" = "IdBuque"))
                                                          
# limpiar variables
names(InfoCapturasCalculadas_All)[grepl(".x",   names(InfoCapturasCalculadas_All))]
    # dim(subset(InfoCapturasCalculadas_All, IdNotaDeVenta.x !=IdNotaDeVenta.y))     
    # InfoCapturasCalculadas_All <- InfoCapturasCalculadas_All %>% select(-c( IdNotaDeVenta.y))
    # names(InfoCapturasCalculadas_All)[names(InfoCapturasCalculadas_All)=="IdNotaDeVenta.x"] <- "IdNotaDeVenta"      

# crear variables
InfoCapturasCalculadas_All$CatchCategory <- "CapturasCalculadas"


  # Some checkings
  dim(InfoCapturasCalculadas)
  dim(InfoCapturasCalculadas_All)
  sum(InfoCapturasCalculadas$PesoConsumo)
  sum(InfoCapturasCalculadas_All$PesoConsumo)
     


# Crear tabla completa Descartes     #####
######################################## #

    temp<- InfoCapturasCalculadas_All %>% anti_join(InfoDescartes, by="IdDiario")  ; dim(temp)
    temp<- InfoDescartes %>% anti_join(InfoCapturasCalculadas_All, by="IdDiario")  ; dim(temp)  # Hay tres mareas que no estan en capturas calculadas
    temp<- InfoDescartes %>% anti_join(InfoCapturaLance0, by="IdDiario")  ; dim(temp)
    temp<- InfoDescartes %>% anti_join(InfoDiarios, by="IdDiario")  ; dim(temp)
    temp<- InfoDescartes %>% anti_join(InfoCapturasCalculadas_All, by="IdDiario") %>% anti_join(InfoCapturaLance0, by="IdDiario") ; dim(temp)
    

InfoDescartes_join <- select(InfoDescartes, -c(PaisBandera_AL3))
InfoDescartes_All <- InfoDescartes_join %>%  left_join(InfoDiariosUnique_join, by= c("IdDiario" = "IdDiario"))  %>%
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
  
  temp<- InfoCapturasCalculadas_All %>% anti_join(InfoCapturaLance0, by="IdDiario")  ; dim(temp)
  temp<- InfoCapturaLance0 %>% anti_join(InfoCapturasCalculadas_All, by="IdDiario")  ; dim(temp)
  temp<- InfoCapturaLance0 %>% anti_join(InfoDiarios, by="IdDiario")  ; dim(temp)
  
  temp<- InfoCapturaLance0 %>% anti_join(InfoCapturasCalculadas_All, by="IdCaptura")  ; dim(temp)
  temp<- InfoCapturaLance0 %>% inner_join(InfoCapturasCalculadas_All, by="IdCaptura")  ; dim(temp) # Todos los IdCaptura son nuevos. No estan en InfoCapturasCalculadas
  unique(temp$IdCaptura)

  
  subset(InfoCapturaLance0, IdCaptura==804106 )
  head(subset(InfoCapturasCalculadas_All, IdCaptura==804106 ))
  head(subset(InfoCapturasCalculadas_All, IdCaptura==804106 ))
  subset(InfoCapturasCalculadas_All, IdCaptura==804106 ) %>% group_by(FcCaptura) %>% summarise(Peso=sum(PesoConsumoTotal))
  
  
#creamos nueva tabla  
  InfoCapturaLance0_join <- select(InfoCapturaLance0, -c(FcSalida, FcRegreso, FcDesembarque, PaisBandera_AL3,
                                                         CodigoPuertoSalida_AL5, CodigoPuertoRegreso_AL5, CodigoPuertoDesembarque_AL5))
  InfoCapturaLance0_All <- InfoCapturaLance0_join %>%  left_join(InfoDiariosUnique_join, by= c("IdDiario" = "IdDiario", "IdBuque" = "IdBuque")) %>%
                                                        left_join(InfoBuquesUnique_join, by= c("IdBuque" = "IdBuque")) 
# limpiar variables
  names(InfoCapturaLance0_All)[grepl(".x",   names(InfoCapturaLance0_All))]
  # dim(subset(InfoCapturaLance0_All, PaisBandera_AL3.x !=PaisBandera_AL3.y))
  # InfoCapturaLance0_All <- InfoCapturaLance0_All %>% select(-c(FcSalida.y, FcRegreso.y, FcDesembarque.y, PaisBandera_AL3.y))
  # names(InfoCapturaLance0_All)[names(InfoCapturaLance0_All)=="PaisBandera_AL3.x"] <- "PaisBandera_AL3"
  
# crear variables
  InfoCapturaLance0_All$CatchCategory <- "CapturasLance0"

  
  
  # Some checkings
  dim(InfoCapturaLance0)
  dim(InfoCapturaLance0_All)
  sum(InfoCapturaLance0$NumOperaciones, na.rm=T)
  sum(InfoCapturaLance0_All$NumOperaciones, na.rm=T)
  

# Unir las tres tablas: PESO CONSUMO Y TODA LA INFO ASOCIADA ####
############################################################## #
  
  names(InfoDescartes_All)[!names(InfoDescartes_All) %in%  names(InfoCapturasCalculadas_All)]
  names(InfoCapturaLance0_All)[!names(InfoCapturaLance0_All) %in%  names(InfoCapturasCalculadas_All)]

  
  library(plyr)
  Dori <- rbind.fill(  InfoCapturasCalculadas_All, InfoDescartes_All, InfoCapturaLance0_All)
  detach(package:plyr)
  

  v1 <- c(names(Dori)[grepl("PesoConsumo|PesoCapturado|PesoDesembarcado|PesoNotaVenta|PesoRetenido|PesoTransferido", names(Dori))],
          names(Dori)[grepl("NumPiezasCapturadas|NumPiezasDesembarcadas|NumPiezasNotaVenta|NumPiezasRetenidas|NumPiezasTransferidas", names(Dori))])
  Dori[Dori$CatchCategory %in% c("Descartes", "CapturasLance0"), v1] <- 0 
  
  v2 <- c(names(Dori)[grepl("PesoDescarte", names(Dori))],
          names(Dori)[grepl("NumPiezasDescartadas", names(Dori))])
  Dori[Dori$CatchCategory %in% c("CapturasCalculadas" ), v2] <- 0 

  Dori <- Dori%>% arrange(IdBuque, FcRegreso, FcCaptura, Especie_AL3) %>% data.frame()

  

# Algunas comprobaciones      ####
################################ #
  
  dim(Dori)
  dim(InfoCapturasCalculadas_All)[1] +   dim(InfoDescartes_All)[1] +   dim(InfoCapturaLance0_All)[1]
  
  sum(Dori$PesoConsumo, na.rm=T)
  sum(InfoCapturasCalculadas$PesoConsumo, na.rm=T)
  
  sum(Dori$PesoDescarte, na.rm=T)
  sum(InfoDescartes$PesoDescarte, na.rm=T)
  

 rm(InfoCapturasCalculadas_All, InfoCapturasCalculadas, InfoDescartes_All, InfoDescartes_join,
    InfoCapturaLance0_All, InfoCapturaLance0_join, InfoCapturas, InfoCapturas_join)
 

 
# Grabar fichero                          ####
############################################### #
  
  save(Dori,InfoVentas, file="Dori2019_v1.RData")



  