# R version 3.6.0 (2019-04-26) -- "Planting of a Tree"

# Path                     ####
############################# #
rm(list=ls())


# Libraries             #####
############################ #

library(fishPiCodes)
#data("UNLOCODE")
#data("ASFIS")
library(lubridate)
library(data.table)
library(reshape2)
library(dplyr)
#library(plyr)


Fun_unique <- function(x){length(unique(x))}


# Leer fichero                          ####
############################################### #

load(file="0_Datos/Infobase/2020/Dori2020_v3.Rdata" )

# Preparar Tabla Importación  #####
################################# #

Dori <- Dori_FIN
# Final Check

Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(Nombre )) %>% filter(n > 1)
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(C_PuertoDesembarque )) %>% filter(n > 1)



Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(C_PuertoVenta )) %>% filter(n > 1)
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(C_Area )) %>% filter(n > 1)
Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(LABORATORIO )) %>% filter(n > 1)

Dori %>% group_by(IdMarea) %>% summarize(n=Fun_unique(CodigoMarea )) %>% filter(n > 1)
subset(Dori, is.na(CodigoMarea))

table(Dori$C_PuertoDesembarque [Dori$LABORATORIO =="IEO" ])
table(Dori$C_PuertoDesembarque [Dori$LABORATORIO =="AZTI" ])

table(Dori$PuertoBase [Dori$LABORATORIO =="AZTI" & Dori$LAB_PuertoDesembarco=="OTH"])
table(Dori$Nombre     [Dori$LABORATORIO =="AZTI" & Dori$LAB_PuertoDesembarco=="OTH"])
                                                                           
table(Dori$PuertoBase [Dori$LABORATORIO =="IEO" & Dori$LAB_PuertoDesembarco=="OTH"])
table(Dori$Nombre     [Dori$LABORATORIO =="IEO" & Dori$LAB_PuertoDesembarco=="OTH"])
table(Dori$PuertoDesembarque [Dori$LABORATORIO =="IEO" & Dori$LAB_PuertoDesembarco=="OTH"])


Dori <- subset(Dori, AZTI_BD=="Incorporar")

names(Dori)[names(Dori)=="Nombre"]                 <- "C_Buque"
names(Dori)[names(Dori)=="CodigoCFR"]              <- "C_CodUE"
names(Dori)[names(Dori)=="C_FcRegreso"]  
Dori$C_FcDesembarque                                 <- Dori$C_FcRegreso  # Lo igualamso solo para la cabecera. En un futuro creo que desaparecerá y nos quedarmso solo con FcRegreso 
names(Dori)[names(Dori)=="C_FcVentaMax"]           <- "C_FcVenta"
names(Dori)[names(Dori)=="C_PuertoDesembarque"]
names(Dori)[names(Dori)=="C_PuertoVenta"]
names(Dori)[names(Dori)=="C_Area"]
names(Dori)[names(Dori)=="IdMareaIEO"]             <- "C_IdMareaIEO"       # creado para el IEO (completo)

#Dori$IdBuqueIEO                                    <- NA                  # Lo quitamos. en un futuro ira en Mestros y se podra relacionar dentro de la BD
names(Dori)[names(Dori)=="CodigoMarea"]                                   # Tal y como nos viene en DO
Dori$FcSalida                                      <- Dori$FcSalida  + Dori$HrSalida
Dori$FcRegreso                                     <- Dori$FcRegreso + Dori$HrRegreso
Dori$FcDesembarque                                 <- Dori$FcDesembarque + Dori$HrDesembarque
names(Dori)[names(Dori)=="PuertoSalida"]
names(Dori)[names(Dori)=="PuertoRegreso"]
names(Dori)[names(Dori)=="PuertoDesembarque"]
names(Dori)[names(Dori)=="PuertoVenta"]            <- NA                   # de momento lo dejo en NA ( se podria cruzar con lso datos de Infobase). # lo mantenemos porque está en el historico. ya veremos si luego nos interesa mantenerlo
names(Dori)[names(Dori)=="LABORATORIO"]            <- "Laboratorio"

names(Dori)[names(Dori)=="CatchCategory"]
Dori$FcCaptura                                      <- Dori$FcCaptura  + Dori$HrCaptura
#names(Dori)[names(Dori)=="Especie_Oficial"]
names(Dori)[names(Dori)=="Especie_AL3"]
names(Dori)[names(Dori)=="PesoConsumo"]            <- "PesoVivo"
names(Dori)[names(Dori)=="PesoConsumoBajoTalla"]   <- "PesoVivoBajoTalla"
names(Dori)[names(Dori)=="PesoDescarte"]
names(Dori)[names(Dori)=="PesoDescarteBajoTalla"]
names(Dori)[names(Dori)=="MotivoDescarte"]
names(Dori)[names(Dori)=="CodigoDivision"]
names(Dori)[names(Dori)=="RectanguloEstadistico"]
names(Dori)[names(Dori)=="Lat"]
names(Dori)[names(Dori)=="Lon"]
names(Dori)[names(Dori)=="TiempoPescaMin"]
names(Dori)[names(Dori)=="NumOperaciones"]
names(Dori)[names(Dori)=="CodigoArte_FaoAL3"]
names(Dori)[names(Dori)=="Arte"]
Dori$FcInicioCaptura                                 <- Dori$FcInicio + Dori$HrInicio
Dori$FcFinCaptura                                    <- Dori$FcFin + Dori$HrFin
names(Dori)[names(Dori)=="PaisCaptura_AL3"]
names(Dori)[names(Dori)=="Profundidad_m"]
names(Dori)[names(Dori)=="TamañoMedioAnzuelos_mm"]<-"TamanoMedioAnzuelos_mm"
names(Dori)[names(Dori)=="Altura_m"]
names(Dori)[names(Dori)=="AlturaMediaRedes_m"]
names(Dori)[names(Dori)=="LongitudMediaRedes_m"]
names(Dori)[names(Dori)=="Longitud_m"]
names(Dori)[names(Dori)=="TamanoMalla_m"]
names(Dori)[names(Dori)=="NumeroTotalAnzuelos"]
names(Dori)[names(Dori)=="NumeroNasas"]
names(Dori)[names(Dori)=="NumeroPalangresLineasLanzadas"]
names(Dori)[names(Dori)=="NumeroRedesLanzadas"]
names(Dori)[names(Dori)=="NumeroTotalRedes"]
names(Dori)[names(Dori)=="PerimetroApertura_m"]
names(Dori)[names(Dori)=="LongitudTotalRedes"]
names(Dori)[names(Dori)=="IdDori"]
names(Dori)[names(Dori)=="IdDiario"]
names(Dori)[names(Dori)=="IdInfoOrigenLinea"]
names(Dori)[names(Dori)=="IdDescarte"]
names(Dori)[names(Dori)=="IdCaptura"]
names(Dori)[names(Dori)=="IdDesembarqueEspecie"]
names(Dori)[names(Dori)=="IdNotaDeVenta"]
names(Dori)[names(Dori)=="OrigenIdentificador"]
names(Dori)[names(Dori)=="IdBuque"]




DoriBD <- Dori %>% select(C_Buque,  C_CodUE, C_FcRegreso,   C_FcVenta, C_PuertoDesembarque, C_PuertoVenta, C_Area, C_IdMareaIEO, 
                          IdBuqueIEO, CodigoMarea, FcSalida, FcRegreso, FcDesembarque, PuertoSalida, PuertoRegreso, PuertoDesembarque, Laboratorio,
                          CatchCategory, FcCaptura, Especie_AL3, PesoVivo,  PesoVivoBajoTalla, PesoDescarte,  PesoDescarteBajoTalla, MotivoDescarte,
                          CodigoDivision, RectanguloEstadistico, Lat, Lon, TiempoPescaMin, NumOperaciones, CodigoArte_FaoAL3, Arte, FcInicioCaptura, 
                              FcFinCaptura, PaisCaptura_AL3,
                          Profundidad_m, TamanoMedioAnzuelos_mm, Altura_m,  AlturaMediaRedes_m, LongitudMediaRedes_m, Longitud_m, TamanoMalla_m,
                              NumeroTotalAnzuelos,  NumeroNasas,  NumeroPalangresLineasLanzadas, NumeroRedesLanzadas, NumeroTotalRedes, PerimetroApertura_m, LongitudTotalRedes,
                          IdDori, IdDiario, IdInfoOrigenLinea, IdDescarte, IdCaptura,  IdDesembarqueEspecie,  IdNotaDeVenta,  OrigenIdentificador, IdBuque
)



# Formatos
##
  # DoriBD <- DoriBD %>% mutate_at(vars(starts_with('C_Fc')),stamp("31/12/2030"))
  # 
  # DoriBD <- DoriBD %>% mutate_at(vars(starts_with('Fc')),stamp("31/12/2030 00:00:00"))
  # DoriBD <- DoriBD %>% mutate_at(vars(starts_with('Fc')), ~ ifelse(is.na(.), "", .))



# Grabar Tabla
save(DoriBD, file="0_Datos/Infobase/2020/DoriBD_Importar.RData")
write.table(DoriBD, file="0_Datos/Infobase/2020/DoriBD_Importar.csv", sep=";", dec=",", row.names = F)


# cuanto es texto no es problema. 
# en fecha y datos numericos es un problema
