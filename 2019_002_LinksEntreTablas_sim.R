# 
# R version 3.6.0 (2019-04-26) -- "Planting of a Tree"

# readme:
# de este script no sale ningun fichero nuevo ni se crean variables.
# sirve para chequear las relaciones entre las diferentes tablas


# Load                     ####
############################# #
# se cargan los datos derivados de la depuracion
rm(list=ls())
load(file="Datos/Infobase2019_Unique_20200724.Rdata"   )

# Libraries             #####
############################ #
library(dplyr)
library(lubridate)
library(data.table)
library(reshape2)

# EXPLORE LINKS OF THE TABLES      ########################
########################################################### #

head(InfoBase)
head(InfoBuquesUnique)
head(InfoVentas)
head(InfoOrigenLineas)
head(InfoCapturas)
head(InfoParametrosArteCapturas_wide)
head(InfoCapturasCalculadas)
head(InfoCapturaLance0)
head(InfoDescartes)


#
# InfoCapturas - InfoCapturasCalculadas             #####
######################################################### #

# Todas las IdCaptura de InfoCapturas tienen un Idcaptura en InfoCapturasCalculadas
# Pero no todas las IdCaptura en InfoCapturasCalculadas tienen un Idcaptura en InfoCapturas (62485 lineas)
# ya que son registros que vienen de Notas de venta (IdNotaDeVenta, 17290) o de Desembarqes (IdDesembarqueEspecie,8898)


temp<- InfoCapturas %>% anti_join(InfoCapturasCalculadas, by="IdCaptura")  ; dim(temp)
temp<- InfoCapturasCalculadas %>% anti_join(InfoCapturas, by="IdCaptura")  ; dim(temp)

dim(temp)
dim(subset(temp, is.na(IdCaptura)))
dim(subset(temp, !is.na(IdNotaDeVenta)))
dim(subset(temp, is.na(IdNotaDeVenta)))
dim(subset(temp, is.na(IdCaptura) & is.na(IdNotaDeVenta) & !is.na(IdDesembarqueEspecie)))
dim(subset(temp, is.na(IdCaptura) & is.na(IdNotaDeVenta) & is.na(IdDesembarqueEspecie)))
head(subset(temp, is.na(IdCaptura) & is.na(IdNotaDeVenta) & is.na(IdDesembarqueEspecie)))
t<- (subset(temp, is.na(IdCaptura) & is.na(IdNotaDeVenta) & is.na(IdDesembarqueEspecie)))
sum(t$PesoConsumo)
dim(subset(temp, is.na(IdCaptura) & is.na(IdNotaDeVenta) & is.na(IdDesembarqueEspecie) & is.na(IdMareaOrigen)))

tdiario <- unique (t$IdDiario)

test <- InfoCapturasCalculadas %>% filter(IdDiario %in% tdiario) %>% group_by(IdDiario) %>% 
  summarize(peso=sum(PesoConsumo)) %>% filter(peso==0)
test

InfoCapturasCalculadas%>% filter(IdDiario==437136)
InfoCapturaLance0 %>% filter(IdDiario==437136)
test%>% filter(IdDiario %in% unique(InfoCapturaLance0$IdDiario))
test%>% filter(!IdDiario %in% unique(InfoCapturaLance0$IdDiario))

subset(temp, is.na(IdNotaDeVenta) & is.na(IdDesembarqueEspecie)) %>% 
  filter(IdDiario %in% unique(InfoCapturaLance0$IdDiario)) %>% dim()
subset(temp, is.na(IdNotaDeVenta) & is.na(IdDesembarqueEspecie)) %>% 
  filter(!IdDiario %in% unique(InfoCapturaLance0$IdDiario)) %>% dim()

z<-tapply(InfoCapturasCalculadas$PesoConsumo, list(InfoCapturasCalculadas$IdDiario, InfoCapturasCalculadas$Desembarcado) , sum)
write.table(z, "test desembarco.csv", sep=",", dec=".", row.names = TRUE)


sum(InfoCapturasCalculadas$PesoNotaVenta)
sum(InfoVentas$Peso)


# mareas con peso consumo total=0
test <- InfoCapturasCalculadas %>% group_by(IdMareaOrigen, IdDiario) %>% 
  summarize(peso=sum(PesoConsumo))%>% filter(IdDiario>0); dim(test)

test0 <- InfoCapturasCalculadas %>% group_by(IdMareaOrigen) %>% 
  summarize(peso=sum(PesoConsumo))  %>% filter(peso==0)

dim(test)
dim(test0)

test$lance0 <- InfoCapturaLance0$IdDiario[match(test$IdDiario, InfoCapturaLance0$IdDiario)]

test00<- subset(test, !is.na(lance0) & peso==0)

dim(test00)


testcap0 <- InfoCapturaLance0 %>% group_by(IdDiario) %>% 
  summarize(NumOperaciones=sum(NumOperaciones))  
dim(testcap0)
testcap0$capcal<- InfoCapturasCalculadas$IdDiario[match(testcap0$IdDiario, InfoCapturasCalculadas$IdDiario)]

testcap00<- subset(testcap0, !is.na(capcal) )

dim(testcap0)
dim(testcap00)


# InfoOrigenLineas - InfoCapturasCalculadas             #####
######################################################### #
# Todas las InfoCapturasCalculadas tienen un IdInfoOrigenLinea en InfoOrigenLineas. 
# Hay IdInfoOrigenLinea en InfoOrigenLineas que no aprecen en InfoCapturasCalculadas

temp<- InfoCapturasCalculadas %>% anti_join(InfoOrigenLineas, by=c("IdInfoOrigenLinea"="IdInfoOrigenLinea"))  ; dim(temp)
temp<- InfoOrigenLineas %>% anti_join(InfoCapturasCalculadas, by=c("IdInfoOrigenLinea"="IdInfoOrigenLinea"))  ; dim(temp)

aa<- unique(temp$IdDiario)



# InfoOrigenLineas - InfoDiariosUnique                    #####
######################################################### #
# Todas los IdDiario de InfoOrigenLineas tienen un IdDiario en InfoDiariosUnique 
# Existen los IdDiario en InfoDiariosUnique sin informacion en InfoOrigenLineas

temp<- InfoOrigenLineas %>% anti_join(InfoDiarios, by="IdDiario")  ; dim(temp)
temp<- InfoDiarios %>% anti_join(InfoOrigenLineas, by="IdDiario")  ; dim(temp)

aa2<- unique(temp$IdDiario)

# InfoDescartes - InfoDiariosUnique                 #####
######################################################### #
# Todas los IdDiario de InfoDescartes tienen un IdDiario en InfoDiariosUnique 
# Existen los IdDiario en InfoDiariosUnique sin informacion en InfoDescartes

temp<- InfoDescartes %>% anti_join(InfoDiarios, by="IdDiario")  ; dim(temp)
temp<- InfoDiarios %>% anti_join(InfoDescartes, by="IdDiario")  ; dim(temp)


# InfoCapturaLance0 - InfoDiariosUnique             #####
######################################################### #
# Todas los IdDiario de InfoCapturaLance0 tienen un IdDiario en InfoDiariosUnique 
# Existen los IdDiario en InfoDiariosUnique sin informacion en InfoCapturaLance0

temp<- InfoCapturaLance0 %>% anti_join(InfoDiarios, by="IdDiario")  ; dim(temp)
temp<- InfoDiarios %>% anti_join(InfoCapturaLance0, by="IdDiario")  ; dim(temp)

temp<- InfoCapturaLance0 %>% inner_join(InfoCapturasCalculadas, by="IdCaptura")  ; dim(temp)
unique(temp$IdCaptura)
head(subset(temp, PesoConsumo==0))

# InfoOrigenLineas/InfoDescartes/ InfoCapturaLance0/ - InfoDiariosUnique             #####
########################################################################################## #
# Todas los IdDiario de InfoDiariosUnique tienen un IdDiario en InfoOrigenLineas o InfoDescartes o InfoCapturaLance0 


allDiario <- data.frame(IdDiario=c(InfoOrigenLineas$IdDiario, InfoDescartes$IdDiario, InfoCapturaLance0$IdDiario ))

temp<- InfoDiarios %>% anti_join(allDiario, by="IdDiario")  ; dim(temp)



# Comprobaciones problemas del ano pasado            #### #
######################################################### #

# Codigo de diario duplicado en algunas ventas

head(subset(InfoVentas, IdDiario<0))
head(subset(InfoVentas, IdDiario<0))
MareasNeg <-(-1)*(InfoDiarios$IdDiario[InfoDiarios$IdDiario<0]) # MareasNeg= IdDiario de las Mareas negativas en InfoDiarios

InfoVentas_check <- (InfoVentas[InfoVentas$IdVenta %in% MareasNeg & InfoVentas$IdDiario>0,]) # en InfoVentas: registros que tienen IdVenta en MareasNeg y IdDiario>0
dim(InfoVentas_check)  # en InfoVentas: No hay registros que tienen IdVenta en MareasNeg y IdDiario>0
InfoOrigenLineas_check <- (InfoOrigenLineas[InfoOrigenLineas$IdNotaDeVenta %in% MareasNeg & InfoOrigenLineas$IdDiario>0,])
dim(InfoOrigenLineas_check)  # en InfoOrigenLineas: No hay registros que tienen IdVenta en MareasNeg y IdDiario>0


# Mareas huerfanas: mareas con esloramayor de 10m, sin diario de pesca pero con NV, que en peso consumo aparece 0. SGM nos dice que mantengamos el peso Consumo=0

InfoCapturasCalculadas_check <- InfoCapturasCalculadas %>% left_join(select(InfoDiarios, IdDiario , IdBuque), by= c("IdDiario" = "IdDiario")) %>%
                                                           left_join(select(InfoBuquesUnique, IdBuque,Nombre,EsloraTotal), by= c("IdBuque" = "IdBuque")) 
                                                          

a <- filter(InfoCapturasCalculadas_check,EsloraTotal<10 ); head(a); dim(a)
a <- filter(InfoCapturasCalculadas_check,EsloraTotal<10 & PesoConsumo>0 ); head(a); dim(a)
a <- filter(InfoCapturasCalculadas_check,EsloraTotal<10 & PesoConsumo==0 ); head(a); dim(a)
a <- filter(InfoCapturasCalculadas_check,EsloraTotal>10 ); dim(a); head(a); dim(a)
