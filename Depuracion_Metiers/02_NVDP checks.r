################################################################################
#                   Muestreo Analisis
################################################################################
################################################################################

#  Desembarcos 

# agrupar por tamaño
# puerto base españa
#
################################################################################

#      Previo                  #####
####################################
rm(list=(ls()))

#library(data.table)
library (stringr)
library(doBy)

Fun_CountUnique <- function (x) { length(unique(x))}   

path <- "C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\BD AZTI\\"  #Base de datos en mi C:
dat.path <- paste(path,"data\\",sep="")
res.path <- paste(path,"results\\",sep="")
setwd(dat.path)



#      Leer los ficheros       ####
###################################

#Desembarcos
DB0 <-read.table("2017_Ventas.csv", sep=";",dec=",",header=T, stringsAsFactors = FALSE)
#DB <- as.data.table(DB0)
 DB <- DB0
 dim(DB); class(DB)
head(DB)

#Conversiones
conv_base<- read.csv("Conv_puerto_base.txt",header=T,sep=",",dec=".", stringsAsFactors = FALSE); head(conv_base)
buques<- read.csv("buques.csv",header=T,sep=";",dec=",", stringsAsFactors = FALSE); head(buques)
buques$Caladero.principal <- toupper(buques$Caladero.principal)
sort(unique(buques$Caladero.principal))
buques$Buque <- toupper(buques$Buque)



#Tabla resumen
###################

DB$Nombre_Buque <- toupper(DB$Nombre_Buque)
DB$CodigoUE <- buques$Codigo.UE [match(DB$Nombre_Buque, buques$Buque)]
DB$Codigo <- buques$Codigo [match(DB$Nombre_Buque, buques$Buque)]
DB$CaladeroPrincipal  <- buques$Caladero.principal  [match(DB$Nombre_Buque, buques$Buque)]


DB$Trip<- paste(DB$Nombre_Buque, DB$Dia, DB$Mes, DB$Ano)


temp1<- summaryBy(Trip ~ Nombre_Buque + CodigoUE + Codigo + CaladeroPrincipal + Puerto_Base + Puerto_Venta,
               data=DB, FUN=Fun_CountUnique)
names(temp1)[7] <- "Ntrips"

temp2<- summaryBy(Kg_Desembarcados ~ Nombre_Buque + CodigoUE + Codigo + CaladeroPrincipal + Puerto_Base + Puerto_Venta,
                   data=DB, FUN=sum, na.rm=T)
names(temp2)[7] <- "Kg_Desembarcados"


DB_Sum_PV <- merge(temp1, temp2, by=c("Nombre_Buque" , "Puerto_Base" , "Puerto_Venta","CodigoUE",  "Codigo","CaladeroPrincipal" ))

DB_Sum <- summaryBy(Ntrips + Kg_Desembarcados ~ Nombre_Buque + CodigoUE + Codigo + CaladeroPrincipal + Puerto_Base ,
                    data=DB_Sum_PV, FUN=sum, na.rm=T)

# guardar ficheros
setwd(res.path)

write.table(DB_Sum, "Resumen por barco_Ventas.csv", row.names = FALSE, sep=",")
write.table(DB_Sum_PV, "Resumen por barco y puerto venta_Ventas.csv", row.names = FALSE, sep=",")




