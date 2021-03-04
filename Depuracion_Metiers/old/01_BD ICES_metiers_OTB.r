################################################################################
#                   Muestreo Analisis
################################################################################
################################################################################

#  Desembarcos 

# agrupar por tamaño
#
# Si sale el metier OTB_SPF valorar si es realmente necesario, ya qu enos crea muchos problemas en IC.
# Igual podemos cambiarlo a OTB:MPD
################################################################################

#      Previo                  #####
####################################
rm(list=(ls()))

#library(data.table)
library (stringr)
library(doBy)
library(dplyr)
library(reshape2)

Fun_CountUnique <- function (x) { length(unique(x))}   

path <- "C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\BD AZTI\\"  #Base de datos en mi C:
dat.path <- paste(path,"data\\",sep="")
res.path <- paste(path,"results\\",sep="")
setwd(dat.path)

Año <- 2018


#      Leer los ficheros       ####
###################################

#Desembarcos
DB0 <-read.table("2018_Ventas.csv", sep=";", dec=",",header=T, stringsAsFactors = FALSE)
#DB <- as.data.table(DB0)
 DB <- DB0
 dim(DB); class(DB)
head(DB)

#Conversiones
spOTB<- read.csv("spOTB.csv",header=T,sep=";",stringsAsFactors = FALSE); head(spOTB)


#      Ajustar Meties OTB        #####
##################################################################

#Metier
######
sort(unique(DB$Metier))

sort(unique(DB$Metier))

DB$Metier_cod <- substr(DB$Metier,1,3)
otb<- subset(DB, Metier_cod %in% c("OTB") )
ptb<- subset(DB, Metier_cod %in% c("PTB"))

tapply(otb$Kg_Desembarcados, list(otb$Zona, otb$Metier), sum, na.rm=T)
tapply(otb$Kg_Desembarcados, list(otb$Puerto_Base, otb$Metier), sum, na.rm=T)

tapply(ptb$Kg_Desembarcados, list(ptb$Zona, ptb$Metier), sum, na.rm=T)
tapply(ptb$Kg_Desembarcados, list(ptb$Puerto_Base, ptb$Metier), sum, na.rm=T)

temp<- subset(DB, Puerto_Base=="Ondarroa" & Metier_cod %in% c("OTB", "PTB"))
tapply(temp$Kg_Desembarcados, list(temp$Nombre_Buque, temp$Metier_cod), sum, na.rm=T)

temp$Fecha_venta <- as.Date(format(ISOdate(temp$Ano,temp$Mes,temp$Dia),"%d/%m/%Y"),"%d/%m/%Y")
temp<- subset(temp, Nombre_Buque %in% c("Gure Gaskuna", "Gure Kantabriko"))
tapply(temp$Kg_Desembarcados, list(temp$Fecha_venta, temp$Metier_cod), sum, na.rm=T)
tapply(temp$Kg_Desembarcados, list(temp$Fecha_venta, temp$Metier_cod, temp$Nombre_Buque), sum, na.rm=T)


#      Metier - OTB            #########
########################################

#creamos variable marea

otb$Trip<- paste(otb$Nombre_Buque, otb$Dia, otb$Mes, otb$Ano)


# asignar grupos de especies

otb$Grupo<- spOTB$Grupo[match(otb$Especie_ALFA3,spOTB$Cod.ALFA.3)]
otb$Grupo[is.na(otb$Grupo)] <- "Otras"


    #alguna comprobacion de que no nos dejamos ningun sp importante fuera
    sp<- summaryBy(Kg_Desembarcados ~ Especie_Oficial + Grupo,
                                   data=otb, FUN=sum, na.rm=TRUE)
    temp<-subset(sp, Grupo=="Otras") %>% arrange(-Kg_Desembarcados.sum)
    
    sp <-sp %>% arrange(Grupo, -Kg_Desembarcados.sum)
    #


otb_sp<- summaryBy(Kg_Desembarcados ~ Nombre_Buque + Trip  + Grupo,
                     data=otb, FUN=sum, na.rm=TRUE)

otb_met <- dcast(otb_sp,  Nombre_Buque + Trip ~ Grupo,
                   fill=0,value.var = "Kg_Desembarcados.sum") #el fill=0 hace que en vez de poner NA a las celdas vacías las rellene como = 0 que es lo real.

otb_met$Puerto_Base <- otb$Puerto_Base[match(otb_met$Trip,otb$Trip)]
otb_met$Zona <- otb$Zona[match(otb_met$Trip,otb$Trip)]
otb_met$Metier <- otb$Metier[match(otb_met$Trip,otb$Trip)]
otb_met$Dia <- otb$Dia[match(otb_met$Trip,otb$Trip)]
otb_met$Mes <- otb$Mes[match(otb_met$Trip,otb$Trip)]
otb_met$Ano <- otb$Ano[match(otb_met$Trip,otb$Trip)]


otb_met <- otb_met[,c("Nombre_Buque", "Puerto_Base",  "Trip", "Dia", "Mes", "Ano",          
                          "Zona",   "Metier",   "Demersales", "Pelagico",   "Mixto" , "Otras")]

#Puerto_Venta +Puerto_Base +Dia +Mes +Trimestre + Ano + Zona + Metier

otb_met$Total <- otb_met$Demersales + otb_met$Pelagico + otb_met$Mixto +  otb_met$Otras 
otb_met$P_dem <- otb_met$Demersales /  otb_met$Total 
otb_met$P_pel <- otb_met$Pelagico /  otb_met$Total 
otb_met$P_mix <- otb_met$Mixto /  otb_met$Total 
otb_met$P_otr <- otb_met$Otras /  otb_met$Total 



#Asignar metiers
otb_met$Metier_Rev <- "OTB_DEF_>=70_0_0"

# subset por areas
table(otb_met$Zona)
otb_met_2678c<- subset(otb_met, Zona %in% c("27.1.b", "27.2.b", "27.2.b.2", "27.6", "27.6.a", "27.6.b","27.7", "27.7.j",
                                            "27.8.c.e", "27.8.c.w", "27.9", "27.9.a"))
otb_met_8<- subset(otb_met, Zona %in% c("27.8.a","27.8.abd", "27.8.b" ,"27.8.d"))

sum(otb_met$Total)
sum(otb_met_2678c$Total) + sum(otb_met_8$Total)



#VIIIc
otb_met_2678c$Metier_Rev[otb_met_2678c$Zona%in% c("27.8.c.e", "27.8.c.w" ,"27.8.c","27.9", "27.9.a" )] <- "OTB_DEF_>=55_0_0"
sort(unique(otb_met_2678c$Puerto_Base[otb_met_2678c$Metier=="OTB_DEF_>=55_0_0"]))

otb_met_2678c$Metier_Rev[otb_met_2678c$Zona%in% c("27.8.c.e", "27.8.c.w" ,"27.8.c","27.9", "27.9.a" ) &
                           otb_met_2678c$P_pel>=0.9 ] <- "OTB_SPF_>=55_0_0"


#VII
otb_met_2678c$Metier_Rev[otb_met_2678c$Zona%in% c("27.7", "27.7.j" )] <- "OTB_DEF_70-99_0_0"
sort(unique(otb_met_2678c$Puerto_Base[otb_met_2678c$Metier_Rev=="OTB_DEF_70-99_0_0"]))

#VI
otb_met_2678c$Metier_Rev[otb_met_2678c$Zona%in% c("27.6", "27.6.a", "27.6.b")] <- "OTB_DEF_100-119_0_0"
sort(unique(otb_met_2678c$Puerto_Base[otb_met_2678c$Metier_Rev=="OTB_DEF_100-119_0_0"]))

#II
otb_met_2678c$Metier_Rev[otb_met_2678c$Zona%in% c("27.1.b","27.2.b", "27.2.b.2","27.2.a","27.2","27.1.a"  )] <- "OTB_DEF_>=120_0_0"
sort(unique(otb_met_2678c$Puerto_Base[otb_met_2678c$Metier_Rev=="OTB_DEF_>=120_0_0"]))


#VIIIabd
otb_met_8$Metier_Rev <- NA
otb_met_8$Metier_Rev[otb_met_8$P_pel>0.8] <- "OTB_SPF_>=70_0_0"
otb_met_8$Metier_Rev[is.na(otb_met_8$Metier_Rev) & otb_met_8$P_pel<=0.8 & otb_met_8$P_pel>=0.5 ] <- "OTB_MPD_>=70_0_0"
otb_met_8$Metier_Rev[is.na(otb_met_8$Metier_Rev) & otb_met_8$P_mix>=0.25] <- "OTB_MCF_>=70_0_0"
otb_met_8$Metier_Rev[is.na(otb_met_8$Metier_Rev) & otb_met_8$P_mix<0.25 & otb_met_8$P_dem>=0.5 ] <- "OTB_DEF_>=70_0_0"
otb_met_8$Metier_Rev[is.na(otb_met_8$Metier_Rev) & otb_met_8$P_mix<0.2] <- "OTB_DEF_>=70_0_0"

otb_met_8$Metier_Rev[is.na(otb_met_8$Metier_Rev) & otb_met_8$P_mix<0.2] <- "OTB_DEF_>=70_0_0"


# unimos las dos bd

otb_fin <- rbind(otb_met_8,otb_met_2678c)
otb_fin <- otb_fin[order(otb_fin$Nombre_Buque,otb_fin$Mes,otb_fin$Dia),]
otb_fin$check <- otb_fin$Metier==otb_fin$Metier_Rev

otb_fin[is.na(otb_fin$Metier_Rev),]

# hay que tomar decisiones sobre los NA
subset(otb_fin, Nombre_Buque=="Gure Gaskuna")
subset(otb_fin, Nombre_Buque=="Intxortamendi")

# guardar ficheros
setwd(res.path)

write.table(otb_fin, paste(Año,"_DB_metierizada_OTB_porMarea.csv", sep=""), row.names = FALSE, sep=";", dec=",")
write.table(sp, paste(Año,"_DB_metierizada_clasificacion_especies_OTB.csv", sep=""), row.names = FALSE, sep=";", dec=",")



# falta revisar PTB

