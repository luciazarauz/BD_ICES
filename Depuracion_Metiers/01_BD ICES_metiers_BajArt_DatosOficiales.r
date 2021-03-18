################################################################################
#                   Muestreo Analisis
################################################################################
################################################################################

#  Desembarcos 
#  Puerto Base: españa:
#  Unidad Temporal:	Dia;    
#
#  Datos Oficiales (informe de datos oficiales - desembarcos)

# link ventas mareas.listado y actualizacion de metiers mareas (origen ventas) 
# idioma?

################################################################################

#      Previo                  #####
####################################
rm(list=(ls()))


library (stringr)
library(doBy)
library(plyr)
library(dplyr)
library(data.table)

Fun_CountUnique <- function (x) { length(unique(x))}   

path <- "C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\BD_AZTI_Metiers\\"  #Base de datos en mi C:
dat.path <- paste(path,"data\\",sep="")
res.path <- paste(path,"results\\",sep="")
setwd(dat.path)



#      Leer los ficheros       ####
###################################


#Desembarcos
DB <-read.table("2018_DO.csv", sep=";",dec=",",header=T, stringsAsFactors = FALSE)
#DB <- as.data.table(DB0)
head(DB); dim(DB)


#Conversiones
conv_base<- read.csv("Conv_puerto_base.txt",header=T,sep=",",dec=".", stringsAsFactors = FALSE); head(conv_base)

buques<- read.csv("2018_buques.csv",header=T,sep=";",dec=",", stringsAsFactors = FALSE); head(buques)
buques$Caladero.principal <- toupper(buques$Caladero.principal)
sort(unique(buques$Caladero.principal))
buques$Buque <- toupper(buques$Buque)

spAM<- read.csv("spAM.csv",header=T,sep=";",dec=",", stringsAsFactors = FALSE); head(spAM)

buquesLLSGNS<- read.csv("buques_LLS_GNS.csv",header=T,sep=";",dec=",", stringsAsFactors = FALSE); head(buquesLLSGNS)

setwd(res.path)

  # DB: Ventas
####################################
####################################

# Subset inicial. Quitamos la altura
DB$Pais_Base <- conv_base$origen_cod[match(DB$Puerto_Base, conv_base$puerto)]
unique(DB$Nombre_Buque[is.na(DB$Pais_Base)])
unique(DB$Puerto_Base[is.na(DB$Pais_Base)])

DB<- subset(DB, !substr(DB$Metier, 1, 3) %in% c("OTB", "PTB") )
DB<- subset(DB, DB$Pais_Base %in% c("ARM", "BER", "BIO", "DON", "ESP", "GET", "HON", "LEK", "MUN",
                                    "MUT", "OND", "ORI", "PAS", "PLE", "SAN", "ZIE") )


# Crear variables
Ano<- unique(DB$Ano)
DB$Nombre_Buque <- toupper(DB$Nombre_Buque)

DB$Vessel_length <- buques$Eslora [match(DB$Cod._UE_Buque, buques$Codigo.UE)]
unique(DB$Nombre_Buque[is.na(DB$Vessel_length)])

DB$Fecha_desembarco <- as.Date(DB$FECHA_DESEMBARQUE, tz="Europe/Madrid")
DB$Trip_id_V_desembarco <- paste(DB$Nombre_Buque, as.character(DB$Fecha_venta), DB$Fecha_desembarco, sep="_")

DB<- DB %>% group_by(Nombre_Buque, Fecha_desembarco) %>%  
            mutate(Metier_col=paste(unique(Metier), collapse="//"), Puerto_col=paste(unique(Puerto_Desembarco), collapse="//")) %>%
            ungroup() %>%
            as.data.frame
  

DB$Metier_Principal <- buquesLLSGNS$Metier.principal.2018[match(DB$Cod._UE_Buque, buquesLLSGNS$Codigo.UE)]
DB$Metier_Principal[is.na(DB$Metier_Principal)]<- "Otros"
sort(unique(DB$Nombre_Arte))


DB$Censo <- buques$Caladero.principal[match(DB$Cod._UE_Buque, buques$Codigo.UE)]
unique(DB$Nombre_Buque[is.na(DB$Censo)])
unique(DB$Nombre_Buque[DB$Censo==""])
unique(DB$Censo)

DB$Puerto_Base_CA <- conv_base$origen[match(DB$Puerto_Base, conv_base$puerto)]
unique(DB$Nombre_Buque[is.na(DB$Puerto_Base_CA)])

DB$Zona2 <- NA
DB$Zona2 [grep("27.8.a|27.8.b|27.8.d|27.8.e",DB$Zona)]<- "27.8.abde"
DB$Zona2 [grep("27.8.c",DB$Zona)]<- "27.8.c"
DB$Zona2 [grep("27.7",DB$Zona)]<- "27.7"
DB$Zona2 [grep("27.6",DB$Zona)]<- "27.6"
DB$Zona2 [grep("27.9.a",DB$Zona)]<- "27.9.a"
DB$Zona2 [grep("27.10",DB$Zona)]<- "27.10"

unique(DB$Nombre_Buque[is.na(DB$Zona2)])
unique(DB$Zona[is.na(DB$Zona2)])

sum(DB$Peso, na.rm=TRUE)           # 59747020



#depuracion sp
####################################

# rapes lophiidae (ANF) a rapes lophius (MNZ)
# rayas spp (SKA) a rajidae (RAJ)
# calamares loligo (SQc) a calamares loliginidae (SQZ)
# varios/variado a peces marinos (MZZ)
# camarones a camarones palaemon
# escorpaenidae (SCO) a rascacies (SCS)

unique(DB$Especie_Oficial[grep("pulp",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_ALFA3[grep("pulp",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("chicha",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_ALFA3[grep("chicha",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("salmo",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_ALFA3[grep("salmo",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("gallo",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_ALFA3[grep("gallo",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("rape",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_ALFA3[grep("rape",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("calama",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_ALFA3[grep("calama",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Científico[grep("calama",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("raya",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_ALFA3[grep("raya",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("scor",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_ALFA3[grep("scor",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Científico[grep("scor",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("scy",DB$Especie_Científico, ignore.case = TRUE)])
unique(DB$Especie_Científico[grep("scy",DB$Especie_Científico, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("vari",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("camaro",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("cabra",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_ALFA3[grep("cabra",DB$Especie_Oficial, ignore.case = TRUE)])
unique(DB$Especie_Oficial[grep("alg",DB$Especie_Oficial, ignore.case = TRUE)])


#asignacion de metier
####################################

sort(unique(DB$Censo))
sort(unique(DB$Zona))

DB$SpGroup <- NA
DB$SpGroup <- spAM$Grupo[match(DB$Especie_ALFA3 , spAM$Cod.ALFA.3)]
DB$SpGroup[is.na(DB$SpGroup)] <- "z_Otras"

db_sp<- DB %>% arrange(Nombre_Buque, Fecha_desembarco,Puerto_Venta) %>%
                group_by(Trip_id_V_desembarco) %>%
                mutate(Nsp=length(unique(Especie_ALFA3))) %>%
                ungroup() 

db_sp<-   db_sp %>% group_by(Nombre_Buque , Cod._UE_Buque , Eslora_total , Censo , Puerto_Base , Puerto_Base_CA  , Fecha_desembarco , 
                         Trip_id_V_desembarco ,  Metier , Zona2 , Metier_Principal , Nsp , SpGroup) %>%
                    summarise(Kg_Desemb_Peso_Vivo=sum(Peso, na.rm=TRUE))

db_met <- dcast(db_sp,  Nombre_Buque  + Eslora_total + Censo + Puerto_Base + Puerto_Base_CA  + Fecha_desembarco + 
                  Trip_id_V_desembarco +  Metier + Zona2 + Metier_Principal + Nsp  ~ SpGroup,
                fill=0, value.var = "Kg_Desemb_Peso_Vivo") #el fill=0 hace que en vez de poner NA a las celdas vacías las rellene como = 0 que es lo real.
db_met <- db_met %>% group_by(Nombre_Buque) %>% mutate(PrevMet=lag(Metier), NextMet=lead(Metier)) %>%
                ungroup() %>%
            select(Nombre_Buque:Nsp, PrevMet,NextMet, ALG:z_Otras )
db_met <- as.data.frame(db_met)



#selvar <- c("Nombre_Buque", "Cód_UE","Eslora_total", "Puerto_Base","Censo", "Fecha_desembarco", "Puerto_Venta", "Trip_id_V_desembarco",
#            "Metier", "Zona2", "Especie_Oficial", "Kg_Desembarcados", "SpGroup","Metier_Principal", "Nsp", "PrevMet",  "NextMet" ) 


db_met$Total <- db_met$ALG + db_met$CRU + db_met$GNS + db_met$GTR + db_met$LHM + 
                db_met$MIS + db_met$MOL + db_met$TUN + db_met$CEP + db_met$z_Otras 
db_met$P_ALG <-  db_met$ALG /  db_met$Total 
db_met$P_CRU <-  db_met$CRU /  db_met$Total 
db_met$P_GNS <-  db_met$GNS /  db_met$Total 
db_met$P_GTR <-  db_met$GTR /  db_met$Total 
db_met$P_LHM <-  db_met$LHM /  db_met$Total 
db_met$P_MIS <-  db_met$MIS /  db_met$Total 
db_met$P_MOL <-  db_met$MOL /  db_met$Total 
db_met$P_TUN <-  db_met$TUN /  db_met$Total 
db_met$P_CEP <-  db_met$CEP /  db_met$Total 
db_met$P_otr <-  db_met$z_Otras /  db_met$Total
db_met$P_Total <- db_met$P_ALG + db_met$P_CRU + db_met$P_GNS + db_met$P_GTR + db_met$P_LHM + 
                  db_met$P_MIS + db_met$P_MOL + db_met$P_TUN + db_met$P_CEP +db_met$P_otr
db_met$P2_GNS <-  (db_met$LHM + db_met$GNS) /  db_met$Total 

#db_met<- db_met %>% mutate_if( is.numeric, function(x){round(x,digits=2)})

Nombre_Arte_tab<-   DB %>% group_by( Trip_id_V_desembarco) %>%
                        summarise(Nombre_Arte_Col=paste(unique(Nombre_Arte), collapse="_"))
db_met$Nombre_Arte <- Nombre_Arte_tab$Nombre_Arte_Col[match(db_met$Trip_id_V_desembarco, Nombre_Arte_tab$Trip_id_V_desembarco)]

#mareas con dos ventas
CheckTripId<- names(table(db_met$Trip_id_V_desembarco)[table(db_met$Trip_id_V_desembarco)>1])
Check1V <- subset(DB, Trip_id_V_desembarco %in% CheckTripId)

subset(Check1V, Trip_id_V_desembarco=="MANUELAK_2018-09-11")
subset(Check1V, Censo=="ARTES MENORES EN CANTABRICO NW")

#################################
#     rasco y volanta
#################################
#################################

DBrv<- subset(DB, DB$Censo %in% c("RASCO EN CANTABRICO NW","VOLANTA EN CANTABRICO NW"))
rv_met<- subset(db_met, db_met$Censo %in% c("RASCO EN CANTABRICO NW","VOLANTA EN CANTABRICO NW"))

sort(unique(DBrv$Censo))
sort(unique(DBrv$Puerto_Base_CA))
sort(unique(DBrv$Metier))
sort(unique(DBrv$Zona2))
sort(unique(DBrv$Metier_Principal))
table(DBrv$Metier_Principal)

#Comprobar costeras
summary(rv_met$P_TUN[rv_met$Metier=="LTL_LPF_0_0_0"])
summary(rv_met$P_LHM[rv_met$Metier=="LHM_SPF_0_0_0"])
filter(rv_met,Metier=="LTL_LPF_0_0_0" & P_TUN<0.85)
filter(rv_met,Metier!="LTL_LPF_0_0_0" & P_TUN>0.75)
filter(rv_met,Metier=="LHM_SPF_0_0_0" & P_LHM<0.9)
filter(rv_met,Metier!="LHM_SPF_0_0_0" & P_LHM>0.75)
filter(rv_met,Metier=="MIS_ALG_0" & P_ALG<0.9)
filter(rv_met,Metier!="MIS_ALG_0" & P_ALG>0.75)
filter(rv_met,Metier=="FPO_CRU_0_0_0" & P_CRU<0.9)
filter(rv_met,Metier!="FPO_CRU_0_0_0" & P_CRU>0.75)
filter(rv_met,Metier=="FPO_MOL_0_0_0" & P_CEP<0.9)
filter(rv_met,Metier!="FPO_MOL_0_0_0" & P_CEP>0.75)
filter(rv_met,Metier=="MIS_MIS_0_0_0_HC" & P_MIS<0.9)
filter(rv_met,Metier!="MIS_MIS_0_0_0_HC" & P_MIS>0.75)

subset(DBrv, Trip_id_V_desembarco=="GOIENKALE (EX GURE LEPORRE BERRIA)__2018-12-19")
subset(DB, Trip_id_V_desembarco=="GOIENKALE (EX GURE LEPORRE BERRIA)__2018-12-19")
subset(rv_met, Trip_id_V_desembarco=="GOIENKALE (EX GURE LEPORRE BERRIA)__2018-12-19")

sort(unique(DBrv$Metier))
rv_met$Metier_Rev<-NA
rv_met$Metier_Rev[rv_met$Metier %in% c("LHM_SPF_0_0_0")]  <- "LHM_SPF_0_0_0"
rv_met$Metier_Rev[rv_met$Metier %in% c("LTL_LPF_0_0_0")]  <- "LTL_LPF_0_0_0"
rv_met$Metier_Rev[!(rv_met$Metier %in% c("LHM_SPF_0_0_0","LTL_LPF_0_0_0") ) &
                    rv_met$Zona2 %in% c("27.8.abde", "27.8.c" )]  <- "GNS_DEF_>=100_0_0"
rv_met$Metier_Rev[!(rv_met$Metier %in% c("LHM_SPF_0_0_0","LTL_LPF_0_0_0") ) &
                    rv_met$Puerto_Base_CA %in% c("Euskadi") &
                    rv_met$Zona %in% c("27.7","27.6" )]  <- "GNS_DEF_100-119_0_0"

rv_met[is.na(rv_met$Metier_Rev),]
rv_met$Metier_Check <- rv_met$Metier==rv_met$Metier_Rev 

DBrv$Metier_Rev <- rv_met$Metier_Rev[match(DBrv$Trip_id_V_desembarco, rv_met$Trip_id_V_desembarco )]
DBrv$Metier_Check <- DBrv$Metier==DBrv$Metier_Rev 

subset(DBrv, is.na(Metier_Rev))
sort(unique(DBrv$Metier_Rev))



SpSum_rv <- summaryBy(Peso~Metier_Rev +Especie_Oficial, data=DBrv, FUN=sum, na.rm=TRUE)
SpSum_rv <- arrange(SpSum_rv, Metier_Rev, desc( Peso.sum))



CheckTripId<- unique(DBrv$Trip_id_V_desembarco[DBrv$Especie_Oficial!="Verdel, Caballa" & DBrv$Metier_Rev=="LHM_SPF_0_0_0"])
subset(DBrv, Trip_id_V_desembarco %in% CheckTripId)

subset(DBrv, Metier!=Metier_Rev)

##########################
#   Palangre bajura
##########################
##########################

DBpa<- subset(DB, DB$Censo %in% c("PALANGRE DE FONDO EN CANTABRICO NW", "PALANGRE DE FONDO MENORES 100 TRB EN VIIIABDE."))
pa_met<- subset(db_met, db_met$Censo %in% c("PALANGRE DE FONDO EN CANTABRICO NW", "PALANGRE DE FONDO MENORES 100 TRB EN VIIIABDE."))

sort(unique(DBpa$Censo))
sort(unique(DBpa$Puerto_Base_CA))
sort(unique(DBpa$Metier))
sort(unique(DBpa$Zona2))
sort(unique(DBpa$Metier_Principal))

#Comprobar costeras
summary(pa_met$P_TUN[pa_met$Metier=="LTL_LPF_0_0_0"])
summary(pa_met$P_LHM[pa_met$Metier=="LHM_SPF_0_0_0"])
summary(pa_met$P_ALG[pa_met$Metier=="MIS_ALG_0"])

filter(pa_met,Metier=="LTL_LPF_0_0_0" & P_TUN<0.9)
filter(pa_met,Metier!="LTL_LPF_0_0_0" & P_TUN>0.75)
filter(pa_met,Metier=="LHM_SPF_0_0_0" & P_LHM<0.9)
filter(pa_met,Metier!="LHM_SPF_0_0_0" & P_LHM>0.75)
filter(pa_met,Metier=="MIS_ALG_0" & P_ALG<0.9)
filter(pa_met,Metier!="MIS_ALG_0" & P_ALG>0.75)
filter(pa_met,Metier=="FPO_CRU_0_0_0" & P_CRU<0.9)
filter(pa_met,Metier!="FPO_CRU_0_0_0" & P_CRU>0.75)
filter(pa_met,Metier=="FPO_MOL_0_0_0" & P_CEP<0.9)
filter(pa_met,Metier!="FPO_MOL_0_0_0" & P_CEP>0.75)
filter(pa_met,Metier=="MIS_MIS_0_0_0_HC" & P_MIS<0.9)
filter(pa_met,Metier!="MIS_MIS_0_0_0_HC" & P_MIS>0.75)
filter(pa_met,Metier=="GNS_DEF_>=100_0_0" & (P_GNS<0.8 & P_GTR<0.8))
filter(pa_met,Metier!="GNS_DEF_>=100_0_0" & P_MIS>0.75)

tapply(pa_met$Total ,list(pa_met$Nombre_Buque, pa_met$Metier), sum)
pa_met[(grep("Redes", pa_met$Nombre_Arte)),] %>% filter(Metier!="GNS_DEF_>=100_0_0")
pa_met[(grep("Palangres", pa_met$Nombre_Arte)),] %>% filter(Metier!="LLS_DEF_<24LOA")


subset(pa_met, Nombre_Buque=="EL DAVID",select=c("Nombre_Buque","Censo","Fecha_desembarco","Trip_id_V_desembarco","Metier","Nombre_Arte","Metier_Rev"))
subset(DBpa, Trip_id_V_desembarco=="BETI LAGUN BI__2018-02-10")
table(pa_met$Metier_Rev[pa_met$Nombre_Buque=="BETI LAGUN BI"])

sort(unique(DBpa$Metier))
pa_met$Metier_Rev<-NA
pa_met$Metier_Rev[pa_met$Metier %in% c("LHM_SPF_0_0_0")]  <- "LHM_SPF_0_0_0"

pa_met$Metier_Rev[pa_met$Metier %in% c("LTL_LPF_0_0_0")]  <- "LTL_LPF_0_0_0"
pa_met$Metier_Rev[pa_met$Metier %in% c("MIS_ALG_0")]  <- "MIS_ALG_0"
pa_met$Metier_Rev[pa_met$Metier %in% c("LLS_DEF_<24LOA")]  <- "LLS_DEF_<24LOA"
pa_met$Metier_Rev[pa_met$Metier %in% c("GNS_DEF_>=100_0_0")]  <- "GNS_DEF_>=100_0_0"
pa_met$Metier_Rev[pa_met$Metier %in% c("GNS_DEF_>=100_0_0") & (pa_met$P_GNS<0.8 & pa_met$P_GTR<0.8)]  <- NA
pa_met[(grep("Redes", pa_met$Nombre_Arte)),] %>% filter(Metier!="GNS_DEF_>=100_0_0")

pa_met$Metier_Check <- pa_met$Metier==pa_met$Metier_Rev 


DBpa$Metier_Rev <- pa_met$Metier_Rev[match(DBpa$Trip_id_V_desembarco, pa_met$Trip_id_V_desembarco )]
DBpa$Metier_Check <- DBpa$Metier==DBpa$Metier_Rev 

SpSum_pa <- summaryBy(Peso~Metier_Rev +Especie_Oficial, data=DBpa, FUN=sum, na.rm=TRUE)
SpSum_pa <- arrange(SpSum_pa, Metier_Rev, desc( Peso.sum))

CheckTripId<- unique(DBpa$Trip_id_V_desembarco[DBpa$Especie_Oficial!="Verdel, Caballa" & DBpa$Metier_Rev=="LHM_SPF_0_0_0"])
subset(DBpa, Trip_id_V_desembarco %in% CheckTripId)

CheckTripId<- unique(DBpa$Trip_id_V_desembarco[DBpa$Especie_Oficial %in% c("Percebe") & DBpa$Metier_Rev=="LLS_DEF_<24LOA"])
subset(DBpa, Trip_id_V_desembarco %in% CheckTripId)


##########################
#   Artes menores
##########################
##########################

DBam<- subset(DB, DB$Censo %in% c("ARTES MENORES EN CANTABRICO NW"))
am_met<- subset(db_met, db_met$Censo %in% c("ARTES MENORES EN CANTABRICO NW"))

sort(unique(DBam$Censo))
sort(unique(DBam$Puerto_Base_CA))
sort(unique(DBam$Metier))
sort(unique(DBam$Zona2))
sort(unique(DBam$Metier_Principal))


#Comprobar costeras
summary(am_met$P_TUN[am_met$Metier=="LTL_LPF_0_0_0"])
summary(am_met$P_LHM[am_met$Metier=="LHM_SPF_0_0_0"])
summary(am_met$P_ALG[am_met$Metier=="MIS_ALG_0"])
summary(am_met$P_CRU[am_met$Metier=="FPO_CRU_0_0_0"])
summary(am_met$P_MOL[am_met$Metier=="FPO_MOL_0_0_0"])
summary(am_met$P_MIS[am_met$Metier=="MIS_MIS_0_0_0_HC"])

filter(am_met,Metier=="LHM_SPF_0_0_0" & P_LHM<0.9)
filter(am_met,Metier=="LHM_SPF_0_0_0" & P_LHM<0.9 & am_met$Nombre_Arte!="Líneas de mano y líneas de cana (mecanizadas)")
filter(am_met,Metier!="LHM_SPF_0_0_0" & P_LHM>0.75)
filter(am_met,Metier=="LTL_LPF_0_0_0" & P_TUN<0.9)
filter(am_met,Metier!="LTL_LPF_0_0_0" & P_TUN>0.75)
filter(am_met,Metier=="MIS_ALG_0" & P_ALG<0.9)
filter(am_met,Metier!="MIS_ALG_0" & P_ALG>0.75)
filter(am_met,Metier=="FPO_CRU_0_0_0" & P_CRU<0.9)
filter(am_met,Metier=="FPO_CRU_0_0_0" & P_CRU<0.9 & am_met$Nombre_Arte!="Nasas")
filter(am_met,Metier!="FPO_CRU_0_0_0" & P_CRU>0.75)
filter(am_met,Metier=="FPO_MOL_0_0_0" & P_CEP<0.9)
filter(am_met,Metier!="FPO_MOL_0_0_0" & P_CEP>0.75)
filter(am_met,Metier=="MIS_MIS_0_0_0_HC" & P_MIS<0.9)
filter(am_met,Metier!="MIS_MIS_0_0_0_HC" & P_MIS>0.75)

filter(am_met,P_CEP>0.5)

#Nuevo metier
sort(unique(DBam$Metier))
am_met$Metier_Rev<-NA


#lineas de mano
am_met$Metier_Rev[am_met$Metier %in% c("LHM_SPF_0_0_0")]  <- "LHM_SPF_0_0_0"
#am_met$Metier_Rev[ am_met$Metier=="LHM_SPF_0_0_0" & am_met$P_LHM<0.8 & am_met$PrevMet!="LHM_SPF_0_0_0" &  am_met$PrevMet!="LHM_SPF_0_0_0"] <- "LHM_SPF_0_0_0"
am_met$Metier_Rev[ am_met$Metier!="LHM_SPF_0_0_0" & am_met$P_LHM>0.75 & am_met$P_LHM>1000] <- "LHM_SPF_0_0_0"
am_met$Metier_Rev[ am_met$Metier!="LHM_SPF_0_0_0" & am_met$P_LHM>0.8 & (am_met$PrevMet=="LHM_SPF_0_0_0" | am_met$PrevMet=="LHM_SPF_0_0_0")] <- "LHM_SPF_0_0_0"
am_met$Metier_Rev[ am_met$Metier=="LHM_SPF_0_0_0" & am_met$P_LHM<0.9 & am_met$Nombre_Arte!="Líneas de mano y líneas de cana (mecanizadas)"] <- "LHM_SPF_0_0_0"

#cacea
am_met$Metier_Rev[am_met$Metier %in% c("LTL_LPF_0_0_0")]  <- "LTL_LPF_0_0_0"
am_met$Metier_Rev[am_met$P_TUN>0.9]  <- "LTL_LPF_0_0_0"

#algas
am_met$Metier_Rev[am_met$Metier %in% c("MIS_ALG_0")]  <- "MIS_ALG_0"
#anemonas
am_met$Metier_Rev[am_met$Metier %in% c("MIS_MIS_0_0_0_HC")]  <- "MIS_MIS_0_0_0_HC"


#nasas
am_met$Metier_Rev[am_met$P_CRU>0.75 ] <- "FPO_CRU_0_0_0"
am_met$Metier_Rev[am_met$P_MOL>0.75 ] <- "FPO_MOL_0_0_0"

am_met$Metier_Rev[ substr(am_met$Metier,1,3)=="FPO" & am_met$Nombre_Arte!="Nasas" ] <- NA


am_met$Metier_Rev[(am_met$P_MOL+ am_met$P_CRU)>0.75  & am_met$P_MOL>am_met$P_CRU] <- "FPO_MOL_0_0_0"
am_met$Metier_Rev[(am_met$P_MOL+ am_met$P_CRU)>0.75  & am_met$P_MOL<am_met$P_CRU] <- "FPO_CRU_0_0_0"
am_met$Metier_Rev[(am_met$P_MOL+ am_met$P_CRU)>0.75  & am_met$P_MOL==am_met$P_CRU] <- "FPO_CRU_0_0_0"


#redes y palangres
head(buquesLLSGNS)
sort(unique(buquesLLSGNS$Metier.principal.2018))

am_met$Metier_Rev[am_met$Metier %in% c("GNS_DEF_80-99_0_0", "GNS_DEF_60-79_0_0","GTR_DEF_60-79_0_0")] <- "GNS_DEF_60-79_0_0"
am_met$Metier_Rev[am_met$Metier=="LLS_DEF_<=1000" ] <- "LLS_DEF_<=1000"

am_met$Metier_Rev[am_met$Metier_Rev=="LLS_DEF_<=1000" & am_met$Metier_Principal=="GNS_DEF_60-79_0_0"  ] <- "GNS_DEF_60-79_0_0"
am_met$Metier_Rev[am_met$Metier_Rev=="GNS_DEF_60-79_0_0" & am_met$Metier_Principal=="LLS_DEF_<=1000"  ] <- "LLS_DEF_<=1000"

am_met$Metier_Rev[am_met$Metier_Rev=="GNS_DEF_60-79_0_0" & am_met$Puerto_Base_CA!="Euskadi"  ] <- "GNS_DEF_80-99_0_0"
am_met$Metier_Rev[am_met$Metier_Rev=="GNS_DEF_80-99_0_0" & am_met$Puerto_Base_CA=="Euskadi"  ] <- "GNS_DEF_60-79_0_0"


am_met$Metier_Rev[am_met$Metier_Rev=="GNS_DEF_60-79_0_0" & am_met$P2_GNS>=0.5 ] <- "GNS_DEF_60-79_0_0"
am_met$Metier_Rev[am_met$Metier_Rev=="GNS_DEF_60-79_0_0" &  am_met$P_GTR>=0.5 ] <- "GTR_DEF_60-79_0_0"
am_met$Metier_Rev[am_met$Metier_Rev=="GNS_DEF_60-79_0_0" &  am_met$P_GTR>=0.4 & am_met$P2_GNS<0.2] <- "GTR_DEF_60-79_0_0"

#lineas de mano a calamares
am_met$Metier_Rev[am_met$P_CEP>0.5]  <- "LHM_CEP_0_0_0"

am_met[is.na(am_met$Metier_Rev),]

am_met$Metier_Check <- am_met$Metier==am_met$Metier_Rev 


DBam$Metier_Rev <- am_met$Metier_Rev[match(DBam$Trip_id_V_desembarco, am_met$Trip_id_V_desembarco )]
DBam$Metier_Check <- DBam$Metier==DBam$Metier_Rev 



SpSum_am <- summaryBy(Peso~Metier_Rev +Especie_Oficial, data=DBam, FUN=sum, na.rm=TRUE)
SpSum_am <- arrange(SpSum_am, Metier_Rev, desc( Peso.sum))

CheckTripId<- unique(DBam$Trip_id_V_desembarco[DBam$Especie_Oficial!="Verdel, Caballa" & DBam$Metier_Rev=="LHM_SPF_0_0_0"])
subset(DBam, Trip_id_V_desembarco %in% CheckTripId)





##########################
#   Cerco
##########################
##########################

DBps<- subset(DB, DB$Censo %in% c("CERCO EN CANTABRICO NW"))
ps_met<- subset(db_met, db_met$Censo %in% c("CERCO EN CANTABRICO NW"))

Buque_cercoLHM <- c("AITA RAMON", "ANDUIZA ANAIAK", "BETI ITXAS ARGI", "MARIA DIGNA DOS")
Buque_cercoLTL <- c("AITA RAMON", "AMATXO (3BI21-96)", "ANDUIZA ANAIAK", "BETI EUSKAL HERRIA", "BETI ITXAS ARGI", "DEMAR",
                    "LEKANDA",   "OSKARBI", "MARIA DIGNA DOS", "NUEVO ROBER")


sort(unique(DBps$Censo))
sort(unique(DBps$Puerto_Base_CA))
sort(unique(DBps$Metier))
sort(unique(DBps$Zona2))
sort(unique(DBps$Metier_Principal))


#Comprobar costeras
ps_met[ ps_met$Metier %in% c("LHP_LPF_0_0_0", "LTL_LPF_0_0_0") & ps_met$P_TUN<0.8,]
ps_met[ !ps_met$Metier %in% c("LHP_LPF_0_0_0", "LTL_LPF_0_0_0") & ps_met$P_TUN>0.75,]

ps_met[ ps_met$Metier %in% c("LHM_SPF_0_0_0") & !ps_met$Nombre_Buque %in% Buque_cercoLHM,]
ps_met[ ps_met$Metier %in% c("PS_SPF_0_0_0") & ps_met$Nombre_Buque %in% Buque_cercoLHM,]

ps_met[ ps_met$Metier %in% c("LTL_SPF_0_0_0") & !ps_met$Nombre_Buque %in% Buque_cercoLTL,]
ps_met[ ps_met$Metier %in% c("LHP_SPF_0_0_0") & ps_met$Nombre_Buque %in% Buque_cercoLTL,]


#Creamos metier revisado
ps_met$Metier_Rev<-NA
ps_met$Metier_Rev<-ps_met$Metier

ps_met$Metier_Rev[ !ps_met$Metier %in% c("LHP_LPF_0_0_0","LTL_LPF_0_0_0") & ps_met$P_TUN>0.75] <- "LHP_LPF_0_0_0"
ps_met$Metier_Rev[  ps_met$Metier %in% c("LHP_LPF_0_0_0") & ps_met$Nombre_Buque %in% Buque_cercoLTL] <- "LTL_LPF_0_0_0"
ps_met$Metier_Rev[  ps_met$Metier %in% c("LTL_LPF_0_0_0") & !ps_met$Nombre_Buque %in% Buque_cercoLTL] <- "LHP_LPF_0_0_0"

ps_met$Metier_Rev[ ps_met$Metier %in% c("PS_SPF_0_0_0") &  ps_met$Nombre_Buque %in% Buque_cercoLHM] <- "LHM_SPF_0_0_0"
ps_met$Metier_Rev[ ps_met$Metier %in% c("LHM_SPF_0_0_0") &  !ps_met$Nombre_Buque %in% Buque_cercoLHM] <- "PS_SPF_0_0_0"


ps_met$Metier_Check <- ps_met$Metier==ps_met$Metier_Rev 

DBps$Metier_Rev <- ps_met$Metier_Rev[match(DBps$Trip_id_V_desembarco, ps_met$Trip_id_V_desembarco )]
DBps$Metier_Check <- DBps$Metier==DBps$Metier_Rev 



SpSum_ps <- summaryBy(Peso~Metier_Rev +Especie_Oficial, data=DBps, FUN=sum, na.rm=TRUE)
SpSum_ps <- arrange(SpSum_ps, Metier_Rev, desc( Peso.sum))

CheckTripId<- unique(DBps$Trip_id_V_desembarco[DBps$Especie_Oficial!="Verdel, Caballa" & DBps$Metier_Rev=="LHM_SPF_0_0_0"])
subset(DBps, Trip_id_V_desembarco %in% CheckTripId)




### JUNTAR TODO

dim(DBrv)
dim(DBpa)
dim(DBam)
dim(DBps)

DBall<-rbind(DBrv,DBpa,DBam,DBps)
DBall <- arrange(DBall, Nombre_Buque, Fecha_desembarco, desc(Peso))
DBall <- DBall[,c("Nombre_Buque", "Cod._UE_Buque","Puerto_Base", "Eslora_total", "Censo", "Zona2", 
                  "Fecha_desembarco", "Puerto_Venta", "Trip_id_V_desembarco", 
                  "Metier_col", "Puerto_col", "Metier_Principal", "Nombre_Arte", "Especie_Oficial","SpGroup",
                  "Peso", "Metier", "Metier_Rev",  "Metier_Check" )]

dim(rv_met)
dim(pa_met)
dim(am_met)
dim(ps_met)
met_all<- rbind(rv_met,pa_met,am_met,ps_met)
met_all <- arrange(met_all, Nombre_Buque, Fecha_desembarco, desc(Total))


SpSum <- summaryBy(Peso~Censo + Metier_Rev +Especie_Oficial, data=DBall, FUN=sum, na.rm=TRUE)
SpSum <- arrange(SpSum, Censo, Metier_Rev, desc( Peso.sum))

CheckTripId<- unique(DBall$Trip_id_V_desembarco[DBall$Especie_Oficial!="Verdel, Caballa" & DBall$Metier_Rev=="LHM_SPF_0_0_0"])
subset(DBall, Trip_id_V_desembarco %in% CheckTripId)


#guardar resultados

write.table(DBall, paste(Ano, "DB_metierizada_BajuraArt_DatosOficiales.csv", sep="_"), row.names = FALSE,sep=";", dec=",")
write.table(met_all, paste(Ano, "DB_metierizada_BajuraArt_porMarea_DatosOficiales.csv", sep="_"), row.names = FALSE,sep=";", dec=",")
write.table(Check1V, paste(Ano, "DB_mareas con misma fecha de venta y dos registros_DatosOficiales.csv", sep="_"), row.names = FALSE,sep=";", dec=",")


db_met[db_met$Trip_id_V_desembarco=="ALAIN BI_2018-11-01",]
db_sp[db_sp$Trip_id_V_desembarco=="ALAIN BI_2018-11-01",]


