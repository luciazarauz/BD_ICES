getStrata <- function(DB, censo, metier, nombre_buque){
  names(DB)[names(DB)==censo] <- "censo"
  names(DB)[names(DB)==metier] <- "Metier"
  names(DB)[names(DB)==nombre_buque] <- "Nombre_Buque"
  DB$strata <- NA
  DB$strata[DB$censo %in% c("BACALADEROS") ]  <- "Bacalao" 
  DB$strata[DB$censo %in% c("ARTES FIJAS EN ZONAS CIEM VB, VI,VII Y VIIIABDE") ]  <- "Fijas_alt" 
  DB$strata[DB$censo %in% c("PALANGRE DE SUPERFICIE CALADERO NACIONAL", "VOLANTA EN CANTABRICO NW", 
                            "RASCO EN CANTABRICO NW","PALANGRE DE FONDO MENORES 100 TRB EN VIIIABDE",
                            "PALANGRE DE FONDO EN CANTABRICO NW")]  <- "Baj"
  DB$strata[DB$censo %in% c("CERCO EN CANTABRICO NW")]  <- "PS_LHP"    
  DB$strata[DB$censo %in% c("ARTES MENORES EN CANTABRICO NW")]  <- "ART"  
  DB$strata[DB$censo %in% c("ARRASTRE DE FONDO EN CANTABRICO NW", "ARRASTRE EN AGUAS DE PORTUGAL") 
            & DB$Metier %in% c("OTB_DEF_>=55_0_0","OTB_SPF_>=55_0_0","OTB_MPD_>=55_0_0" )]  <- "OTBlit"  
  DB$strata[DB$censo %in% c("ARRASTRE DE FONDO EN CANTABRICO NW") 
            & DB$Metier %in% c("PTB_MPD_>=55_0_0","PTB_DEF_>=55_0_0" )]  <- "PTBlit"  
  DB$strata[DB$censo %in% c("ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII Y VIIIABDE") 
            & DB$Metier %in% c("OTB_DEF_>=70_0_0","OTB_MCF_>=70_0_0","OTB_SPF_>=70_0_0", 
                               "OTB_MPD_>=70_0_0", "OTB_DEF_100-119_0_0", "OTB_DEF_70-99_0_0")]  <- "OTBbb" 
  DB$strata[DB$censo %in% c("ARRASTRE DE FONDO EN ZONAS CIEM VB, VI,VII Y VIIIABDE") 
            & DB$Metier %in% c("PTB_DEF_>=70_0_0")]  <- "PTBbb" 
  
  print(paste("número de registros sin estrato",unique(DB$Nombre_Buque[is.na(DB$strata)])))
  
  DB$strata[DB$Nombre_Buque %in% c("Kala Berri", "El David", "Mar de Pedro")] <- "ART"  
  DB$strata[DB$Nombre_Buque %in% c("Canalechebarria", "Izurdia Maitea", "Ostarte (ex Krexal)", "Ostarte (ex Krexal)", 
                                   "Gure Ama Martina", "Beti Begonako Ama (ex Beti Barrenetxea)")] <- "Baj"  # son barcos grandes
  
  print(paste("barcos en bajura con menos de 15m de eslora que no hemos cambiado a artesanal",
              unique(DB$Nombre_Buque[DB$strata =="Baj" & DB$Eslora_total<15 ])))
  print(paste("barcos en artes menores con mas de 15m de eslora que no hemos cambiado a bajura",
              unique(DB$Nombre_Buque[DB$strata =="ART" & DB$Eslora_total>15])))
  
  
  DB$strata
}


getFrame <- function(DB, puerto_venta, strata, CA_puerto_base, weekday ){
  names(DB)[names(DB)==puerto_venta] <- "Puerto_Venta"
  names(DB)[names(DB)==strata] <- "strata"
  names(DB)[names(DB)==CA_puerto_base] <- "Puerto_Base2"
  names(DB)[names(DB)==weekday] <- "weekdayNum"
  DB$frame <- NA
  DB$frame <- "N"
  # Comprobar que solo tenemos puerto de venta euskadi
  DB$frame[DB$Puerto_Venta %in% c("Armintza", "Bermeo", "Getaria", "Hondarribia", "Lekeitio", "Ondarroa", "Pasaia", "Mutriku", "Santurtzi")] <- "Y"
  DB$frame [DB$Puerto_Venta %in% c("Mutriku", "Santurtzi")]  <- "N"
  DB$frame [DB$strata %in% c("Fijas_alt", "OTBlit")]  <- "N"
  DB$frame [DB$strata %in% c("OTBbb", "OTBlit", "PTBbb", "PTBlit") ]  <- "N"
  DB$frame [DB$strata %in% c("OTBbb", "PTBbb") & DB$Puerto_Venta %in% c("Ondarroa")
            & DB$Puerto_Base2=="Euskadi" & DB$weekdayNum %in% c(1)]  <- "Y"
  DB$frame [DB$strata %in% c("PTBlit") & DB$Puerto_Venta %in% c("Ondarroa")
            & DB$Puerto_Base2=="Euskadi" & DB$weekdayNum %in% c(1,4)]  <- "Y"
  DB$frame [DB$strata %in% c("ART") & DB$Puerto_Venta %in% c("Getaria")]  <- "N"
  DB$frame [DB$strata %in% c("PS_LHP") & DB$Puerto_Venta %in% c("Pasaia")]  <- "N"
  DB$frame [DB$strata %in% c("PS_LHP") & DB$Puerto_Venta %in% c("Lekeitio")]  <- "N"
  DB$frame [DB$strata %in% c("Baj") & DB$Puerto_Venta %in% c("Pasaia")]  <- "N" # aclarar xq cn estanis. no me acuerdo
  
  DB$frame
}

