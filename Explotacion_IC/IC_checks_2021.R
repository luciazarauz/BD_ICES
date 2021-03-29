
########################################################
##                                                    ##
##             Data checks for IC                     ##
##                                                    ##
########################################################

# R version 3.3.0

#

require(knitr)
library(pander)
library(ggplot2) 
library(doBy)
require(reshape2)
require(dplyr)
require(plyr)
require(data.table)

## Leer archivos   ####

#x <- read.table(azti, header = FALSE, stringsAsFactors=FALSE, sep = ",", col.names = paste0("V",seq_len(33)), fill = TRUE)

rm(HI_AZTI)
rm(SI_AZTI)
rm(SD_AZTI)



  HI_AZTI<-subset(x[,1:12], V1=="HI") 
  colnames(HI_AZTI)<-c("RecordType", "Country", "Year", "SeasonType", "Season","Fleet", "AreaType", "FishingArea", "DepthRange", "UnitEffort", "Effort", "AreaQualifier")
  
  SI_AZTI<-subset(x[,1:24], V1=="SI") 
  colnames(SI_AZTI)<-c("RecordType", "Country", "Year", "SeasonType", "Season","Fleet", "AreaType", "FishingArea", "DepthRange", "Species", "Stock", "CatchCategory", "ReportingCategory", "DataToFrom", "Usage",  "SamplesOrigin", "QualityFlag", "UnitCATON", "CATON", "OffLandings", "varCATON", "InfoFleet",  "InfoStockCoordinator", "InfoGeneral")
  
  if("SD" %in% unique(x$V1)){
    SD_AZTI<-subset(x[,1:33], V1=="SD") 
    colnames(SD_AZTI)<-c("RecordType", "Country", "Year", "SeasonType", "Season","Fleet", "AreaType", "FishingArea", "DepthRange", "Species", "Stock", "CatchCategory", "ReportingCategory", "Sex", "CANUMtype", "AgeLength", "PlusGroup", "SampledCatch", "NumSamplesLngt", "NumLngtMeas", "NumSamplesAge",  "NumAgeMeas", "unitMeanWeight", "unitCANUM", "UnitAgeOrLength", "UnitMeanLength", "Maturity", "NumberCaught", "MeanWeight", "MeanLength", "varNumLanded", "varWGTLanded", "varLGTLanded")  

  
  SD_AZTI$NumSamplesLngt[ SD_AZTI$NumSamplesLngt==-9]<-0
  SD_AZTI$NumLngtMeas[ SD_AZTI$NumLngtMeas==-9]<-0
  SD_AZTI$AgeLength <- as.numeric(SD_AZTI$AgeLength )
  } 

## explorar datos   ####
  #str(HI_AZTI)
  #str(SI_AZTI)
  #str(SD_AZTI)
  
  apply(subset(HI_AZTI, select=-c(Effort)), 2, unique)
  apply(subset(HI_AZTI, select=c(Effort)), 2, function(x) {c(min = min(x, na.rm=TRUE), max = max(x, na.rm=TRUE)) })
  
  apply(subset(SI_AZTI, select=-c(CATON)), 2, unique)
  apply(subset(SI_AZTI, select=c(CATON)), 2, function(x) {c(min = min(x, na.rm=TRUE), max = max(x, na.rm=TRUE)) })
  
  if("SD" %in% unique(x$V1)){
    apply(subset(SD_AZTI, select=-c(AgeLength,NumSamplesLngt,NumLngtMeas,NumberCaught,MeanWeight,MeanLength)),
        2, unique)
  apply(subset(SD_AZTI, select=c(AgeLength,NumSamplesLngt,NumLngtMeas,NumberCaught,MeanWeight,MeanLength)),
        2, function(x) {c(min = min(x, na.rm=TRUE), max = max(x, na.rm=TRUE)) })
  } 

## Crear tablas principales   ####
  
  ## Esfuerzo por metier y Area (Kw/day)
      temp<-NA
      EffortMetierArea<-NA
      temp <- tapply(HI_AZTI$Effort,list(HI_AZTI$Fleet,HI_AZTI$FishingArea), sum)
      temp<-cbind(temp,Total=rowSums(temp, na.rm=T))
      temp [which(is.na(temp))] <- 0
      temp <- round(temp, digits = 2)
      EffortMetierArea <- temp
      
  ## Esfuerzo por metier y trimestre (Kw/day)
      temp<-NA
      EffortMetierSeason<-NA
      temp <- tapply(HI_AZTI$Effort,list(HI_AZTI$Fleet,HI_AZTI$Season), sum)
      temp<-cbind(temp,Total=rowSums(temp, na.rm=T))
      temp [which(is.na(temp))] <- 0
      temp <- round(temp, digits = 2)
      EffortMetierSeason <- temp  

## SI     
      SI_AZTI$CATON[SI_AZTI$CatchCategory=="R"] <- SI_AZTI$OffLandings[SI_AZTI$CatchCategory=="R"]
      temp0 <- tapply(SI_AZTI$CATON,list(SI_AZTI$Fleet,SI_AZTI$FishingArea,SI_AZTI$CatchCategory,SI_AZTI$ReportingCategory), sum)
## Reported landings por metier y Area (ton) - CATON      
      temp<-NA
      LandRepMetierArea<-NA
      if("L" %in% attributes(temp0)$dimnames[[3]] && "R" %in%  attributes(temp0)$dimnames[[4]]) {  
        temp<-cbind(temp0[,,"L","R"],Total=rowSums(temp0[,,"L","R", drop=FALSE], na.rm=T))
        temp [which(is.na(temp))] <- 0
        temp <- round(temp, digits = 2)
        LandRepMetierArea <- temp  
      }
      

      
  ## Unreported landings por metier y Area (ton)
      temp<-NA
      LandUnrepMetierArea<-NA
      if("L" %in% attributes(temp0)$dimnames[[3]] && "N" %in%  attributes(temp0)$dimnames[[4]]) {  
        temp<-cbind(temp0[,,"L","N" ],Total=rowSums(temp0[,,"L","N", drop=FALSE], na.rm=T))
        #temp [which(is.na(temp))] <- 0
        temp <- round(temp, digits = 2)
        LandUnrepMetierArea <- temp  
      }
  
      
 ## Reported BMS por metier y Area (ton) - CATON
      temp<-NA
      BMSRepMetierArea<-NA
      if("B" %in% attributes(temp0)$dimnames[[3]] && "R" %in%  attributes(temp0)$dimnames[[4]]) {  
        temp<-cbind(temp0[,,"B","R"],Total=rowSums(temp0[,,"B","R", drop=FALSE], na.rm=T))
        #temp [which(is.na(temp))] <- 0
        temp <- round(temp, digits = 2)
        BMSRepMetierArea <- temp  
      }
      
  ## Estimated Discards por metier y Area (ton)
      temp<-NA
      DiscMetierArea<-NA
      if("D" %in% attributes(temp0)$dimnames[[3]] && "R" %in%  attributes(temp0)$dimnames[[4]]) {  
        temp<-cbind(temp0[,,"D","R" ],Total=rowSums(temp0[,,"D","R", drop=FALSE], na.rm=T))
        #temp [which(is.na(temp))] <- 0
        temp <- round(temp, digits = 2)
        DiscMetierArea <- temp  
      }
      
  ## Reported Discards por metier y Area (ton)
      temp<-NA
      RepDiscMetierArea<-NA
      if("R" %in% attributes(temp0)$dimnames[[3]] && "R" %in%  attributes(temp0)$dimnames[[4]]) {  
        temp<-cbind(temp0[,,"R","R" ],Total=rowSums(temp0[,,"R","R", drop=FALSE], na.rm=T))
        #temp [which(is.na(temp))] <- 0
        temp <- round(temp, digits = 2)
        RepDiscMetierArea <- temp  
      }
      
  ## Reported landings por metier y trimestre (ton)
      temp0 <- tapply(SI_AZTI$CATON,list(SI_AZTI$Fleet,SI_AZTI$Season,SI_AZTI$CatchCategory,SI_AZTI$ReportingCategory), sum)
      
      temp<-NA
      LandRepMetierSeason<-NA
      if("L" %in% attributes(temp0)$dimnames[[3]] && "R" %in%  attributes(temp0)$dimnames[[4]]) {  
        temp<-cbind(temp0[,,"L","R"],Total=rowSums(temp0[,,"L","R", drop=FALSE], na.rm=T))
        #temp [which(is.na(temp))] <- 0
        temp <- round(temp, digits = 2)
        LandRepMetierSeason <- temp  
      }
  
  ## Unreported landings por metier y trimestre (ton)
      temp<-NA
      LandUnrepMetierSeason<-NA
      if("L" %in% attributes(temp0)$dimnames[[3]] && "N" %in%  attributes(temp0)$dimnames[[4]]) {  
        temp<-cbind(temp0[,,"L","N" ],Total=rowSums(temp0[,,"L","N", drop=FALSE], na.rm=T))
        #temp [which(is.na(temp))] <- 0
        temp <- round(temp, digits = 2)
        LandUnrepMetierSeason <- temp  
      }

## BMS landings por metier y trimestre (ton)
      temp<-NA
      BMSRepMetierSeason<-NA
      if("B" %in% attributes(temp0)$dimnames[[3]] && "R" %in%  attributes(temp0)$dimnames[[4]]) {  
        temp<-cbind(temp0[,,"B","R" ],Total=rowSums(temp0[,,"B","R", drop=FALSE], na.rm=T))
        #temp [which(is.na(temp))] <- 0
        temp <- round(temp, digits = 2)
        BMSRepMetierSeason <- temp  
      }

  ## Discards por metier y trimestre (ton)
      temp<-NA
      DiscMetierSeason <- NA
      
      if("D" %in% attributes(temp0)$dimnames[[3]] && "R" %in%  attributes(temp0)$dimnames[[4]]) {  
        temp<-cbind(temp0[,,"D","R" ],Total=rowSums(temp0[,,"D","R", drop=FALSE], na.rm=T))
        #temp [which(is.na(temp))] <- 0
        temp <- round(temp, digits = 2)
        DiscMetierSeason <- temp  
      }  

## Reported Discards por metier y trimestre (ton)
      temp<-NA
      RepDiscMetierSeason <- NA
      
      if("R" %in% attributes(temp0)$dimnames[[3]] && "R" %in%  attributes(temp0)$dimnames[[4]]) {  
        temp<-cbind(temp0[,,"R","R" ],Total=rowSums(temp0[,,"R","R", drop=FALSE], na.rm=T))
        #temp [which(is.na(temp))] <- 0
        temp <- round(temp, digits = 2)
        RepDiscMetierSeason <- temp  
      }  
      
  ## Sampling effort por metier y trimestre (NÂº de mareas/lances muestreados)
      temp<-NA
      NTripsFisOp<-NA
      NTripsFisOp2<-NA
      if (exists("SD_AZTI") ){
        temp<-summaryBy(NumSamplesLngt ~ Fleet + FishingArea +CatchCategory+Season, data = SD_AZTI, FUN = unique )
        NTripsFisOp <- dcast(temp, Fleet+FishingArea+CatchCategory ~ Season, value.var="NumSamplesLngt.unique", sum)
        NTripsFisOp <- cbind(NTripsFisOp, Total=rowSums(NTripsFisOp[,4:ncol(NTripsFisOp), drop=FALSE]))
        NTripsFisOp <- NTripsFisOp[order(NTripsFisOp$CatchCategory),]       
        NTripsFisOp <- subset(NTripsFisOp, Total>0)
        NTripsFisOp2 <- dcast(temp, Fleet+CatchCategory ~ Season, value.var="NumSamplesLngt.unique", sum)
        NTripsFisOp2 <- cbind(NTripsFisOp2, Total=rowSums(NTripsFisOp2[,3:ncol(NTripsFisOp2), drop=FALSE]))

        }
      
  ## Sampling effort por metier y trimestre (NÂº de individuos)
      temp<-NA
      NIndiv<-NA
      NIndiv2<-NA
      if (exists("SD_AZTI") ){
        temp<-summaryBy(NumLngtMeas ~ Fleet + FishingArea +CatchCategory+Season, data = SD_AZTI, FUN = unique )
        NIndiv <- dcast(temp, Fleet+FishingArea+CatchCategory ~ Season, value.var="NumLngtMeas.unique",  sum)
        NIndiv <- cbind(NIndiv, Total=rowSums(NIndiv[,4:ncol(NIndiv), drop=FALSE]))
        NIndiv <- NIndiv[order(NIndiv$CatchCategory),]
        NIndiv <- subset(NIndiv, Total>0)
        NIndiv2 <- dcast(temp, Fleet+CatchCategory ~ Season, value.var="NumLngtMeas.unique",  sum)
        NIndiv2 <- cbind(NIndiv2, Total=rowSums(NIndiv2[,3:ncol(NIndiv2), drop=FALSE]))
        }

      
  ## Talla media de los landings por metier
    
      MeanWeightLand<-NA
      if (exists("SD_AZTI")  && "L" %in% unique(SD_AZTI$CatchCategory)){
            SD_AZTI <- as.data.table(SD_AZTI)
            mSeasonLand <-SD_AZTI[CatchCategory=="L"& ReportingCategory=="R" ,
                            list(MeanLength = sum(AgeLength*NumberCaught)/sum(NumberCaught)), 
                            by=.(Fleet=Fleet, Season=Season)]
            SeasonL <- dcast(mSeasonLand, Fleet~ Season, value.var="MeanLength",  sum)
            mTotalL <-SD_AZTI[CatchCategory=="L"& ReportingCategory=="R" ,
                                 list(Total = sum(AgeLength*NumberCaught)/sum(NumberCaught)), 
                                 by=.(Fleet=Fleet)]      
            MeanWeightLand <- merge(SeasonL,mTotalL, by="Fleet")
      }

## Crear gráficas Distribucion tallas landings   ####
      temp<-NA
      LandingLDYear<-NA
      LandingLDSeason<-NA
      
      if (exists("SD_AZTI")  && "L" %in% unique(SD_AZTI$CatchCategory))  {
        
  ## Landings length distribution annual (nº at length)
        SD_AZTI <- as.data.table(SD_AZTI)
        tempLandYear <- SD_AZTI[CatchCategory=="L"& ReportingCategory=="R" ,
                         list(NumberCaught = sum(NumberCaught)), 
                         by=.(Fleet=Fleet, AgeLength=AgeLength)]

      LandingLDYear<-ggplot(tempLandYear,aes(x=AgeLength,y=NumberCaught, group = Fleet))+
         geom_line()+
         facet_wrap(~ Fleet, scales="free_y", ncol=2)+
         geom_vline(data=MeanWeightLand,aes(xintercept=Total, col="red"))+ 
         geom_text(data=MeanWeightLand,aes(label = round(Total,2),  x=Inf, y=Inf, hjust=1.2, vjust=1.2, col="red"), size=3.5) +
         labs(x="Length",y="Number_Caught")+
         theme(legend.position="none")
       
      # windows(9,7)
      # LandingLDYear
    
  ## Landings length distribution per season (nº at length)
      tempLandSeason <- SD_AZTI[CatchCategory=="L"& ReportingCategory=="R" ,
                          list(NumberCaught = sum(NumberCaught)), 
                          by=.(Fleet=Fleet, Season=Season, AgeLength=AgeLength)]
      
      plot_list = list()
      table_list = list()
      
      for (i in unique(tempLandSeason$Fleet)) {
        tempFleet <- subset(tempLandSeason, Fleet==i)
        TempMean <- subset(mSeasonLand, Fleet==i)
        
        LandingLDSeason<-ggplot(tempFleet,aes(x=AgeLength,y=NumberCaught, group = Fleet))+
          geom_line()+
          facet_wrap(~ Fleet +Season, ncol=2)+
          geom_vline(data=TempMean,aes(xintercept=MeanLength, col="red"))+ 
          geom_text(data=TempMean,aes(label = round(MeanLength,2), x=Inf, y=Inf, hjust=1.2, vjust=1.2, col="red"), size=5) +
          labs(x="Length",y="Number_Caught")+
          theme(legend.position="none")
        
        plot_list[[i]] = LandingLDSeason
        
        
        tempLandi<- as.data.frame(cbind(subset(LandRepMetierSeason, rownames(LandRepMetierSeason)==i), Fleet=i, CatchCategory="L"))
        tempTripsi<-subset(NTripsFisOp2, Fleet==i & CatchCategory=="L")
        tempIndivi<-subset(NIndiv2, Fleet==i & CatchCategory=="L")
        temp <- rbind.fill(tempLandi,tempTripsi,tempIndivi)
        temp$variable <- c("Landings", "Trips/FO sampled", "N indiv sampled")
        temp <- temp[,c(6,7,8,1,2,3,4,5)]
        table_list[[i]] <-temp
        
        #windows(9,7)
        #print(LandingLDSeason )
      }
  
      }
       
 
    
      ## Talla media de los discards por metier
      MeanWeightDis<-NA
      DiscardsLDYear<-NA
      DiscardsLDSeason<-NA
      
      if (exists("SD_AZTI")  && "D" %in% unique(SD_AZTI$CatchCategory)){
        SD_AZTI <- as.data.table(SD_AZTI)
        mSeasonDis <-SD_AZTI[CatchCategory=="D"& ReportingCategory=="R" ,
                              list(MeanLength = sum(AgeLength*NumberCaught)/sum(NumberCaught)), 
                              by=.(Fleet=Fleet, Season=Season)]
        SeasonD <- dcast(mSeasonDis, Fleet~ Season, value.var="MeanLength",  sum)
        mTotalD <-SD_AZTI[CatchCategory=="D"& ReportingCategory=="R" ,
                          list(Total = sum(AgeLength*NumberCaught)/sum(NumberCaught)), 
                          by=.(Fleet=Fleet)]      
        MeanWeightDis <- merge(SeasonD,mTotalD, by="Fleet")
      }  

      
      
      ## Crear gráficas Distribucion tallas discards   ####

      
      if (exists("SD_AZTI")  && "D" %in% unique(SD_AZTI$CatchCategory))  {
        
        ## Discards length distribution annual (nº at length)
        SD_AZTI <- as.data.table(SD_AZTI)
        tempDisYear <- SD_AZTI[CatchCategory=="D"& ReportingCategory=="R" ,
                                list(NumberCaught = sum(NumberCaught)), 
                                by=.(Fleet=Fleet, AgeLength=AgeLength)]
        
        DiscardsLDYear<-ggplot(tempDisYear,aes(x=AgeLength,y=NumberCaught, group = Fleet))+
          geom_line()+
          facet_wrap(~ Fleet, scales="free_y", ncol=2)+
          geom_vline(data=MeanWeightDis,aes(xintercept=Total, col="red"))+ 
          geom_text(data=MeanWeightDis,aes(label = round(Total,2),  x=Inf, y=Inf, hjust=1.2, vjust=1.2, col="red"), size=3.5) +
          labs(x="Length",y="Number_Caught")+
          theme(legend.position="none")
        
        # windows(9,7)
        # DiscardsLDYear
        
        ## Discards length distribution per season (nº at length)
        tempDisSeason <- SD_AZTI[CatchCategory=="D"& ReportingCategory=="R" ,
                                  list(NumberCaught = sum(NumberCaught)), 
                                  by=.(Fleet=Fleet, Season=Season, AgeLength=AgeLength)]
        
        plot_list_D = list()
        table_list_D = list()
        
        for (i in unique(tempDisSeason$Fleet)) {
          tempFleet <- subset(tempDisSeason, Fleet==i)
          TempMean <- subset(mSeasonDis, Fleet==i)
          
          DiscardsLDSeason<-ggplot(tempFleet,aes(x=AgeLength,y=NumberCaught, group = Fleet))+
            geom_line()+
            facet_wrap(~ Fleet +Season, ncol=2)+
            geom_vline(data=TempMean,aes(xintercept=MeanLength, col="red"))+ 
            geom_text(data=TempMean,aes(label = round(MeanLength,2), x=Inf, y=Inf, hjust=1.2, vjust=1.2, col="red"), size=5) +
            labs(x="Length",y="Number_Caught")+
            theme(legend.position="none")
          
          plot_list_D[[i]] = DiscardsLDSeason
          
          
          tempDisi<- as.data.frame(cbind(subset(DiscMetierSeason, rownames(DiscMetierSeason)==i), Fleet=i, CatchCategory="D"))
          tempTripsi<-subset(NTripsFisOp2, Fleet==i & CatchCategory=="D")
          tempIndivi<-subset(NIndiv2, Fleet==i & CatchCategory=="D")
          temp <- rbind.fill(tempDisi,tempTripsi,tempIndivi)
          temp$variable <- c("Discards", "Trips/FO sampled", "N indiv sampled")
          temp <- temp[,c(6,7,8,1,2,3,4,5)]
          table_list_D[[i]] <-temp
          
          #windows(9,7)
          #print(DiscardsLDSeason )
        }
        
      }
      
      
