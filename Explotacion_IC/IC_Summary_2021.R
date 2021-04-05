
################################################
##      RESUMEN DE LO ENVIADO A IC
################################################
################################################
## R version 3.3.0 (2016-05-03)

rm (list=ls())
library(data.table)
options(digits=5)

 #dir.data <- "C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\ICES Data Transmission\\2021 DataCall\\3_Checks\\"
 #dir.data <- "C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\ICES Data Transmission\\2019 DataCall\\"
 dir.data <- "\\\\dok\\nas\\P04\\O-SUK\\GPSDatos\\3 SegPes\\Datos\\Datos Enviados\\2021\\ICES\\"
 
 
folder <- c("2021 DC WGCEPH", "2021 DC WGDEEP", "2021 DC WGBIE", "2021 DC WGCSE")
# folder <- c("2020 WGBIE\\Gallo Boscii Datos históricos")
# folder <- c( "2020 DC WGDEEP","2020 DC WGBIE", "2020 DC AFWG", "2020 DC WGCSE", "2020 DC WGCEPH", "2020 DC WGHANSA", 
#              "2020 DC WGWIDE", "2020 DC WGEF", "2020 DC WKWEST", "2020 DC WKCOLIAS")  


Sum_IC <- NULL

# Leer la carpeta
for (j in folder) {       
  dir.files <- paste(dir.data, j, sep="")
  setwd(dir.files)
  
  #files <- gsub(pattern = "\\.csv$", "",  list.files(pattern = "\\.csv$"))
  files <- gsub(pattern = "\\.csv$", "",  intersect(list.files(pattern = "\\.csv$"), list.files(pattern = "2021 DC")))
 
                     
  # Leer los ficheros
  for (z in 1:length(files)) {

  # azti<- "2018 DC WGBIE ldb.27.8c9a AZTI L tallasL"
    azti<- files[z]
    
    x <- read.table(paste(azti, ".csv", sep=""), header = FALSE, stringsAsFactors=FALSE, sep = ",", na.strings=c(NA, ""),
                    col.names = paste0("V",seq_len(33)), fill = TRUE)
  
   
    # Crear ficheros
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
    
    
    head(HI_AZTI)
    HI_AZTI <- as.data.table(HI_AZTI)
    HI_temp <-HI_AZTI[ , list(Group= j, File=azti, HI = sum(Effort)), 
                         by=.(Year=Year, SeasonType=SeasonType, Season=Season, Fleet=Fleet, 
                              FishingArea=FishingArea)]
    setkey(HI_temp,  Group, File,Year, SeasonType, Season, Fleet, FishingArea)
    
    
    
    SI_AZTI <- as.data.table(SI_AZTI)
    SI_AZTI$CATON[SI_AZTI$CatchCategory=="R"] <- SI_AZTI$OffLandings[SI_AZTI$CatchCategory=="R"]
    SI_temp0 <-SI_AZTI[ , list( Group= j, File=azti,  SI = sum(CATON)), 
                       by=.(Year=Year, SeasonType=SeasonType, Season=Season, Fleet=Fleet, 
                            FishingArea=FishingArea, Species=Species, Category=paste("SI_", CatchCategory,ReportingCategory, sep=""))] 
    
    SI_temp <- dcast.data.table(SI_temp0,Group + File + Year + SeasonType + Season + Fleet + FishingArea + Species   ~ Category,
                     value.var="SI",  sum, fill=NA)
    setkey(SI_temp,   Group, File,Year, SeasonType, Season, Fleet, FishingArea)
    
    
    
    if (exists("SD_AZTI"))  {
        SD_AZTI <- as.data.table(SD_AZTI)
        SD_temp0 <-SD_AZTI[ , list( Group= j, File=azti,  NSamp = unique(NumSamplesLngt), NInd = unique(NumLngtMeas), SD=sum(NumberCaught*MeanWeight)), 
                        by=.(Year=Year, SeasonType=SeasonType, Season=Season, Fleet=Fleet, 
                             FishingArea=FishingArea, 
                             Category=paste( CatchCategory,ReportingCategory, sep=""))] 
        SD_temp <- dcast.data.table(SD_temp0,Group + File + Year + SeasonType + Season + Fleet + FishingArea   ~ Category,
                     value.var=c("NSamp","NInd", "SD"),  sum, fill=NA)
        setkey(SD_temp,  Group, File, Year, SeasonType, Season, Fleet, FishingArea)
    }
    
        
  

    
    Sum_IC_1 <- merge(HI_temp,SI_temp, all=TRUE)
    
    if (exists("SD_AZTI")) {
    Sum_IC_1<- merge(Sum_IC_1,SD_temp, all=TRUE)}
    
    
    Sum_IC <- rbindlist(list(Sum_IC,Sum_IC_1), fill=TRUE)
    
  }


  }


if (!"SI_LN" %in% names(Sum_IC)){
  Sum_IC$SI_LN <- NA
}


  col<-c("Group",	"File",	"Year",	"SeasonType",	"Season",	"Fleet",	"FishingArea","Species",
        "HI",	
        "SI_LR", "SI_LN", "SI_DR", 
        "NSamp_LR", "NInd_LR", "SD_LR", "NSamp_DR",  "NInd_DR",  "SD_DR"  )



setcolorder(Sum_IC, col)



setwd(dir.data)
write.table(Sum_IC, "SummaryIC.csv", sep=",", dec=".", row.names = FALSE)
#write.table(Sum_IC, "SummaryIC.csv", sep=";", dec=",", row.names = FALSE)



