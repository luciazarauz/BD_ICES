
#C:\use\0_Lucia\1_Proyectos\AA_SegPes\ICES Data Transmission\2019 DataCall\WGBIE
#setwd("C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\ICES Data Transmission\\2018 DataCall\\data sent\\")
# chequear: suma e productos
# comparar entre años
# ver si tienen unallocated

options(digits=5)


rm (list=ls())
library(dplyr)
library(reshape2)
library (ggplot2)

dir.data18 <- "C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\ICES Data Transmission\\2018 DataCall\\Checks"
dir.data19 <- "C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\ICES Data Transmission\\2019 DataCall\\2_Checks"
dir.data20 <- "C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\ICES Data Transmission\\2020 DataCall\\3_Checks"
#dir.data21 <- "C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\ICES Data Transmission\\2021 DataCall\\3_Checks"
dir.data21 <- "\\\\dok\\nas\\P04\\O-SUK\\GPSDatos\\3 SegPes\\Datos\\Datos Enviados\\2021\\ICES\\"

dir.res <- dir.data21

# setwd(dir.data18 )
# data18 <- read.table("SummaryIC.csv", sep=",", dec=".", header=TRUE, stringsAsFactors = F)

setwd(dir.data19 )
data19 <- read.table("SummaryIC.csv", sep=";", dec=",", header=TRUE, stringsAsFactors = F)
data19 <- subset(data19, select=-c(SI_BR,SI_RR))
  
setwd(dir.data20 )
data20 <- read.table("SummaryIC.csv", sep=",", dec=".", header=TRUE, stringsAsFactors = F)
data20 <- subset(data20, select=-c(SI_BR,SI_RR))

setwd(dir.data21 )
data21 <- read.table("SummaryIC.csv", sep=",", dec=".", header=TRUE, stringsAsFactors = F)

data <- rbind(  data19, data20, data21)
data$Group <- gsub("2020 DC ", "", data$Group )
data$Group <- gsub("2021 DC ", "", data$Group )


# data <- rbind( dataLDB)
# data$Group[data$Group=="2020 WGBIE\\Gallo Boscii Datos históricos"] <- "WGBIE"

data$Year <- paste0 ("y",data$Year)
data$SI_LR <- round(data$SI_LR, digits=3)
data$SI_LN <- round(data$SI_LN, digits=3)
data$SI_DR <- round(data$SI_DR, digits=3)
data$SD_LR <- round(data$SD_LR, digits=3)
data$SD_DR <- round(data$SD_DR, digits=3)

# lineas in especie
data[is.na(data$Species) & data$Year=="y2020",]


# # explorar general ldb
# data %>%  filter(!is.na(SD_LR) & SD_LR>0 & SI_LR!=SD_LR) %>% select(File, Year, Fleet, Species,Season, FishingArea, Species, SI_LR, SD_LR)
# 
# data %>% group_by(Group, Year, Fleet, Species) %>% summarise(SI_LR=sum(SI_LR, na.rm=TRUE)) %>%
#   dcast(Group  + Fleet + Species ~ Year, sum) 
# data %>% group_by(Group, Year, Fleet, Species) %>% summarise(SI_LN=sum(SI_LN, na.rm=TRUE)) %>%
#   dcast(Group  + Fleet + Species ~ Year, sum) 
# data %>% group_by(Group, Year, Fleet, Species) %>% summarise(SI_DR=sum(SI_DR, na.rm=TRUE)) %>%
#   dcast(Group  + Fleet + Species ~ Year, sum) 


#data <- data %>% filter (Group %in% c("WGBIE", "WGCSE", "WGDEEP", "WGCEPH", "WGHANSA", "WGWIDE", "WGEF"))
data <- data %>% filter (Group %in% c("WGCEPH"))

#data <- data %>% filter(Species=="HKE")

# explorar general 
data %>% filter (Year %in% c( "y2020")) %>% group_by(File, Group, Year, Species) %>% summarise(SI_LR=sum(SI_LR, na.rm=TRUE), 
                                                                                         SI_LN=sum(SI_LN, na.rm=TRUE),
                                                                                         SI_DR=sum(SI_DR, na.rm=TRUE)) 



data %>% filter (Year %in% c( "y2020")) %>% group_by(File, Group, Year, Species, Fleet) %>% 
                                            summarise(SI_LR=sum(SI_LR, na.rm=TRUE), 
                                                      SI_LN=sum(SI_LN, na.rm=TRUE),
                                                      SI_DR=sum(SI_DR, na.rm=TRUE),
                                                      SD_LR=sum(SD_LR, na.rm=TRUE),
                                                      SD_DR=sum(SD_DR, na.rm=TRUE)) %>%
                                            data.frame()

table(temp$Fleet)


subset(data, is.na(Species))

# SI landings
data %>% filter (Year %in% c("y2018", "y2019", "y2020")) %>% group_by(Group, Year, Fleet, Species) %>% summarise(SI_LR=sum(SI_LR, na.rm=TRUE)) %>%
  dcast(Group  + Fleet + Species ~ Year, sum)

data %>% filter (Year %in% c("y2018", "y2019", "y2020")) %>% group_by(Group, Year, Fleet, Species) %>% summarise(SI_LR=sum(SI_LR, na.rm=TRUE)) %>%
  dcast(Group  + Fleet + Species ~ Year, sum) %>% filter((abs(y2018-y2019)/(y2018+y2019))>0.3 & (y2019>1 | y2018>1)) 

data  %>% group_by(Group, Year, Fleet, Species) %>% summarise(SI_LR=sum(SI_LR, na.rm=TRUE)) %>%
  dcast(Group  + Fleet + Species ~ Year, sum)

# SI non reported
data %>% filter (Year %in% c("y2018", "y2019", "y2020")) %>% group_by(Group, Year, Fleet, Species) %>% summarise(SI_LN=sum(SI_LN, na.rm=TRUE)) %>%
  dcast(Group  + Fleet + Species ~ Year, sum) 

data %>% filter (Year %in% c("y2018", "y2019", "y2020")) %>% group_by(Group, Year, Fleet, Species) %>% summarise(SI_LN=sum(SI_LN, na.rm=TRUE)) %>%
  dcast(Group  + Fleet + Species ~ Year, sum) %>% filter((abs(y2018-y2019)/(y2018+y2019))>0.3) 

data %>% filter (Year %in% c("y2018", "y2019", "y2020")) %>% group_by(Group, Year, Fleet, Species) %>% summarise(SI_LN=sum(SI_LN, na.rm=TRUE)) %>%
  dcast(Group  + Fleet + Species ~ Year, sum)  %>% filter(y2019>10)


data %>% group_by(Group, File) %>% summarise(SI_LN=sum(SI_LN, na.rm=TRUE))%>% filter(SI_LN==0 & Group!="WGDEEP")

data %>% group_by(Group, Year, Fleet, Species) %>% summarise(SI_LN=sum(SI_LN, na.rm=TRUE)) %>%
  dcast(Group  + Fleet + Species ~ Year, sum) 


# discards
data %>% filter (Year %in% c("y2018", "y2019", "y2020")) %>% group_by(Group, Year, Fleet, Species) %>% summarise(SI_DR=sum(SI_DR, na.rm=TRUE)) %>%
  dcast(Group  + Fleet + Species ~ Year, sum) 

data %>% filter (Year %in% c("y2018", "y2019", "y2020")) %>% group_by(Group, Year, Fleet, Species) %>% summarise(SI_DR=sum(SI_DR, na.rm=TRUE)) %>%
  dcast(Group  + Fleet + Species ~ Year, sum) %>% filter((abs(y2018-y2019)/(y2018+y2019))>0.3 & (y2019>1 | y2018>1)) 

data %>% group_by(Group, Year, Fleet, Species) %>% summarise(SI_DR=sum(SI_DR, na.rm=TRUE)) %>%
  dcast(Group  + Fleet + Species ~ Year, sum) 

data %>% group_by(Group, Year, Fleet, Species) %>% summarise(NSamp_DR=sum(NSamp_DR, na.rm=TRUE)) %>% filter(NSamp_DR==1) %>%
  dcast(Group  + Fleet + Species ~ Year, sum) 


# Cero capturas oficiales pero muestreo de tallas en puerto
filter(data, Year == "y2020" & SI_LR==0 & !is.na(SD_LR) & NInd_LR>0)
filter(data, SI_LR==0 & !is.na(SD_LR) & NInd_LR>0)


# Cero capturas oficiales pero muestreo de tallas en la mar
filter(data, Year == "y2020" & SI_DR==0 & !is.na(SD_DR) & NInd_DR)
filter(data, SI_DR==0 & !is.na(SD_DR) & NInd_DR)



# Sum of products
 data %>%  filter(!is.na(SD_LR) & SD_LR>0 & SI_LR!=SD_LR ) %>% select(File, Year, Fleet, Species,Season, FishingArea, Species, SI_LR, SD_LR) %>%
   mutate(DifSOP=(abs(SI_LR-SD_LR)/(SI_LR))) %>% filter(Year=="y2020" & DifSOP>0.05)

 data %>%  filter(!is.na(SD_DR) & SD_DR>0 & SI_DR!=SD_DR ) %>% select(File, Year, Fleet, Species,Season, FishingArea, Species, SI_DR, SD_DR) %>%
   mutate(DifSOP=(abs(SI_DR-SD_DR)/(SI_DR))) %>% filter(Year=="y2020" & DifSOP>0.05)
 
 
 
#tallas 
 
 data %>% filter (Year %in% c( "y2020")) %>% group_by(File, Group, Year, Species) %>% summarise(SD_DR=sum(SD_DR, na.rm=TRUE), 
                                                                                                SD_LR=sum(SD_LR, na.rm=TRUE)) 
 
 
 

# go to original files
 
 dir.data20.ini <- "\\\\dok\\nas\\P04\\O-SUK\\GPSDatos\\3 SegPes\\Datos\\Datos Enviados\\2020\\DC ICES\\2020 DC WGHANSA"
 setwd(dir.data20.ini)
 

 files <- gsub(pattern = "\\.csv$", "",  intersect(list.files(pattern = "\\.csv$"), list.files(pattern = "2020 DC")))
 files
 # "2020 DC WGHANSA ane.27.8 ES AZTI" "2020 DC WGHANSA ane.27.8 MES"  "2020 DC WGHANSA pil.27.8abd ES AZTI" "2020 DC WGHANSA pil.27.8c9a"
 filename <- "2020 DC WGHANSA ane.27.8 ES AZTI.csv" # 
 data20ini <- read.table(filename, header = FALSE, stringsAsFactors=FALSE, sep = ",", na.strings=c(NA, ""),
                          col.names = paste0("V",seq_len(33)), fill = TRUE)
 
 data20ini<- subset(data20ini, V1=="SD")
 colnames(data20ini)<-c("RecordType", "Country", "Year", "SeasonType", "Season","Fleet", "AreaType", "FishingArea", "DepthRange", "Species", "Stock", "CatchCategory", "ReportingCategory", "Sex", "CANUMtype", "AgeLength", "PlusGroup", "SampledCatch", "NumSamplesLngt", "NumLngtMeas", "NumSamplesAge",  "NumAgeMeas", "unitMeanWeight", "unitCANUM", "UnitAgeOrLength", "UnitMeanLength", "Maturity", "NumberCaught", "MeanWeight", "MeanLength", "varNumLanded", "varWGTLanded", "varLGTLanded")  
 
 # Range of length distributions
  p<- ggplot(data20ini, aes(x=MeanLength))  + geom_histogram(binwidth=1) +
   facet_wrap(Species ~ CatchCategory + Season , nrow=2)
     
  windows(14,9) ;   p

  # Length-weigth relationships
  LWR	<- data.frame(rbind(c(year= 2019,    mod= "Model.S1",      sp= "pil.8abd",   a=	0.010543,		  b= 2.893004),
                          c(year= 2019,    mod= "Model.S2",      sp= "pil.8abd",   a=	0.004268,		  b= 3.208967),

                          c(year= 2019,    mod= "Model.S1",      sp= "ane.8",   a= 0.002676,     b=	3.342114),
                          c(year= 2019,    mod= "Model.S2",      sp= "ane.8",   a= 0.004942,     b=	3.11735),
 
                          c(year= 2019,    mod= "Model.Q1",      sp= "pil.8c9a",   a= 0.001984,     b= 3.473901),
                          c(year= 2019,    mod= "Model.Q2",      sp= "pil.8c9a",   a= 0.001991,     b= 3.488657),
                          c(year= 2019,    mod= "Model.Q3",      sp= "pil.8c9a",   a= 0.005403,     b= 3.18111),
                          c(year= 2019,    mod= "Model.Q4",      sp= "pil.8c9a",   a= 0.002983,     b= 3.401279)))
  LWR$a <- as.numeric(as.character(LWR$a))
  LWR$b <- as.numeric(as.character(LWR$b))
  
  sp <- "ane.8"
  data_S1_L <- subset(data20ini, Season %in% c(1:2) & CatchCategory =="L")
  data_S1_L$MeanWeight_Checks <-  ((LWR$a[LWR$sp==sp & LWR$mod=="Model.S1"] * (data_S1_L$MeanLength+0.25) ^ LWR$b[LWR$sp==sp & LWR$mod=="Model.S1"]))/1000
  subset(data_S1_L , abs(MeanWeight-MeanWeight_Checks)>0.00001)
  
  data_S1_D <- subset(data20ini, Season %in% c(1:2) & CatchCategory =="D")
  data_S1_D$MeanWeight_Checks <-  ((LWR$a[LWR$sp==sp & LWR$mod=="Model.S1"] * (data_S1_D$MeanLength+0.25) ^ LWR$b[LWR$sp==sp & LWR$mod=="Model.S1"]))/1000
  subset(data_S1_D , abs(MeanWeight-MeanWeight_Checks)>0.00001)
  
  data_S2_L <- subset(data20ini, Season %in% c(1:2) & CatchCategory =="L")
  data_S2_L$MeanWeight_Checks <-  ((LWR$a[LWR$sp==sp & LWR$mod=="Model.S1"] * (data_S2_L$MeanLength+0.25) ^ LWR$b[LWR$sp==sp & LWR$mod=="Model.S1"]))/1000
  subset(data_S2_L , abs(MeanWeight-MeanWeight_Checks)>0.00001)
  
  data_S2_D <- subset(data20ini, Season %in% c(1:2) & CatchCategory =="D")
  data_S2_D$MeanWeight_Checks <-  ((LWR$a[LWR$sp==sp & LWR$mod=="Model.S1"] * (data_S2_D$MeanLength+0.25) ^ LWR$b[LWR$sp==sp & LWR$mod=="Model.S1"]))/1000
  subset(data_S2_D , abs(MeanWeight-MeanWeight_Checks)>0.00001)  
  
  
  