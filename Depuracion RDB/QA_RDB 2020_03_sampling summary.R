
# R version 3.3.0 (2016-05-03)


## Leer RData

rm(list=ls())
library(ggplot2)
library(COSTcore)
#library(COSTeda)
library(data.table)
library(fishPifct)
Fun_CountUnique <- function (x) { length(unique(x))}
#devtools::install_github("guiastrennec/ggplus")



# cargar datos
data.path <- "C:/use/0_Lucia/1_Proyectos/AA_SegPes/RDB/Data 2018" 
res.path <- "C:/use/0_Lucia/1_Proyectos/AA_SegPes/RDB/Data 2018/QA results" 

setwd(data.path)
options(scipen=999)
load("RDB2018.RData")
ls()

## sampling summary ####
########################

hh<- data.table(hh)
sl<- data.table(sl)
hl<- data.table(hl)
ca<- data.table(ca)

hl_sum<- hl
hl_sum$Month <- month(hl_sum$Date)
hl_sum$Quarter[hl_sum$Month %in% 1:3] <- 1 
hl_sum$Quarter[hl_sum$Month %in% 4:6] <- 2 
hl_sum$Quarter[hl_sum$Month %in% 7:9] <- 3 
hl_sum$Quarter[hl_sum$Month %in% 10:12] <- 4 

# mareas muestreadas
SampTrips <- hl_sum[, list(Ntrips=length(unique(Trip_code))), by=list(Sampling_type, Catch_category, FAC_EC_lvl6, Area2)]
SampTrips <- dcast(SampTrips, FAC_EC_lvl6 + Area2 ~ Sampling_type + Catch_category, value.var= "Ntrips" )

# distribuciones talla per quarter
LSamp <- hl_sum[ ,list(Ntrips=Fun_CountUnique(Trip_code), Nind=sum(Number_at_length),  Lmin=min(Length_class),Lmax=max(Length_class), 
                       Lmean=round(mean(Length_class), 0), Lsd=round(sd(Length_class), 0)), 
                  by=list(SpeciesName, Sampling_type, Catch_category)]
LSamp <-LSamp[order(Sampling_type, Catch_category, Nind, decreasing=T),]


# comprobar posibles outliers
LSamp$testmax <- LSamp$Lmax/LSamp$Lmean
LSamp[LSamp$testmax>2.5 ,]

LSamp$testmin <- LSamp$Lmin/LSamp$Lmean
LSamp[LSamp$testmin<0.3,]


xsp <- "Bonito Atlantico"
xlen <- 200
hl[hl$SpeciesName==xsp & hl$Length_class>xlen , c("Sampling_type", "SpeciesName", "Length_class", "Number_at_length", "NombreBuque", "Date")]
hl[hl$SpeciesName==xsp & hl$Length_class<xlen , c("Sampling_type", "SpeciesName", "Length_class", "Number_at_length", "NombreBuque", "Date")]

##


# distribuciones talla per quarter
LSamp2 <- hl_sum[Sampling_type=="M",list(Ntrips=Fun_CountUnique(Trip_code), Nind=sum(Number_at_length)), 
                 by=list( SpeciesName, Sampling_type, Catch_category, FAC_EC_lvl6, Quarter)]
LSamp2 <-LSamp2[order(Sampling_type, Catch_category, FAC_EC_lvl6, Quarter, decreasing=F),]

# muestreo biologico
CASamp <- ca[, list(Nlength=length(Length_class), 
                    Nweight=length(Weight[!is.na(Weight)]), 
                    NsexFM=length(Sex[Sex%in%c("F", "M")]),
                    Nsex0=length(Sex[Sex==""]),
                    Nage=length(Age[!is.na(Age)]),
                    mat=length(Maturity_stage[!is.na(Maturity_stage)])), 
             by=list(SpeciesName, Area)]
CASamp <- CASamp[order(SpeciesName, Area),]

  
# guardar tablas

setwd(res.path)

write.table("Mareas muestreadas", "0_samp_summary.csv",  row.names = FALSE, sep=",")
write.table(SampTrips, "0_samp_summary.csv", append=TRUE, row.names = FALSE, sep=",")
write.table("", "0_samp_summary.csv", append=TRUE, row.names = FALSE, sep=",")

write.table("muestreo por especie", "0_samp_summary.csv", append=TRUE, row.names = FALSE, sep=",")
write.table(LSamp, "0_samp_summary.csv", append=TRUE, row.names = FALSE, sep=",")
write.table("", "0_samp_summary.csv", append=TRUE, row.names = FALSE, sep=",")

write.table("muestreo por especie y trimestre", "0_samp_summary.csv", append=TRUE, row.names = FALSE, sep=",")
write.table(LSamp2, "0_samp_summary.csv", append=TRUE, row.names = FALSE, sep=",")
write.table("", "0_samp_summary.csv", append=TRUE, row.names = FALSE, sep=",")

write.table("Muestreo Biológico", "0_samp_summary.csv", append=TRUE, row.names = FALSE, sep=",")
write.table(CASamp, "0_samp_summary.csv", append=TRUE, row.names = FALSE, sep=",")
write.table("", "0_samp_summary.csv", append=TRUE, row.names = FALSE, sep=",")




