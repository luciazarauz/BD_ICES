
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
#library(ggplus)

# cargar datos

options(scipen=999)
load("Depuracion RDB/QA results/RDB2020_NSEA.RData")

ls()


# Seleccionar la especie
    #   "Lophius budegassa", "Engraulis encrasicolus", "Lophius piscatorius",       
    #   "Merluccius merluccius",   "Trachurus trachurus",  "Scomber scombrus"          
    #   "Lepidorhombus whiffiagonis", "Sardina pilchardus"

for(sp_sci in c("Lophius budegassa", "Engraulis encrasicolus", "Lophius piscatorius",
                   "Merluccius merluccius",   "Trachurus trachurus", "Scomber scombrus" ,
                   "Lepidorhombus whiffiagonis", "Sardina pilchardus")){


#sp_sci <- "Sardina pilchardus"
alfa <- especies$ALFA[especies$Nombre.Cientifico==sp_sci]
wormsid <- especies$WORMS[especies$Nombre.Cientifico==sp_sci]

# crear carpetas y nobre de fichero
filename <- paste("Depuracion RDB/QA results/", alfa, "_QA_outliers.csv", sep="")

# subset del fichero cs
cs_stock <- csSubset(cs, spp==wormsid )  
cs_stock@ca <- subset(cs@ca, spp==wormsid & sampType=="V") 
cs_stockM <- csSubset(cs, spp==wormsid & sampType=="M" )  
cs_stockS <- csSubset(cs, spp==wormsid & sampType=="S" )  

csPi_stock <- csSubset(csPi, spp==wormsid )  
csPi_stock@ca <- subset(csPi@ca, spp==wormsid ) 
csPi_stockM <- csSubset(csPi, spp==wormsid & sampType=="M" )  
csPi_stockS <- csSubset(csPi, spp==wormsid & sampType=="S" ) 



# outliers length class ####
############################

png(filename=paste("Depuracion RDB/QA results/",alfa,"_Outliers lenNum.png", sep=""))
#windows()
tablaoutlier<- outliers(csPi_stockM,slot="hl",var="lenNum")
dev.off()

png(filename=paste("Depuracion RDB/QA results/", alfa, "_Outliers lenCls.png", sep=""))
#windows()
tablaoutlier<- outliers(csPi_stockM,slot="hl",var="lenCls")
dev.off()

out <- tablaoutlier[,c("recType","seCode", "year","trpCode", "staNum", "spp","catchCat","lenCls","lenNum")]
out$FAC_EC_lvl6 <- tr$FAC_EC_lvl6[match(out$trpCode, tr$Trip_code)]
out$NombreBuque <- tr$NombreBuque[match(out$trpCode, tr$Trip_code)]
out$Date <- tr$Date[match(out$trpCode, tr$Trip_code)]

write.table("potential outliers length sampling", filename, row.names = FALSE, sep=";")
write.table(out,filename, append=TRUE, row.names = FALSE, sep=";")
write.table("", filename, append=TRUE, row.names = FALSE, sep=";")




# graficos edad / peso por sexo  #####
######################################
unique(cs@ca$area)
ca_stock <- as.data.table(cs_stock@ca)
ca_stock$area[ca_stock$area %in% c("27.8.c.w","27.8.c.e")] <- "27.8.c"
    
          # CASamp <- ca_stock[, list(Nlength=length(lenCls), 
          #                     Nweight=length(indWt[!is.na(indWt)]), 
          #                     NsexFM=length(sex[sex %in% c("F", "M")]),
          #                     Nsex0=length(sex[sex==""]),
          #                     Nage=length(age[!is.na(age)]),
          #                     mat=length(matStage[!is.na(matStage)])), 
          #                    by=list(spp, area)]
          # CASamp$sp_sci <- sp_sci
          # CASamp


# length boxplot per sex and quarter
if (nrow(ca_stock)>0){  
  
  png(filename=paste("Depuracion RDB/QA results/", alfa, "_Length boxplot by sex and quarter.png", sep=""))
    #windows()  
    gg1 <- ggplot(data=ca_stock) + 
            aes(x=sex, y=lenCls) +
            geom_boxplot() + 
            facet_wrap(~quarter, ncol=2) + 
            ggtitle(paste(sp_sci, "- lenCls"))+  theme(plot.title = element_text(hjust = 0.5))
    print(gg1)
  dev.off()

  
# length weight relationship #####
###################################
#ca_stock <- data.frame(ca_stock)
  
  
    
ca_stock$sex[ca_stock$sex==""]<- "0"
colarea <- c("27.8.c" = "red","27.8.a" = "blue","27.8.b" = "cyan", "27.8.d" = "green3","27.6.a" = "magenta")
colsex <- c("0" = "green3","F" = "blue","M" = "red")


# por sex
png(filename=paste("Depuracion RDB/QA results/", alfa, "_Length weight by sex.png", sep=""))
  #windows()
  gg1 <-  ggplot(data=ca_stock, aes(x=lenCls, y=indWt, color=sex)) + 
            geom_point( size=3) + 
            scale_color_manual(values=colsex)+
            ggtitle(paste("Length-Weight -", alfa)) +  theme(plot.title = element_text(hjust = 0.5))
  print(gg1)
  dev.off()

  
#por area
  png(filename=paste("Depuracion RDB/QA results/", alfa, "_Length weight by area.png", sep=""))
  #windows()   
   gg1 <- ggplot(data=ca_stock, aes(x=lenCls, y=indWt, color=area)) + 
            geom_point(size=3) + 
            scale_color_manual(values=colarea)+
            ggtitle(paste("Length-Weight -", alfa)) +  theme(plot.title = element_text(hjust = 0.5))
   print(gg1)
  dev.off()
 
  
#por quarter y sex
  png(filename=paste("Depuracion RDB/QA results/", alfa, "_Length weight by sex and quarter.png", sep=""))
  #windows()
   gg1 <- ggplot(data=ca_stock, aes(x=lenCls, y=indWt, color=sex)) + 
            facet_wrap(~quarter, ncol=2) +
            geom_point(shape=1) + 
            scale_color_manual(values=colsex)+
            ggtitle(paste("Length-Weight -", alfa)) +  theme(plot.title = element_text(hjust = 0.5))
   print(gg1)
  dev.off() 

  
#por quarter y ?rea
  png(filename=paste("Depuracion RDB/QA results/", alfa, "_Length weight by area and quarter.png", sep=""))
  #windows()   
   gg1 <- ggplot(data=ca_stock, aes(x=lenCls, y=indWt, color=area)) + 
            facet_wrap(~quarter, ncol=2) +
            geom_point(shape=1) + 
            scale_color_manual(values=colarea)+
            ggtitle(paste("Length-Weight -", alfa)) +  theme(plot.title = element_text(hjust = 0.5))
   print(gg1)
  dev.off()


#plot general. identificar puntos
# windows()
#   plot(x=ca_stock$lenCls, y=ca_stock$indWt, main=paste(sp_sci, "- Length Weigth"))
#   points(x=ca_stock$lenCls, y= ca_stock$indWtTeoric, col="red")
#   out_temp <- identify(x=ca_stock$lenCls, y= ca_stock$indWt, labels=ca_stock$trpCode, plot=TRUE)
#   
# dev.copy(png,paste("Depuracion RDB/QA results/Outliers length weight_", alfa, ".png", sep=""))
# dev.off() 

# out_lenwt <- ca_stock[out_temp,c("trpCode", "area", "quarter", "month", "fishId", "sex", "lenCls","indWt","age"), with=FALSE]
# out_lenwt$NombreBuque <- tr$NombreBuque[match(out_lenwt$trpCode, tr$Trip_code)]
# out_lenwt$sp_sci <- sp_sci
# 
# write.table("Potential outlier length weight relationship", filename,  append=TRUE, row.names = FALSE, sep=";")
# write.table(out_lenwt,filename, append=TRUE, row.names = FALSE, sep=";")
# write.table("", filename, append=TRUE, row.names = FALSE, sep=";")

  }

}
