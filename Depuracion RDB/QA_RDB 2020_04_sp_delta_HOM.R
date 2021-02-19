
# R version 3.3.0 (2016-05-03)


## Leer RData

rm(list=ls())
library(ggplot2)
library(COSTcore)
library(COSTeda)
library(data.table)
library(fishPifct)
Fun_CountUnique <- function (x) { length(unique(x))}
#devtools::install_github("guiastrennec/ggplus")
library(ggplus)
library(stringr)

# cargar datos
data.path <- "C:/use/0_Lucia/1_Proyectos/AA_SegPes/RDB/Data 2018/" 
res.path <- "C:/use/0_Lucia/1_Proyectos/AA_SegPes/RDB/Data 2018/QA results/" 

setwd(data.path)
options(scipen=999)
load("RDB2018.RData")
ls()


# Seleccionar la especie
#   "Lophius budegassa", "Engraulis encrasicolus", "Lophius piscatorius",       
#   "Merluccius merluccius",   "Trachurus trachurus",  "Scomber scombrus"          
#   "Lepidorhombus whiffiagonis", "Sardina pilchardus"

  
  sp_sci <- "Trachurus trachurus"
  alfa <- speciesAZTI$Cod..ALFA.3[speciesAZTI$Nombre.Cientifico==sp_sci]
  wormsid <- speciesAZTI$WORMS[speciesAZTI$Nombre.Cientifico==sp_sci]
  lenwt_sp <- subset(lenwt, Species==sp_sci)
  
  # crear carpetas y nobre de fichero
  res.sp.path<- paste(res.path, sp_sci, sep="")
  dir.create(res.sp.path)
  setwd(res.sp.path)
  
  filename <- paste("0_QA_",alfa,"_deltaplot.csv", sep="")
  write.table("DeltaPlot Analisis", filename,  row.names = FALSE, sep=",")
  write.table("", filename, append=TRUE, row.names = FALSE, sep=",")
  

  
  # subset del fichero cs
  cs_stock <- csSubset(cs, spp==wormsid )  
  cs_stock@ca <- subset(cs@ca, spp==wormsid & sampType=="V") 
  cs_stockM <- csSubset(cs, spp==wormsid & sampType=="M" )  
  cs_stockS <- csSubset(cs, spp==wormsid & sampType=="S" )  
  
  csPi_stock <- csSubset(csPi, spp==wormsid )  
  csPi_stock@ca <- subset(csPi@ca, spp==wormsid ) 
  csPi_stockM <- csSubset(csPi, spp==wormsid & sampType=="M" )  
  csPi_stockS <- csSubset(csPi, spp==wormsid & sampType=="S" ) 
  
  



# delta plots  Port Sampling  - subset por metier #####
#######################################################

#Crear delta plot
#seleccionamos un metier

  sort(unique(cs_stockM@hh$foCatEu6))
  
for(metier in c("PS_SPF_0_0_0", "PTB_MPD_>=55_0_0", "OTB_DEF_>=70_0_0" ,"GNS_DEF_60-79_0_0" ,"LHM_SPF_0_0_0" ))  {
  
  #metier <- c("PS_SPF_0_0_0" )
  #metier <- c("PTB_MPD_>=55_0_0" )
  #metier <- c("OTB_DEF_>=70_0_0" )
  #metier <- c("GNS_DEF_60-79_0_0")
  #metier <- c("LHM_SPF_0_0_0" )

  metiername <- substr(metier,1,7) 
  
  cs_stockM_metier <- csSubset(cs_stockM, foCatEu6%in%metier)
    strD <- strIni(timeStrata="quarter", techStrata = "foCatEu6")
    deltas <- deltCalc(cs_stockM_metier, strD, species=wormsid, fraction="LAN",strategy="metier")
    
    windows(10,7)    
      delta.out <- plot(deltas,  strat1 = "timeStrata",strat2 = "techStrata", selection = TRUE)
    dev.copy(png,paste("Distr Tallas DeltaPlot_", alfa, paste(metiername,collapse=" & "), ".png", sep=""))
    dev.off() 
    
    #windows()
    png(filename=paste("Distr Tallas DeltaPlot selected_", alfa, paste(metiername,collapse=" & "), ".png", sep=""))
      plot(delta.out)
    dev.off()  
  
  
  # identificar y guardar mareas outlier
        deltatrips <- delta.out@outPut$sampId
        deltatrips$SpeciesSciName <- sp_sci
        deltatrips$NombreBuque <- tr$NombreBuque[match(deltatrips$trpCode, tr$Trip_code)]
        deltatrips$Date <- tr$Date[match(deltatrips$trpCode, tr$Trip_code)]
        deltatrips$FAC_EC_lvl6 <- tr$FAC_EC_lvl6[match(deltatrips$trpCode, tr$Trip_code)]
        deltatrips
        
        alltrips <- deltas@outPut$DFsamp
        alltrips$SpeciesSciName <- sp_sci
        alltrips$NombreBuque <- tr$NombreBuque[match(alltrips$trpCode, tr$Trip_code)]
        alltrips$Date <- tr$Date[match(alltrips$trpCode, tr$Trip_code)]
        alltrips$FAC_EC_lvl6 <- tr$FAC_EC_lvl6[match(alltrips$trpCode, tr$Trip_code)]
        alltrips    
        
        
        write.table("anomalous delta distributions", filename,  append=TRUE, row.names = FALSE, sep=",")
        write.table(deltatrips,filename, append=TRUE, row.names = FALSE, sep=",")
        write.table("", filename, append=TRUE, row.names = FALSE, sep=",")
        
        write.table("all delta distributions", filename, append=TRUE,  row.names = FALSE, sep=",")
        write.table(alltrips,filename, append=TRUE, row.names = FALSE, sep=",")
        write.table("", filename, append=TRUE, row.names = FALSE, sep=",")
        
           
#dibujar distribuciones de talla
  # windows()
  # lenDisPlot(cs_stockM, species=wormsid, fraction="LAN", level="trip",
  #            trpCode=as.character(deltatrips$trpCode))
  # windows()
  # lenDisPlot(cs_stockM, species=wormsid, fraction="LAN", level="trip",
  #            trpCode=as.character(deltas@outPut$DFsamp$trpCode))


##distribuciones de tallas - delta
       
  d <- deltas@outPut$tab
  d$Length <- as.numeric(as.character(d$Length))
  d$trpCode <- deltas@outPut$DFsamp$trpCode[match(d$Unite,deltas@outPut$DFsamp$SampNum)]
  d$FAC_EC_lvl6 <- tr$FAC_EC_lvl6[match(d$trpCode, tr$Trip_code)]

  nmax<-length(unique(d$trpCode))
  nmax
  
  nt<- seq(1,450, by=50)
  nt<- nt[nt<length(unique(d$trpCode))]
  for(i in(nt)){
    nplot<- ifelse(nmax<i+49,nmax, i+49 )
    page <- which(nt %in% i)
    subd<-subset(d,trpCode %in% unique(d$trpCode)[i:nplot])
  
        gg1 <- ggplot(data=subd) +
        geom_rect(data = subset(subd,trpCode %in% deltatrips$trpCode), aes(fill = FAC_EC_lvl6),xmin = -Inf,xmax = Inf,
                  ymin = -Inf,ymax = Inf,alpha = 0.3) +
        geom_bar(mapping=aes(x=Length, y=Number), stat="identity") + 
        #facet_wrap(~ trpCode, ncol=7, nrow=5, scales="free_y") +
        facet_wrap(~ trpCode, ncol=10, nrow=6) +
        theme(legend.position="none") +
        ggtitle(paste("Distr Tallas (deltaPlot) -", alfa, paste(metier,collapse=" & "), "- p",page)) +  theme(plot.title = element_text(hjust = 0.5))
        
        #windows(13,8)
        png(filename=paste(paste("Distr Tallas (deltaPlot) -", alfa, paste(metiername,collapse=" & "), "- p",page), ".png", sep=""),
            width = 1500, height = 900)
        print(gg1)
        dev.off()
  }
  


  ##distribuciones de tallas - lendisplot
  
  samplenDistr <- csAggregate(csObj=csPi_stockM, x=list(lenNum="lenNum"), by=list(lenCls="lenCls", foCatEu6="foCatEu6", trpCode = "trpCode"), sum, na.rm=TRUE)
  d<- subset(samplenDistr, foCatEu6 %in% metier)

  nmax<-length(unique(d$trpCode))
  nmax
  
  nt<- seq(1,450, by=50)
  nt<- nt[nt<length(unique(d$trpCode))]
  for(i in(nt)){
    nplot<- ifelse(nmax<i+49,nmax, i+49 )
    page <- which(nt %in% i)
    subd<-subset(d,trpCode %in% unique(d$trpCode)[i:nplot])
    
    gg1 <- ggplot(data=subd) +
      geom_rect(data = subset(subd,trpCode %in% deltatrips$trpCode), aes(fill = foCatEu6),xmin = -Inf,xmax = Inf,
                ymin = -Inf,ymax = Inf,alpha = 0.3) +
      geom_bar(mapping=aes(x=lenCls, y=lenNum), stat="identity") + 
      #facet_wrap(~ trpCode, ncol=7, nrow=5, scales="free_y") +
      facet_wrap(~ trpCode, ncol=10, nrow=5) +
      theme(legend.position="none") +
      ggtitle(paste("Distr Tallas (deltaPlot) -", alfa, paste(metier,collapse=" & "), "- p",page)) +  theme(plot.title = element_text(hjust = 0.5))
    
    #windows(13,8)
    png(filename=paste(paste("Distr Tallas (lendisPlot) -", alfa, paste(metiername,collapse=" & "), "- p",page), ".png", sep=""),
        width = 1500, height = 900)
    print(gg1)
    dev.off()
    
  }  
  
}  

  
  