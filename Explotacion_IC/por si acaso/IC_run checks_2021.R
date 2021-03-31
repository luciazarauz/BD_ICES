### CREA PDF CON LA INFORMACION DE IC
#####################################
## R version 3.3.0 (2016-05-03


#setwd("C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\ICES Data Transmission\\2017 DataCall\\data sent\\")
rm (list=ls())
library(data.table)

library(dplyr)

folder <- c("2020 DC WKCOLIAS")
#folder <- c("WGDEEP","WGNSSK", "WGBIE","WGCSE","WGHANSA")
#dir.data <- "C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\ICES Data Transmission\\2019 DataCall\\"
dir.data <- "\\\\dok\\NAS\\P04\\O-SUK\\GPSDatos\\3 SegPes\\Datos\\Datos Enviados\\2020\\DC ICES\\"

dir.res <- "C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\ICES Data Transmission\\2020 DataCall\\3_Checks"



# Leer la carpeta
for (j in folder) {       
  dir.files <- paste(dir.data, j, sep="")
  setwd(dir.files)
  
  files <- gsub(pattern = "\\.csv$", "",  intersect(list.files(pattern = "\\.csv$"), list.files(pattern = "2020 DC")))
  #files <- gsub(pattern = "\\.csv$", "",  list.files(pattern = "\\.csv$"))
  #files <- c("2018 DC WGCSE lez.27.4a6a meg AZTI L tallasL")

  
# files <- dir("C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\ICES Data Transmission\\2018 DataCall\\WGBIE") 
# files <- gsub(pattern = "\\.csv$", "", files)


    
    for (z in 1:length(files)) {
    
    # setwd("C:\\use\\0_Lucia\\1_Proyectos\\AA_SegPes\\ICES Data Transmission\\2018 DataCall\\WGBIE\\")
      

      setwd(dir.files)
      azti<- files[z]

    
    x <- read.table(paste(azti, ".csv", sep=""), header = FALSE, stringsAsFactors=FALSE, sep = ",", col.names = paste0("V",seq_len(33)), fill = TRUE)
    
    setwd(dir.res)
    source("IC_checks_2020.R")
    
    
    library(rmarkdown)
    library(knitr)
    render('IC_checks_2020.Rmd',
           output_file = paste(azti, '.pdf', sep=''))
    
    }
}

