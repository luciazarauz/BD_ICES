# ----------------------------------------------------------------- #
# Readme:                                                           #
# ----------------------------------------------------------------- # 
# En este script: comparamos los datos con las Nv de Hazi
#
# INDICE:
#
# Functions
# Libraries.
# Cargar la tabla maestra (dori).
# Correcciones
#
# en el fondo hacemso lo mismo que en Comprobar_Corregir_explorar, pero sin tantas comprobaciones. 
# asumiendo que ya sabemos lo que estamos haciendo
#
# ----------------------------------------------------------------- # 
# R version 3.6.0 (2019-04-26) -- "Planting of a Tree"
# ----------------------------------------------------------------- # 


# # ################## #
# # Libraries          #
# # ################## #

rm(list=ls())

library(fishPiCodes)
data("UNLOCODE")
library(lubridate)
library(data.table)
library(reshape2)
library(dplyr)
#library(plyr)
library(scales)

Fun_unique <- function(x){length(unique(x))}


# # ####################################### #
# # Load                                 ####
# # ####################################### #

load(file="Datos/Dori2019_v2.Rdata"   )
load(file="Datos/Dori2019_v3.Rdata"   )

