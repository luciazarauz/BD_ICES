# ----------------------------------------------------------------- #
# Readme:                                                           #
# ----------------------------------------------------------------- # 
# En este script combinamos la tabla de InfoVentas con InfoDiarios y XXX mediante sus id correspondientes.
#
# INDICE:
#
# Functions
# Libraries.
# Load data from depuracion.
# Link tables
#
# ----------------------------------------------------------------- # 
# R version 3.6.0 (2019-04-26) -- "Planting of a Tree"
# ----------------------------------------------------------------- # 

InfoVentas_All <- read.table(file="Importacion_InfoBase/InfoVentas_All.csv", sep=";", dec=",", row.names = FALSE)


head(InfoVentas_All)


write.table(InfoVentas_All[1:1000,], file="Importacion_InfoBase/InfoVentas_All_subset2.csv", sep=";", dec=",", row.names = FALSE)

