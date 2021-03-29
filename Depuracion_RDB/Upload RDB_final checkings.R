
# R version 3.4.4 (2016-05-03)
# 


rm(list=ls())
Fun_CountUnique <- function (x) { length(unique(x))}


# Instalar librerias ####
#########################

 library(ggplot2)
 library(data.table)
 library(fishPiCodes)
 data("ASFIS_WoRMS")
 library(plyr)
 library(lubridate)
 
# Set path ####
####################

 codes.path <- "C:/use/0_Lucia/1_Proyectos/AA_SegPes/RDB/Data 2017/codes"  
 data.path <- "C:/use/0_Lucia/1_Proyectos/AA_SegPes/RDB/Data 2017"  
 res.path <- "C:/use/0_Lucia/1_Proyectos/AA_SegPes/RDB/Data 2017/QA results/" 
 

# Read Data
########################################
setwd(data.path)

# if separate CS files 
tr <- read.table("TR.csv", sep=",", header=TRUE, stringsAsFactors =FALSE, strip.white=TRUE)
hh <- read.table("HH.csv", sep=",", header=TRUE, stringsAsFactors =FALSE, strip.white=TRUE)
sl <- read.table("SL.csv", sep=",", header=TRUE, stringsAsFactors =FALSE, strip.white=TRUE)
hl <- read.table("HL.csv", sep=",", header=TRUE, stringsAsFactors =FALSE, strip.white=TRUE)

# Change areas
########################################
# 27.8.c.e  &  27.8.c.w
hh[hh$Area=="27.8.c.e"] <- "27.8.c"
hh[hh$Area=="27.8.c.w"] <- "27.8.c"

# 27.8.c.e  &  27.8.c.w
hh[hh$Area=="27.8.d.2"] <- "27.8.d"

# Remove trips
########################################
# number of fish sampled is too high (>1000). 
# becasue of many small pelagic coming into the net

tr <- subset(tr, Trip_code!="S536452")
hh <- subset(hh, Trip_code!="S536452")
sl <- subset(sl, Trip_code!="S536452")
hl <- subset(hl, Trip_code!="S536452")



table(hh$Trip_code[hh$Sampling_type=="S" & hh$Area=="27.6.a"])


# Create RDB file
########################################

# no he conseguido juntar los tres ficheros en uno solo y ordenarlo como debe ordenarse


