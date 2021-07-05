##--------------
## librerias
##--------------

library(shinythemes)
library(shinyBS)
library(shinyTime)
library(shinyjs)
library(shinyWidgets)
library(epitools) # for date
library(data.table)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(xlsx)
library(rdrop2)
library(googledrive)
library(rsconnect)

## ************************************
## fishermen entries, googledrive
## ************************************

options(
  gargle_oauth_cache = "token",
  gargle_oauth_email = "aztirecrea@gmail.com"
)


##--------------
## Default data.frames
##--------------

per<-data.frame(Edad = NA,
                Genero = NA, 
                Experiencia = NA,
                Avidez = NA)



ret<- data.frame(Latitud = NA,
                 Longitud = NA,
                 Especie = NA, 
                 Tipo = NA,
                 Peso.Total = NA,
                 Numero = NA)



tal<-data.frame (Especie = NA,
                 Tipo = NA, 
                 Talla = NA, 
                 Latitud = NA, 
                 Longitud = NA)

retc<- data.frame(Latitud = NA,
                  Longitud = NA,
                  Especie = NA, 
                  Tipo = NA,
                  Peso.Total = NA,
                  Numero = NA)



talc<-data.frame (Especie = NA,
                  Tipo = NA, 
                  Talla = NA, 
                  Latitud = NA, 
                  Longitud = NA)

##--------------
## species list
##--------------


speciesList <- c("Alfonsinos/reyes (Beryx spp.)",
                 "Aligote (Pagellus acarne)",
                 "Besugo  (Pagellus bogaraveo)",
                 "Bonito Atlantico (Sarda sarda)",
                 "Bonito del norte (Thunnus alalunga)",
                 "Breca (Pagellus erythrinus)",
                 "Cabrilla (Serranus cabrilla)",
                 "Calamares (Loligo spp.)",
                 "Chicharros (Trachurus spp.)",
                 "Chopa (Spondyliosoma cantharus)",
                 "Congrio (Conger conger)",
                 "Denton (Dentex dentex)",
                 "Dorada (Sparus aurata)",
                 "Estornino/Makarel (Scomber colias)",
                 "Faneca (Trisopterus luscus)",
                 "Gallano (Labrus mixtus)",
                 "Herrera (Lithognatus mormyrus)",
                 "Julia (Coris julis)",
                 "Lisa (Chelon labrosus)",
                 "Lubina (Dicentrarchus labrax)",
                 "Maragota  (Labrus bergylta)",
                 "Merluza  (Merluccius merluccius)",
                 "Oblada (Oblada melanura)",
                 "Pulpo comun (Octopus vulgaris)",
                 "Rascacios (Scorpaena spp.)",
                 "Salmonete de roca (Mullus  surmuletus)",
                 "Sargo  (Diplodus sargus)",
                 "Sargo breado (Diplodus cervinus)",
                 "Sargo picudo (Diplodus puntazzo)",
                 "Sepia comun (Sepia officinalis)",
                 "Serranidos (Serranus spp.)",
                 "Tordos (Labrus spp.)",
                 "Verdel (Scomber scombrus)"
)

