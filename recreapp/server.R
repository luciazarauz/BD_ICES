## ---------------------------------------------------- ##
## IM21SEGPRECREA                                       ##
## Diario de pesca recreativa - Shiny                   ##
## JUn.2021                                             ##
## AZTI (@mkorta)                                       ##
## UPDATES                                              ##
## ---------------------------------------------------- ##

shinyServer(function(input, output, session) {
   
   for (file in list.files("server")) {
      source(file.path("server", file), local = TRUE, encoding = 'UTF-8')$value
   }
   
}

)
