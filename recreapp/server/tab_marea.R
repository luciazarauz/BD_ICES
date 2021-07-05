# ----------------------------------- #
# tab Marea                           #
# ----------------------------------- #

# -------------- #
# Map            #
# -------------- #

output$map <- renderLeaflet({
  input$new.button   
  leaflet() %>% addTiles() %>% 
    addMouseCoordinates() %>% 
    addScaleBar ("bottomleft") %>% 
    setView(lng = -2.5,lat =  43.5, zoom = 9)    
})


# ISAAC> se haga un reset de la longitud y la latitud al clicar en nueva marea
observeEvent(input$new.button,{
  data$clickMarker$lng <- -2.5
  data$clickMarker$lat <- 43.5
})



# -------------------------- #
# Reactive data lat long     #
# -------------------------- #

#Reactive data to click on map   
data <- reactiveValues(clickMarker=NULL)

observeEvent(input$map_click,
             {data$clickMarker <- input$map_click})


#Reactive text to click o map
output$latA<- renderPrint({
  data$clickMarker$lat
})

#Reactive text to click o map
output$lonA <- renderText({
  data$clickMarker$lng
})

#Reactive marker to click on map
observeEvent(input$map_click,{
  leafletProxy('map')%>% 
    clearMarkers()%>% 
    addMarkers(lng = data$clickMarker$lng, lat = data$clickMarker$lat)
  #leafletProxy('map')%>% addMarkers(lng = data$clickMarker$lng, lat = data$clickMarker$lat) 
})



# --------------------- #
#  Tabla de marea       #
# --------------------- #

datM <- data.table("Parametros" = c("<strong>Fecha</strong>",
                                    "<strong>Hora Inicio</strong>", 
                                    "<strong>Hora Fin</strong>", 
                                    "<strong>Modalidad</strong>", 
                                    "<strong>Técnica de pesca</strong>",
                                    "<strong>Tipo de barco</strong>"))


# Entradas del usuario

datM_val <- reactive({
  #actual reactive
  if (input$moda == "Tierra"){
    datM[,"Datos" := c(as.character(input$fech),
                       strftime(input$horaI, "%R"),
                       strftime(input$horaF, "%R"), 
                       as.character(input$moda),
                       as.character (input$moda4),
                       as.character (input$moda3))]
  }else{
    datM[,"Datos" := c(as.character(input$fech),
                       strftime(input$horaI, "%R"),
                       strftime(input$horaF, "%R"), 
                       as.character(input$moda),
                       as.character (input$moda2),
                       as.character (input$moda3))]
  }
})


# Tabla salida de Marea

output$tableM <- renderTable({
  req(input$moda)
  if (input$moda == "Submarina"){
    datM_val()[1:4]
  }
  else{
    if (input$moda == "Tierra"){
      datM_val()[1:5]}
    else{
      datM_val()  
    }
  }}, striped = TRUE, 
  bordered = TRUE,  
  hover = TRUE, 
  colnames = FALSE, 
  sanitize.text.function=function(x){x})



# ------------------- #
#  Tabla de captura   #
# ------------------- #

values <- reactiveValues()
values$df <- ret


# Añade evento

observeEvent(input$add.button,{
  cat("addEntry\n")
  print(input$lat)
  print(input$lon)
  #print(lat())
  #print(lon())
  print(input$espeC)
  print(input$pesoC)
  print(input$nindC)
  #newRow <- data.frame(input$lat, input$lon, input$espeC, input$tipoC, input$pesoC, input$nindC)
  if(input$zona =="manu")
    newRow <- data.frame(input$lat, input$lon, input$espeC, input$tipoC, input$pesoC, input$nindC)
  #}else{
  if(input$zona =="auto")
    newRow <- data.frame(data$clickMarker$lat, data$clickMarker$lng, input$espeC, input$tipoC, input$pesoC, input$nindC)
  #}
  #return(newRow)
  colnames(newRow)<-colnames(values$df)
  values$df <- rbind(values$df,newRow)
  values$df <- na.omit(values$df)
  print(nrow(values$df))
})


# Elimina evento

observeEvent(input$delete.button,{
  cat("deleteEntry\n")
  if(is.na(input$row.selection)){
    values$df <- values$df[-nrow(values$df), ]
  } else {
    values$df <- values$df[-input$row.selection, ]
  }
})  


# Tabla salida

output$tableC <- renderTable({
  req(input$pesoC|input$nindC)
  #req(input$moda)
  values$df }, striped = TRUE, bordered = TRUE,  hover = TRUE)



# ------------------ #
# Tabla de tallas    # 
# ------------------ #

valuesT <- reactiveValues()
valuesT$dfT <- tal

observeEvent(input$add.buttonT,{
  cat("addEntry\n")
  print(input$espeT)
  print(input$tipoT)
  print(input$tallaT)
  #newRowT <- data.frame(input$espeT, input$tipoT, input$tallaT)
  if(input$zona =="manu")
    newRowT <- data.frame(input$espeT, input$tipoT, input$tallaT, input$lat, input$lon)
  #}else{
  if(input$zona =="auto")
    newRowT <- data.frame(input$espeT, input$tipoT, input$tallaT, data$clickMarker$lat, data$clickMarker$lng)
  colnames(newRowT)<-colnames(valuesT$dfT)
  valuesT$dfT <- rbind(valuesT$dfT,newRowT)
  valuesT$dfT <- na.omit(valuesT$dfT)
  print(nrow(valuesT$dfT))
})



##----------------------------------- #
## Desacarga de datos tab marea       #
##----------------------------------  #

observeEvent(input$download.button,{
  showModal(modalDialog("Recuerda que los datos de marea y captura y tallas se descargan de acuerdo a la sesion actual.",
                        br(),br(),
                        #radioButtons("type", "Type", c("pdf", "png", "xlsx")),
                        downloadButton("down", "OK"), 
                        easyClose = TRUE,
                        footer = NULL))
})


output$down <- downloadHandler(
  
  filename = function(){
    #paste("Marea_captura_", input$type, sep =".")
    paste("Marea_captura_", input$fech, "_M_",input$new.button, "_",input$user, ".xlsx", sep ="")
  },
  content = function(file){
    
    if (nrow(ntal())== 0){
      
      if(input$moda == "Embarcación"){
        
        final <- values$df[,c(3,4,1,2,5,6)]
        final$Num.Tallados <- NA
        final$Fecha <- as.character(input$fech)
        final$HoraInicio <- strftime(input$horaI, "%R")
        final$HoraFin <- strftime(input$horaF, "%R")
        final$Modalidad <- input$moda
        final$Tecnica.Pesca <- input$moda3
        final$Tipo.Barco<- input$moda2
        final$Numero.Marea <- as.character(input$new.button)
        
        final2 <- values$df
        final2$Talla <- NA
        final2 <-final2[,c(3,4,7,1,2)]
        final2$Fecha <- as.character(input$fech)
        final2$HoraInicio <- strftime(input$horaI, "%R")
        final2$HoraFin <- strftime(input$horaF, "%R")
        final2$Modalidad <- input$moda
        final2$Tecnica.Pesca <- input$moda3
        final2$Tipo.Barco<- input$moda2
        final2$Numero.Marea <- as.character(input$new.button)}
      
      else{
        if(input$moda == "Tierra"){
          
          final <- values$df[,c(3,4,1,2,5,6)]
          final$Num.Tallados <- NA
          final$Fecha <- as.character(input$fech)
          final$HoraInicio <- strftime(input$horaI, "%R")
          final$HoraFin <- strftime(input$horaF, "%R")
          final$Modalidad <- input$moda
          final$Tecnica.Pesca <- input$moda4
          final$Numero.Marea <- as.character(input$new.button)
          
          final2 <- values$df
          final2$Talla <- NA
          final2 <-final2[,c(3,4,7,1,2)]
          final2$Fecha <- as.character(input$fech)
          final2$HoraInicio <- strftime(input$horaI, "%R")
          final2$HoraFin <- strftime(input$horaF, "%R")
          final2$Modalidad <- input$moda
          final2$Tecnica.Pesca <- input$moda4
          final2$Numero.Marea <- as.character(input$new.button)
        }
        else{
          final <- values$df[,c(3,4,1,2,5,6)]
          final$Num.Tallados <- NA
          final$Fecha <- as.character(input$fech)
          final$HoraInicio <- strftime(input$horaI, "%R")
          final$HoraFin <- strftime(input$horaF, "%R")
          final$Modalidad <- input$moda
          final$Numero.Marea <- as.character(input$new.button)
          
          final2 <- values$df
          final2$Talla <- NA
          final2 <-final2[,c(3,4,7,1,2)]
          final2$Fecha <- as.character(input$fech)
          final2$HoraInicio <- strftime(input$horaI, "%R")
          final2$HoraFin <- strftime(input$horaF, "%R")
          final2$Modalidad <- input$moda
          final2$Numero.Marea <- as.character(input$new.button)
          
        }
      }
      
      
    }else{
      
      if(input$moda == "Embarcación"){
        
        final <- merge(values$df, ntal(), by = c("Especie", "Tipo", "Latitud", "Longitud"), all = TRUE)
        final$Fecha <- as.character(input$fech)
        final$HoraInicio <- strftime(input$horaI, "%R")
        final$HoraFin <- strftime(input$horaF, "%R")
        final$Modalidad <- input$moda
        final$Tecnica.Pesca <- input$moda3
        final$Tipo.Barco<- input$moda2
        final$Numero.Marea <- as.character(input$new.button)
        
        final2 <- valuesT$dfT
        final2$Fecha <- as.character(input$fech)
        final2$HoraInicio <- strftime(input$horaI, "%R")
        final2$HoraFin <- strftime(input$horaF, "%R")
        final2$Modalidad <- input$moda
        final2$Tecnica.Pesca <- input$moda3
        final2$Tipo.Barco<- input$moda2
        final2$Numero.Marea <- as.character(input$new.button)}
      
      else{
        if(input$moda == "Tierra"){
          
          final <- merge(values$df, ntal(), by = c("Especie", "Tipo", "Latitud", "Longitud"), all = TRUE)
          final$Fecha <- as.character(input$fech)
          final$HoraInicio <- strftime(input$horaI, "%R")
          final$HoraFin <- strftime(input$horaF, "%R")
          final$Modalidad <- input$moda
          final$Tecnica.Pesca <- input$moda4
          final$Numero.Marea <- as.character(input$new.button)
          
          final2 <- valuesT$dfT
          final2$Fecha <- as.character(input$fech)
          final2$HoraInicio <- strftime(input$horaI, "%R")
          final2$HoraFin <- strftime(input$horaF, "%R")
          final2$Modalidad <- input$moda
          final2$Tecnica.Pesca <- input$moda4
          final2$Numero.Marea <- as.character(input$new.button)
        }
        else{
          final <- merge(values$df, ntal(), by = c("Especie", "Tipo", "Latitud", "Longitud"), all = TRUE)
          final$Fecha <- as.character(input$fech)
          final$HoraInicio <- strftime(input$horaI, "%R")
          final$HoraFin <- strftime(input$horaF, "%R")
          final$Modalidad <- input$moda
          final$Numero.Marea <- as.character(input$new.button)
          
          final2 <- valuesT$dfT
          final2$Fecha <- as.character(input$fech)
          final2$HoraInicio <- strftime(input$horaI, "%R")
          final2$HoraFin <- strftime(input$horaF, "%R")
          final2$Modalidad <- input$moda
          final2$Numero.Marea <- as.character(input$new.button)
          
        }
      }
    }
    #write.xlsx(datM_val(), file, sheetName="sheet1", row.names=FALSE)
    #write.xlsx(final, file, sheetName="sheet2", append=TRUE, row.names=FALSE)
    write.xlsx(final, file, sheetName="sheet1", row.names=FALSE)
    write.xlsx(final2, file, sheetName="sheet2", append=TRUE, row.names=FALSE)
    
    #dev.off()
  }
)

##------------------------------------------- #
## Envio de datos a google drive  tab marea   #
##------------------------------------------- #

observeEvent(input$send.button,{
  
  
  googledrive::drive_download("Marea.csv", overwrite=TRUE)
  
  if(nrow(ntal())== 0){
    
    if(input$moda == "Embarcación"){
      
      final <- values$df[,c(3,4,1,2,5,6)]
      final$Num.Tallados <- NA
      final$Fecha <- as.character(input$fech)
      final$HoraInicio <- strftime(input$horaI, "%R")
      final$HoraFin <- strftime(input$horaF, "%R")
      final$Modalidad <- input$moda
      final$Tecnica.Pesca <- input$moda3
      final$Tipo.Barco<- input$moda2
      final$Numero.Marea <- as.character(input$new.button)
      final$Campeonato <- "NO"
      final$Nombre.Campeonato <- NA
      final$Usuario <- input$user
    }else{
      if(input$moda == "Tierra"){
        
        final <- values$df[,c(3,4,1,2,5,6)]
        final$Num.Tallados <- NA
        final$Fecha <- as.character(input$fech)
        final$HoraInicio <- strftime(input$horaI, "%R")
        final$HoraFin <- strftime(input$horaF, "%R")
        final$Modalidad <- input$moda
        final$Tecnica.Pesca <- input$moda4
        final$Tipo.Barco<- NA
        final$Numero.Marea <- as.character(input$new.button)
        final$Campeonato <- "NO"
        final$Nombre.Campeonato <- NA
        final$Usuario <- input$user
      }else{
        final <- values$df[,c(3,4,1,2,5,6)]
        final$Num.Tallados <- NA
        final$Fecha <- as.character(input$fech)
        final$HoraInicio <- strftime(input$horaI, "%R")
        final$HoraFin <- strftime(input$horaF, "%R")
        final$Modalidad <- input$moda
        final$Tecnica.Pesca <- NA
        final$Tipo.Barco <- NA
        final$Numero.Marea <- as.character(input$new.button)
        final$Campeonato <- "NO"
        final$Nombre.Campeonato <- NA
        final$Usuario <- input$user
      }
    }}
  else{
    
    if(input$moda == "Embarcación"){
      
      final <- merge(values$df, ntal(), by = c("Especie", "Tipo", "Latitud", "Longitud"), all = TRUE)
      final$Fecha <- as.character(input$fech)
      final$HoraInicio <- strftime(input$horaI, "%R")
      final$HoraFin <- strftime(input$horaF, "%R")
      final$Modalidad <- input$moda
      final$Tecnica.Pesca <- input$moda3
      final$Tipo.Barco<- input$moda2
      final$Numero.Marea <- as.character(input$new.button)
      final$Campeonato <- "NO"
      final$Nombre.Campeonato <- NA
      final$Usuario <- input$user
      
    }
    
    else{
      if(input$moda == "Tierra"){
        
        final <- merge(values$df, ntal(), by = c("Especie", "Tipo", "Latitud", "Longitud"), all = TRUE)
        final$Fecha <- as.character(input$fech)
        final$HoraInicio <- strftime(input$horaI, "%R")
        final$HoraFin <- strftime(input$horaF, "%R")
        final$Modalidad <- input$moda
        final$Tecnica.Pesca <- input$moda4
        final$Tipo.Barco<- NA
        final$Numero.Marea <- as.character(input$new.button)
        final$Campeonato <- "NO"
        final$Nombre.Campeonato <- NA
        final$Usuario <- input$user
        
      }
      else{
        
        final <- merge(values$df, ntal(), by = c("Especie", "Tipo", "Latitud", "Longitud"), all = TRUE)
        final$Fecha <- as.character(input$fech)
        final$HoraInicio <- strftime(input$horaI, "%R")
        final$HoraFin <- strftime(input$horaF, "%R")
        final$Modalidad <- input$moda
        final$Tecnica.Pesca <- NA
        final$Tipo.Barco <- NA
        final$Numero.Marea <- as.character(input$new.button)
        final$Campeonato <- "NO"
        final$Nombre.Campeonato <- NA
        final$Usuario <- input$user
      }
    }
  }
  
  temp <- final
  
  #write(temp, file="Marea.csv", append=TRUE)
  write.table(temp, "Marea.csv", append=T, row.names=F, col.names=F,  sep=",")
  googledrive::drive_update(file="Marea.csv", media="Marea.csv")
  
  showModal(modalDialog("Gracias por rellenar el formulario. Los datos se han guardado correctamente.",
                        easyClose = TRUE,
                        footer = NULL))
  
})

##----------------------------------------------------- #
## Envio de datos tallas a google drive  tab marea      #
##----------------------------------------------------- #

observeEvent(input$send.button,{
  
  
  googledrive::drive_download("Talla.csv", overwrite=TRUE)
  
  if(nrow(ntal())== 0){
    
    if(input$moda == "Embarcación"){
      
      final2 <- values$df
      final2$Talla <- NA
      final2 <-final2[,c(3,4,7,1,2)]
      final2$Fecha <- as.character(input$fech)
      final2$HoraInicio <- strftime(input$horaI, "%R")
      final2$HoraFin <- strftime(input$horaF, "%R")
      final2$Modalidad <- input$moda
      final2$Tecnica.Pesca <- input$moda3
      final2$Tipo.Barco<- input$moda2
      final2$Numero.Marea <- as.character(input$new.button)
      final2$Campeonato <- "NO"
      final2$Nombre.Campeonato <- NA
      final2$Usuario <- input$user}
    
    
    else{
      if(input$moda == "Tierra"){
        
        final2 <- values$df
        final2$Talla <- NA
        final2 <-final2[,c(3,4,7,1,2)]
        final2$Fecha <- as.character(input$fech)
        final2$HoraInicio <- strftime(input$horaI, "%R")
        final2$HoraFin <- strftime(input$horaF, "%R")
        final2$Modalidad <- input$moda
        final2$Tecnica.Pesca <- input$moda4
        final2$Tipo.Barco<- NA
        final2$Numero.Marea <- as.character(input$new.button)
        final2$Campeonato <- "NO"
        final2$Nombre.Campeonato <- NA
        final2$Usuario <- input$user
        
      }
      else{
        
        final2 <- values$df
        final2$Talla <- NA
        final2 <-final2[,c(3,4,7,1,2)]
        final2$Fecha <- as.character(input$fech)
        final2$HoraInicio <- strftime(input$horaI, "%R")
        final2$HoraFin <- strftime(input$horaF, "%R")
        final2$Modalidad <- input$moda
        final2$Tecnica.Pesca <- NA
        final2$Tipo.Barco <- NA
        final2$Numero.Marea <- as.character(input$new.button)
        final2$Campeonato <- "NO"
        final2$Nombre.Campeonato <- NA
        final2$Usuario <- input$user
        
      }
    }
    
    
    
  }else{
    
    
    if(input$moda == "Embarcación"){
      
      final2 <- valuesT$dfT
      final2$Fecha <- as.character(input$fech)
      final2$HoraInicio <- strftime(input$horaI, "%R")
      final2$HoraFin <- strftime(input$horaF, "%R")
      final2$Modalidad <- input$moda
      final2$Tecnica.Pesca <- input$moda3
      final2$Tipo.Barco<- input$moda2
      final2$Numero.Marea <- as.character(input$new.button)
      final2$Campeonato <- "NO"
      final2$Nombre.Campeonato <- NA
      final2$Usuario <- input$user}
    
    
    else{
      if(input$moda == "Tierra"){
        
        final2 <- valuesT$dfT
        final2$Fecha <- as.character(input$fech)
        final2$HoraInicio <- strftime(input$horaI, "%R")
        final2$HoraFin <- strftime(input$horaF, "%R")
        final2$Modalidad <- input$moda
        final2$Tecnica.Pesca <- input$moda4
        final2$Tipo.Barco<- NA
        final2$Numero.Marea <- as.character(input$new.button)
        final2$Campeonato <- "NO"
        final2$Nombre.Campeonato <- NA
        final2$Usuario <- input$user
        
      }
      else{
        
        final2 <- valuesT$dfT
        final2$Fecha <- as.character(input$fech)
        final2$HoraInicio <- strftime(input$horaI, "%R")
        final2$HoraFin <- strftime(input$horaF, "%R")
        final2$Modalidad <- input$moda
        final2$Tecnica.Pesca <- NA
        final2$Tipo.Barco <- NA
        final2$Numero.Marea <- as.character(input$new.button)
        final2$Campeonato <- "NO"
        final2$Nombre.Campeonato <- NA
        final2$Usuario <- input$user
        
      }
    }
  }
  
  temp <- final2
  
  #write(temp, file="Marea.csv", append=TRUE)
  write.table(temp, "Talla.csv", append=T, row.names=F, col.names=F,  sep=",")
  googledrive::drive_update(file="Talla.csv", media="Talla.csv")
  
  showModal(modalDialog("Gracias por rellenar el formulario. Los datos se han guardado correctamente.",
                        easyClose = TRUE,
                        footer = NULL))
  
})




observeEvent(input$delete.buttonT,{
  cat("deleteEntry\n")
  if(is.na(input$row.selectionT)){
    valuesT$dfT <- valuesT$dfT[-nrow(valuesT$dfT), ]
  } else {
    valuesT$dfT <- valuesT$dfT[-input$row.selectionT, ]
  }
})  


output$tableT <- renderTable({
  req(input$tallaT)
  valuesT$dfT }, striped = TRUE, bordered = TRUE,  hover = TRUE)


ntal <- reactive({
  req(input$pesoC)
  #req(input$tallaT)
  #valuesT$dfT[, (.count = .N), by = list(Especie,Tipo)]
  dat <-aggregate (valuesT$dfT, by = list(valuesT$dfT$Especie, valuesT$dfT$Tipo, valuesT$dfT$Latitud, valuesT$dfT$Longitud), FUN = length)
  dat <- dat[, c(1:5)]
  names(dat) <- c("Especie", "Tipo", "Latitud", "Longitud", "Num.Tallados")
  dat
})


# output$prueba <- renderTable({
# 
#    valuesc$dfc
# })

# output$prueba2 <- renderText({
#    
#    dim(valuesc$dfc)
# }
# )


output$tableF <- renderTable({
  if (is.na(valuesT$dfT$Especie)){
    final <-values$df
    final$Num.Tallados <- NA
  }else{
    #req(input$add.buttonT)
    final <- merge(values$df, ntal(), by = c("Especie", "Tipo", "Latitud", "Longitud"), all = TRUE)
    #final <- final[,c(1:6,8)]
    #names(final) <- c("Especie","Tipo","Latitud", "Longitud", "Peso.Total", "Numero", "Num.Tallados")
  }
  
  final
}, striped = TRUE, bordered = TRUE,  hover = TRUE)


