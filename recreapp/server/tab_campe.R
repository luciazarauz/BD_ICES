# ----------------------------------- #
# Tab campeonato                      #
# ----------------------------------- #


# ----------------------------------- #
# Map                                 #
# ----------------------------------- #

output$mapc <- renderLeaflet({
  leaflet() %>% addTiles() %>% 
    addMouseCoordinates() %>% 
    addScaleBar ("bottomleft") %>% 
    setView(lng = -2.5,lat =  43.5, zoom = 9)    
})

# ----------------------------------- #
# Reactive data lat long              #
# ----------------------------------- #

#Reactive data to click on map   
datac <- reactiveValues(clickMarkerc=NULL)

observeEvent(input$mapc_click,
             {datac$clickMarkerc <- input$mapc_click})


#Reactive text to click o map
output$latAc<- renderPrint({
  datac$clickMarkerc$lat
})

#Reactive text to click o map
output$lonAc <- renderText({
  datac$clickMarkerc$lng
})

#Reactive marker to click on map
observeEvent(input$mapc_click,{
  leafletProxy('mapc')%>% 
    clearMarkers()%>% 
    addMarkers(lng = datac$clickMarkerc$lng, lat = datac$clickMarkerc$lat)
  #leafletProxy('map')%>% addMarkers(lng = data$clickMarker$lng, lat = data$clickMarker$lat) 
})


# -------------------- #
# Tabla de campeonato  #
# -------------------- #
  
  datMc <- data.table("Parametros" = c("<strong>Nombre</strong>",
                                       "<strong>Fecha</strong>",
                                       "<strong>Modalidad</strong>", 
                                       "<strong>Técnica de pesca</strong>",
                                       "<strong>Tipo de barco</strong>"))
  # Entradas del usuario
  
  datMc_val <- reactive({
    #actual reactive
    if (input$modac == "Tierra"){
      datMc[,"Datos" := c(as.character(input$textc),
                          as.character(input$fechc),
                          as.character(input$modac),
                          as.character (input$moda4c),
                          as.character (input$moda3c))]
    }else{
      datMc[,"Datos" := c(as.character(input$textc),
                          as.character(input$fechc),
                          as.character(input$modac),
                          as.character (input$moda2c),
                          as.character (input$moda3c))]
    }
    
  })
  
  
  # Tabla salida de campeonato
  
  output$tableMc <- renderTable({
    req(input$modac)
    if (input$modac == "Submarina"){
      datMc_val()[1:3]
    }
    else{
      if (input$modac == "Tierra"){
        datMc_val()[1:4]}
      else{
        datMc_val()  
      }
    }}, striped = TRUE, 
    bordered = TRUE,  
    hover = TRUE, 
    colnames = FALSE, 
    sanitize.text.function=function(x){x})
  

  
  ##----------------------------------------------------
  ## Tabla de captura en tab campeonato
  ##---------------------------------------------------
  
  valuesc <- reactiveValues()
  valuesc$dfc <- retc
  
  # AÃ±ade evento
  
  observeEvent(input$add.buttonc,{
    cat("addEntry\n")
    print(input$latc)
    print(input$lonc)
    #print(lat())
    #print(lon())
    print(input$espeCc)
    print(input$pesoCc)
    print(input$nindCc)
    #newRow <- data.frame(input$lat, input$lon, input$espeC, input$tipoC, input$pesoC, input$nindC)
    if(input$zonac =="manu")
      newRowc <- data.frame(input$latc, input$lonc, input$espeCc, input$tipoCc, input$pesoCc, input$nindCc)
    #}else{
    if(input$zonac =="auto")
      newRowc <- data.frame(datac$clickMarkerc$lat, datac$clickMarkerc$lng, input$espeCc, input$tipoCc, input$pesoCc, input$nindCc)
    #}
    #return(newRow)
    colnames(newRowc)<-colnames(valuesc$dfc)
    valuesc$dfc <- rbind(valuesc$dfc,newRowc)
    valuesc$dfc <- na.omit(valuesc$dfc)
    print(nrow(values$df))
  })
  
  # Elimina evento
  
  observeEvent(input$delete.buttonc,{
    cat("deleteEntry\n")
    if(is.na(input$row.selectionc)){
      valuesc$dfc <- valuesc$dfc[-nrow(valuesc$dfc), ]
    } else {
      valuesc$dfc <- valuesc$dfc[-input$row.selectionc, ]
    }
  })  
  
  
  # Tabla de salida
  
  output$tableCc <- renderTable({
    req(input$pesoCc|input$nindCc)
    valuesc$dfc }, striped = TRUE, bordered = TRUE,  hover = TRUE)
  
  
  
  ##----------------------------------------------------------------------------------------------------------------
  ## Desacarga de datos tab comp
  ##----------------------------------------------------------------------------------------------------------
  
  observeEvent(input$download.buttonc,{
    showModal(modalDialog("Recuerda que los datos de captura y tallas de la competición se descargan de acuerdo a la sesion actual.",
                          br(),br(),
                          #radioButtons("type", "Type", c("pdf", "png", "xlsx")),
                          downloadButton("downc", "OK"), 
                          easyClose = TRUE,
                          footer = NULL))
  })
  
  
  output$downc <- downloadHandler(
    
    filename = function(){
      #paste("Marea_captura_", input$type, sep =".")
      paste("Comp_", input$textc, "_",input$fechc,"_",input$user, ".xlsx", sep ="")
      
    },
    content = function(file){
      
      if (nrow(ntalc())== 0){
        
        if(input$modac == "Embarcación"){
          
          final <- valuesc$dfc[,c(3,4,1,2,5,6)]
          final$Num.Tallados <- NA
          final$Fecha <- as.character(input$fechc)
          final$Modalidad <- input$modac
          final$Tecnica.Pesca <- input$moda3c
          final$Tipo.Barco<- input$moda2c
          
          
          final2 <- valuesc$dfc
          final2$Talla <- NA
          final2 <-final2[,c(3,4,7,1,2)]
          final2$Fecha <- as.character(input$fechc)
          final2$Modalidad <- input$modac
          final2$Tecnica.Pesca <- input$moda3c
          final2$Tipo.Barco<- input$moda2c
        }
        
        else{
          if(input$modac == "Tierra"){
            
            final <- valuesc$dfc[,c(3,4,1,2,5,6)]
            final$Num.Tallados <- NA
            final$Fecha <- as.character(input$fechc)
            final$Modalidad <- input$modac
            final$Tecnica.Pesca <- input$moda4c
            
            final2 <- valuesc$dfc
            final2$Talla <- NA
            final2 <-final2[,c(3,4,7,1,2)]
            final2$Fecha <- as.character(input$fechc)
            final2$Modalidad <- input$modac
            final2$Tecnica.Pesca <- input$moda4c
            
          }
          else{
            
            final <- valuesc$dfc[,c(3,4,1,2,5,6)]
            final$Num.Tallados <- NA
            final$Fecha <- as.character(input$fechc)
            final$Modalidad <- input$modac
            
            
            final2 <- valuesc$dfc
            final2$Talla <- NA
            final2 <-final2[,c(3,4,7,1,2)]
            final2$Fecha <- as.character(input$fechc)
            final2$Modalidad <- input$modac
            
          }
        }
        
        
      }else{
        
        if(input$modac == "Embarcación"){
          
          final <- merge(valuesc$dfc, ntalc(), by = c("Especie", "Tipo", "Latitud", "Longitud"), all = TRUE)
          final$Fecha <- as.character(input$fechc)
          final$Modalidad <- input$modac
          final$Tecnica.Pesca <- input$moda3c
          final$Tipo.Barco<- input$moda2c
          
          
          final2 <- valuesTc$dfTc
          final2$Fecha <- as.character(input$fechc)
          final2$Modalidad <- input$modac
          final2$Tecnica.Pesca <- input$moda3c
          final2$Tipo.Barco<- input$moda2c
        }
        
        else{
          if(input$modac == "Tierra"){
            
            final <- merge(valuesc$dfc, ntalc(), by = c("Especie", "Tipo", "Latitud", "Longitud"), all = TRUE)
            final$Fecha <- as.character(input$fechc)
            final$Modalidad <- input$modac
            final$Tecnica.Pesca <- input$moda4c
            
            
            final2 <- valuesTc$dfTc
            final2$Fecha <- as.character(input$fechc)
            final2$Modalidad <- input$modac
            final2$Tecnica.Pesca <- input$moda4c
            
          }
          else{
            final <- merge(valuesc$dfc, ntalc(), by = c("Especie", "Tipo", "Latitud", "Longitud"), all = TRUE)
            final$Fecha <- as.character(input$fechc)
            final$Modalidad <- input$modac
            
            
            final2 <- valuesT$dfT
            final2$Fecha <- as.character(input$fechc)
            final2$Modalidad <- input$modac
            
            
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
  
  ## https://rpubs.com/berdaniera/shinyshop-remotedata
  ## https://deanattali.com/blog/shiny-persistent-data-storage/
  
 
  
  ##----------------------------------------------------------------------------------------------------------------
  ## Envio de datos a google drive tab comp
  ##----------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$send.buttonc,{
    
    googledrive::drive_download("Marea.csv", overwrite=TRUE)
    
    if(nrow(ntalc())== 0){
      
      if(input$modac == "Embarcación"){
        
        final <- valuesc$dfc[,c(3,4,1,2,5,6)]
        final$Num.Tallados <- NA
        final$Fecha <- as.character(input$fechc)
        final$HoraInicio <- NA
        final$HoraFin <- NA
        final$Modalidad <- input$modac
        final$Tecnica.Pesca <- input$moda3c
        final$Tipo.Barco<- input$moda2c
        final$Numero.Marea <- NA
        final$Campeonato <- "SI"
        final$Nombre.Campeonato <- as.character (input$textc)
        final$Usuario <- input$user
        
      }
      
      else{
        if(input$modac == "Tierra"){
          
          final <- valuesc$dfc[,c(3,4,1,2,5,6)]
          final$Num.Tallados <- NA
          final$Fecha <- as.character(input$fechc)
          final$HoraInicio <- NA
          final$HoraFin <- NA
          final$Modalidad <- input$modac
          final$Tecnica.Pesca <- input$moda4c
          final$Tipo.Barco<- NA
          final$Numero.Marea <- NA
          final$Campeonato <- "SI"
          final$Nombre.Campeonato <- as.character (input$textc)
          final$Usuario <- input$user
          
        }
        else{
          final <- valuesc$dfc[,c(3,4,1,2,5,6)]
          final$Num.Tallados <- NA
          final$Fecha <- as.character(input$fechc)
          final$HoraInicio <- NA
          final$HoraFin <- NA
          final$Modalidad <- input$modac
          final$Tecnica.Pesca <- NA
          final$Tipo.Barco<- NA
          final$Numero.Marea <- NA
          final$Campeonato <- "SI"
          final$Nombre.Campeonato <- as.character (input$textc)
          final$Usuario <- input$user
        }
      }
      
      
    }else{
      
      if(input$modac == "Embarcación"){
        
        final <- merge(valuesc$dfc, ntalc(), by = c("Especie", "Tipo", "Latitud", "Longitud"), all = TRUE)
        final$Fecha <- as.character(input$fechc)
        final$HoraInicio <- NA
        final$HoraFin <- NA
        final$Modalidad <- input$modac
        final$Tecnica.Pesca <- input$moda3c
        final$Tipo.Barco<- input$moda2c
        final$Numero.Marea <- NA
        final$Campeonato <- "SI"
        final$Nombre.Comp <- as.character (input$textc)
        final$Usuario <- input$user
        
      }
      
      else{
        if(input$modac == "Tierra"){
          
          final <- merge(valuesc$dfc, ntalc(), by = c("Especie", "Tipo", "Latitud", "Longitud"), all = TRUE)
          final$Fecha <- as.character(input$fechc)
          final$HoraInicio <- NA
          final$HoraFin <- NA
          final$Modalidad <- input$modac
          final$Tecnica.Pesca <- input$moda4c
          final$Tipo.Barco<- NA
          final$Numero.Marea <- NA
          final$Campeonato <- "SI"
          final$Nombre.Campeonato <- as.character (input$textc)
          final$Usuario <- input$user
          
        }
        else{
          final <- merge(valuesc$dfc, ntalc(), by = c("Especie", "Tipo", "Latitud", "Longitud"), all = TRUE)
          final$Fecha <- as.character(input$fechc)
          final$HoraInicio <- NA
          final$HoraFin <- NA
          final$Modalidad <- input$modac
          final$Tecnica.Pesca <- NA
          final$Tipo.Barco<- NA
          final$Numero.Marea <- NA
          final$Campeonato <- "SI"
          final$Nombre.Campeonato <- as.character (input$textc)
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
  
 
  ##---------------------------------------------------------------------------------------------------------------
  ## Envio de datos tallas a google drive  tab campeonato
  ##--------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$send.buttonc,{
    
    
    googledrive::drive_download("Talla.csv", overwrite=TRUE)
    
    if(nrow(ntalc())== 0){
      
      if(input$modac == "Embarcación"){
        
        final2 <- valuesc$dfc
        final2$Talla <- NA
        final2 <-final2[,c(3,4,7,1,2)]
        final2$Fecha <- as.character(input$fechc)
        final2$HoraInicio <- NA
        final2$HoraFin <- NA
        final2$Modalidad <- input$modac
        final2$Tecnica.Pesca <- input$moda3c
        final2$Tipo.Barco<- input$moda2c
        final2$Numero.Marea <- NA
        final2$Campeonato <- "SI"
        final2$Nombre.Campeonato <- as.character (input$textc)
        final2$Usuario <- input$user}
      
      
      else{
        if(input$modac == "Tierra"){
          
          final2 <- valuesc$dfc
          final2$Talla <- NA
          final2 <-final2[,c(3,4,7,1,2)]
          final2$Fecha <- as.character(input$fechc)
          final2$HoraInicio <- NA
          final2$HoraFin <- NA
          final2$Modalidad <- input$modac
          final2$Tecnica.Pesca <- input$moda4c
          final2$Tipo.Barco<- NA
          final2$Numero.Marea <- NA
          final2$Campeonato <- "SI"
          final2$Nombre.Campeonato <- as.character (input$textc)
          final2$Usuario <- input$user
          
        }
        else{
          
          final2 <- valuesc$dfc
          final2$Talla <- NA
          final2 <-final2[,c(3,4,7,1,2)]
          final2$Fecha <- as.character(input$fechc)
          final2$HoraInicio <- NA
          final2$HoraFin <- NA
          final2$Modalidad <- input$modac
          final2$Tecnica.Pesca <- NA
          final2$Tipo.Barco <- NA
          final2$Numero.Marea <- NA
          final2$Campeonato <- "SI"
          final2$Nombre.Campeonato <- as.character (input$textc)
          final2$Usuario <- input$user
          
        }
      }
      
      
      
    }else{
      
      
      if(input$modac == "Embarcación"){
        
        final2 <- valuesTc$dfTc
        final2$Fecha <- as.character(input$fechc)
        final2$HoraInicio <- NA
        final2$HoraFin <- NA
        final2$Modalidad <- input$modac
        final2$Tecnica.Pesca <- input$moda3c
        final2$Tipo.Barco<- input$moda2c
        final2$Numero.Marea <- NA
        final2$Campeonato <- "SI"
        final2$Nombre.Campeonato <- as.character (input$textc)
        final2$Usuario <- input$user}
      
      
      else{
        if(input$modac == "Tierra"){
          
          final2 <- valuesTc$dfTc
          final2$Fecha <- as.character(input$fechc)
          final2$HoraInicio <- NA
          final2$HoraFin <- NA
          final2$Modalidad <- input$modac
          final2$Tecnica.Pesca <- input$moda4c
          final2$Tipo.Barco<- NA
          final2$Numero.Marea <- NA
          final2$Campeonato <- "SI"
          final2$Nombre.Campeonato <- as.character (input$textc)
          final2$Usuario <- input$user
          
        }
        else{
          
          final2 <- valuesTc$dfTc
          final2$Fecha <- as.character(input$fechc)
          final2$HoraInicio <- NA
          final2$HoraFin <- NA
          final2$Modalidad <- input$moda
          final2$Tecnica.Pesca <- NA
          final2$Tipo.Barco <- NA
          final2$Numero.Marea <- NA
          final2$Campeonato <- "SI"
          final2$Nombre.Campeonato <- as.character (input$textc)
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


  
  
  ##--------------
  ## Tabla de tallas en tab comp
  ##--------------
  
  valuesTc <- reactiveValues()
  valuesTc$dfTc <- talc
  
  observeEvent(input$add.buttonTc,{
    cat("addEntry\n")
    print(input$espeTc)
    print(input$tipoTc)
    print(input$tallaTc)
    #newRowT <- data.frame(input$espeT, input$tipoT, input$tallaT)
    if(input$zonac =="manu")
      newRowTc <- data.frame(input$espeTc, input$tipoTc, input$tallaTc, input$latc, input$lonc)
    #}else{
    if(input$zonac =="auto")
      newRowTc <- data.frame(input$espeTc, input$tipoTc, input$tallaTc, datac$clickMarkerc$lat, datac$clickMarkerc$lng)
    colnames(newRowTc)<-colnames(valuesTc$dfTc)
    valuesTc$dfTc <- rbind(valuesTc$dfTc,newRowTc)
    valuesTc$dfTc <- na.omit(valuesTc$dfTc)
    print(nrow(valuesTc$dfTc))
  })
  
  
  observeEvent(input$delete.buttonTc,{
    cat("deleteEntry\n")
    if(is.na(input$row.selectionTc)){
      valuesTc$dfTc <- valuesT$dfT[-nrow(valuesT$dfTc), ]
    } else {
      valuesTc$dfTc <- valuesT$dfT[-input$row.selectionTc, ]
    }
  })  
  
  
  output$tableTc <- renderTable({
    req(input$tallaTc)
    valuesTc$dfTc }, striped = TRUE, bordered = TRUE,  hover = TRUE)
  
  
  ntalc <- reactive({
    req(input$pesoCc)
    #req(input$tallaTc)
    #valuesT$dfT[, (.count = .N), by = list(Especie,Tipo)]
    dat <-aggregate (valuesTc$dfTc, by = list(valuesTc$dfTc$Especie, valuesTc$dfTc$Tipo, valuesTc$dfTc$Latitud, valuesTc$dfTc$Longitud), FUN = length)
    dat <- dat[, c(1:5)]
    names(dat) <- c("Especie", "Tipo", "Latitud", "Longitud", "Num.Tallados")
    dat
  })
  
  
  
  
  
  output$tableFc <- renderTable({
    if (is.na(valuesTc$dfTc$Especie)){
      final <-valuesc$dfc
      final$Num.Tallados <- NA
    }else{
      #req(input$add.buttonT)
      final <- merge(valuesc$dfc, ntalc(), by = c("Especie", "Tipo", "Latitud", "Longitud"), all = TRUE)
      #final <- final[,c(1:6,8)]
      #names(final) <- c("Especie","Tipo","Latitud", "Longitud", "Peso.Total", "Numero", "Num.Tallados")
    }
    
    final
  }, striped = TRUE, bordered = TRUE,  hover = TRUE)
  
  
