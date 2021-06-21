## ---------------------------------------------------- ##
## IM19RECLUB                                           ##
## Diario de pesca recreativa - Shiny                   ##
## Feb. 2020                                            ##
## AZTI (@mkorta)                                       ##
## ---------------------------------------------------- ##

##--------------
## server
##--------------

server <- function(input, output, session){
  
  
  
  ##--------------
  ## show/hide app when login
  ##--------------
  
  observeEvent(input$check, {
    load("data/userTable.RData")
    if (nrow(subset(userTable, user==input$user & password==input$pass))!=0){
      #if (nrow(subset(userTable, password==input$pass))!=0){
      shinyjs::show("myapp", FALSE)
      shinyjs::hide("passScreen", FALSE)
    }
  })
  
  
  
  # -----------------------------------
  # Map itselfe tab Marea
  # -----------------------------------
  
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
  
  
  
  # -----------------------------------
  # Map itselfe tab comp
  # -----------------------------------
  
  output$mapc <- renderLeaflet({
    leaflet() %>% addTiles() %>% 
      addMouseCoordinates() %>% 
      addScaleBar ("bottomleft") %>% 
      setView(lng = -2.5,lat =  43.5, zoom = 9)    
  })
  
  
  
  # -----------------------------------
  # Reactive data lat long tab Marea
  # -----------------------------------
  
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
  
  
  # -----------------------------------
  # Reactive data lat long Tab comp
  # -----------------------------------
  
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
  
  
  
  ##---------------------------------------------------------------
  ## Tabla de marea
  ##---------------------------------------------------------------
  
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
  
  
  ##--------------
  ## Tabla de campeonato
  ##--------------
  
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
  
  
  
  ##--------------------------------------------------------
  ## Tabla de captura en tab marea
  ##--------------------------------------------------------
  
  values <- reactiveValues()
  values$df <- ret
  
  
  # AÃ±ade evento
  
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
  
  ##-----------------------------------------------------------------------------------------------------------
  ## Desacarga de datos tab marea 
  ##-----------------------------------------------------------------------------------------------------------
  
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
  
  ##----------------------------------------------------------------------------------------------------------------
  ## Desacarga de datos tab comp
  ##----------------------------------------------------------------------------------------------------------
  
  observeEvent(input$download.buttonc,{
    showModal(modalDialog("Recuerda que los datos de captura y tallas de la competiciÃÂ³n se descargan de acuerdo a la sesion actual.",
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
  ## Envio de datos a google drive  tab marea
  ##----------------------------------------------------------------------------------------------------------------
  
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
  ## Envio de datos tallas a google drive  tab marea
  ##--------------------------------------------------------------------------------------------------------------
  
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
  
  # ##--------------
  # ## Envio perfil a googledrive
  # ##--------------
  
  dat_per <- reactive({
    per
    per$Edad <-input$edad
    per$Genero <- as.character(input$gene)
    per$Experiencia <- as.character(input$expe)
    per$Avidez <- as.character(input$avid)
    #per$user <- input$pass
    per
  })
  
  observeEvent(input$saveP,{
    
    if(input$edad!=""&&input$gene!=""&&input$expe!=""&&input$avid!="") {
      
      googledrive::drive_download("Perfil.csv", overwrite=TRUE)
      temp <- paste(c(input$edad,input$gene,input$expe,input$avid,input$user), collapse=",")
      write(temp, file="Perfil.csv", append=TRUE)
      googledrive::drive_update(file="Perfil.csv", media="Perfil.csv")
      
      showModal(modalDialog("Gracias por rellenar el formulario. Los datos se han guardado correctamente.",
                            easyClose = TRUE,
                            footer = NULL))
    }
    else{
      
      showModal(modalDialog("Por favor no olvide rellenar todos los campos.",
                            easyClose = TRUE,
                            footer = NULL))
    }
    
  })
  
  
  ##--------------
  ## Tabla de tallas en tab mareas
  ##--------------
  
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
  
  
  
  
  ##--------------
  ## UI: Se refresca la interface a valores originales
  ##-------------- 
  
  output$resetable_input <- renderUI({
    
    times <- input$new.button
    div(id=letters[(times %% length(letters)) + 1],
        
        fluidRow(
          column(6, 
                 br(),br(),
                 bsCollapse(id = "collapse", 
                            bsCollapsePanel("Datos marea",
                                            dateInput("fech", "Fecha:", value = "2020-03-01"),
                                            # Use %H:%M format
                                            div(style="display: inline-block;vertical-align:top; width: 200px;",timeInput("horaI", "Hora inicio pesca:",  value = strptime("00:00:00", "%T"), seconds = F)),
                                            div(style="display: inline-block;vertical-align:top; width: 200px;",timeInput("horaF", "Hora fin pesca:",     value = strptime("00:00:00", "%T"), seconds = F)),
                                            selectInput("moda","Modalidad",  c("", "Tierra","Embarcación", "Submarina"), selected = ""),
                                            conditionalPanel(
                                              condition = "input.moda == 'Embarcación'",
                                              selectInput("moda2","Técnica de pesca", c("", "Pesca de fondo","Pesca de calamares", "Curricán-cacea de altura",
                                                                                         "Curricán-cacea de costero", "spinning-jigging costero", "Pesca de fondo en calas"), selected = ""),
                                              selectInput("moda3","Tipo de barco", c("", "Motor","Vela"), selected = "")),
                                            conditionalPanel(
                                              condition = "input.moda == 'Tierra'",
                                              selectInput("moda4","Técnica de pesca", c("", "Casting-pesca de fondo","Señuelo artificial", "Pesca con corcho"), selected = ""))
                            ),
                            # bsCollapsePanel("Datos zona",
                            #   radioButtons ("zona", "", c("Manual" = "manu", "Automatico"="auto")),
                            #   conditionalPanel(
                            #   condition = "input.zona == 'manu'",
                            #   div(style="display: inline-block;vertical-align:top; width: 200px;",numericInput("lat", "Latitutd", value = "")),
                            #   div(style="display: inline-block;vertical-align:top; width: 200px;",numericInput("lon", "Longitud", value = ""))
                            #   ),
                            #   conditionalPanel(
                            #   condition = "input.zona == 'auto'",
                            #   leafletOutput("map", height = "400px"),
                            #   br(), br(),
                            #   div(style="display: inline-block;vertical-align:top; width: 200px;",verbatimTextOutput("latA")),
                            #   div(style="display: inline-block;vertical-align:top; width: 200px;",verbatimTextOutput("lonA"))
                            #   )
                            #   ),
                            bsCollapsePanel("Datos zona y captura",
                                            fluidRow(column(12, offset = 0.5, 
                                                            radioButtons ("zona", "", c("Manual" = "manu", "Automatico"="auto")),
                                                            conditionalPanel(
                                                              condition = "input.zona == 'manu'",
                                                              div(style="display: inline-block;vertical-align:top; width: 200px;",numericInput("lat", "Latitutd", value = "")),
                                                              div(style="display: inline-block;vertical-align:top; width: 200px;",numericInput("lon", "Longitud", value = ""))
                                                            ),
                                                            conditionalPanel(
                                                              condition = "input.zona == 'auto'",
                                                              leafletOutput("map", height = "400px"),
                                                              br(), br(),
                                                              div(style="display: inline-block;vertical-align:top; width: 200px;",verbatimTextOutput("latA")),
                                                              div(style="display: inline-block;vertical-align:top; width: 200px;",verbatimTextOutput("lonA"))
                                                            )
                                                            
                                                            
                                            )),
                                            
                                            fluidRow(column(12, offset = 0.5, 
                                                            #div(style="display: inline-block;vertical-align:top; width: 200px;", selectInput("espeC","Especie", c("Hegaluze","Lubina", "Calamar"), selected = NULL)),
                                                            div(style="display: inline-block;vertical-align:top; width: 300px;", selectInput("espeC","Especie", speciesList, selected = NULL)),
                                                            div(style="display: inline-block;vertical-align:top; width: 200px;", selectInput("tipoC","Tipo", c("Retenida", "Devuelta"), selected = NULL))
                                            )),
                                            fluidRow(column(12, offset = 0.5, 
                                                            div(style="display: inline-block;vertical-align:top; width: 200px;", numericInput ("pesoC", "Peso captura total (Kg)", value = 0,  min = 0, max = 5000)),
                                                            div(style="display: inline-block;vertical-align:top; width: 200px;", numericInput("nindC", "Numero total individuos", value = 0,  min = 0, max = 5000))
                                            )),
                                            hr(),
                                            # Add button
                                            div(style="display: inline-block;margin-top: 25px;",
                                                actionButton(inputId = "add.button", label = "Anadir", icon =  icon("plus"))), 
                                            # Delete button 
                                            div(style="display: inline-block;margin-top: 25px;",
                                                actionButton(inputId = "delete.button", label = "Borrar", icon = icon("minus"))),
                                            # Row selection
                                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                numericInput(inputId = "row.selection", label = "Fila a borrar", min = 1, max = 100, value = ""))
                            ), 
                            bsCollapsePanel("Datos tallas",
                                            #div(style="display: inline-block;vertical-align:top; width: 200px;",selectInput("espeT","Especie", c("Hegaluze","Lubina", "Calamar"), selected = NULL)),
                                            div(style="display: inline-block;vertical-align:top; width: 300px;",selectInput("espeT","Especie",speciesList, selected = NULL)),
                                            div(style="display: inline-block;vertical-align:top; width: 200px;",selectInput("tipoT","Tipo", c("Retenida", "Devuelta"), selected = NULL)),
                                            div(style="display: inline-block;vertical-align:top; width: 200px;",numericInput("tallaT", "Talla individual (cm)", value = 0, min = 0, max = 5000)), 
                                            hr(),
                                            # Add button
                                            div(style="display: inline-block;margin-top: 25px;",
                                                actionButton(inputId = "add.buttonT", label = "Anadir", icon = icon("plus"))), 
                                            # Delete button 
                                            div(style="display: inline-block;margin-top: 25px;",
                                                actionButton(inputId = "delete.buttonT", label = "Borrar", icon = icon("minus"))),
                                            # Row selection
                                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                numericInput(inputId = "row.selectionT", label = "Fila a borrar", min = 1, max = 100, value = ""))#,
                                            # Send button
                                            # div(style="display: inline-block;margin-top: 25px;width: 150px;",
                                            #     actionButton(inputId = "send.buttonT", label = "Enviar", icon = 
                                            #                    icon("paper-plane"),  class = "btn-primary"))
                            )
                 ), # end of the bscollapse
                 # Send button
                 # div(style="display: inline-block;margin-top: 25px;width: 150px;",
                 #     actionButton(inputId = "new.button", label = "Nueva marea", icon = 
                 #                     icon("servicestack"),  class = "btn-primary")),
                 div(style="display: inline-block;margin-top: 25px;width: 150px;",
                     actionButton(inputId = "download.button", label = "Descargar datos", icon = icon("cloud-download-alt"),  class = "btn-primary")),
                 div(style="display: inline-block;margin-top: 25px;width: 150px;",
                     actionButton(inputId = "send.button", label = "Enviar datos", icon = icon("paper-plane"),  class = "btn-primary"))
          ),
          column(6, 
                 tabsetPanel(
                   tabPanel ("Datos de la marea y captura",
                             br(),br(),
                             tableOutput("tableM"), 
                             br(), br(),
                             tableOutput("tableC")
                   ),
                   tabPanel ("Datos de tallas",
                             br(),br(),
                             tableOutput("tableT"),
                             br(),br(),
                             tableOutput("tableF"),
                             br(),br(),
                             tableOutput("prueba"),
                             br(),br(),
                             textOutput("prueba2")
                   )
                 ) # end of the tabsetPanel
          )
        )
    )
  })
  
  
  ##--------------
  ## UI competiciones: 1 marea
  ##-------------- 
  
  output$comp_input <- renderUI({
    
    times <- input$new.button
    div(id=letters[(times %% length(letters)) + 1],
        
        fluidRow(
          column(6, 
                 br(), br(),
                 bsCollapse(id = "collapse", 
                            bsCollapsePanel("Datos marea",
                                            textInput("textc", "Nombre del campeonato"),
                                            dateInput("fechc", "Fecha:", value = "2020-03-01"),
                                            selectInput("modac","Modalidad",  c("", "Tierra","Embarcación", "Submarina"), selected = ""),
                                            conditionalPanel(
                                              condition = "input.modac == 'Embarcación'",
                                              selectInput("moda2c","Técnica de pesca", c("", "Pesca de fondo",
                                                                                         "Pesca de calamares", 
                                                                                         "Curricán-cacea de altura",
                                                                                         "Curricán-cacea de costero", 
                                                                                         "spinning-jigging costero", 
                                                                                         "Pesca de fondo en calas"), selected = ""),
                                              selectInput("moda3c","Tipo de barco", c("", "Motor","Vela"), selected = "")),
                                            conditionalPanel(
                                              condition = "input.modac == 'Tierra'",
                                              selectInput("moda4c","Técnica de pesca", c("", "Casting-pesca de fondo",
                                                                                            "Señuelo artificial", 
                                                                                            "Pesca con corcho"), selected = ""))
                            ),
                            
                            bsCollapsePanel("Datos zona y captura",
                                            fluidRow(column(12, offset = 0.5, 
                                                            radioButtons ("zonac", "", c("Manual" = "manu", "Automatico"="auto")),
                                                            conditionalPanel(
                                                              condition = "input.zonac == 'manu'",
                                                              div(style="display: inline-block;vertical-align:top; width: 200px;",numericInput("latc", "Latitutd", value = "")),
                                                              div(style="display: inline-block;vertical-align:top; width: 200px;",numericInput("lonc", "Longitud", value = ""))
                                                            ),
                                                            conditionalPanel(
                                                              condition = "input.zonac == 'auto'",
                                                              leafletOutput("mapc", height = "400px"),
                                                              br(), br(),
                                                              div(style="display: inline-block;vertical-align:top; width: 200px;",verbatimTextOutput("latAc")),
                                                              div(style="display: inline-block;vertical-align:top; width: 200px;",verbatimTextOutput("lonAc"))
                                                            )
                                            )),
                                            fluidRow(column(12, offset = 0.5, 
                                                            #div(style="display: inline-block;vertical-align:top; width: 200px;", selectInput("espeC","Especie", c("Hegaluze","Lubina", "Calamar"), selected = NULL)),
                                                            div(style="display: inline-block;vertical-align:top; width: 300px;", selectInput("espeCc","Especie", speciesList, selected = NULL)),
                                                            div(style="display: inline-block;vertical-align:top; width: 200px;", selectInput("tipoCc","Tipo", c("Retenida", "Devuelta"), selected = NULL))
                                            )),
                                            fluidRow(column(12, offset = 0.5, 
                                                            div(style="display: inline-block;vertical-align:top; width: 200px;", numericInput ("pesoCc", "Peso captura total (Kg)", value = 0,  min = 0, max = 5000)),
                                                            div(style="display: inline-block;vertical-align:top; width: 200px;", numericInput("nindCc", "Numero total individuos", value = 0,  min = 0, max = 5000))
                                            )),
                                            hr(),
                                            # Add button
                                            div(style="display: inline-block;margin-top: 25px;",
                                                actionButton(inputId = "add.buttonc", label = "Anadir", icon =  icon("plus"))), 
                                            # Delete button 
                                            div(style="display: inline-block;margin-top: 25px;",
                                                actionButton(inputId = "delete.buttonc", label = "Borrar", icon = icon("minus"))),
                                            # Row selection
                                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                numericInput(inputId = "row.selectionc", label = "Fila a borrar", min = 1, max = 100, value = ""))
                            ), 
                            bsCollapsePanel("Datos tallas",
                                            #div(style="display: inline-block;vertical-align:top; width: 200px;",selectInput("espeTc","Especie", c("Hegaluze","Lubina", "Calamar"), selected = NULL)),
                                            div(style="display: inline-block;vertical-align:top; width: 300px;",selectInput("espeTc","Especie", speciesList, selected = NULL)),
                                            div(style="display: inline-block;vertical-align:top; width: 200px;",selectInput("tipoTc","Tipo", c("Retenida", "Devuelta"), selected = NULL)),
                                            div(style="display: inline-block;vertical-align:top; width: 200px;",numericInput("tallaTc", "Talla individual (cm)", value = 0,  min = 0, max = 5000)), 
                                            hr(),
                                            # Add button
                                            div(style="display: inline-block;margin-top: 25px;",
                                                actionButton(inputId = "add.buttonTc", label = "Anadir", icon = icon("plus"))), 
                                            # Delete button 
                                            div(style="display: inline-block;margin-top: 25px;",
                                                actionButton(inputId = "delete.buttonTc", label = "Borrar", icon = icon("minus"))),
                                            # Row selection
                                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                numericInput(inputId = "row.selectionTc", label = "Fila a borrar", min = 1, max = 100, value = ""))#,
                                            # Send button
                                            # div(style="display: inline-block;margin-top: 25px;width: 150px;",
                                            #     actionButton(inputId = "send.buttonT", label = "Enviar", icon = 
                                            #                    icon("paper-plane"),  class = "btn-primary"))
                            )
                 ), # end of the bscollapse
                 # Send button
                 # div(style="display: inline-block;margin-top: 25px;width: 150px;",
                 #     actionButton(inputId = "new.button", label = "Nueva marea", icon = 
                 #                     icon("servicestack"),  class = "btn-primary")),
                 div(style="display: inline-block;margin-top: 25px;width: 150px;",
                     actionButton(inputId = "download.buttonc", label = "Descargar datos", icon = icon("cloud-download-alt"),  class = "btn-primary")),
                 div(style="display: inline-block;margin-top: 25px;width: 150px;",
                     actionButton(inputId = "send.buttonc", label = "Enviar datos", icon = icon("paper-plane"),  class = "btn-primary"))
          ),
          column(6, 
                 tabsetPanel(
                   tabPanel ("Datos de la marea y captura",
                             br(),br(),
                             tableOutput("tableMc"), 
                             br(), br(),
                             tableOutput("tableCc")
                   ),
                   tabPanel ("Datos de tallas",
                             br(),br(),
                             tableOutput("tableTc"),
                             br(),br(),
                             tableOutput("tableFc")
                   )
                 ) # end of the tabsetPanel
          )
        )
    )
  })
  
  
  ##--------------
  ## Se refresca la datos originales
  ##-------------- 
  
  #default db values
  observeEvent (input$new.button,{
    values$df <- ret
    valuesT$dfT <- tal
  })
  
  ##--------------
  ## Se refresca mapa
  ##-------------- 
  
  # # clean markers
  # observeEvent(input$new.button,{
  #    leafletProxy('map')%>% clearMarkers()
  # })
  
  
} # end of the server