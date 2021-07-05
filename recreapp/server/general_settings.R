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



##--------------
## UI MAREA: Se refresca la interface a valores originales
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
                                                            div(style="display: inline-block;vertical-align:top; width: 200px;",numericInput("lat", "Latitud", value = "")),
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
