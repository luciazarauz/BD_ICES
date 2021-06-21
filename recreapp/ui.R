## ---------------------------------------------------- ##
## IM19RECLUB                                           ##
## Diario de pesca recreativa - Shiny                   ##
## Feb. 2020                                            ##
## AZTI (@mkorta)                                       ##
## ---------------------------------------------------- ##

##--------------
## ui
##--------------

ui <- fluidPage(
  
  useShinyjs(),
  
  fluidRow(column(4, offset=4,
                  
                  ##--------------
                  ## Panel de password
                  ##--------------
                  
                  div(id="passScreen",
                      br(),br(),br(),
                      wellPanel(
                        #h2(span("Diario de pesca recreativa", align = "center", style = "color:black")),
                        #h2("Diario de pesca recreativa", align = "center"),
                        br(), 
                        includeHTML ("data/DescriptionClave.txt"),
                        # p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, 
                        #   sed eiusmod tempor incidunt ut labore et dolore magna aliqua. 
                        #   Ut enim ad minim veniam, quis nostrud exercitation ullamco 
                        #   laboris nisi ut aliquid ex ea commodi consequat."),
                        hr(),
                        #passwordInput("pass","Introduce tu clave",""),
                        textInput("user",label = tags$div(HTML('<i class="fa fa-user-circle" style = "color:#000000;"></i> Usuario:'))),
                        passwordInput("pass", label = tags$div(HTML('<i class="fa fa-lock" style = "color:#000000;"></i> clave:'))),
                        div(style="text-align:center",actionButton("check","Validar", icon = icon("check-circle"),class = "btn-primary")),
                        br(),
                        br(),
                        p("Si has olvidado tu nombre de usuario o clave", style = "font-size:15px",
                          a("emugerza@azti.es",  href = "mailto:emugerza@azti.es"))
                      )
                  )
  )),
  
  ##--------------
  ## App
  ##--------------
  
  shinyjs::hidden(
    div(id="myapp",
        
        #theme = shinytheme("spacelab"),
        headerPanel(
          # HTML("   <table style='width:100%'>
          #          <tr>
          #          <td style='width:5%;padding-right:3%'>
          #          <img src='potera2.png', alt='', height='100px'>
          #          </td>
          #          <td style='width:95%'>
          #          <div style='font-size:40px'> Diario de pesca recreativa</div>
          #          </td>  
          #          </tr>
          #          </table>
          #          <hr>"
          # ),
          
          tagList(
            img(src = "cana.png", height = 120),
            span("Diario de pesca recreativa", 
                 span(actionButton('eus_inputs', 'EUS'),
                      actionButton('cas_inputs', 'CAS'),
                      bsTooltip("eus_inputs", "Euskarazko bertsioa laister izango duzu, mila esker!",
                                "left", options = list(container = "body")),
                      style = "position:absolute;top:2em;right:2em;")
            ),
            hr(),
          ),
          windowTitle="RecreAPP"
        ),
        #HTML('<left><img src="tuna_2.png"></left>'),
        #HTML("<br><br><br><br>"),
        #hr(),
        
        
        ##--------------
        ## Panel de pestanas
        ##--------------
        
        tabsetPanel(type="pills",
                    
                    ##--------------
                    ## Panel sobre
                    ##--------------
                    
                    tabPanel("La App",
                             #HTML("<br><br><br>"),
                             br(),br(),br(),br(),
                             includeHTML ("data/DescriptionApp.txt"),
                             #HTML("<br><br><br><br><hr>"),
                             br(),br(),br(),br(),
                             hr(),
                             br(),
                             HTML('<center><img src="allLogos.png"></center>')
                    ),
                    
                    ##--------------
                    ## panel mi perfil
                    ##--------------
                    
                    
                    tabPanel("Mi perfíl", 
                             fluidRow(
                               column(4, offset = 4,
                                      br(),
                                      br(),
                                      wellPanel(
                                        textInput("edad", "EDAD"),
                                        selectInput("gene","GENERO", c("","Femenino", "Masculino", "No binario", "No deseo responder"), selected = NULL),
                                        numericInput("expe", "AÑOS DE EXPERIENCIA", 1),
                                        radioButtons("avid", "AVIDEZ", choices = list("0-25 días/año" = 1, "26-50 días/año" = 2, "51-75 días/año" = 3, "76-100 días/año" = 4, "> 100 días/año" = 5)),
                                        div(style="text-align:center",actionButton("saveP", "Enviar perfíl", icon = icon ("paper-plane"), class = "btn-primary")),
                                        bsTooltip("saveP", "Si ya has enviado tu perfíl en otra ocasión, no es necesario que lo envíes de nuevo.",
                                                  "left", options = list(container = "body")),
                                      )
                               )
                             )
                    ),
                    
                    ##--------------
                    ## Panel marea
                    ##--------------
                    
                    tabPanel("Marea",
                             uiOutput("resetable_input"),
                             hr(),
                             fluidRow(column(6,
                                             div(style="display: inline-block;margin-top: 25px;width: 150px;",
                                                 actionButton(inputId = "new.button", label = "Nueva marea", icon = 
                                                                icon("servicestack"),  class = "btn-primary"))
                             ))
                    ), # end of tabPanel
                    
                    ##--------------
                    ## Panel competiciones
                    ##--------------
                    
                    tabPanel("Campeonato",
                             uiOutput("comp_input")
                    ),
                    
                    ##--------------
                    ## Panel estadistica
                    ##--------------
                    
                    tabPanel("Estadísticas", 
                             br(), br(),
                             HTML('<center><img src="wk.png"></center>'),
                             p("Próximamente podrá visualizar la evolución de sus capturas. Estamos trabajando en ello.", style = "font-size:15px; text-align: center"))
                    
        ) # end of the tabsetPanel
    ) # end of div
  ) # end of hidden 
) # end of fluidPage