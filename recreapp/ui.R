## ---------------------------------------------------- ##
## IM21SEPRECREA                                        ##
## Diario de pesca recreativa - Shiny                   ##
## Jun  2020                                            ##
## AZTI (@mkorta)                                       ##
## UPDATES                                              ##
## ---------------------------------------------------- ##

##--------------
## ui
##--------------

ui <- fluidPage(
  
  # title in browser need to add icon 
  windowTitle="RecreAPP",
  
  # background image
  setBackgroundImage(src = "cana.png"),

  # titulua
  headerPanel(
          
    tagList(
          span("Diario de pesca recreativa", 
          span(actionButton('eus_inputs', 'EUS'),
               actionButton('cas_inputs', 'CAS'),
               bsTooltip("eus_inputs", "Euskarazko bertsioa laister izango duzu, mila esker!",
                                "left", options = list(container = "body")),
                            style = "position:absolute;top:1em;right:2em;")
            ),
            hr(),
          )),
  sidebarPanel(
    includeHTML ("data/DescriptionApp.txt")
    ),
  


        ##--------------
        ## Panel de pestanas
        ##--------------
        
        # tabsetPanel(type="pills",
        #             
        #             ##--------------
        #             ## Panel sobre
        #             ##--------------
        #             
        #             tabPanel("La App",
        #                      #HTML("<br><br><br>"),
        #                      br(),br(),br(),br(),
        #                      includeHTML ("data/DescriptionApp.txt"),
        #                      #HTML("<br><br><br><br><hr>"),
        #                      br(),br(),br(),br(),
        #                      hr(),
        #                      br(),
        #                      HTML('<center><img src="allLogos.png"></center>')
        #             ),
        #             
        #             ##--------------
        #             ## panel mi perfil
        #             ##--------------
        #             
        #             
        #             tabPanel("Mi perfíl", 
        #                      fluidRow(
        #                        column(4, offset = 4,
        #                               br(),
        #                               br(),
        #                               wellPanel(
        #                                 textInput("edad", "EDAD"),
        #                                 selectInput("gene","GENERO", c("","Femenino", "Masculino", "No binario", "No deseo responder"), selected = NULL),
        #                                 numericInput("expe", "AÑOS DE EXPERIENCIA", 1),
        #                                 radioButtons("avid", "AVIDEZ", choices = list("0-25 días/año" = 1, "26-50 días/año" = 2, "51-75 días/año" = 3, "76-100 días/año" = 4, "> 100 días/año" = 5)),
        #                                 div(style="text-align:center",actionButton("saveP", "Enviar perfíl", icon = icon ("paper-plane"), class = "btn-primary")),
        #                                 bsTooltip("saveP", "Si ya has enviado tu perfíl en otra ocasión, no es necesario que lo envíes de nuevo.",
        #                                           "left", options = list(container = "body")),
        #                               )
        #                        )
        #                      )
        #             ),
        #             
        #             ##--------------
        #             ## Panel marea
        #             ##--------------
        #             
        #             tabPanel("Marea",
        #                      uiOutput("resetable_input"),
        #                      hr(),
        #                      fluidRow(column(6,
        #                                      div(style="display: inline-block;margin-top: 25px;width: 150px;",
        #                                          actionButton(inputId = "new.button", label = "Nueva marea", icon = 
        #                                                         icon("servicestack"),  class = "btn-primary"))
        #                      ))
        #             ), # end of tabPanel
        #             
        #             ##--------------
        #             ## Panel competiciones
        #             ##--------------
        #             
        #             tabPanel("Campeonato",
        #                      uiOutput("comp_input")
        #             ),
        #             
        #             ##--------------
        #             ## Panel estadistica
        #             ##--------------
        #             
        #             tabPanel("Estadísticas", 
        #                      br(), br(),
        #                      HTML('<center><img src="wk.png"></center>'),
        #                      p("Próximamente podrá visualizar la evolución de sus capturas. Estamos trabajando en ello.", style = "font-size:15px; text-align: center"))
        #             
        # ) # end of the tabsetPanel
  # ) # end of div
  #  ) # end of hidden 
) # end of fluidPage