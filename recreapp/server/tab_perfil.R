# ----------------------------------- #
# Tab perfil                          #
# ----------------------------------- #

# -------------------------- #
# Inputs perfil              #
# -------------------------- #


dat_per <- reactive({
  per
  per$Edad <-input$edad
  per$Genero <- as.character(input$gene)
  per$Experiencia <- as.character(input$expe)
  per$Avidez <- as.character(input$avid)
  #per$user <- input$pass
  per
})


# -------------------------- #
# Envio perfil a googledrive #
# -------------------------- #

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

