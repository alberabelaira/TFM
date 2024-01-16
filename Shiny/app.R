library(shiny)
library(caret)
library(glmnet)

load("optimal_model.RData") # Cargamos el objeto mod con el modelo entrenado
load("preProcess.RData")# Cargamos los parámetros para preprocesar los datos

info_variables <- data.frame(
  Variable = c("Edad", "Forma", "Margen", "Densidad"),
  Descripción = c("Edad del sujeto", "Forma de la tumoración", "Margen de la tumoración", "Densidad de la tumoración"),
  Tipo = c("Numérica", rep("Categórica", 3)),
  Rango = c("18 - 96", "1 - 4", "1 - 5", "1 - 4")
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
            body { 
            background-image: url('61802.jpg');
            background-size: cover;  /* Asegura que la imagen cubra toda la página */
            background-repeat: no-repeat;
            background-attachment: fixed;
            font-size: 18px;
            }
            .my-custom-class {
                 color: #0044cc;  /* Color personalizado */
            }
            .img-2 {
                width: 700px;  /* Ancho de la imagen */
                border-radius: 8px;  /* Bordes redondeados */
            }
            .img {
                width: 700px;  /* Ancho deseado del recorte */
                overflow: hidden;
                border-radius: 8px;  /* Bordes redondeados */
                margin-right: 100px;
            }
            .img, .img-2 {
            max-width: 80%;  /* Asegura que las imágenes no sean más grandes que su contenedor */
            height: auto;     /* Mantiene la relación de aspecto de las imágenes */
            margin-bottom: 20px;
            }
            .btn{
                background-color: #ccc2d0;  /* Color de fondo para los botones */
                color: black;  /* Color del texto en los botones */
            }
            .form-control {
                border-color: blue;  /* Color del borde de los campos de entrada */
            }
            .shiny-output-error { color: #000; }
            .shiny-output-error:before { content: 'Error: '; }
            .custom-help-text {
                font-size: 16px;  /* Ajusta el tamaño de la fuente */
                margin: 5px 0;  /* Ajusta el margen */
            }
            #prev_tab {
               position: absolute;
               bottom: 20px;
               left: 20px;
               width: 150px;  /* Ajusta el ancho*/
               height: 65px;  /* Ajusta la altura*/
            }
            #next_tab {
               position: absolute;
               bottom: 20px;
               right: 20px;
               width: 125px;  /* Ajusta el ancho*/
               height: 65px;  /* Ajusta la altura*/
            }
             #inputPanel, #inputPanel > .form-group {
              text-align: center;
              margin: 0 auto;
            }
            .shiny-numeric-input input[type='number'] {
            text-align: center;
            }
            .shiny-input-container {
            margin: auto;
            width: 150%;
            height: 150%;
            text-align: center;
            }
            .shiny-input-container > label {
            display: block;
            text-align: center;
            }
            .form-control {
            width: 100%;
            box-sizing: border-box; /* Asegura que el padding no afecte el ancho */
            }
            #submit {
            margin-left: auto; /* Centra el botón horizontalmente */
            margin-right: auto; /* Centra el botón horizontalmente */
            margin-top: 25px; /* Espacio arriba del botón */
            height: 100x;
            width: 300px;
            display: block;
            font-size: 25px;
            }
            .nav-tabs {
            background-color: #f8f9fa; /* Color de fondo de las pestañas */
            border-bottom: 2px solid #dee2e6; /* Color del borde inferior */
            }
            .nav-tabs .nav-item.show .nav-link, .nav-tabs .nav-link.active {
            color: #495057; /* Color de texto para la pestaña activa */
            background-color: #e9ecef; /* Color de fondo para la pestaña activa */
            border-color: #dee2e6 #dee2e6 #f8f9fa; /* Colores de borde */
            }
            .nav-link {
            border: 1px solid transparent; /* Borde transparente por defecto para las pestañas */
            border-radius: 0.25rem; /* Redondez de las esquinas */
            }
            .nav-tabs > li {
            width: 33.33%;
            text-align: center;
            }
            .nav-tabs > li > a {
            margin-right: 0px;
            }
      
        "))
  ),
  
  titlePanel(
    div(
      style = "background-color: #007bff; color: white; padding: 20px; border-radius: 8px; text-align: center;",
      "Aplicación para la predicción de cáncer de mama a través de características de mamografías"
    ),
    windowTitle = "Predicción Cáncer de Mama"
  ),
  
  tags$style(HTML(".text-action-link {color: blue; cursor: pointer;}")),
  
  tabsetPanel(id = "tabs",  # Añadir un ID aquí
              tabPanel("Información", 
                       h1("Aquí encontrarás información importante sobre la aplicación y el modelo implementado", style = "text-align: center;"),
                       p("Esta aplicación es el resultado de un proyecto de investigación desarrollado como parte del Trabajo de Fin de Máster en Bionformática y Bioestadística en la UOC en el área de Estadística y aprendizaje automático. Su objetivo es demostrar la aplicación práctica de técnicas avanzadas de análisis de datos en el campo de la predicción de cáncer de mama."),
                       p(a(href = "https://github.com/albertoreyabelaira/TFM", target = "_blank", "Haz clic aquí para acceder al repositorio del proyecto y obtener más detalles sobre el TFM.")),
                       div(HTML(paste("El modelo de clasificación implementado en esta aplicación utiliza la clasificación basada en Regresión Logística para predecir si una tumoración mamaria es benigna o maligna. Este modelo fue entrenado y validado utilizando un conjunto de datos del repositorio ",
                                      a(href = "https://archive.ics.uci.edu/ml/datasets/mammographic+mass", "UCI", target = "_blank"), 
                                      ", recopilados en el estudio ", "<i>", "'The prediction of breast cancer biopsy outcomes using two CAD approaches that both emphasize an intelligible decision process'", "</i>", 
                                      a(href = "https://doi.org/10.1118/1.2786864", " (doi:10.1118/1.2786864).", target = "_blank"),
                                      " La predicción que se obtiene con este modelo tiene una precisión del 83.5%, una sensibilidad y especificidad del 84% y 83% respectivamente, y un valor predictivo positivo del 80.4%. En las imágenes siguientes se muestran los gráficos de rendimiento del modelo:")
                       )),
                       tags$br(),
                       div(style = "display: flex; justify-content: center; text-align: center;",  # Contenedor para centrar las imágenes
                           img(src = "rplot1.png", class = "img"),  # Ajusta las dimensiones según necesites
                           img(src = "rplot2.png", class = "img-2")
                       ),
                       tags$p(
                         "Haz clic ",
                         actionLink("Instrucciones", "aquí", class = "text-action-link"),
                         " para detalles sobre cómo obtener una predicción. En la siguiente tabla puedes consultar los parámetros de entrada."
                       ),
                       tableOutput("tablaValores"),
                       p(paste("","", sep = "\n")),
                       div(style = "display: flex; justify-content: space-between; align-items: center;",  # Contenedor para alinear los elementos
                           div(  # Contenedor para el contenido de la izquierda
                             HTML(paste("© 2024, Alberto Rey Abelaira - ", 
                                        a(href = "mailto:albertorey@uoc.edu", 
                                          "albertorey@uoc.edu", target = "_blank")))
                           ),
                           a(href = "https://www.freepik.es/vector-gratis/diseno-plantilla-papel-tapiz-medica-abstracta_3439405.htm#query=medicine&position=27&from_view=search&track=sph&uuid=abdfb367-fc1a-4824-8646-ff2956c4a42a", target = "_blank", "Imagen de rawpxel.com en Freepik"),
                       ),
              ),
              tabPanel("Clasificación",
                       div(style = "display: flex; justify-content: center; flex-direction: column; font-size: 13pt;",
                           id = "inputPanel",  # Contenedor para centrar los elementos
                           uiOutput("ageInput"),
                           selectInput("Shape", "Forma del tumor (BIRADS)",
                                       choices = c("Selecciona una opción" = "",
                                                   "Redondeada (1)" = 1, 
                                                   "Oval (2)" = 2,
                                                   "Lobular (3)" = 3,
                                                   "Irregular (4)" = 4),
                                       selected = ""),
                           selectInput("Margin", "Margen del tumor (BIRADS)",
                                       choices = c("Selecciona una opción" = "",
                                                   "Circunscrito (1)" = 1, 
                                                   "Microlobulado (2)" = 2,
                                                   "Indistinto (3)" = 3,
                                                   "Mal definido (4)" = 4,
                                                   "Espiculado (5)" = 5),
                                       selected = NA),
                           selectInput("Density", "Densidad del tumor (BIRADS)",
                                       choices = c("Selecciona una opción" = "",
                                                   "Alta (1)" = 1, 
                                                   "Iso (2)" = 2,
                                                   "Baja (3)" = 3,
                                                   "Contiene grasa (4)" = 4),
                                       selected = ""),
                           actionButton("submit", "Obtener clasificación")
                       ),
                       textOutput("inputSummary"),  # Resumen de los inputs
                       mainPanel(
                         textOutput("prediction"),
                         textOutput("additionalText")
                       )
              ),
              tabPanel("Resultados",
                       h2("Resultado de la Predicción"),
                       verbatimTextOutput("result"),
                       verbatimTextOutput("detailedResult"),
                       # Texto detallado con saltos de línea
                       h3(icon("exclamation-triangle"), HTML("<i> Información importante </i>")),
                       HTML("
                        <ul>
                          <li><i>La clasificación obtenida es una estimación basada en un algoritmo y no debe sustituir al consejo médico profesional.</i></li>
                          <li><i>Recuerda que esta herramienta es solo para fines demostrativos y en ningún caso puede usarse como un diagnóstico médico.</i></li>
                       </ul>
                       ")
              )
  ),
  uiOutput("myNumericInput"),
)


server <- function(input, output, session) {
  
  # Crear el numericInput inicialmente vacío
  output$ageInput <- renderUI({
    numericInput("Age", "Edad", value = "", min = 18, max = 96)
  })
  
  output$tablaValores <- renderTable(info_variables)
  
  currentTab <- reactiveVal("Información")
  
  result <- reactiveVal()
  
  # Creamos una expresión reactiva para manejar los datos del usuario
  user_data <- reactive({
    req(input$submit)  # Nos aseguramos de que se pulse el botón
    data.frame(
      Age = as.integer(input$Age),
      Shape = as.integer(input$Shape),
      Margin = as.integer(input$Margin),
      Density = as.integer(input$Density)
    )
  })
  
  observeEvent(input$start, {
    updateTabsetPanel(session, "tabs", selected = "Clasificación")
    currentTab("Clasificación")  # Actualiza la pestaña actual
  })
  
  observeEvent(input$Instrucciones, {
    showModal(modalDialog(
      title = "Detalles del funcionamiento de la aplicación",
      "Se recomienda leer los detalles del modelo en el TFM desarrollado",
      HTML("
        <ol>
          <li>Acceder a la pestaña de clasificación</li>
          <li>Introducir los parámetros del tumor en el panel de entrada</li>
          <li>Pulsar el botón 'Obtener predicción' para acceder al resultado</li>
          <li>Visualizar los resultados de la predicción</li>
        </ol>
      "),
      easyClose = TRUE,
      footer = modalButton("Cerrar")
    ))
  })
  
  observeEvent(input$submit, {
    if(!is.numeric(input$Age) || input$Age < 18 || input$Age > 96 ||
       input$Shape == "" || input$Margin == "" || input$Density == "") {
      # Mostrar una ventana emergente de error
      showModal(modalDialog(
        title = "Datos Incorrectos",
        "Por favor, asegúrese de ingresar datos correctos en todos los campos.",
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
      return() # Detener la ejecución adicional del código de este evento
    }
    # Procesamos los datos y realizar la predicción
    if (is.numeric(input$Age) && input$Age >= 18 && input$Age <= 96 &&
        input$Shape != "" && input$Margin != "" && input$Density != "") {
      user_data <- data.frame(
        Age = as.integer(input$Age),
        Shape = factor(input$Shape, levels = 1:4),
        Margin = factor(input$Margin, levels = 1:5),
        Density = factor(input$Density, levels = 1:4)
      )
      user_data_OH <- predict(dummies, user_data)
      user_data_OH <- predict(preProcessAge, user_data_OH)
      prediction <- predict(mod, newdata = user_data_OH)
      result(paste("Para los siguientes parámetros de entrada:",
                   "\nEdad: ", input$Age,
                   "\nForma del tumor: ", input$Shape,
                   "\nMargen del tumor: ", input$Margin,
                   "\nDensidad del tumor: ", input$Density,
                   "\nLa clasificación es: ", ifelse(prediction == 1, "Maligno", "Benigno"),
                   sep = ""))
    }
    
    # Actualizamos la pestaña de resultados y mostramos el resultado
    updateTabsetPanel(session, "tabs", selected = "Resultados")
    currentTab("Resultados")  # Actualiza la pestaña actual
    
    # Mostramos el resultado detallado
    output$result <- renderText({
      result()
    })
    
    if (input$tabs == "Clasificación") {
      output$ageInput <- renderUI({
        numericInput("Age", "Edad", value = "", min = 18, max = 96)
      })
      # Resetear también los otros inputs
      updateSelectInput(session, "Shape", selected = "")
      updateSelectInput(session, "Margin", selected = "")
      updateSelectInput(session, "Density", selected = "")
    }
  })
  
  # Inicializamos la pestaña actual
  session$userData$currentTab <- "Información"
  
}

shinyApp(ui = ui, server = server)