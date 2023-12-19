# Instala e inicia Shiny si no lo has hecho
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

# Carga el paquete shiny
library(shiny)

# Define la base de datos inicial de pacientes
pacientes <- data.frame(
  ID = integer(),
  NOMBRE = character(),
  PAIS = character(),
  CIUDAD = character(),
  CEL = integer(),
  ESTADO = character(),
  TERAPIA = character(),
  FRECUENCIA = character(),
  stringsAsFactors = FALSE
)

# Define la interfaz de usuario
ui <- fluidPage(
  titlePanel("Gestión de Pacientes"),
  sidebarLayout(
    sidebarPanel(
      textInput("id", "ID (Cedula):", ""),
      textInput("nombre", "Nombre:", ""),
      textInput("pais", "País:", ""),
      textInput("ciudad", "Ciudad:", ""),
      numericInput("cel", "Contacto:", value = NULL),
      textInput("estado", "Estado:", ""),
      textInput("terapia", "Terapia:", ""),
      textInput("frecuencia", "Frecuencia:", ""),
      actionButton("registrar", "Registrar Paciente"),
      br(),
      br(),
      actionButton("modificar", "Modificar Información"),
      br(),
      br(),
      actionButton("verificar", "Verificar Información")
    ),
    mainPanel(
      tableOutput("pacientesTable")
    )
  )
)

# Define el servidor
server <- function(input, output) {
  # Almacena la información de los pacientes
  pacientes_data <- reactiveVal(pacientes)
  
  # Observa el evento de registrar un paciente
  observeEvent(input$registrar, {
    # Valida que ID y NOMBRE no estén vacíos
    if (input$id != "" && input$nombre != "") {
      # Crea un nuevo paciente con la información proporcionada
      nuevo_paciente <- data.frame(
        ID = as.integer(input$id),
        NOMBRE = input$nombre,
        PAIS = input$pais,
        CIUDAD = input$ciudad,
        CEL = as.integer(input$cel),
        ESTADO = input$estado,
        TERAPIA = input$terapia,
        FRECUENCIA = input$frecuencia,
        stringsAsFactors = FALSE
      )
      # Combina el nuevo paciente con la base de datos existente
      pacientes_data(rbind(pacientes_data(), nuevo_paciente))
    } else {
      # Si ID o NOMBRE están vacíos, muestra un mensaje de error
      showModal(modalDialog(
        title = "Error",
        "Debe completar tanto el ID como el Nombre para registrar al paciente."
      ))
    }
  })
  
  # Observa el evento de modificar información
  observeEvent(input$modificar, {
    # Valida que ID no esté vacío
    if (input$id != "") {
      # Busca al paciente por ID
      paciente_seleccionado <- pacientes_data() %>%
        filter(ID == as.integer(input$id))
      
      # Si se encuentra, modifica la información
      if (nrow(paciente_seleccionado) > 0) {
        modificar_info <- updateTextInput(session, "nombre", value = paciente_seleccionado$NOMBRE)
        modificar_info <- updateTextInput(session, "pais", value = paciente_seleccionado$PAIS)
        modificar_info <- updateTextInput(session, "ciudad", value = paciente_seleccionado$CIUDAD)
        modificar_info <- updateNumericInput(session, "cel", value = paciente_seleccionado$CEL)
        modificar_info <- updateTextInput(session, "estado", value = paciente_seleccionado$ESTADO)
        modificar_info <- updateTextInput(session, "terapia", value = paciente_seleccionado$TERAPIA)
        modificar_info <- updateTextInput(session, "frecuencia", value = paciente_seleccionado$FRECUENCIA)
      } else {
        # Si no se encuentra, muestra un mensaje de error
        showModal(modalDialog(
          title = "Error",
          "Paciente no encontrado."
        ))
      }
    }
  })
  
  # Observa el evento de verificar información
  observeEvent(input$verificar, {
    # Valida que ID no esté vacío
    if (input$id != "") {
      # Busca al paciente por ID
      paciente_seleccionado <- pacientes_data() %>%
        filter(ID == as.integer(input$id))
      
      # Si se encuentra, muestra la información
      if (nrow(paciente_seleccionado) > 0) {
        showModal(modalDialog(
          title = "Información del Paciente",
          paste("ID: ", paciente_seleccionado$ID, "\n"),
          paste("Nombre: ", paciente_seleccionado$NOMBRE, "\n"),
          paste("País: ", paciente_seleccionado$PAIS, "\n"),
          paste("Ciudad: ", paciente_seleccionado$CIUDAD, "\n"),
          paste("Contacto: ", paciente_seleccionado$CEL, "\n"),
          paste("Estado: ", paciente_seleccionado$ESTADO, "\n"),
          paste("Terapia: ", paciente_seleccionado$TERAPIA, "\n"),
          paste("Frecuencia: ", paciente_seleccionado$FRECUENCIA, "\n")
        ))
      } else {
        # Si no se encuentra, muestra un mensaje de error
        showModal(modalDialog(
          title = "Error",
          "Paciente no encontrado."
        ))
      }
    }
  })
  
  # Actualiza la tabla de pacientes
  output$pacientesTable <- renderTable({
    pacientes_data()
  })
}

# Ejecuta la aplicación Shiny
shinyApp(ui, server)
