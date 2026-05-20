

library(rsconnect)
library(treemapify)
library(ggbrick)
library(ggfittext)
library(treemap)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(haven)
library(DT)
library(plotly)
library(rsconnect)


huancayo <- read_spss("huancayo2022.sav")


huancayo <- huancayo %>%
  mutate(
    across(where(is.numeric), ~ round(., 0)),
    TAMANO_EMPRESA = case_when(
      PERSONAL_OCUPADO <= 10 ~ "Micro (1-10)",
      PERSONAL_OCUPADO <= 50 ~ "Pequeña (11-50)",
      PERSONAL_OCUPADO <= 250 ~ "Mediana (51-250)",
      PERSONAL_OCUPADO > 250 ~ "Grande (250+)",
      TRUE ~ "Sin dato"
    )
  )


huancayo <- huancayo %>%
  mutate(
    INNOVA = ifelse(as.character(C2P11) == "1", "Sí innova", "No innova"),
    USA_INTERNET = ifelse(as.character(C4P4_N) == "1", "Sí", "No")
  )


#################################################################################
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Dashboard Económico - Huancayo 2022"),
  
  dashboardSidebar(
    selectInput("filtro_distrito", "Seleccionar Distrito:",
                choices = c("Todos", sort(unique(huancayo$DISTRITO))),
                selected = "Todos"),
    selectInput("filtro_sector", "Seleccionar Sector:",
                choices = c("Todos", sort(unique(huancayo$ACT_EC_DESC))),
                selected = "Todos"),
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Resumen General", tabName = "resumen", icon = icon("chart-line")),
      menuItem("Estructura por Tamaño", tabName = "tamano", icon = icon("building")),
      menuItem("Sectores y Distritos", tabName = "sectores", icon = icon("map-marker-alt")),
      menuItem("Empleo e Innovación", tabName = "empleo", icon = icon("users")),
      menuItem("Datos", tabName = "datos", icon = icon("table"))
    )
  ),
  
  dashboardBody(tags$head(tags$style(HTML('
  .skin-black .main-header .logo { background-color: #8B7355; }
  .skin-black .main-header .navbar { background-color: #8B7355; }
  .skin-black .main-sidebar { background-color: #f5f5dc; }
  .skin-black .main-sidebar .sidebar .sidebar-menu a { color: #5c4033; }
  .skin-black .main-sidebar .sidebar .sidebar-menu a:hover { background-color: #e8d5b7; }
  .content-wrapper, .right-side { background-color: #faf0e6; }
  .box { background-color: #fffaf0; border-top: 3px solid #8B7355; }
  .control-label { color: #5c4033; }
  .form-control { background-color: #f5e6c8; color: #5c4033; border-color: #8B7355; }
  .selectize-input { background-color: #f5e6c8 !important; color: #5c4033 !important; }
  .selectize-input .item { color: #5c4033 !important; }
  .selectize-dropdown { background-color: #f5e6c8 !important; color: #5c4033 !important; }
  .selectize-dropdown .active { background-color: #e8d5b7 !important; }
  .selectize-control.single .selectize-input:after { border-color: #5c4033 transparent transparent transparent; }
  ..box.box-primary { border-top-color: #8B7355 !important; }
.box.box-primary .box-header { background-color: #8B7355 !important; color: white !important; }
.box.box-primary { border-top-color: #8B7355 !important; border: none !important; }
.box.box-primary .box-header { background-color: #8B7355 !important; color: white !important; }
.box.box-primary .box-body { background-color: #fffaf0 !important; }
.box { border-top-color: #8B7355 !important; background-color: #fffaf0 !important; }
.box .box-header { background-color: #8B7355 !important; color: white !important; }
.box .box-body { background-color: #fffaf0 !important; }
.box-primary { border-top-color: #8B7355 !important; }
.box-success { border-top-color: #8B7355 !important; }
.box-info { border-top-color: #8B7355 !important; }
.box-warning { border-top-color: #8B7355 !important; }
.box-danger { border-top-color: #8B7355 !important; }
.box { border: 1px solid #8B7355 !important; border-top: 3px solid #8B7355 !important; background-color: #fffaf0 !important; }
.box .box-header { background-color: #8B7355 !important; color: white !important; border-bottom: 1px solid #8B7355 !important; }
.box .box-body { background-color: #fffaf0 !important; }
.box-primary { border: 1px solid #8B7355 !important; border-top: 3px solid #8B7355 !important; }
.box-success { border: 1px solid #8B7355 !important; border-top: 3px solid #8B7355 !important; }
.box-info { border: 1px solid #8B7355 !important; border-top: 3px solid #8B7355 !important; }
.box-warning { border: 1px solid #8B7355 !important; border-top: 3px solid #8B7355 !important; }
.box-danger { border: 1px solid #8B7355 !important; border-top: 3px solid #8B7355 !important; }
'))),
    tabItems(
      
      tabItem(tabName = "inicio",
              fluidRow(
                box(title = "Dashboard Económico de Huancayo 2022", status = "primary", solidHeader = TRUE, width = 12,
                    h4("¿Qué es este dashboard?"),
                    p("Este dashboard interactivo presenta los principales indicadores económicos de las empresas en la provincia de Huancayo, basado en el Censo Nacional Económico 2022 del INEI."),
                    
                    br(),
                    h4("¿Qué puedes encontrar?"),
                    tags$ul(
                      tags$li(strong("Resumen General:"), " KPIs principales (empresas, empleos, ventas, valor agregado), ventas por sector y por distrito."),
                      tags$li(strong("Estructura por Tamaño:"), " Comparativa entre micro, pequeña, mediana y grande empresa en cantidad, ventas, valor agregado y empleos."),
                      tags$li(strong("Sectores y Distritos:"), " Análisis detallado de ventas y valor agregado por actividad económica y ubicación geográfica."),
                      tags$li(strong("Empleo e Innovación:"), " Empleos por género, horas trabajadas, innovación, uso de internet y productividad."),
                      tags$li(strong("Datos:"), " Tabla interactiva con todos los registros.")
                    ),
                    
                    br(),
                    h4("Fuente de datos"),
                    p("Instituto Nacional de Estadística e Informática (INEI) - Censo Nacional Económico 2022."),
                    p("Periodo de referencia: 2021"),
                    p("Cobertura geográfica: Provincia de Huancayo, Departamento de Junín"),
                    
                    br(),
                    h4("Definiciones clave"),
                    tags$ul(
                      tags$li(strong("Valor Agregado:"), " Riqueza generada por la empresa (ventas - costos de insumos). Incluye sueldos, ganancias e impuestos."),
                      tags$li(strong("Microempresa:"), " 1 a 10 trabajadores."),
                      tags$li(strong("Pequeña empresa:"), " 11 a 50 trabajadores."),
                      tags$li(strong("Mediana empresa:"), " 51 a 250 trabajadores."),
                      tags$li(strong("Grande empresa:"), " Más de 250 trabajadores.")
                    ),
                    
                    br(),
                    p(em("Última actualización:", Sys.Date()))
                )
              )
      ),
      
      tabItem(tabName = "resumen",
              fluidRow(
                valueBoxOutput("total_empresas"),
                valueBoxOutput("total_empleos"),
                valueBoxOutput("ventas_totales"),
                valueBoxOutput("valor_agregado")
              ),
              fluidRow(
                box(title = "Ventas por sector económico", status = "primary", solidHeader = TRUE,
                    plotlyOutput("grafico_ventas_sector"), width = 12)
              ),
              fluidRow(
                box(title = "Ventas por distrito", status = "success", solidHeader = TRUE,
                    plotlyOutput("grafico_ventas_distrito"), width = 12)
              ),
              fluidRow(
                box(title = "Empleos por género", status = "info", solidHeader = TRUE,
                    plotlyOutput("grafico_empleos_genero"), width = 12)
              )
      ),
      
      tabItem(tabName = "tamano",
              fluidRow(
                box(title = "Cantidad de empresas por tamaño", status = "primary", solidHeader = TRUE,
                    plotOutput("grafico_cantidad_tamano", height = "500px"), width = 12)
              ),
              fluidRow(
                box(title = "Ventas por tamaño", status = "success", solidHeader = TRUE,
                    plotOutput("grafico_ventas_treemap", height = "500px"), width = 12)
              ),
              fluidRow(
                box(title = "Valor Agregado vs Personal ocupado", status = "info", solidHeader = TRUE,
                    plotOutput("grafico_va_dispersion", height = "600px"), width = 12)
              ),
              fluidRow(
                box(title = "Empleos por tamaño de empresa", status = "warning", solidHeader = TRUE,
                    plotOutput("grafico_empleos_tamano", height = "500px"), width = 12)
              ),
              fluidRow(
                box(title = "Ventas promedio por tamaño", status = "danger", solidHeader = TRUE,
                    plotOutput("grafico_ventas_promedio_tamano", height = "500px"), width = 12)
              )
      ),
      
      tabItem(tabName = "sectores",
              fluidRow(
                box(title = "Ventas por sector", status = "primary", solidHeader = TRUE,
                    plotlyOutput("grafico_sector_ventas_participacion"), width = 12)
              ),
              fluidRow(
                box(title = "Ventas por distrito", status = "success", solidHeader = TRUE,
                    plotOutput("grafico_distrito_treemap", height = "600px"),
                    verbatimTextOutput("ventas_distrito_nota"), width = 12)
              ),
              fluidRow(
                box(title = "Valor Agregado por sector", status = "primary", solidHeader = TRUE,
                    plotlyOutput("grafico_sector_va_burbujas", height = "600px"), width = 12)
              ),
              fluidRow(
                box(title = "Valor Agregado por distrito", status = "warning", solidHeader = TRUE,
                    plotlyOutput("grafico_distrito_va_pastel", height = "600px"), width = 12)
              )
      ),
      
      tabItem(tabName = "empleo",
              fluidRow(
                box(title = "Empleos por género", status = "primary", solidHeader = TRUE,
                    plotOutput("grafico_genero", height = "500px"), width = 12)
              ),
              fluidRow(
                box(title = "Horas trabajadas por género", status = "success", solidHeader = TRUE,
                    plotOutput("grafico_horas", height = "500px"), width = 12)
              ),
              fluidRow(
                box(title = "Innovación", status = "info", solidHeader = TRUE,
                    plotOutput("grafico_innovacion", height = "500px"), width = 12)
              )
              
      ),
      
      tabItem(tabName = "datos",
              fluidRow(
                box(title = "Base de datos filtrada", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("tabla_datos"),
                    br(),
                    p(em("Nota: 'Otros tipos de intermediación monetaria' incluye cooperativas de ahorro y crédito, cajas municipales, cajas rurales y otras entidades de microfinanzas que no son bancos comerciales."))
                )
              )
      )
    )
  )
)

################################################################################

server <- function(input, output) {
  
  datos_filtrados <- reactive({
    data <- huancayo
    if(input$filtro_distrito != "Todos") {
      data <- data %>% filter(DISTRITO == input$filtro_distrito)
    }
    if(input$filtro_sector != "Todos") {
      data <- data %>% filter(ACT_EC_DESC == input$filtro_sector)
    }
    data
  })
  
  output$total_empresas <- renderValueBox({
    valueBox(
      value = formatC(nrow(datos_filtrados()), format = "d", big.mark = ","),
      subtitle = "Total de empresas",
      icon = icon("building"),
      color = "blue"
    )
  })
  
  output$total_empleos <- renderValueBox({
    total <- sum(datos_filtrados()$PERSONAL_OCUPADO, na.rm = TRUE)
    valueBox(
      value = formatC(total, format = "d", big.mark = ","),
      subtitle = "Total de empleos",
      icon = icon("users"),
      color = "green"
    )
  })
  
  output$ventas_totales <- renderValueBox({
    total <- sum(datos_filtrados()$VENTAS_2021, na.rm = TRUE) / 1e6
    valueBox(
      value = paste("S/", formatC(round(total, 1), format = "f", big.mark = ",", digits = 1), "M"),
      subtitle = "Ventas totales",
      icon = icon("dollar-sign"),
      color = "yellow"
    )
  })
  
  output$valor_agregado <- renderValueBox({
    total <- sum(datos_filtrados()$VALOR_AGREGADO, na.rm = TRUE) / 1e6
    valueBox(
      value = paste("S/", formatC(round(total, 1), format = "f", big.mark = ",", digits = 1), "M"),
      subtitle = "Valor Agregado total",
      icon = icon("chart-line"),
      color = "red"
    )
  })
###################################
  output$grafico_ventas_sector <- renderPlotly({
    data_plot <- datos_filtrados() %>%
      group_by(ACT_EC_DESC) %>%
      summarise(Ventas = sum(VENTAS_2021, na.rm = TRUE) / 1e6) %>%
      arrange(desc(Ventas)) %>%
      head(10) %>%
      mutate(ACT_EC_DESC_CORTE = substr(ACT_EC_DESC, 1, 30))
    
    p <- ggplot(data_plot, aes(x = reorder(ACT_EC_DESC_CORTE, Ventas), y = Ventas, fill = ACT_EC_DESC_CORTE,
                               text = paste(ACT_EC_DESC, "<br>Ventas:", round(Ventas, 1), "millones S/"))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "", y = "Ventas (millones S/)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  #############################################
  
  output$grafico_ventas_distrito <- renderPlotly({
    data_plot <- datos_filtrados() %>%
      group_by(DISTRITO) %>%
      summarise(Ventas = sum(VENTAS_2021, na.rm = TRUE) / 1e6) %>%
      arrange(desc(Ventas)) %>%
      head(10) %>%
      mutate(DISTRITO_CORTE = substr(DISTRITO, 1, 25))
    
    p <- ggplot(data_plot, aes(x = reorder(DISTRITO_CORTE, Ventas), y = Ventas, fill = DISTRITO_CORTE,
                               text = paste(DISTRITO, "<br>Ventas:", round(Ventas, 1), "millones S/"))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "", y = "Ventas (millones S/)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
 ############################################
  output$grafico_empleos_genero <- renderPlotly({
    data <- datos_filtrados()
    
    empleos <- data.frame(
      Genero = c("Hombres", "Mujeres"),
      Cantidad = c(
        sum(data$C3P7_TOT_H, na.rm = TRUE),
        sum(data$C3P7_TOT_M, na.rm = TRUE)
      )
    )
    
    p <- ggplot(empleos, aes(x = Genero, y = Cantidad, fill = Genero,
                             text = paste(Genero, "<br>Empleos:", formatC(Cantidad, format = "d", big.mark = ",")))) +
      geom_bar(stat = "identity") +
      labs(x = "", y = "Total de empleos") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  ############################################
  output$grafico_innovacion <- renderPlot({
    si <- sum(huancayo$C2P11_N == 1, na.rm = TRUE)
    no <- sum(huancayo$C2P11_N == 0, na.rm = TRUE)
    total <- si + no
    porcentaje_si <- round(si / total * 100, 1)
    
    data <- data.frame(
      Categoria = c("Sí innova", "No innova"),
      Cantidad = c(si, no)
    )
    
    ggplot(data, aes(x = "", y = Cantidad, fill = Categoria)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y", start = 0) +
      geom_text(aes(label = ifelse(Categoria == "Sí innova", 
                                   paste0(porcentaje_si, "%"), "")),
                position = position_stack(vjust = 0.5), size = 8, fontface = "bold") +
      scale_fill_manual(values = c("Sí innova" = "#2E86C1", "No innova" = "#E74C3C")) +
      theme_void() +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      ggtitle(paste("Innovación -", porcentaje_si, "% sí innova"))
  })
  
  output$grafico_internet <- renderPlot({
    si <- sum(huancayo$C4P4_N == 1, na.rm = TRUE)
    no <- sum(huancayo$C4P4_N == 0, na.rm = TRUE)
    total <- si + no
    porcentaje_si <- round(si / total * 100, 1)
    
    data <- data.frame(
      Categoria = c("Sí usa internet", "No usa internet"),
      Cantidad = c(si, no)
    )
    
    ggplot(data, aes(x = "", y = Cantidad, fill = Categoria)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y", start = 0) +
      geom_text(aes(label = ifelse(Categoria == "Sí usa internet", 
                                   paste0(porcentaje_si, "%"), "")),
                position = position_stack(vjust = 0.5), size = 8, fontface = "bold") +
      scale_fill_manual(values = c("Sí usa internet" = "#27AE60", "No usa internet" = "#E67E22")) +
      theme_void() +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      ggtitle(paste("Uso de internet -", porcentaje_si, "% sí usa internet"))
  })
  #########################################
  #----------------------------------------------empresas por tamaño##############################
  output$grafico_cantidad_tamano <- renderPlot({
    data <- datos_filtrados() %>%
      filter(TAMANO_EMPRESA %in% c("Micro (1-10)", "Pequeña (11-50)", "Mediana (51-250)", "Grande (250+)")) %>%
      group_by(TAMANO_EMPRESA) %>%
      summarise(Cantidad = n())
    
    omitidas <- huancayo %>%
      filter(!TAMANO_EMPRESA %in% c("Micro (1-10)", "Pequeña (11-50)", "Mediana (51-250)", "Grande (250+)")) %>%
      nrow()
    
    ggplot(data, aes(x = reorder(TAMANO_EMPRESA, -Cantidad), y = Cantidad, fill = TAMANO_EMPRESA)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = format(Cantidad, big.mark = ",")), vjust = -0.5, size = 4) +
      labs(x = "", y = "Número de empresas", 
           title = "Cantidad de empresas por tamaño",
           caption = paste("Nota: Se omitieron", format(omitidas, big.mark = ","), "empresas que no registraron número de trabajadores")) +
      scale_fill_manual(values = c("Micro (1-10)" = "#3498DB", 
                                   "Pequeña (11-50)" = "#2ECC71", 
                                   "Mediana (51-250)" = "#F39C12", 
                                   "Grande (250+)" = "#E74C3C")) +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.caption = element_text(hjust = 0, size = 8, color = "gray50"))
  })
  #####################################
  output$grafico_ventas_treemap <- renderPlot({
    data <- datos_filtrados() %>%
      filter(TAMANO_EMPRESA %in% c("Micro (1-10)", "Pequeña (11-50)", "Mediana (51-250)", "Grande (250+)")) %>%
      group_by(TAMANO_EMPRESA) %>%
      summarise(Ventas = sum(VENTAS_2021, na.rm = TRUE) / 1e6) %>%
      mutate(porcentaje = Ventas / sum(Ventas) * 100)
    
    ggplot(data, aes(area = Ventas, fill = TAMANO_EMPRESA, label = paste0(TAMANO_EMPRESA, "\n", round(Ventas, 1), " M S/"))) +
      geom_treemap() +
      geom_treemap_text(place = "centre", size = 15, colour = "white") +
      scale_fill_manual(values = c("Micro (1-10)" = "#3498DB", 
                                   "Pequeña (11-50)" = "#2ECC71", 
                                   "Mediana (51-250)" = "#F39C12", 
                                   "Grande (250+)" = "#E74C3C")) +
      labs(title = "Ventas totales por tamaño (millones S/)") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  })
  ########################################
  output$grafico_va_dispersion <- renderPlot({
    excluidas <- huancayo %>%
      filter(TAMANO_EMPRESA %in% c("Micro (1-10)", "Pequeña (11-50)", "Mediana (51-250)", "Grande (250+)")) %>%
      filter(PERSONAL_OCUPADO == 0 | is.na(PERSONAL_OCUPADO) | VALOR_AGREGADO == 0 | is.na(VALOR_AGREGADO)) %>%
      nrow()
    
    p <- huancayo %>%
      filter(TAMANO_EMPRESA %in% c("Micro (1-10)", "Pequeña (11-50)", "Mediana (51-250)", "Grande (250+)")) %>%
      filter(PERSONAL_OCUPADO > 0, VALOR_AGREGADO > 0) %>%
      sample_n(min(5000, n())) %>%
      ggplot(aes(x = VALOR_AGREGADO / 1000, y = PERSONAL_OCUPADO, color = TAMANO_EMPRESA)) +
      geom_point(alpha = 0.6, size = 3) +
      scale_x_log10(labels = scales::comma) +
      scale_y_log10(labels = scales::comma) +
      labs(x = "Valor Agregado (miles S/)", 
           y = "Personal ocupado ",
           title = "Relación entre valor agregado y personal ocupado por tamaño",
           subtitle = paste("Se excluyeron", excluidas, "empresas sin datos válidos"),
           color = "Tamaño de empresa") +
      scale_color_manual(values = c("Micro (1-10)" = "#3498DB", 
                                    "Pequeña (11-50)" = "#2ECC71", 
                                    "Mediana (51-250)" = "#F39C12", 
                                    "Grande (250+)" = "#E74C3C")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.subtitle = element_text(hjust = 0, size = 10, color = "gray50"),
            legend.position = "bottom")
    
    print(p)
  })
  ##########################################
  output$grafico_empleos_tamano <- renderPlot({
    data <- datos_filtrados() %>%
      filter(TAMANO_EMPRESA %in% c("Micro (1-10)", "Pequeña (11-50)", "Mediana (51-250)", "Grande (250+)")) %>%
      filter(!is.na(PERSONAL_OCUPADO)) %>%
      group_by(TAMANO_EMPRESA) %>%
      summarise(Empleos = sum(PERSONAL_OCUPADO, na.rm = TRUE))
    
    ggplot(data, aes(x = reorder(TAMANO_EMPRESA, Empleos), y = Empleos, fill = TAMANO_EMPRESA)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      geom_text(aes(label = format(Empleos, big.mark = ",")), hjust = -0.1, size = 4) +
      labs(x = "", y = "Número de empleos", title = "Total de empleos generados por tamaño de empresa") +
      scale_fill_manual(values = c("Micro (1-10)" = "#3498DB", 
                                   "Pequeña (11-50)" = "#2ECC71", 
                                   "Mediana (51-250)" = "#F39C12", 
                                   "Grande (250+)" = "#E74C3C")) +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  })
  #########################################3
  output$grafico_ventas_promedio_tamano <- renderPlot({
    data <- datos_filtrados() %>%
      filter(TAMANO_EMPRESA %in% c("Micro (1-10)", "Pequeña (11-50)", "Mediana (51-250)", "Grande (250+)")) %>%
      filter(!is.na(VENTAS_2021)) %>%
      group_by(TAMANO_EMPRESA) %>%
      summarise(Ventas_promedio = mean(VENTAS_2021, na.rm = TRUE) / 1000)
    
    # Ordenar por tamaño (micro, pequeña, mediana, grande)
    data$TAMANO_EMPRESA <- factor(data$TAMANO_EMPRESA, 
                                  levels = c("Micro (1-10)", "Pequeña (11-50)", "Mediana (51-250)", "Grande (250+)"))
    
    ggplot(data, aes(x = TAMANO_EMPRESA, y = Ventas_promedio, group = 1)) +
      geom_line(size = 1.5, color = "#3498DB") +
      geom_point(size = 4, color = "#E74C3C") +
      geom_text(aes(label = paste0(round(Ventas_promedio, 0), " miles S/")), vjust = -1, size = 4) +
      labs(x = "", y = "Ventas promedio (miles S/)", 
           title = "Ventas promedio por tamaño de empresa en el año 2021") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  })
  ##############################################
  #------------------------------------------------------distrtiso----------------------
  
  output$grafico_sector_ventas_participacion <- renderPlotly({
    data <- datos_filtrados() %>%
      group_by(ACT_EC_DESC) %>%
      summarise(Ventas = sum(VENTAS_2021, na.rm = TRUE) / 1e6) %>%
      arrange(desc(Ventas)) %>%
      head(10) %>%
      mutate(ACT_EC_DESC_CORTE = substr(ACT_EC_DESC, 1, 40))
    
    p <- ggplot(data, aes(x = reorder(ACT_EC_DESC_CORTE, Ventas), y = Ventas, fill = ACT_EC_DESC_CORTE,
                          text = paste(ACT_EC_DESC, "<br>Ventas:", round(Ventas, 1), "millones S/"))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "", y = "Ventas (millones S/)", 
           title = "Top 10 sectores - Ventas totales") +
      scale_fill_brewer(palette = "Set3") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  ##################################################
  output$grafico_distrito_treemap <- renderPlot({
    data <- datos_filtrados() %>%
      group_by(DISTRITO) %>%
      summarise(Ventas = sum(VENTAS_2021, na.rm = TRUE) / 1e6) %>%
      arrange(desc(Ventas)) %>%
      head(15) %>%
      mutate(etiqueta = paste0(DISTRITO, "\n", round(Ventas, 1), " M S/"))
    
    p <- ggplot(data, aes(area = Ventas, fill = Ventas, label = etiqueta)) +
      geom_treemap() +
      geom_treemap_text(place = "centre", size = 12, colour = "white") +
      scale_fill_gradient(low = "#3498DB", high = "#E74C3C", name = "Ventas (M S/)") +
      labs(title = "Ventas por distrito",
           caption = "Cifras en millones de soles. Solo se muestran los 15 distritos con mayores ventas.") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.caption = element_text(hjust = 0, size = 9, color = "gray50"))
    
    print(p)
  })
  output$ventas_distrito_nota <- renderPrint({
    data <- datos_filtrados() %>%
      group_by(DISTRITO) %>%
      summarise(Ventas = sum(VENTAS_2021, na.rm = TRUE) / 1e6) %>%
      arrange(desc(Ventas)) %>%
      head(10)
    
    cat("Top 10 distritos por ventas (millones S/):\n")
    cat("----------------------------------------\n")
    for(i in 1:nrow(data)) {
      cat(sprintf("%-20s : S/ %8.0f millones\n", data$DISTRITO[i], data$Ventas[i]))
    }
  })
  #######################################################
  output$grafico_sector_va_burbujas <- renderPlotly({
    data <- datos_filtrados() %>%
      group_by(ACT_EC_DESC) %>%
      summarise(VA = sum(VALOR_AGREGADO, na.rm = TRUE) / 1e6,
                Ventas = sum(VENTAS_2021, na.rm = TRUE) / 1e6) %>%
      arrange(desc(VA)) %>%
      head(15) %>%
      mutate(ACT_EC_DESC_CORTE = substr(ACT_EC_DESC, 1, 40))
    
    p <- ggplot(data, aes(x = VA, y = reorder(ACT_EC_DESC_CORTE, VA), size = Ventas, color = VA,
                          text = paste(ACT_EC_DESC, "<br>Valor Agregado:", round(VA, 1), "M S/",
                                       "<br>Ventas:", round(Ventas, 1), "M S/"))) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(range = c(3, 15), name = "Ventas (M S/)") +
      scale_color_gradient(low = "#3498DB", high = "#E74C3C", name = "Valor Agregado") +
      labs(x = "Valor Agregado (millones S/)", y = "", 
           title = "Top 15 sectores - Valor Agregado") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.text.y = element_text(size = 9))
    
    ggplotly(p, tooltip = "text")
  })
  #######################################################
  output$grafico_distrito_va_pastel <- renderPlotly({
    data <- datos_filtrados() %>%
      group_by(DISTRITO) %>%
      summarise(VA = sum(VALOR_AGREGADO, na.rm = TRUE) / 1e6) %>%
      arrange(desc(VA))
    
    top8 <- data %>% head(8)
    otros <- data %>% slice(9:n()) %>% summarise(VA = sum(VA, na.rm = TRUE)) %>% mutate(DISTRITO = "Otros")
    
    data_final <- bind_rows(top8, otros)
    
    # Mostrar también los nombres de los distritos en "Otros"
    nombres_otros <- data %>% slice(9:n()) %>% pull(DISTRITO) %>% paste(collapse = ", ")
    
    plot_ly(data_final, labels = ~DISTRITO, values = ~VA, type = "pie",
            textposition = "inside",
            textinfo = "label+percent",
            insidetextorientation = "radial",
            hoverinfo = "text",
            text = ~ifelse(DISTRITO == "Otros", 
                           paste("Otros (", nombres_otros, ")", "<br>", round(VA, 1), "millones S/"),
                           paste(DISTRITO, "<br>", round(VA, 1), "millones S/")),
            marker = list(colors = c("#3498DB", "#2ECC71", "#F39C12", "#E74C3C", 
                                     "#9B59B6", "#1ABC9C", "#E67E22", "#34495E", "#7F8C8D"))) %>%
      layout(title = "Valor Agregado por distrito",
             showlegend = TRUE)
  })
  ########################################################
  #------------------------------------------generoempleo
  output$grafico_genero <- renderPlot({
    data <- data.frame(
      Genero = c("Hombres", "Mujeres"),
      Empleos = c(
        sum(huancayo$C3P7_TOT_H, na.rm = TRUE),
        sum(huancayo$C3P7_TOT_M, na.rm = TRUE)
      )
    )
    
    ggplot(data, aes(x = Genero, y = Empleos, fill = Genero)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = format(Empleos, big.mark = ",")), vjust = -0.5, size = 5) +
      labs(x = "", y = "Número de empleos", title = "Empleos por género") +
      scale_fill_manual(values = c("Hombres" = "#3498DB", "Mujeres" = "#E74C3C")) +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  })
  #######################################################
  output$grafico_horas <- renderPlot({
    data <- data.frame(
      Genero = c("Hombres", "Mujeres"),
      Horas = c(
        sum(huancayo$C3P7_TOT_H, na.rm = TRUE),
        sum(huancayo$C3P7_TOT_M, na.rm = TRUE)
      )
    )
    
    total_horas <- sum(data$Horas)
    
    data <- data %>%
      mutate(porcentaje = Horas / total_horas * 100,
             etiqueta = paste0(Genero, "\n", round(porcentaje, 1), "%"))
    
    ggplot(data, aes(x = "", y = Horas, fill = Genero)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5), size = 5) +
      labs(title = paste("Distribución de horas trabajadas por género\nTotal:", format(total_horas, big.mark = ","), "horas")) +
      scale_fill_manual(values = c("Hombres" = "#2ECC71", "Mujeres" = "#F39C12")) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
  })
  #####################################################
  output$grafico_innovacion <- renderPlot({
    data <- data.frame(
      Tipo = c("Sí innova", "No innova"),
      Cantidad = c(
        sum(huancayo$C2P11_N == 1, na.rm = TRUE),
        sum(huancayo$C2P11_N == 0, na.rm = TRUE)
      )
    )
    
    total <- sum(data$Cantidad)
    data <- data %>%
      mutate(porcentaje = Cantidad / total * 100,
             xmax = cumsum(porcentaje),
             xmin = lag(xmax, default = 0))
    
    ggplot(data, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = Tipo)) +
      geom_rect() +
      geom_text(aes(x = (xmin + xmax)/2, y = 0.5, label = paste0(Tipo, ": ", round(porcentaje, 1), "%")), size = 4) +
      labs(title = paste("Innovación en empresas\nTotal:", format(total, big.mark = ","), "empresas")) +
      scale_fill_manual(values = c("Sí innova" = "#A8D5BA", "No innova" = "#F4B6C2")) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))
  })
  ####################################################
  #---------------------------------datos------------------------------
  output$tabla_datos <- renderDT({
    datos_filtrados() %>%
      select(DISTRITO, ACT_EC_DESC, TAMANO_EMPRESA, PERSONAL_OCUPADO, 
             VENTAS_2021, VALOR_AGREGADO, INNOVA, USA_INTERNET) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          language = list(
            url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json"
          )
        ),
        filter = "top",
        rownames = FALSE
      ) %>%
      formatCurrency(columns = c("VENTAS_2021", "VALOR_AGREGADO"), currency = "S/ ", interval = 3, mark = ",")
  })
  ##################################################
}

shinyApp(ui = ui, server = server)

