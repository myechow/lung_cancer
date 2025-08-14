library(shiny)
library(tidyverse)
library(janitor)
library(forcats)
library(scales)
library(ggplot2)
library(stringr)

read_tsv_clean <- function(path) {
  readr::read_tsv(path, show_col_types = FALSE) %>%
    janitor::remove_empty(c("rows","cols"))
}

clinical <- read_tsv_clean("clinical.tsv")

clinical$cases.disease_type <- factor(clinical$cases.disease_type)
clinical$demographic.country_of_residence_at_enrollment <- factor(clinical$demographic.country_of_residence_at_enrollment)
clinical$demographic.race <- factor(clinical$demographic.race)
clinical$demographic.gender <- factor(clinical$demographic.gender)
clinical$demographic.vital_status <- factor(clinical$demographic.vital_status)

theme_apa <- function(base_size = 12, base_family = "sans") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.5),
      axis.title      = element_text(face = "bold"),
      legend.position = "right",
      legend.title    = element_text(face = "bold"),
      axis.text.x     = element_text(angle = 0, hjust = 0.5)
    )
}

build_plot_df <- function(var_name) {
  col_map <- list(
    Gender  = "demographic.gender",
    Race    = "demographic.race",
    Country = "demographic.country_of_residence_at_enrollment",
    Vital   = "demographic.vital_status",
    Age     = "demographic.age_at_index"  # will be grouped below
  )
  
  stopifnot(var_name %in% names(col_map))
  source_col <- col_map[[var_name]]
  
  df <- clinical %>%
    transmute(
      Disease = factor(cases.disease_type),
      value_raw = .data[[source_col]]
    )
  
  # Age group
  if (var_name == "Age") {
    df <- df %>%
      mutate(
        age_num = suppressWarnings(as.numeric(value_raw)),
        value = cut(
          age_num,
          breaks = c(-Inf, 40, 60, 80, Inf),
          labels = c("<40","40–59","60–79","80+"),
          right = FALSE
        )
      )
  } else {
    df <- df %>%
      mutate(value = value_raw)
  }
  

  df %>%
    mutate(value = fct_drop(as.factor(value))) %>%
    transmute(!!var_name := value, Disease)
}

# Horizontal bar chart (counts) with labels
plot_bar_counts <- function(df, var_name) {
  ggplot(df, aes(x = .data[[var_name]], fill = Disease)) +
    geom_bar(position = position_dodge(width = 0.9)) +
    geom_text(
      stat = "count",
      aes(label = ..count..),
      position = position_dodge(width = 0.9),
      hjust = -0.1, size = 3
    ) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = var_name, y = "Count", fill = "Disease Type") +
    theme_apa()
}

# Shiny UI
ui <- fluidPage(
  titlePanel("Demographics by Disease Type"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "demo_var", "Select demographic:",
        choices = c("Gender","Race","Country","Vital","Age"),
        selected = "Gender"
      ),
      downloadButton("download_png", "Download Plot (PNG)")
    ),
    mainPanel(
      plotOutput("demo_plot", height = "520px")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  
  current_plot <- reactive({
    df <- build_plot_df(input$demo_var)
    validate(need(nrow(df) > 0, "No data available for the selected demographic after cleaning."))
    plot_bar_counts(df, input$demo_var)
  })
  
  output$demo_plot <- renderPlot({
    current_plot()
  })
  

  output$download_png <- downloadHandler(
    filename = function() {
      paste0("disease_by_", input$demo_var, ".png")
    },
    content = function(file) {
      plt <- current_plot()
      ggplot2::ggsave(
        filename = file,
        plot     = plt,
        width    = 8,
        height   = 5,
        dpi      = 300,
        units    = "in",
        device   = "png"
      )
    }
  )
}

shinyApp(ui, server)