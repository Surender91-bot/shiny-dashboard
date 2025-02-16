##################################################
setwd("C:/Users/Admin/Downloads")
library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)

# Function to load and merge data from a single Excel file with multiple sheets
load_data <- function(file) {
  sheet_names <- excel_sheets(file)
  data_list <- list()
  
  for (sheet in sheet_names) {
    split_name <- strsplit(sheet, " ")[[1]]
    if (length(split_name) < 2) next  # Ensure valid sheet name format
    country <- split_name[1]
    metric <- toupper(split_name[2])  # Ensure metric is uppercase
    
    sheet_data <- tryCatch(read_excel(file, sheet = sheet), error = function(e) NULL)
    if (!is.null(sheet_data)) {
      colnames(sheet_data) <- c("Sr.no", "Company.Name", "10Q", "40Q", "60Q") # Standardizing column names
      sheet_data <- sheet_data %>% 
        pivot_longer(cols = c("10Q", "40Q", "60Q"), names_to = "Quarter", values_to = "Value") %>%
        mutate(Metric = metric, Country = country)
      data_list[[sheet]] <- sheet_data
    }
  }
  
  if (length(data_list) == 0) return(data.frame())  # Return empty data frame if no data
  bind_rows(data_list, .id = "Source")
}

# Load data from the provided file
file_path <- "E:/sample/All countries sample data.xlsx"
data <- load_data(file_path)

# Ensure Country column exists and is not empty
if(nrow(data) == 0 || !"Country" %in% colnames(data)) {
  stop("No country data found. Please check the input file.")
}

# Ensure Value column is numeric
data$Value <- as.numeric(data$Value)

# UI
ui <- fluidPage(
  titlePanel("Country Performance Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = unique(na.omit(data$Country))),
      selectInput("company", "Select Company:", choices = NULL, multiple = FALSE)
    ),
    mainPanel(
      fluidRow(
        column(6, plotlyOutput("performancePlotHPR")),
        column(6, plotlyOutput("performancePlotEPS"))
      ),
      DTOutput("dataTable")
    )
  )
)

# Server
server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "company", choices = c("All", unique(na.omit(data$Company.Name[data$Country == input$country]))))
  })
  
  filtered_data_HPR <- reactive({
    df <- data %>% filter(Country == input$country, Metric == "HPR")
    df
  })
  
  filtered_data_EPS <- reactive({
    df <- data %>% filter(Country == input$country, Metric == "EPS")
    df
  })
  
  output$performancePlotHPR <- renderPlotly({
    plot_data <- filtered_data_HPR()
    if (input$company == "All") {
      p <- ggplot(plot_data, aes(x = Value, fill = Quarter)) +
        geom_density(alpha = 0.5) +
        labs(title = paste("HPR Density Plot for", input$country), x = "Performance Value", y = "Density") +
        theme_minimal() +
        theme(legend.position = "right")
    } else {
      plot_data <- plot_data %>% filter(Company.Name == input$company)
      p <- ggplot(plot_data, aes(x = Quarter, y = Value, group = Company.Name, color = Company.Name)) +
        geom_line() +
        geom_point() +
        labs(title = paste("HPR Performance for", input$company, "in", input$country), x = "Quarter", y = "Value") +
        theme_minimal() +
        theme(legend.position = "right")
    }
    ggplotly(p)
  })
  
  output$performancePlotEPS <- renderPlotly({
    plot_data <- filtered_data_EPS()
    if (input$company == "All") {
      p <- ggplot(plot_data, aes(x = Value, fill = Quarter)) +
        geom_density(alpha = 0.5) +
        labs(title = paste("EPS Density Plot for", input$country), x = "Performance Value", y = "Density") +
        theme_minimal() +
        theme(legend.position = "right")
    } else {
      plot_data <- plot_data %>% filter(Company.Name == input$company)
      p <- ggplot(plot_data, aes(x = Quarter, y = Value, group = Company.Name, color = Company.Name)) +
        geom_line() +
        geom_point() +
        labs(title = paste("EPS Performance for", input$company, "in", input$country), x = "Quarter", y = "Value") +
        theme_minimal() +
        theme(legend.position = "right")
    }
    ggplotly(p)
  })
  
  output$dataTable <- renderDT({
    datatable(data %>% filter(Country == input$country))
  })
}

# Run App
shinyApp(ui, server)


################################## trying for web link##############
dir.create("C:/ShinyApp")
library(readxl)
library(dplyr)

# Load the Excel file from your local drive
file_path <- "E:/sample/All countries sample data.xlsx"
sheet_names <- excel_sheets(file_path)

# Read all sheets into a list and merge them
data_list <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet) %>% mutate(Sheet = sheet)
})
data <- bind_rows(data_list)

# Save the full dataset as RDS
saveRDS(data, "C:/ShinyApp/data.rds")

# Load the pre-saved dataset (instead of Excel)
file_path <- "data.rds"  # Use RDS file inside the app folder
data <- readRDS(file_path)
list.files("C:/ShinyApp")
file.exists("C:/ShinyApp/app.R")
file.rename("C:/ShinyApp/dashboard.R", "C:/ShinyApp/app.R")

list.files("C:/Users/Admin/Downloads", recursive = TRUE, full.names = TRUE, pattern = "app.R$")
list.files("E:/sample", recursive = TRUE, full.names = TRUE, pattern = "app.R$")
file.copy("full/path/to/app.R", "C:/ShinyApp/app.R")
list.files("C:/ShinyApp")






install.packages("rsconnect")
library(rsconnect)
setwd("C:/ShinyApp")
rsconnect::deployApp(appFiles = c("app.R", "data.rds"))
rsconnect::setAccountInfo(name='surender-kaur',
                          token='9376CE884299EDB1AB7F6D808C1C6407',
                          secret='SyifKWLmEs/3OsZMTRchroXF+CqeObX0qIM6o7df')

rsconnect::deployApp()


