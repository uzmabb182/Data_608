# Load required libraries
library(shiny)
library(tidyverse)
library(lubridate)
library(readr)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(DT)
library(reshape)

# Load datasets ----
unemployment_data <- read.csv("https://raw.githubusercontent.com/uzmabb182/Data_608/refs/heads/main/Week_2/unemployment_rate.csv")
fed_data <- read.csv("https://raw.githubusercontent.com/uzmabb182/Data_608/refs/heads/main/Week_2/fed_fund_rate.csv")
cpi_data <- read.csv("https://raw.githubusercontent.com/uzmabb182/Data_608/refs/heads/main/Week_2/consumer_price_index.csv")

# CPI preprocessing
cpi_df <- cpi_data[, !(names(cpi_data) %in% c("HALF1", "HALF2"))]
cpi_long <- reshape(cpi_df, 
                    varying = list(2:ncol(cpi_df)), 
                    v.names = "CPI", 
                    timevar = "Month", 
                    times = names(cpi_df)[2:ncol(cpi_df)], 
                    idvar = "Year", 
                    direction = "long")
cpi_df <- cpi_long %>%
  group_by(Month) %>%
  mutate(inflation_rate = round(((CPI - lag(CPI)) / lag(CPI)) * 100, 2)) %>%
  ungroup() %>%
  filter(!is.na(inflation_rate), Year != 1999)

# Fed data preprocessing
fed_df <- fed_data %>%
  mutate(observation_date = mdy(observation_date),
         Year = year(observation_date),
         Month = month(observation_date, label = TRUE, abbr = TRUE)) %>%
  filter(Year != 1999)

# Unemployment preprocessing
unemp_df <- unemployment_data %>%
  mutate(Month = word(Label, 2)) %>%
  arrange(Year, Month, unemployment_rate) %>%
  filter(Year != 1999)

# Merge and clean data ----
merged_df <- unemp_df %>%
  inner_join(cpi_df, by = c("Year", "Month")) %>%
  inner_join(fed_df, by = c("Year", "Month")) %>%
  mutate(
    MonthNum = match(Month, month.abb),
    Date = as.Date(paste(Year, MonthNum, "01", sep = "-"))
  ) %>%
  distinct(Date, .keep_all = TRUE) %>%
  arrange(Date)

# UI ----
ui <- dashboardPage(
  dashboardHeader(title = "FED Mandate Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Series", tabName = "timeseries", icon = icon("chart-line")),
      menuItem("Mandate Status", tabName = "mandate", icon = icon("balance-scale")),
      menuItem("Dataset", tabName = "data", icon = icon("table")),
      menuItem("FED Effectiveness", tabName = "summary", icon = icon("chart-pie")),
      menuItem("Impact on Fed Rate", tabName = "impact", icon = icon("chart-scatter")),
      sliderInput("yearRange", "Select Year Range:",
                  min = min(merged_df$Year), max = max(merged_df$Year),
                  value = c(2000, 2025), sep = ""),
      numericInput("inflationThreshold", "Inflation Threshold (%):", value = 2, min = 0, max = 10, step = 0.1),
      numericInput("unempThreshold", "Unemployment Threshold (%):", value = 6, min = 0, max = 20, step = 0.1)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "timeseries",
              fluidRow(
                box(plotlyOutput("fedPlot"), width = 12),
                box(plotlyOutput("inflationPlot"), width = 12),
                box(plotlyOutput("unempPlot"), width = 12)
              )
      ),
      tabItem(tabName = "mandate",
              fluidRow(
                box(plotOutput("inflationBar"), width = 6),
                box(plotOutput("unempBar"), width = 6)
              )
      ),
      tabItem(tabName = "data",
              fluidRow(
                box(DTOutput("dataTable"), width = 12)
              )
      ),
      tabItem(tabName = "summary",
              fluidRow(
                valueBoxOutput("achievedBox"),
                valueBoxOutput("partialBox"),
                valueBoxOutput("missedBox")
              ),
              fluidRow(
                box(title = "Interpretation", width = 12, status = "primary", solidHeader = TRUE,
                    p("The Federal Reserve aims to control inflation while maintaining full employment."),
                    p("This dashboard shows how often both goals were achieved (green), missed (red), or partially met (orange), based on the thresholds you've set."),
                    p("Change the thresholds or year range in the sidebar to explore how performance varies.")
                )
              )
      ),
      tabItem(tabName = "impact",
              fluidRow(
                box(selectInput("predictor", "Select Predictor Variable:",
                                choices = c("Inflation Rate" = "inflation_rate",
                                            "Unemployment Rate" = "unemployment_rate")),
                    plotOutput("scatterPlot"), width = 6),
                box(title = "Regression Summary", verbatimTextOutput("regSummary"), width = 6)
              )
      )
    )
  )
)

# Server ----
server <- function(input, output) {
  
  filtered_data <- reactive({
    merged_df %>%
      filter(Year >= input$yearRange[1], Year <= input$yearRange[2]) %>%
      mutate(
        inflation_criteria = ifelse(inflation_rate > input$inflationThreshold, "Not Achieved", "Achieved"),
        unemp_criteria = ifelse(unemployment_rate > input$unempThreshold, "Not Achieved", "Achieved"),
        status = case_when(
          unemp_criteria == "Achieved" & inflation_criteria == "Achieved" ~ "Yes",
          unemp_criteria == "Not Achieved" & inflation_criteria == "Not Achieved" ~ "No",
          TRUE ~ "Partial Achieved"
        )
      ) %>%
      arrange(Date)
  })
  
  # Value Boxes
  output$achievedBox <- renderValueBox({
    data <- filtered_data()
    percent <- round(mean(data$status == "Yes", na.rm = TRUE) * 100, 1)
    valueBox(paste0(percent, "%"), "Mandate Fully Achieved", icon = icon("check-circle"), color = "green")
  })
  
  output$partialBox <- renderValueBox({
    data <- filtered_data()
    percent <- round(mean(data$status == "Partial Achieved", na.rm = TRUE) * 100, 1)
    valueBox(paste0(percent, "%"), "Partially Achieved", icon = icon("exclamation-triangle"), color = "orange")
  })
  
  output$missedBox <- renderValueBox({
    data <- filtered_data()
    percent <- round(mean(data$status == "No", na.rm = TRUE) * 100, 1)
    valueBox(paste0(percent, "%"), "Mandate Missed", icon = icon("times-circle"), color = "red")
  })
  
  # Line Plots
  output$fedPlot <- renderPlotly({
    plot_ly(filtered_data(), x = ~Date, y = ~fed_fund_rate, type = 'scatter', mode = 'lines',
            line = list(color = 'red')) %>%
      layout(title = "Fed Funds Rate Over Time",
             yaxis = list(title = "Fed Funds Rate (%)"),
             xaxis = list(title = "Year"))
  })
  
  output$inflationPlot <- renderPlotly({
    plot_ly(filtered_data(), x = ~Date) %>%
      add_lines(y = ~inflation_rate, name = "Inflation Rate", line = list(color = 'green')) %>%
      add_lines(y = input$inflationThreshold, name = "Inflation Threshold", line = list(color = 'red', dash = 'dash')) %>%
      layout(title = paste("Inflation Rate vs Threshold (", input$inflationThreshold, "%)", sep = ""),
             yaxis = list(title = "Inflation (%)"),
             xaxis = list(title = "Year"))
  })
  
  output$unempPlot <- renderPlotly({
    plot_ly(filtered_data(), x = ~Date) %>%
      add_lines(y = ~unemployment_rate, name = "Unemployment Rate", line = list(color = 'blue')) %>%
      add_lines(y = input$unempThreshold, name = "Unemployment Threshold", line = list(color = 'red', dash = 'dash')) %>%
      layout(title = paste("Unemployment Rate vs Threshold (", input$unempThreshold, "%)", sep = ""),
             yaxis = list(title = "Unemployment (%)"),
             xaxis = list(title = "Year"))
  })
  
  # Bar Charts
  output$inflationBar <- renderPlot({
    ggplot(filtered_data(), aes(x = factor(Year), fill = inflation_criteria)) +
      geom_bar(position = "stack") +
      scale_fill_manual(values = c("Achieved" = "green", "Not Achieved" = "orange")) +
      labs(
        title = paste("Inflation Mandate Achievement (Threshold:", input$inflationThreshold, "%)"),
        x = "Year", y = "Months", fill = "Status"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$unempBar <- renderPlot({
    ggplot(filtered_data(), aes(x = factor(Year), fill = unemp_criteria)) +
      geom_bar(position = "stack") +
      scale_fill_manual(values = c("Achieved" = "blue", "Not Achieved" = "pink")) +
      labs(
        title = paste("Unemployment Mandate Achievement (Threshold:", input$unempThreshold, "%)"),
        x = "Year", y = "Months", fill = "Status"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Data Table
  output$dataTable <- renderDT({
    datatable(filtered_data(), options = list(scrollX = TRUE))
  })
  
  # Impact Scatter Plot
  output$scatterPlot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes_string(x = input$predictor, y = "fed_fund_rate")) +
      geom_point(color = "steelblue") +
      geom_smooth(method = "lm", se = TRUE, color = "darkred") +
      labs(x = input$predictor,
           y = "Fed Funds Rate (%)",
           title = paste("Fed Rate vs", str_replace_all(input$predictor, "_", " "))) +
      theme_minimal()
  })
  
  # Regression Model Summary
  output$regSummary <- renderPrint({
    data <- filtered_data()
    model <- lm(fed_fund_rate ~ inflation_rate + unemployment_rate, data = data)
    summary(model)
  })
}

# Run the app
shinyApp(ui, server)
