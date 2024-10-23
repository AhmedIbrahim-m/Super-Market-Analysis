# Load required libraries
library(shiny)
library(arules)
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Association Rules Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file"),
      numericInput("centroids", "Number of Centroids:", value = 2),
      actionButton("submit", "Submit")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Summary", verbatimTextOutput("summary")),
        tabPanel("Association Rules", verbatimTextOutput("rules")),
        tabPanel("Plots", plotOutput("plots"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- reactive({
    req(input$file)
    inFile <- input$file
    read.csv(inFile$datapath)
  })
  
  output$summary <- renderPrint({
    summary(data())
  })
  
  output$rules <- renderPrint({
    asso_data <- read.transactions(textConnection(data()$items), sep = ",")
    apriori_rules <- apriori(asso_data, parameter = list(supp = 0.01, conf = 0.01, minlen = 2))
    inspect(apriori_rules)
  })
  
  output$plots <- renderPlot({
    # Your plotting code here
    # Example:
    ggplot(data(), aes(x = age, y = total)) + geom_point()
  })
  
  observeEvent(input$submit, {
    kmean_data <- aggregate(data()$total, list(age = data()$age, rnd = data()$rnd), FUN = sum)
    clusters <- kmeans(kmean_data, centers = input$centroids)
    kmean_data$Cluster <- clusters$cluster
    kmean_data <- data.frame(kmean_data)
    print(kmean_data)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
