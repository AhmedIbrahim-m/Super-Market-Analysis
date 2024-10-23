library(shiny)
library(arules)
library(readxl)
library(dplyr)
library(ggplot2)
ui <- fluidPage(
  titlePanel("Supermarket Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File"),
      selectInput("plotType", "Select Plot Type:",
                  choices = c("Payment Type" = "paymentType",
                              "Age Distribution" = "age",
                              "City Spending" = "city",
                              "Total Spending Distribution" = "total",
                              "Cluster Analysis" = "cluster"),
                  selected = "paymentType"),
      conditionalPanel(
        condition = "input.plotType == 'cluster'",
        numericInput(inputId = "NumCentroids", label = "No. Centroids", value = NULL, min = 2, max = 4)
      )
    ),
    
    
    mainPanel(
      plotOutput("plot"),
      tableOutput("clusterTable")
    )
  )
)

server <- function(input, output) {
  tdata <- reactive({
    req(input$file)
    tdata <- read.csv(input$file$datapath)
    distinct(tdata)
  })
  
  
  output$plot <- renderPlot({
    switch(input$plotType,
           "paymentType" = {
             # Payment types pie chart
             payment_data <- table(tdata()$paymentType)
             percentage <- round(100 * payment_data / sum(payment_data), digits = 2)
             labels <- paste0(percentage, "%")
             colors <- c("red", "blue")
             pie(payment_data, labels = labels, main = "Compare cash and credit totals.", col = colors)
             legend("bottomleft", legend = c("cash", "credit"), fill = colors)
           },
           "age" = {
             # Age distribution plot
             age_total <- aggregate(total ~ age, data = tdata(), FUN = sum)
             plot(age_total, xlab = "Age", ylab = "Total Spending", xlim = c(20, 60), pch = 20)
           },
           "city" = {
             city_total <- aggregate(total ~ city, data = tdata(), FUN = sum)
             ordered_indices <- order(city_total$total, decreasing = TRUE)
             city_total <- city_total[ordered_indices,]
             colors <- c("red", "blue", "green", "orange", "lightblue", "brown", "yellow", "black", "pink", "skyblue")
             barplot(height = city_total$total, names = city_total$city, ylab = "Spending", xlab = "City", col = colors, main = "City and Total Spending")
           },
           "total" = {
             spending <- tdata()$total
             hist(spending, col = "yellow", main = "Distribution of Total Spending", xlab = "Total Spending", ylab = "Frequency")
           },
           "cluster" = {
             groups <- New_data[-c(1, 2, 4, 5, 7, 8)]
             no_centroid <- input$NumCentroids
             clusters <- kmeans(groups, centers = no_centroid)
             cluster_data$Cluster <- clusters$cluster #cluster_data is a copy from New_data
             cluster_data<-data.frame(cluster_data)
             output$clusterTable <- renderTable({cluster_data[c("customer","age","total","Cluster")]})
           }
           
    )
  })
}

shinyApp(ui = ui, server = server)
