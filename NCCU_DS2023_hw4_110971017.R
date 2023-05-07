library(shiny)

data(iris)
# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
library(ggbiplot)
library(ca)
set.seed(2305)

ui <- fluidPage(
  titlePanel("110971017_HW4"),
  h2("PCA"),
  textOutput('text1'),
  actionButton("x_pc1", "PC1"),
  actionButton("x_pc2", "PC2"), 
  actionButton("x_pc3", "PC3"),
  actionButton("x_pc4", "PC4"),
  hr(),
  textOutput('text2'),
  actionButton("y_pc1", "PC1"),
  actionButton("y_pc2", "PC2"), 
  actionButton("y_pc3", "PC3"),
  actionButton("y_pc4", "PC4"),
  plotOutput("plot"),
  
  h2("CA"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "centers",
                  label = "Number of centers:",
                  min = 3,
                  max = 10,
                  value = 3)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "plot_2")
      
    )
  )
)

server <- function(input, output){
  x <- reactiveValues(data = 1)
  y <- reactiveValues(data = 2)
  
  observeEvent(input$x_pc1, {
    x$data <- 1
  })
  observeEvent(input$x_pc2, {
    x$data <- 2
  })
  observeEvent(input$x_pc3, {
    x$data <- 3
  })
  observeEvent(input$x_pc4, {
    x$data <- 4
  })
  
  observeEvent(input$y_pc1, {
    y$data <- 1
  })
  observeEvent(input$y_pc2, {
    y$data <- 2
  })
  observeEvent(input$y_pc3, {
    y$data <- 3
  })
  observeEvent(input$y_pc4, {
    y$data <- 4
  })
  
  output$text1 <- renderText({ paste("X axis: ",x$data) })
  output$text2 <- renderText({ paste("Y axis: ",y$data) })
  
  output$plot <- renderPlot({
    if (is.null(x$data)) return()
    if (is.null(y$data)) return()
    g <- ggbiplot(ir.pca
                  ,choices = c(x$data, y$data)
                  , obs.scale = 1, var.scale = 1, groups = ir.species
                  ,ellipse = TRUE
                  , varname.size = 4
    ) 
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    print(g)
  })
  
  output$plot_2 <- renderPlot({
    # Corresponding Analysis
    model <- kmeans(iris[,1:4], centers = input$centers)
    table(iris$Species, model$cluster)
    ca_model <- ca(table(iris$Species, model$cluster), nd = 2)
    plot(ca_model, arrows = c(TRUE, FALSE))
  })
}

shinyApp(ui, server)