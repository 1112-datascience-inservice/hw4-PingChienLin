library(shiny)

data(iris)
# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
library(ggbiplot)

ui <- fluidPage(
  titlePanel("HW4"),
  hr(),
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
  plotOutput("plot")
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
}

shinyApp(ui, server)