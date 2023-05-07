library(shiny)

data(iris)
# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Hello World!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "x_bins",
                  label = "Number of X bins:",
                  min = 1,
                  max = 10,
                  value = 1),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "y_bins",
                  label = "Number of Y bins:",
                  min = 1,
                  max = 10,
                  value = 1)

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "distPlot")

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  # output$distPlot <- renderPlot({
  # 
  #   x    <- faithful$waiting
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  # 
  #   "hist(x, breaks = bins, col = "#75AADB", border = "white",
  #   hist(x, breaks = bins, col = "#FFA500", border = "white",
  #        xlab = "Waiting time to next eruption (in mins)",
  #        main = "Histogram of waiting times")
  # 
  #   })
  
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    x_bins <- seq(min(x), max(x), length.out = input$x_bins + 1)
    y_bins <- seq(min(x), max(x), length.out = input$y_bins + 1)
    
    
    
    "hist(x, breaks = bins, col = "#75AADB", border = "white",
    hist(x, breaks = bins, col = "#FFA500", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
