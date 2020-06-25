library(shiny)
library(datasets)
data(iris)


ui <- fluidPage(

  titlePanel("Sepal Length to Petal Length"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(inputId = "length",
                  label = "Petal Length in Inches",
                  min = min(iris$Petal.Length),
                  max = max(iris$Petal.Length),
                  value = mean(iris$Petal.Length))
      
    ),
    
    mainPanel(plotOutput("plotted"))
  )
)


server <- function(input, output) {
  output$plotted <- renderPlot({
    plot(
      x = iris$Petal.Length,
      y = iris$Sepal.Length,
      main = "Iris Sepal Length to Petal Length",
      xlab = "Petal Length",
      ylab = "Sepal Length")
    
    model <- lm(iris$Sepal.Length ~ iris$Petal.Length)
    
    p_length <- input$length
    
    s_length <- model$fitted
    
    lines(
      x = iris$Petal.Length,
      y = model$fitted,
      col = "blue",
      lwd = 3)
    
    points(x=p_length, y=7, #Don't know what to put for y here to follow the regression line
           col = "red",
           lwd = 6)
  })
}
?points

shinyApp(ui = ui, server = server)