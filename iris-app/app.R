library(shiny)
library(datasets)
data(iris)

model <- lm(Sepal.Length ~ Petal.Length, data = iris)

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
    
    p_length <- data.frame(input$length)
    
    new.data <- data.frame(Petal.Length = input$length)
    
    s_length <- predict(model, newdata = new.data)
    
    
    lines(
      x = iris$Petal.Length,
      y = model$fitted,
      col = "blue",
      lwd = 3)
    
    points(x=p_length, y=s_length, #Don't know what to put for y here to follow the regression line
           col = "red",
           lwd = 6)
  })
}
?points

shinyApp(ui = ui, server = server)