library(shiny)
library(datasets)
data(iris)

model <- lm(Sepal.Length ~ Petal.Length, data = iris)

ui <- fluidPage(
  
  titlePanel("Sepal Length to Petal Length"),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Predict an iris's sepal length from a prediction model using petal length on a slider"),
      
      sliderInput(inputId = "length",
                  label = "Petal Length in Inches",
                  min = min(iris$Petal.Length),
                  max = max(iris$Petal.Length),
                  value = mean(iris$Petal.Length)),
      br(),
      br(),
      h4("Diagram of a ",span("Blue Flag Iris", style = "color:blue")),
      img(src = "blueflagiris_flower.jpg", height = 190, width = 220)
      
    ),
    
    mainPanel(
      h1("Prediction Plot"),
      br(),
      plotOutput("plotted"),
      h3("Predicted sepal length in inches"),
      h4(textOutput("texted")))
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
  output$texted <- renderText({
    p_length <- data.frame(input$length)
    
    new.data <- data.frame(Petal.Length = input$length)
    
    s_length <- predict(model, newdata = new.data)
    
    print(s_length)
  })
}
?points

shinyApp(ui = ui, server = server)