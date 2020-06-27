library(shiny)
library(dplyr)
data <- read.csv("data/housing.csv")
data$ocean_proximity <- as.factor(data$ocean_proximity)
head(data)

data %>% count(ocean_proximity, "NEAR BAY")
 
ui <- fluidPage(
  titlePanel("Visualizations of Houses by Region"),
  
  sidebarLayout(
    
    sidebarPanel(
      h3("The visualizations will change with each chosen region."),
      
      radioButtons("ocean", label = h4("Choose a region"), 
                         choices = list("Near the bay"="NEAR BAY", 
                                        "Within an hour of the ocean"="<1H OCEAN", 
                                        "Inland"="INLAND",
                                        "Near the ocean"="NEAR OCEAN",
                                        "On an island"="ISLAND"),
                         selected = "NEAR BAY"),
      
      h3("There are", span(textOutput("ocean"),style = "color:blue"), "neighborhoods"),
      
    ),
    
    mainPanel(
      plotOutput("plotted"),
      plotOutput("plotted2"),
      h4(textOutput("texted")))
  )
)


server <- function(input, output) {
  
  output$ocean <- renderText({
    newdat <- data %>% count(ocean_proximity == input$ocean)
    print(newdat$n[2])
  })
  
  output$plotted <- renderPlot({
    plot(
      x = data$population[data$ocean_proximity == input$ocean],
      y = data$households[data$ocean_proximity == input$ocean],
      main = "Poulation vs. Households",
      xlab = "Population",
      ylab = "Households"
    )
  })
  
  output$plotted2 <- renderPlot({
    plot(
      x = data$total_rooms[data$ocean_proximity == input$ocean],
      y = data$total_bedrooms[data$ocean_proximity == input$ocean],
      main = "Total Rooms to Total Bedrooms",
      xlab = "Total Rooms",
      ylab = "Total Bedrooms"
    )
  })
}

shinyApp(ui = ui, server = server)