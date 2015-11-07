library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  
  output$countryPlot <- renderPlot({
    
    adm0 <- raster::getData('GADM', country=input$country, level=0)
    fadm0 = fortify(adm0)
    #data  <- importEOBS('tg', '2014', adm0, "grid")
    #meanData <- data[, .(TGG = mean(tg)), by = .(lon, lat)]
    
    ggplot(fadm0, aes(x = long, y = lat, group = group)) +
      geom_path() +
      coord_map()
  })
  
})
