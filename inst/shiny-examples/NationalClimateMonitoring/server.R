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
    #raster::getData raises a warning (rename file does not work / should be copy instead)
    fadm0 = fortify(adm0)
    data  <- importEOBS(input$variableName, input$period, adm0, input$grid)
    meanData <- data[, .(avg = mean(eval(parse(text=input$variableName)))), by = .(lon, lat)]
    
    ggplot(fadm0, aes(x = long, y = lat, group = group)) +
      geom_path() +
      coord_map() +
      geom_tile(aes(x =lon, y = lat, fill = avg, group = NULL),
                alpha=0.5,
                data = meanData) +
      scale_fill_distiller(type='div', palette='RdBu', trans='reverse',
                           guide = guide_legend(reverse=TRUE))
  })
  
})
