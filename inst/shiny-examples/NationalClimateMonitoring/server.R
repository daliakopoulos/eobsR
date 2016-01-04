library(shiny)
#library(geosphere)
library(ggplot2)
library(dygraphs)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  
  polygon <- reactive({
    raster::getData('GADM', country=input$country, level=0)
    #raster::getData raises a warning (rename file does not work / should be copy instead)
  })
  
  fortifiedPolygon <- reactive({
    fortify(polygon())
  })
  
  basemap <- reactive({
    ggplot(fortifiedPolygon(), aes(x = long, y = lat, group = group)) +
      geom_path() +
      coord_map()
  })
  
  dataInput <- reactive({
    importEOBS(input$variableName, input$period, polygon(), input$grid)
  })
  
  uniquePoints <- reactive({
    dataInput()[, .(unique(lat), unique(lon)), by = pointID]
  })
  
  subPeriod <- reactiveValues(start=NULL, end=NULL)
  observeEvent(input$timeSeriesPlot_date_window, {
    if(!is.null(input$timeSeriesPlot_date_window)) {
      subPeriod$start <- as.Date(input$timeSeriesPlot_date_window[1])
      subPeriod$end   <- as.Date(input$timeSeriesPlot_date_window[2])
    }
  })
  #output$info <- renderText({paste("Period: ", subPeriod$start, "/", subPeriod$end, sep="")})
  
  output$countryPlot <- renderPlot({
    
    if (is.null(subPeriod$start)) {
      meanData <- dataInput()[, .(avg = mean(eval(parse(text=input$variableName)))), by = .(lon, lat)]
    } else {
      meanData <- dataInput()[time %in% seq.Date(subPeriod$start, subPeriod$end, by='day'), .(avg = mean(eval(parse(text=input$variableName)))), by = .(lon, lat)]
    }
    basemap() +
      geom_tile(aes(x =lon, y = lat, fill = avg, group = NULL),
                alpha=0.5,
                data = meanData) +
      scale_fill_distiller(type='div', palette='RdBu', trans='reverse',
                           guide = guide_legend(reverse=TRUE))
  })
  
  selectedPointID <- reactiveValues(id=1)
  observeEvent(input$locationClick, {
      lon <- input$locationClick$x
      lat <- input$locationClick$y
      distancePoints <- uniquePoints()[, geosphere::distGeo(c(V1, V2), c(lat, lon)), by=pointID]
      setkey(distancePoints, V1)
      selectedPointID$id <- distancePoints[1, pointID]
  })
  
  output$timeSeriesPlot <- renderDygraph({
    dygraph(xts::xts(dataInput()[pointID==selectedPointID$id, eval(parse(text=input$variableName))], dataInput()[pointID==selectedPointID$id, time])) %>% 
      dySeries("V1", label=input$variableName)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste('Earthquakes', '.csv', sep='') },
    content = function(file) {
      write.csv2(dataInput(), file)
    }
  )
  
})
