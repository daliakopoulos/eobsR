library(shiny)
library(dygraphs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("National Climate Monitoring using EOBS"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("variableName", "variable", list("tg", "tn", "tx", "pp", "rr"),
                  selected = "tg"),
      selectInput("country", "Country", list("BE", "DE", "GBR", "NL"),
                  selected = "NL"),
      selectInput("period", "Year", as.list(paste(2015:1950)), #list("2014", "2013", "2012"),
                  selected = "2014"),
      selectInput("grid", "Grid", list("0.50reg", "0.25reg"),
                  selected = "0.50reg")#,
      #verbatimTextOutput("info")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("countryPlot",
                 click = "locationClick"),
      dygraphOutput("timeSeriesPlot")
    )
  )
))