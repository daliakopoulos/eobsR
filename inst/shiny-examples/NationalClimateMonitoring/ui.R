library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("National Climate Monitoring using EOBS"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Country", list("DE", "NL", "GBR"),
                  selected = "NL"),
      selectInput("period", "Year", list("2014", "2013", "2012"),
                  selected = "2014")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("countryPlot")
    )
  )
))