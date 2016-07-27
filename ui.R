library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("CarePath Comparison!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(sidebarPanel(
    selectInput("Client",
                "Select a client",
                # c('Client 1', 'Client 2', 'Client 3', 'Client 4')
                parent_orgs)
  )
  ,
  # Show a plot of the generated distribution
  mainPanel(plotOutput("CarePath")))
))