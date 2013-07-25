library(shiny)

# Define UI for application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("likert Demo"),
  
  # 
  sidebarPanel(#TODO read in defaults rather than setting
    selectInput("dataset", "Select an Item:", 
                choices=c('l24','l29')),
    checkboxInput("include.center", "Include center", TRUE),
    checkboxInput("centered", "Centered", TRUE),
    checkboxInput("ordered", "Ordered", TRUE),
    numericInput("center", "Center", 3, step=0.5),#TODO generalize based on default
    numericInput("wrap", "Length of wrapped text", 30)
  ),
  
  
  mainPanel(
    verbatimTextOutput("summary"),
    plotOutput("demoPlot")
  )
))
