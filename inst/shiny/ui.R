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
    numericInput("wrap", "Length of wrapped text", 30),
    textInput('caption','Table Caption:','This is a table.'),#may have to wrap xtable for reactivity here
    checkboxInput('include.n','Include n',TRUE),
    checkboxInput('include.mean','Include mean',TRUE),
    checkboxInput('include.sd','Include sd',TRUE),
    checkboxInput('include.low','Include low',TRUE),
    checkboxInput('include.neutral','Include neutral',TRUE),
    checkboxInput('include.high','Include high',TRUE),
    checkboxInput('include.missing','Include missing',TRUE)
    ),
  

  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot")), 
      tabPanel("Summary", verbatimTextOutput("summary")),
      tabPanel("Print", tableOutput('print')),
      tabPanel("Table", tableOutput('table'))
      )
  )
))
