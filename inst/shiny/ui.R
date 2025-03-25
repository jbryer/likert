shinyUI(pageWithSidebar(
  headerPanel("Likert Package Demo"),
  
  sidebarPanel(
    style = "height: 90vh; overflow-y: auto;",
    tabsetPanel(
      tabPanel('Data',
               uiOutput('dataset_ui'),
               uiOutput('grouping_ui'),
               uiOutput('items_ui')
      ),
      tabPanel('Options',
               uiOutput('options_ui')
               # checkboxInput("include.center", "Include center", TRUE),
               # checkboxInput("centered", "Centered", TRUE),
               # checkboxInput("ordered", "Ordered", TRUE),
               # numericInput("center", "Center", 3, step=0.5),#TODO generalize based on default
               # numericInput("wrap", "Length of wrapped text", 30),
               # textInput('caption','Table Caption:','This is a table.'),#may have to wrap xtable for reactivity here
               # checkboxInput('include.n','Include n',TRUE),
               # checkboxInput('include.mean','Include mean',TRUE),
               # checkboxInput('include.sd','Include sd',TRUE),
               # checkboxInput('include.low','Include low',TRUE),
               # checkboxInput('include.neutral','Include neutral',TRUE),
               # checkboxInput('include.high','Include high',TRUE),
               # checkboxInput('include.missing','Include missing',TRUE)
      )
    )
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot", height = '800px')), 
      # tabPanel("Summary", verbatimTextOutput("summary")),
      # tabPanel("Print", tableOutput('print')),
      tabPanel("Data", DT::dataTableOutput('table')),
      tabPanel("Code", shiny::verbatimTextOutput('code'))
    )
  )
))
