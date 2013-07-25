require(shiny)
#library(pisa)
require(devtools)
require(likert)
data(pisaitems)

items24 <- pisaitems[,substr(names(pisaitems), 1,5) == 'ST24Q']

items24 <- rename(items24, c(
  ST24Q01="I read only if I have to.",
  ST24Q02="Reading is one of my favorite hobbies.",
  ST24Q03="I like talking about books with other people.",
  ST24Q04="I find it hard to finish books.",
  ST24Q05="I feel happy if I receive a book as a present.",
  ST24Q06="For me, reading is a waste of time.",
  ST24Q07="I enjoy going to a bookstore or a library.",
  ST24Q08="I read only to get information that I need.",
  ST24Q09="I cannot sit still and read for more than a few minutes.",
  ST24Q10="I like to express my opinions about books I have read.",
  ST24Q11="I like to exchange books with my friends."))
l24 = likert(items24)

l24g <- likert(items24, grouping=pisaitems$CNT)

items29 <- pisaitems[,substr(names(pisaitems), 1,5) == 'ST25Q']
names(items29) = c("Magazines", "Comic books", "Fiction", "Non-fiction books", "Newspapers")
l29 <- likert(items29)
l29g <- likert(items29, grouping=pisaitems$CNT)

# Define server logic
shinyServer(function(input, output) {
  
  # Return the requested dataset #TODO have this switch between pisa items
  datasetInput <- reactive({
    switch(input$dataset,
            "l24" = l24,
            "l29" = l29
            )
  })
  
    
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)#TODO change with plot changes
  })
  
   
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested

  output$demoPlot <- renderPlot({
    dataset <- datasetInput()
    p<-plot(dataset, 
            include.center=input$include.center, 
            centered=input$centered,
            ordered=input$ordered,
            center=input$center,
            wrap=input$wrap,
            )
    print(p)
  })
})

