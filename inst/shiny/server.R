require(shiny)
#library(pisa)
require(devtools)
#install_github('likert','kspeer')
require(likert)
data(pisaitems)
source('C:/Users/User/Dropbox/Github/likert/R/xtable.likert.R')#rm this when github syncs

items24 <- pisaitems[,substr(names(pisaitems), 1,5) == 'ST24Q']

names(items24) <- c(
	  "I read only if I have to.",
	  "Reading is one of my favorite hobbies.",
	  "I like talking about books with other people.",
	  "I find it hard to finish books.",
	  "I feel happy if I receive a book as a present.",
	  "For me, reading is a waste of time.",
	  "I enjoy going to a bookstore or a library.",
	  "I read only to get information that I need.",
	  "I cannot sit still and read for more than a few minutes.",
	  "I like to express my opinions about books I have read.",
	  "I like to exchange books with my friends.")
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
            "l29" = l29)
  })
  
    
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset, 
            center=input$center,
            ordered=input$ordered)
  })
  
  output$print<-renderTable({
    dataset<-datasetInput()
    print(dataset)
  })
 
#   output$table<-renderTable({
#     dataset<-datasetInput()
#     xtab<-xtable(dataset)
#     print(xtab, include.rownames=FALSE)
#   })
  
#   output$table<-renderTable({
#     datasetInput()
#   }, 
#                             caption=input$caption,
#                             include.rownames=FALSE,
#                             include.n=input$include.n, 
#                             include.mean=input$include.mean,
#                             include.sd=input$include.sd,
#                             include.low=input$include.low,
#                             include.neutral=input$include.neutral,
#                             include.high=input$include.high,
#                             include.missing=input$include.missing
#                             #include.levels=input$include.levels
#                             )
  
  output$table<-renderTable({
    dataset <- datasetInput()
    xtab<-xtable(dataset,
                  caption=input$caption,
                  include.n=input$include.n, 
                  include.mean=input$include.mean,
                  include.sd=input$include.sd,
                  include.low=input$include.low,
                  include.neutral=input$include.neutral,
                  include.high=input$include.high,
                  include.missing=input$include.missing, 
                  center=input$center,
                  ordered=input$ordered
                  #include.levels=input$include.levels
                  )
    xtab
  })
     #add ,caption.placement='top',include.rownames=FALSE
  output$plot <- renderPlot({
    dataset <- datasetInput()
    p<-plot(dataset, 
            include.center=input$include.center, 
            centered=input$centered,
            ordered=input$ordered,
            center=input$center,
            wrap=input$wrap
            )
    print(p)
  })
})

