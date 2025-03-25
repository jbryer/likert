library(shiny)
library(DT)
library(likert)
source('data_import_module.R')

data(mass)

##### User Interface ###############################################################################
ui <- fluidPage(
    titlePanel("Data Import Test App"),

    DT::dataTableOutput('data_table')
)

##### Server  ######################################################################################
server <- function(input, output) {
	data <- data_import_server('likert')
	
	output$data_table <- DT::renderDataTable({
		data()
	})
}

##### Run the app ##################################################################################
shinyApp(ui = ui, server = server)
