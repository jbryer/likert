col_types <- c('character', 'numeric', 'integer', 'logicial', 'factor')

#' A Shiny module for uploading a dataset.
#' 
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param auto_load whether the user should be prompted automatically the first time the data.frame
#'        is requested.
#' @return a reactive expression that will return a `data.frame`.
#' @export
data_import_server <- function(id, auto_load = TRUE) {
	shiny::moduleServer(id, function(input, output, session) {
		data <- shiny::reactiveVal(value = NULL)
		page <- shiny::reactiveVal(value = 1)
		
		shiny::observeEvent(input$data_file, {
			ext <- tools::file_ext(input$data_file$name)
			newdata <- switch(ext,
				   csv = utils::read.csv(input$data_file$datapath),
				   xlsx = readxl::read_excel(input$data_file$datapath),
				   validate("Invalid file type; Please upload a .csv or .xlsx file")
			)
			page(2)
			data(newdata)
		})
		
		output$page_one <- shiny::renderUI({
			shiny::div(
				p('Please select a CSV or Excel file to upload'),
				shiny::fileInput(
					inputId = shiny::NS(id, 'data_file'), 
					label = '', 
					buttonLabel = 'Browse...',
					accept = c('.csv', '.xlsx')
				)
			)
		})
		
		output$page_two <- shiny::renderUI({
			ui <- list()
			df <- data()
			if(!is.null(df) & is.data.frame(df)) {
				for(i in 1:ncol(df)) {
					ui[[length(ui) + 1]] <- shiny::fluidRow(
						shiny::column(12,
							shiny::checkboxInput(
								inputId = shiny::NS(id, names(df)[i]),
								label = names(df)[i],
								value = TRUE
							)
						)
						# shiny::column(6,
						# 	shiny::selectInput(
						# 		inputId = shiny::NS(id, paste0(names(df)[i], '_type')),
						# 		label = 'Type',
						# 		choices = col_types,
						# 		selected = class(df[,i])
						# 	)
						# )
					)
				}
			}
			do.call(shiny::div, ui)
		})
		
		shiny::reactive({
			if(is.null(data()) | page() == 1) {
				shiny::showModal(
					shiny::modalDialog(
						shiny::uiOutput(shiny::NS(id, 'page_one')),
						footer = shiny::modalButton('Cancel'),
						title = 'Data import: Upload file',
						size = 'xl',
						easyClose = FALSE,
						fade = TRUE
					)
				)
			} else if(page() == 2) {
				shiny::showModal(
					shiny::modalDialog(
						shiny::uiOutput(shiny::NS(id, 'page_two')),
						footer = shiny::modalButton('Cancel'),
						title = 'Data import: Select colunns',
						size = 'xl',
						easyClose = FALSE,
						fade = TRUE
					)
				)
			} else {
				shiny::removeModal()
			}
			return(data())
		})
	})
}