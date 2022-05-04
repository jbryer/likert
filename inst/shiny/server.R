shinyServer(function(input, output) {
	##### Utility reactive functions ###########################################
	getData <- reactive({
		req(input$dataset)
		dataset <- datasets[[input$dataset]]$data
		if(!is.null(input$columns) & all(input$columns %in% names(dataset))) {
			dataset <- dataset[,input$columns,drop=FALSE]
		}
		return(dataset)
	})
	
	getLikert <- reactive({
		req(input$grouping)
		
		dataset <- getData()
		if(input$grouping != 'None' & input$grouping %in% names(datasets[[input$dataset]]$grouping)) {
			return(likert(items = dataset, 
						  grouping = datasets[[input$dataset]]$grouping[,input$grouping,drop=TRUE])
			)
		} else {
			return(likert(items = dataset))
		}
	})
	
	getOptions <- reactive({
		options <- formals(likert.options)
		opt <- list()
		for(param in names(options)) {
			if('...' == param) {
				next;
			}
			inputId <- paste0('param_', param)
			opt[[param]] <- input[[inputId]]
		}
		return(do.call(likert.options, opt))
	})
	
	##### UI Components ########################################################
	output$grouping_ui <- renderUI({
		req(input$dataset)
		
		grouping_names <- c('None')
		groupings <- datasets[[input$dataset]]$groupings
		if(!is.null(groupings)) {
			# TODO: Maybe allow for a vector as well
			if(is.data.frame(groupings)) {
				grouping_names <- c(grouping_names, names(datasets[[input$dataset]]$grouping))
			} else {
				warning(paste0(input$dataset, '$grouping is not a data.frame. Ignoring.'))
			}
		}
		selectInput(inputId = 'grouping',
					label = 'Grouping',
					choices = grouping_names)
	})
	
	output$items_ui <- renderUI({
		dataset <- getData()
		selectInput(inputId = 'columns',
					label = 'Include items',
					choices = names(dataset),
					selected = names(dataset),
					multiple = TRUE)
	})
	
	output$dataset_ui <- renderUI({
		selectInput('dataset',
					label = 'Select a dataset',
					choices = names(datasets))
	})
	
	output$options_ui <- renderUI({
		options <- formals(likert.options)
		ui <- list()
		for(param in names(options)) {
			if('...' == param) {
				next;
			}
			inputId <- paste0('param_', param)
			val <- options[[param]]
			if(is.call(val)) {
				val <- eval(val)
			}
			if(length(grep('color', param)) > 0) { # Color parameter
				ui[[length(ui) + 1]] <- colourpicker::colourInput(inputId = inputId,
																  label = param,
																  value = val)
			} else if(length(val) > 1) {
				ui[[length(ui) + 1]] <- shiny::selectInput(inputId = inputId,
														   label = param,
														   choices = val,
														   selected = val[1])
			} else if(is.numeric(val)) {
				ui[[length(ui) + 1]] <- shiny::numericInput(inputId = inputId,
															label = param,
															value = val)
			} else if(is.logical(val)) {
				ui[[length(ui) + 1]] <- shiny::checkboxInput(inputId = inputId,
															 label = param,
															 value = val)
			} else {
				ui[[length(ui) + 1]] <- shiny::textInput(inputId = inputId,
														 label = param,
														 value = val)
			}
		}
		return(do.call(div, ui))
	})
	
	##### Outputs ##############################################################
	output$plot <- renderPlot({
		l <- getLikert()
		opt <- getOptions()
		opt$x <- l
		opt$type <- 'bar'
		do.call(likert:::plot.likert, opt)
		
		# p <- plot(l
		# 		  # include.center = input$include.center, 
		# 		  # centered = input$centered,
		# 		  # ordered = input$ordered,
		# 		  # center = input$center,
		# 		  # wrap = input$wrap
		# )
		# p
	})
	
	output$summary <- renderPrint({
		dataset <- getData()
		summary(dataset, 
				center=input$center,
				ordered=input$ordered)
	})
	
	output$print <- renderTable({
		dataset <- getData()
		print(dataset)
	})
	
	output$table <- shiny::renderDataTable({
		getData()
	})	
})

