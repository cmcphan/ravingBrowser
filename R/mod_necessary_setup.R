#' UI elements common to all plot types
#'
#' @description Module for the initial browser setup options. These options are required for visualizations 
#'  regardless of the desired plot, e.g. which plots to show, which region to show, and determine what elements
#'  to populate the rest of the sidebar with
#' @param id,input,output,session Internal parameters for `{shiny}`.
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyjs disabled toggleState
mod_necessary_setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxGroupInput(
      inputId = ns('plot_type_select'),
      label = 'Select any number of plot types:',
      choices = c('Hi-C', 'Gene Features', 'ChIP-seq', 'snRNA-seq', 'eQTLs', 'FANTOM5')
    ),
    selectInput(
      inputId = ns('region_chr'),
      label = 'Chromosome:',
      choices = browser_data$hic_info$name,
      selected = browser_data$default_chr,
      multiple = FALSE
    ),
    sliderInput(
      inputId = ns('region_size_slider'),
      label = 'Region limits (in base pairs):',
      min = 1,
      max = browser_data$hic_info[browser_data$default_chr, 'length'],
      value = c(1, browser_data$default_chr_length)
    ),
    actionButton(
      inputId = ns('toggle_region_size'),
      label = 'Switch range input'
    ),
    shinyjs::disabled(
      numericInput(
        inputId = ns('region_size_direct_min'),
        label = 'Region start:',
        value = 1,
        min = 1,
        max = browser_data$default_chr_length-1
      )
    ),
    shinyjs::disabled(
      numericInput(
        inputId = ns('region_size_direct_max'),
        label = 'Region end:',
        value = browser_data$default_chr_length,
        min = 2,
        max = browser_data$default_chr_length
      )
    )
  )
}
    
#' necessary_setup Server Functions
#'
#' @noRd 
mod_necessary_setup_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$toggle_region_size, {
      shinyjs::toggleState(id='region_size_slider')
      shinyjs::toggleState(id='region_size_direct_min')
      shinyjs::toggleState(id='region_size_direct_max')
      # Slider input starts enabled (button == 0), therefore if the button is odd,
      # 		the numeric input must be enabled, otherwise the slider input must be enabled
      if(input$toggle_region_size %% 2 == 1){
      		# Copy currently selected values onto the other input method
      		updateNumericInput(session, 'region_size_direct_min', value=input$region_size_slider[1])
      		updateNumericInput(session, 'region_size_direct_max', value=input$region_size_slider[2])
      }
      else{
      		updateSliderInput(session, 'region_size_slider', 
      			value=c(input$region_size_direct_min, input$region_size_direct_max))
      }
    })
    observeEvent(input$region_chr, {
    		updated_max = browser_data$hic_info[input$region_chr, 'length']
    		updateSliderInput(session, 'region_size_slider', max=updated_max, value=c(1, updated_max))
    		updateNumericInput(session, 'region_size_direct_min', max=updated_max-1, value=1)
    		updateNumericInput(session, 'region_size_direct_max', max=updated_max, value=updated_max)
    })
    
    # Error checking for direct inputs
    observeEvent(input$region_size_direct_min, {
    		current_max = browser_data$hic_info[input$region_chr, 'length']
    		if(input$region_size_direct_min >= input$region_size_direct_max){
    			# Warn the user and disable plot painting
    		}
    		# Invalid inputs, e.g. non numeric strings, get returned as NULL
    		if(is.null(input$region_size_direct_min)){
    			# Warn the user and disable plot painting
    		}
    		# Values beyond the allowable ranges can be directly input and be returned, so need to check
    		if(input$region_size_direct_min < 1 | input$region_size_direct_min > current_max-1){
    			# Warn the user and disable plot painting
    		}
    })
    return(
    		reactive({
		  		inputs = list(plot_types = input$plot_type_select,
					region_chr = input$region_chr,
					region_size_slider = input$region_size_slider,
					toggle_region_size = input$toggle_region_size,
					region_size_direct_min = input$region_size_direct_min,
					region_size_direct_max = input$region_size_direct_max
				)
    		})
    	)
  })
}
    
## To be copied in the UI
# mod_necessary_setup_ui("necessary_setup_1")
    
## To be copied in the server
# mod_necessary_setup_server("necessary_setup_1")
