#' UI elements common to all plot types
#'
#' @description Module for the initial browser setup options. These options are required for visualizations 
#'  regardless of the desired plot, e.g. which plots to show, which region to show, and determine what elements
#'  to populate the rest of the sidebar with
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyjs useShinyjs disabled toggleState
mod_necessary_setup_ui <- function(id) {
  ns <- NS(id)
  useShinyjs()
  default_chr = hic_info$name[1]
  default_chr_length = hic_info[default_chr, 'length']
  tagList(
    checkboxGroupInput(
      inputId = ns('plot_type_select'),
      label = 'Select any number of plot types:',
      choices = c('Hi-C', 'Gene Features', 'ChIP-seq', 'snRNA-seq', 'eQTLs', 'FANTOM5'),
      width = '80%'
    ),
    selectInput(
      inputId = ns('region_chr'),
      label = 'Chromosome:',
      choices = hic_info$name,
      selected = default_chr,
      multiple = FALSE
    ),
    sliderInput(
      inputId = ns('region_size_slider'),
      label = 'Region limits (in base pairs):',
      min = 1,
      max = hic_info[default_chr, 'length'],
      value = c(1, default_chr_length)
    ),
    actionButton(
      inputId = ns('toggle_region_size'),
      label = 'Switch range input'
    ),
    disabled(
      numericInput(
        inputId = ns('region_size_direct_min'),
        label = 'Region start:',
        value = 1,
        min = 1,
        max = default_chr_length-1
      )
    ),
    disabled(
      numericInput(
        inputId = ns('region_size_direct_max'),
        label = 'Region end:',
        value = default_chr_length,
        min = 2,
        max = default_chr_length
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
    		print('Switch clicked')
      toggleState(id='region_size_slider')
      toggleState(id='region_size_direct_min')
      toggleState(id='region_size_direct_max')
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
