#' Controls for configuring Hi-C plot output
#'
#' @description Provides a set of inputs to allow the user to configure plot parameters specific to 
#' 	Hi-C contact matrix plots.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags
mod_configure_hic_ui <- function(id) {
  ns <- NS(id)
  tagList(
  		tags$hr(),
  		tags$h2('Hi-C'),
 		checkboxGroupInput(
      inputId = ns('plot_elements'),
      label = 'Select any number of elements to plot:',
      choices = c('TADs', 'Loops', 'A/B Compartment Scores (PCA)')
    ),
    selectInput(
      inputId = ns('plot_resolution'),
      label = 'Resolution (base pairs):',
      choices = browser_data$resolutions,
      selected = browser_data$resolutions[1],
      multiple = FALSE
    )
  )
}
    
#' configure_hic Server Functions
#'
#' @noRd 
mod_configure_hic_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 		
  })
}
    
## To be copied in the UI
# mod_configure_hic_ui("configure_hic_1")
    
## To be copied in the server
# mod_configure_hic_server("configure_hic_1")
