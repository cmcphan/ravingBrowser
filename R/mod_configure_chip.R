#' Controls for configuring ChIP-seq plot output
#'
#' @description Provides a set of inputs to allow the user to configure plot parameters specific to 
#' 	ChIP-seq plots.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags
mod_configure_chip_ui <- function(id) {
  ns <- NS(id)
  tagList(
 		tags$hr(),
  		tags$h3('ChIP-seq')
  		# UI elements go here
  )
}
    
#' configure_chip Server Functions
#'
#' @noRd 
mod_configure_chip_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_configure_chip_ui("configure_chip_1")
    
## To be copied in the server
# mod_configure_chip_server("configure_chip_1")
