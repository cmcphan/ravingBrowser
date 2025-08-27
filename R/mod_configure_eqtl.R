#' Controls for configuring eQTL plot output
#'
#' @description Provides a set of inputs to allow the user to configure plot parameters specific to 
#' 	eQTL plots.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags
mod_configure_eqtl_ui <- function(id) {
  ns <- NS(id)
  tagList(
 		tags$hr(),
  		tags$h3('eQTLs')
  		# UI elements go here
  )
}
    
#' configure_eqtl Server Functions
#'
#' @noRd 
mod_configure_eqtl_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_configure_eqtl_ui("configure_eqtl_1")
    
## To be copied in the server
# mod_configure_eqtl_server("configure_eqtl_1")
