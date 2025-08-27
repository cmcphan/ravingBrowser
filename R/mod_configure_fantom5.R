#' Controls for configuring FANTOM5 plot output
#'
#' @description Provides a set of inputs to allow the user to configure plot parameters specific to 
#' 	FANTOM5 plots.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags 
mod_configure_fantom5_ui <- function(id) {
  ns <- NS(id)
  tagList(
 		tags$hr(),
  		tags$h3('FANTOM5')
  		# UI elements go here
  )
}
    
#' configure_fantom5 Server Functions
#'
#' @noRd 
mod_configure_fantom5_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_configure_fantom5_ui("configure_fantom5_1")
    
## To be copied in the server
# mod_configure_fantom5_server("configure_fantom5_1")
