#' Controls for configuring snRNA-seq plot output
#'
#' @description Provides a set of inputs to allow the user to configure plot parameters specific to 
#' 	snRNA-seq plots.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags 
mod_configure_rnaseq_ui <- function(id) {
  ns <- NS(id)
  tagList(
 		tags$hr(),
  		tags$h3('snRNA-seq')
  		# UI elements go here
  )
}
    
#' configure_rnaseq Server Functions
#'
#' @noRd 
mod_configure_rnaseq_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_configure_rnaseq_ui("configure_rnaseq_1")
    
## To be copied in the server
# mod_configure_rnaseq_server("configure_rnaseq_1")
