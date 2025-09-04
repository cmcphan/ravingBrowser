#' Controls for configuring gene feature plot output
#'
#' @description Provides a set of inputs to allow the user to configure plot parameters
#'  specific to gene feature plots.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags
mod_configure_genes_ui <- function(id) {
  ns <- NS(id)
  tagList(
 		tags$hr(),
  		tags$h3('Gene Features')
  		# UI elements go here
  )
}
    
#' configure_genes Server Functions
#'
#' @noRd 
mod_configure_genes_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_configure_genes_ui("configure_genes_1")
    
## To be copied in the server
# mod_configure_genes_server("configure_genes_1")
