#' plot_hic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_hic_ui <- function(id) {
  ns <- NS(id)
  tagList(
 		plotOutput('hic_plot')
		# Can add a brushOpts() object to the plotOutput function here to enable brushing
  )
}
    
#' plot_hic Server Functions
#'
#' @noRd 
mod_plot_hic_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 		
  })
}
    
## To be copied in the UI
# mod_plot_hic_ui("plot_hic_1")
    
## To be copied in the server
# mod_plot_hic_server("plot_hic_1")
