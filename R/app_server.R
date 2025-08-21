#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
	initial_choices = mod_necessary_setup_server("necessary_setup_1")
	output$plot_types = renderText({ initial_choices()$plot_types })
	output$region_chr = renderText({ initial_choices()$region_chr })
	output$region_size_slider = renderText({ initial_choices()$region_size_slider })
	output$region_size_direct_min = renderText({ initial_choices()$region_size_direct_min })
	output$region_size_direct_max = renderText({ initial_choices()$region_size_direct_max })
	output$toggle_region_size = renderText({ initial_choices()$toggle_region_size })
}
