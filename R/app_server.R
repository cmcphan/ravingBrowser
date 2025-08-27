#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
	initial_choices = mod_necessary_setup_server("necessary_setup_1")
	observeEvent(initial_choices()$plot_types, {
		plot_types = initial_choices()$plot_types
		output$plot_controls = renderUI({
			output = tagList()
			if('Hi-C' %in% plot_types){
				output = c(output, mod_configure_hic_ui("configure_hic_1"))
			}
			if('Gene Features' %in% plot_types){
				output = c(output, mod_configure_genes_ui("configure_genes_1"))
			}
			if('ChIP-seq' %in% plot_types){
				output = c(output, mod_configure_chip_ui("configure_chip_1"))
			}
			if('snRNA-seq' %in% plot_types){
				output = c(output, mod_configure_rnaseq_ui("configure_rnaseq_1"))
			}
			if('eQTLs' %in% plot_types){
				output = c(output, mod_configure_eqtl_ui("configure_eqtl_1"))
			}
			if('FANTOM5' %in% plot_types){
				output = c(output, mod_configure_fantom5_ui("configure_fantom5_1"))
			}
			return(output)
		})
  }, ignoreNULL=FALSE )
}
