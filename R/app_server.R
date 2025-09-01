#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
	basic_config = mod_necessary_setup_server("necessary_setup_1")
	plot_types = basic_config$plot_type_select
	# Build dynamic UI
	observeEvent(plot_types(), {
		output$plot_controls = renderUI({
			output = tagList()
			# The below could be turned into a function to make it more
			#  scalable. Dictionary of plot type:mod name, iterate through each and
			#  build the required function name using paste, call using get()
			if('Hi-C' %in% plot_types()){
				output = c(output, mod_configure_hic_ui("configure_hic_1"))
			}
			if('Gene Features' %in% plot_types()){
				output = c(output, mod_configure_genes_ui("configure_genes_1"))
			}
			if('ChIP-seq' %in% plot_types()){
				output = c(output, mod_configure_chip_ui("configure_chip_1"))
			}
			if('snRNA-seq' %in% plot_types()){
				output = c(output, mod_configure_rnaseq_ui("configure_rnaseq_1"))
			}
			if('eQTLs' %in% plot_types()){
				output = c(output, mod_configure_eqtl_ui("configure_eqtl_1"))
			}
			if('FANTOM5' %in% plot_types()){
				output = c(output, mod_configure_fantom5_ui("configure_fantom5_1"))
			}
			return(output)
		})
		output$plot_pane = renderUI({
			output = tagList()
			if('Hi-C' %in% plot_types()){
				output = c(output, mod_plot_hic_ui("plot_hic_1"))
			}
			#if('Gene Features' %in% plot_types){
			#	output = c(output, mod_plot_genes_ui("plot_genes_1"))
			#}
			#if('ChIP-seq' %in% plot_types){
			#	output = c(output, mod_plot_chip_ui("plot_chip_1"))
			#}
			#if('snRNA-seq' %in% plot_types){
			#	output = c(output, mod_plot_rnaseq_ui("plot_rnaseq_1"))
			#}
			#if('eQTLs' %in% plot_types){
			#	output = c(output, mod_plot_eqtl_ui("plot_eqtl_1"))
			#}
			#if('FANTOM5' %in% plot_types){
			#	output = c(output, mod_plot_fantom5_ui("plot_fantom5_1"))
			#}
			return(output)
		})
  }, ignoreNULL=FALSE )
  
  hic_config = mod_configure_hic_server("configure_hic_1")
  observeEvent(hic_config$draw_plot() ,{
		# Build region config list - check which region bounds to use
		region_config = list(
				plot_types = basic_config$plot_type_select(),
				region_chr = basic_config$region_chr()
			)
		if(basic_config$toggle_region_size() %% 2 == 1){
				region_config$region_start = basic_config$region_size_direct_min()
				region_config$region_end = basic_config$region_size_direct_max()
		}
		else{
				region_config$region_start = basic_config$region_size_slider()[1]
				region_config$region_end = basic_config$region_size_slider()[2]
		}
		plot_config = list(
			elements = hic_config$elements(),
			resolution = hic_config$resolution(),
			normalization = hic_config$normalization(),
			format = hic_config$format()
		)
		mod_plot_hic_server("plot_hic_1", region_config, plot_config)
	})
}
