#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
	basic_config = mod_necessary_setup_server("necessary_setup_1")
	plot_types = basic_config$plot_type_select
	hic_config = mod_configure_hic_server("configure_hic_1")
	chip_config = mod_configure_chip_server("configure_chip_1")
	# Initialize session data
	session$userData$configs = list()
	session$userData$plots = list()

	# Build dynamic UI
	observeEvent(plot_types(), {
		output$controls = renderUI({
			output = tagList()
			# The below could be turned into a function to make it more
			#  scalable. Dictionary of plot type:mod name, iterate through each and
			#  build the required function name using paste, call using get()
			if('Hi-C' %in% plot_types()){
				output = c(output, mod_configure_hic_ui("configure_hic_1"))
			}
			else{
				session$userData$plots[['hic']] = NULL
				session$userData$plots[['loops']] = NULL
				session$userData$plots[['pca']] = NULL
			}
			if('Gene Features' %in% plot_types()){
				output = c(output, mod_configure_genes_ui("configure_genes_1"))
			}
			if('ChIP-seq' %in% plot_types()){
				output = c(output, mod_configure_chip_ui("configure_chip_1"))
			}
			else{
				# ChIP plots could be called anything depending on the sample name, so
				#  they should be prefixed to avoid naming clashes
				for(name in names(session$userData$plots)){
					if(startsWith(name, 'chip-')){
						session$userData$plots[[name]] = NULL
					}
				}
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

		output$plots = renderUI({
			output = tagList()
			if('Hi-C' %in% plot_types()){
				output = c(output, mod_plot_hic_ui("plot_hic_1",
					hic_config$elements()))
			}
			#if('Gene Features' %in% plot_types()){
			#	output = c(output, mod_plot_genes_ui("plot_genes_1"))
			#}
			if('ChIP-seq' %in% plot_types()){
				output = c(output, mod_plot_chip_ui("plot_chip_1",
					chip_config$chip_samples()))
			}
			#if('snRNA-seq' %in% plot_types()){
			#	output = c(output, mod_plot_rnaseq_ui("plot_rnaseq_1"))
			#}
			#if('eQTLs' %in% plot_types()){
			#	output = c(output, mod_plot_eqtl_ui("plot_eqtl_1"))
			#}
			#if('FANTOM5' %in% plot_types()){
			#	output = c(output, mod_plot_fantom5_ui("plot_fantom5_1"))
			#}
			return(output)
		})
  }, ignoreNULL = FALSE )
  
  observeEvent(basic_config$draw_plots(),{
		region_config = region_config(basic_config)
		region_change = !identical(session$userData$configs[['region']], region_config)

		if('Hi-C' %in% plot_types()){
			plot_config = list(
				elements = hic_config$elements(),
				resolution = hic_config$resolution(),
				normalization = hic_config$normalization(),
				format = hic_config$format()
			)
			if(!identical(session$userData$configs[['hic']], plot_config) |
					region_change){
				mod_plot_hic_server("plot_hic_1", region_config, plot_config)
			}
			session$userData$configs[['hic']] = plot_config
		}

		if('ChIP-seq' %in% plot_types()){
			plot_config = list(
				chip_samples = chip_config$chip_samples()
			)
			if(!identical(session$userData$configs[['chip']], plot_config) |
					region_change){
				mod_plot_chip_server("plot_chip_1", region_config, plot_config)
			}
			session$userData$configs[['chip']] = plot_config
		}

		session$userData$configs[['region']] = region_config
	})
}
