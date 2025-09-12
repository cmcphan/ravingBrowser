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
		prev_types = session$userData$configs[['plot_types']]
		new_selections = plot_types()[!plot_types() %in% prev_types]
		deselected = prev_types[!prev_types %in% plot_types()]
		for(type in new_selections){
			insertUI(selector=place_ui(type, plot_types()), where='afterEnd',
				ui=get(paste0('mod_configure_',type,'_ui'))(paste0('configure_',type,'_1')))
		}
		for(type in deselected){
			removeUI(paste0('#', type, '_controls'))
			for(name in names(session$userData$plots)){
				if(startsWith(name, paste0(type, '-'))){
					session$userData$plots[[name]] = NULL
				}
			}
		}

		output$plot_ui = renderUI({
			output = tagList()
			for(type in plot_types()){
				elements = get(paste0(type,'_config'))$elements()
				output = c(output, tagList(
						get(paste0('mod_plot_',type,'_ui'))(paste0('plot_',type,'_1'), elements)
					)
				)
			}
			return(output)
		})

		session$userData$configs[['plot_types']] = plot_types()
  }, ignoreNULL = FALSE )
  
  observeEvent(basic_config$draw_plots(),{
		region_config = region_config(basic_config)
		region_change = !identical(session$userData$configs[['region']], region_config)

		### This can be turned into a for loop to avoid doing this process explicitly
		###  for every plot type - for each type in plot types, build up plot_config
		###  based on names(get(paste0(type,'_config')))
		if('hic' %in% plot_types()){
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

		if('chip' %in% plot_types()){
			plot_config = list(
				elements = chip_config$elements()
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
