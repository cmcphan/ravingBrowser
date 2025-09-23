#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinycssloaders showSpinner hideSpinner
#' @importFrom shinyjs addClass removeClass
#' @noRd
app_server <- function(input, output, session) {
	basic_config = mod_necessary_setup_server("necessary_setup_1")
	plot_types = basic_config$plot_type_select
  region = basic_config$region_config
  observeEvent(basic_config$current_tab(), {
    region = basic_config$region_config
  })

	# Initialize session data
	session$userData$configs = list()
	session$userData$plots = list()
  session$userData$plot_types = NULL
  session$userData$regionChange = FALSE
  # Build list of server functions to grab plot specific configs
  # Access using `{type}_config`
  for(type in names(browser_data$plot_types)){
    assign(paste0(type,'_config'),
      get(paste0('mod_configure_',type,'_server'))(paste0('configure_',type,'_1')))
  }

	# Build dynamic UI
	observeEvent(plot_types(), {
    session$userData$region = region()()
		prev_types = session$userData$plot_types
		new_selections = plot_types()[!plot_types() %in% prev_types]
		deselected = prev_types[!prev_types %in% plot_types()]
		for(type in new_selections){
			insertUI(selector=place_ui(type, plot_types()), where='afterEnd',
				ui=get(paste0('mod_configure_',type,'_ui'))(paste0('configure_',type,'_1'), 
          session))
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

		session$userData$plot_types = plot_types()
  }, ignoreNULL = FALSE )

  # Having the nested function call like this looks kind of mangled but is the
  #  only way I could get this to work properly - other configurations either don't
  #  update reactively or for some reason reset the UI every time they're called
  observeEvent(region()(), {
    if(!shiny::isTruthy(region()())){
      return()
    }
    shiny::removeNotification(
      id = 'region_notif',
      session = session
    )
    session$userData$region = region()()
    session$userData$regionChange = TRUE
    for(type in plot_types()){
      get(paste0('mod_configure_',type,'_server'))(paste0('configure_',type,'_1'))
    }
	})

  observeEvent(basic_config$draw_plots(),{
    if(!shiny::isTruthy(region()())){
      shiny::showNotification(
        'Region is not set. Input a region or select a gene.',
        duration = NULL,
        closeButton = FALSE,
        id = 'region_notif',
        type = 'error',
        session = session
      )
      return()
    }
    region_change = session$userData$regionChange

    shinycssloaders::showSpinner('plot_ui')
    for(type in plot_types()){
      shinyjs::addClass(selector='*.shiny-plot-output', class='recalculating')
    }
		for(type in names(browser_data$plot_types)){
      print(paste0('Plotting ',type))
      if(type %in% plot_types()){
        # Build plot config
        plot_config = list()
        for(n in names(get(paste0(type,'_config')))){
          plot_config[[n]] = get(paste0(type,'_config'))[[n]]()
        }
        # Paint plot if configuration or region has changed
        if(!identical(session$userData$configs[[type]], plot_config) | region_change){
          get(paste0('mod_plot_',type,'_server'))(paste0('plot_',type,'_1'), 
            region()(), plot_config)
        }
        else{
          get(paste0('mod_plot_',type,'_server'))(paste0('plot_',type,'_1'), 
            region()(), plot_config, draw=FALSE)
        }
        session$userData$configs[[type]] = plot_config
      }
      # For plots not selected, if the region has changed nullify their outputs
      else if(region_change){
        get(paste0('mod_plot_',type,'_server'))(paste0('plot_',type,'_1'), 
          invalidate=TRUE)
      }
      print(paste0('Finished plotting ',type))
    }
    for(type in plot_types()){
      shinyjs::removeClass(selector='*.shiny-plot-output', class='recalculating')
    }
    shinycssloaders::hideSpinner('plot_ui')

		session$userData$region = region()
    session$userData$regionChange = FALSE
	})
}
