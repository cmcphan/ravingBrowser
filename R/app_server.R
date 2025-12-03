#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinycssloaders showSpinner hideSpinner
#' @importFrom shinyjs addClass removeClass
#' @noRd
app_server <- function(input, output, session) {
	# Given that these are all passed through to each of the plotting functions,
  #  it would probably be more efficient to set these up inside of a single R6 
  #  object and just pass that through instead
  basic_config = mod_necessary_setup_server("necessary_setup_1")
  toolbar_config = mod_toolbar_server("toolbar_1")
	plot_types = basic_config$plot_type_select
  region = basic_config$region
  
	# Initialize session data
	session$userData$configs = list()
	current_plots = reactiveValues()
  prev_configs = reactiveValues()
  session$userData$patchwork_plot = reactiveVal(NULL)
  session$userData$plot_types = NULL
  session$userData$region = reactiveVal(NULL)
  session$userData$plot_heights = reactiveValues()
  session$userData$brush_zoom = reactiveVal(NULL)
  
  # Build list of server functions to grab plot specific configs
  # Access using `{type}_config`
  # Call all plot server functions to set up reactivity web
  for(type in names(browser_data$plot_types)){
    assign(paste0(type,'_config'),
      get(paste0('mod_configure_',type,'_server'))(paste0('configure_',type,'_1')))
  }
  plot_configs = list()
  # This needs to be done with lapply to avoid lazy evaluation handing the last
  #  plot config to all server functions. The above doesn't work properly with lapply
  #  for some reason so leave it in the for loop
  lapply(names(browser_data$plot_types), function(type){
    get(paste0('mod_plot_',type,'_server'))(paste0('plot_',type,'_1'), 
          basic_config, get(paste0(type,'_config')), current_plots, 
          prev_configs, toolbar_config)
    plot_configs[[type]] = get(paste0(type,'_config'))
  })
  mod_plot_server("plot_1", basic_config, current_plots, plot_configs, prev_configs, 
    toolbar_config)

	# Build dynamic UI
	observeEvent(plot_types(), {
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

		session$userData$plot_types = plot_types()
  }, ignoreNULL = FALSE )

  observeEvent(basic_config$draw_plots(),{
    if(!shiny::isTruthy(session$userData$region())){
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
	})

  observeEvent(session$userData$region(), {
    if(!shiny::isTruthy(session$userData$region())){
      return()
    }
    shiny::removeNotification(
      id = 'region_notif',
      session = session
    )
	})
}
