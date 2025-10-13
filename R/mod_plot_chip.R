#' plot_chip UI Function
#'
#' @description Draw a ChIP track plot according to user-specified configuration.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param elements Reactive list of plot elements to include from the relevent 
#'  plot configuration UI server function.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList plotOutput renderPlot
#' @importFrom shinycssloaders withSpinner
mod_plot_chip_ui <- function(id, elements) {
  ns <- NS(id)
  return(NULL)
}
    
#' plot_chip Server Functions
#'
#' @param id Internal Shiny parameter
#' @param basic_config List of reactive functions - output from 
#'  mod_necessary_setup which details overall configuration settings
#' @param plot_config Reactive function from the relevant plot UI function which details
#'  the user selected plot configuration settings
#' @param current_plots reactiveValues object containing all currently active plots
#' @param prev_configs reactiveValues object containing configs for previous plots.
#'  Used to determine whether the plot needs to be redrawn or not.
#'
#' @noRd
#'
#' @importFrom cowplot align_plots ggdraw
mod_plot_chip_server <- function(id, basic_config, plot_config, current_plots,
  prev_configs){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Initialize and set plot order
    for(s in browser_data$chip$bw_sample_names){
      current_plots[[paste0("chip-",s)]] = NULL
    }

    observeEvent(basic_config$draw_plots(), {
      if(!("chip" %in% isolate(basic_config$plot_type_select()))){
        for(s in browser_data$chip$bw_sample_names){
          current_plots[[paste0("chip-",s)]] = NULL
          session$userData$plot_heights[[paste0("chip-",s)]] = 0
          prev_configs[["chip"]]$selected = FALSE
        }
        return()
      }

      region = basic_config$region()
      if(!shiny::isTruthy(region())){
        return()
      }
      config = list()
      config$chr = region()$region_chr
      config$start = as.numeric(region()$region_start)
      config$end = as.numeric(region()$region_end)
      config$resolution = plot_config$resolution()
      config$chip_samples = plot_config$elements()
      config$selected = TRUE
      if(length(config$chip_samples) == 0){
        prev_configs[["chip"]] = config
        for(s in browser_data$chip$bw_sample_names){
          current_plots[[paste0("chip-",s)]] = NULL
          session$userData$plot_heights[[paste0("chip-",s)]] = 0
        }
        return()
      }
      if(identical(config, prev_configs[["chip"]])){
        return()
      }
      # Reset any deselected plots
      for(s in browser_data$chip$bw_sample_names){
        if(!(s %in% config$chip_samples)){
          current_plots[[paste0("chip-",s)]] = NULL
          session$userData$plot_heights[[paste0("chip-",s)]] = 0
        }
      }
      
      plots = plot_chip(config$chr, config$start, config$end, 
        config$resolution, config$chip_samples, session)
      for(s in names(plots)){
        current_plots[[paste0("chip-",s)]] = plots[[s]]
      }

      prev_configs[["chip"]] = config
      return()
    })
  })
}
    
## To be copied in the UI
# mod_plot_chip_ui("plot_chip_1")
    
## To be copied in the server
# mod_plot_chip_server("plot_chip_1")
