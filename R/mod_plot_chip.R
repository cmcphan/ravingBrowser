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
#' @param toolbar_config List of reactive functions - output from mod_toolbar which
#'  contains toolbar button data
#'
#' @noRd
#'
#' @importFrom cowplot align_plots ggdraw
mod_plot_chip_server <- function(id, basic_config, plot_config, current_plots,
  prev_configs, toolbar_config){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Initialize and set plot order
    for(s in browser_data$chip$bw_sample_names){
      current_plots[[paste0("chip-",s)]] = NULL
    }

    config = reactiveValues()
    build_config = function(){
      region = session$userData$region()
      if(!shiny::isTruthy(region)){
        config = reactiveValues()
        return()
      }
      config$chr = region$chr
      config$start = as.numeric(region$start)
      config$end = as.numeric(region$end)
      config$resolution = plot_config$resolution()
      config$chip_samples = plot_config$elements()
      config$selected = TRUE
      return(config)
    }

    draw_plots = function(config){
      if(length(config$chip_samples) == 0){
        prev_configs[["chip"]] = config
        for(s in browser_data$chip$bw_sample_names){
          current_plots[[paste0("chip-",s)]] = NULL
          session$userData$plot_heights[[paste0("chip-",s)]] = 0
        }
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
    }

    plotted_region = reactiveVal(NULL)
    observeEvent(basic_config$draw_plots(), {
      if(!("chip" %in% isolate(basic_config$plot_type_select()))){
        for(s in browser_data$chip$bw_sample_names){
          current_plots[[paste0("chip-",s)]] = NULL
          session$userData$plot_heights[[paste0("chip-",s)]] = 0
          prev_configs[["chip"]]$selected = FALSE
        }
        return()
      }

      config = build_config()
      if(identical(reactiveValuesToList(config), prev_configs[["chip"]])){
        return()
      }
      
      draw_plots(config)
      prev_configs[["chip"]] = reactiveValuesToList(config)
      plotted_region(list(chr = config$chr, start = config$start, end = config$end))
      return()
    })

    observeEvent(toolbar_config$zoom_in(), {
      if(length(reactiveValuesToList(config)) == 0){
        return()
      }
      zoomed_region = zoom(config$chr, config$start, config$end, "in")
      if(zoomed_region$width <= 1){
        return()
      }
      res_scale = config$resolution / (config$end-config$start)
      config$start = zoomed_region$start
      config$end = zoomed_region$end
      config$resolution = floor((config$end-config$start) * res_scale)
      draw_plots(config)
      prev_configs[["chip"]] = reactiveValuesToList(config)
      return()
    })

    observeEvent(toolbar_config$zoom_out(), {
      if(length(reactiveValuesToList(config)) == 0){
        return()
      }
      zoomed_region = zoom(config$chr, config$start, config$end, "out")
      if(config$start == zoomed_region$start & config$end == zoomed_region$end){
        return()
      }
      res_scale = config$resolution / (config$end-config$start)
      config$start = zoomed_region$start
      config$end = zoomed_region$end
      config$resolution = floor((config$end-config$start) * res_scale)
      draw_plots(config)
      prev_configs[["chip"]] = reactiveValuesToList(config)
      return()
    })

    observeEvent(toolbar_config$update_plots(), {
      if(length(reactiveValuesToList(config)) == 0){
        return()
      }
      chr = config$chr
      start = config$start
      end = config$end
      resolution = config$resolution
      config = build_config()
      # If the region has been changed, prevent the resolution from being updated
      #  relative to the new region. This prevents abusing the UI to get a much
      #  higher resolution than should be possible for the given plot.
      comp = plotted_region()
      if(config$chr != comp$chr | 
        config$start != comp$start | 
        config$end != comp$end){
        config$resolution = resolution
      }
      config$chr = chr
      config$start = start
      config$end = end
      draw_plots(config)
      prev_configs[["chip"]] = reactiveValuesToList(config)
      return()
    })
  })
}
    
## To be copied in the UI
# mod_plot_chip_ui("plot_chip_1")
    
## To be copied in the server
# mod_plot_chip_server("plot_chip_1")
