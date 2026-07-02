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
    for(m in browser_data$chip$chip_marks){
      current_plots[[paste0("chip-",m)]] = NULL
    }

    config = reactiveValues()
    build_config = function(){
      region = isolate(session$userData$activeRegion())
      if(!shiny::isTruthy(region)){
        config = reactiveValues()
        return()
      }
      config$chr = region$chr
      config$start = as.numeric(region$start)
      config$end = as.numeric(region$end)
      width = config$end - config$start
      config$resolution = as.integer(width / plot_config$resolution())
      config$marks = plot_config$elements()
      if(!("chip" %in% isolate(basic_config$plot_type_select()))){
        config$selected = FALSE
      }
      else { config$selected = TRUE }
      return(config)
    }

    draw_plots = function(config){
      if(length(config$marks) == 0){
        prev_configs[["chip"]] = config
        for(m in browser_data$chip$chip_marks){
          current_plots[[paste0("chip-",m)]] = NULL
          session$userData$plot_heights[[paste0("chip-",m)]] = 0
        }
        return()
      }
      
      # Reset any deselected plots
      for(m in browser_data$chip$chip_marks){
        if(!(m %in% config$marks)){
          current_plots[[paste0("chip-",m)]] = NULL
          session$userData$plot_heights[[paste0("chip-",m)]] = 0
        }
      }
      
      plots = plot_chip(config$chr, config$start, config$end, 
        config$resolution, config$marks, session)
      for(m in names(plots)){
        current_plots[[paste0("chip-",m)]] = plots[[m]]
      }
    }

    plotted_region = reactiveVal(NULL)
    observeEvent(basic_config$draw_plots(), {
      if(!shiny::isTruthy(session$userData$region())){
        return()
      }
      if(!("chip" %in% isolate(basic_config$plot_type_select()))){
        for(m in browser_data$chip$chip_marks){
          current_plots[[paste0("chip-",m)]] = NULL
          session$userData$plot_heights[[paste0("chip-",m)]] = 0
        }
        return()
      }

      config = build_config()
      if(identical(config, prev_configs[["chip"]]) | !config$selected){
        return()
      }
      draw_plots(config)
      prev_configs[["chip"]] = reactiveValuesToList(config)
      plotted_region(list(chr = config$chr, start = config$start, end = config$end))
      return()
    })

    observeEvent(session$userData$activeRegion(), {
      if(length(reactiveValuesToList(config)) == 0){
        return()
      }
      region = isolate(session$userData$activeRegion())
      config$chr = region$chr
      config$start = region$start
      config$end = region$end
      width = config$end - config$start
      config$resolution = as.integer(width / plot_config$resolution())
      if(identical(config, prev_configs[["chip"]]) | !config$selected){
        return()
      }
      draw_plots(config)
      prev_configs[["chip"]] = reactiveValuesToList(config)
      return()
    })

    observeEvent(toolbar_config$update_plots(), {
      if(length(reactiveValuesToList(config)) == 0){
        return()
      }
      config = build_config()
      if(identical(config, prev_configs[["chip"]]) | !config$selected){
        return()
      }
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
