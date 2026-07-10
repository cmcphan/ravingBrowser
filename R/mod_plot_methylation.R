#' plot_methylation UI Function
#'
#' @description Draw a methylation track plot according to user-specified configuration.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param elements Reactive list of plot elements to include from the relevent 
#'  plot configuration UI server function.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList plotOutput renderPlot
#' @importFrom shinycssloaders withSpinner
mod_plot_methylation_ui <- function(id, elements) {
  ns <- NS(id)
  return(NULL)
}
    
#' plot_methylation Server Functions
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
mod_plot_methylation_server <- function(id, basic_config, plot_config, current_plots,
  prev_configs, toolbar_config){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

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
      config$statuses = plot_config$elements()
      if(!("methylation" %in% isolate(basic_config$plot_type_select()))){
        config$selected = FALSE
      }
      else { config$selected = TRUE }
      return(config)
    }

    draw_plots = function(config){
      if(length(config$statuses) == 0){
        prev_configs[["methylation"]] = config
        current_plots[[paste0("methylation")]] = NULL
        session$userData$plot_heights[["methylation"]] = 0
        return()
      }
      
      plot = plot_methylation(config$chr, config$start, config$end, 
        config$statuses, session)
      current_plots[["methylation"]] = plot
    }

    observeEvent({basic_config$draw_plots() | toolbar_config$update_plots() |
      session$userData$regionChange()}, {
      if(!shiny::isTruthy(session$userData$region())){
        return()
      }
      if(basic_config$draw_plots()==0 & toolbar_config$update_plots()==0){
        return()
      }
      if(!("methylation" %in% isolate(basic_config$plot_type_select()))){
        current_plots[["methylation"]] = NULL
        session$userData$plot_heights[["methylation"]] = 0
        return()
      }

      config = build_config()
      if(identical(reactiveValuesToList(config), prev_configs[["methylation"]]) | 
      !config$selected){
        return()
      }
      draw_plots(config)
      prev_configs[["methylation"]] = reactiveValuesToList(config)
      return()
    })
  })
}
    
## To be copied in the UI
# mod_plot_atac_ui("plot_atac_1")
    
## To be copied in the server
# mod_plot_atac_server("plot_atac_1")
