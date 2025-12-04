#' plot_hic UI Function
#'
#' @description Draw a Hi-C matrix plot according to user-specified configuration.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param elements Reactive list of plot elements to include from the relevent 
#'  plot configuration UI server function.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList plotOutput renderPlot
#' @importFrom shinycssloaders withSpinner
mod_plot_hic_ui <- function(id, elements){
  ns <- NS(id)
  return(NULL)
}
    
#' plot_hic Server Functions
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
mod_plot_hic_server <- function(id, basic_config, plot_config, current_plots, 
  prev_configs, toolbar_config){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Initialize and set plot order
    current_plots[["hic-hic"]] = NULL
    current_plots[["hic-loops"]] = NULL
    current_plots[["hic-pca"]] = NULL

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
      config$resolution = as.numeric(plot_config$resolution())
      config$normalization = plot_config$normalization()
      config$format = plot_config$format()
      config$elements = plot_config$elements()
      config$selected = TRUE
      return(config)
    }

    draw_plots = function(config){
      current_plots[["hic-hic"]] = plot_hic(config$chr, config$start, config$end, 
        config$resolution, config$normalization, config$format, session)
      if("tads" %in% config$elements){
        current_plots[["hic-hic"]] = draw_tads(current_plots[["hic-hic"]], config$chr, 
          config$start, config$end)
      }
      if("loops" %in% config$elements){
        current_plots[["hic-loops"]] = plot_loops(config$chr, config$start, 
          config$end, session)
      }
      else { 
        current_plots[["hic-loops"]] = NULL
        session$userData$plot_heights[["hic-loops"]] = 0
      }
      if("pca" %in% config$elements){
        current_plots[["hic-pca"]] = plot_pca(config$chr, config$start, config$end,
          session)
      }
      else { 
        current_plots[["hic-pca"]] = NULL
        session$userData$plot_heights[["hic-pca"]] = 0
      }
    }

    observeEvent(basic_config$draw_plots(), {
      if(!shiny::isTruthy(session$userData$region())){
        return()
      }
      if(!('hic' %in% isolate(basic_config$plot_type_select()))){
        current_plots[["hic-hic"]] = NULL
        session$userData$plot_heights[["hic-hic"]] = 0
        current_plots[["hic-loops"]] = NULL
        session$userData$plot_heights[["hic-loops"]] = 0
        current_plots[["hic-pca"]] = NULL
        session$userData$plot_heights[["hic-pca"]] = 0
        prev_configs[["hic"]]$selected = FALSE
        return()
      }

      config = build_config()
      if(identical(reactiveValuesToList(config), prev_configs[["hic"]])){
        return()
      }
      
      draw_plots(config)
      prev_configs[["hic"]] = reactiveValuesToList(config)
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
      draw_plots(config)
      prev_configs[["hic"]] = reactiveValuesToList(config)
      return()
    })

    observeEvent(toolbar_config$update_plots(), {
      if(length(reactiveValuesToList(config)) == 0){
        return()
      }
      config = build_config()
      if(identical(config, prev_configs[["hic"]])){
        return()
      }
      draw_plots(config)
      prev_configs[["hic"]] = reactiveValuesToList(config)
      return()
    })
  })  
}
    
## To be copied in the UI
# mod_plot_hic_ui("plot_hic_1")
    
## To be copied in the server
# mod_plot_hic_server("plot_hic_1")
