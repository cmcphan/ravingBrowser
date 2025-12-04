#' plot_genes UI Function
#'
#' @description Draw a gene feature plot according to user-specified configuration.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param elements Reactive list of plot elements to include from the relevent 
#'  plot configuration UI server function.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinycssloaders withSpinner
mod_plot_genes_ui <- function(id, elements) {
  ns <- NS(id)
  return(NULL)
}
    
#' plot_genes Server Functions
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
mod_plot_genes_server <- function(id, basic_config, plot_config, current_plots,
  prev_configs, toolbar_config){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Initialize and set plot order
    current_plots[["genes-gene_track"]] = NULL

    config = reactiveValues()
    build_config = function(){
      region = session$userData$activeRegion()
      if(!shiny::isTruthy(region)){
        config = reactiveValues()
        return()
      }
      config$chr = region$chr
      config$start = as.numeric(region$start)
      config$end = as.numeric(region$end)
      config$elements = plot_config$elements()
      config$selected = TRUE
      return(config)
    }

    draw_plots = function(config){
      current_plots[["genes-gene_track"]] = plot_genes(config$chr, config$start, 
        config$end, config$elements, session)
    }

    observeEvent(basic_config$draw_plots(), {
      if(!shiny::isTruthy(session$userData$region())){
        return()
      }
      if(!("genes" %in% isolate(basic_config$plot_type_select()))){
        current_plots[["genes-gene_track"]] = NULL
        session$userData$plot_heights[["genes-gene_track"]] = 0
        prev_configs[["genes"]]$selected = FALSE
        return()
      }
      
      config = build_config()
      if(identical(reactiveValuesToList(config), prev_configs[["genes"]])){
        return()
      }
      
      draw_plots(config)
      prev_configs[["genes"]] = reactiveValuesToList(config)
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
      prev_configs[["genes"]] = reactiveValuesToList(config)
      return()
    })

    observeEvent(toolbar_config$update_plots(), {
      if(length(reactiveValuesToList(config)) == 0){
        return()
      }
      config = build_config()
      if(identical(config, prev_configs[["genes"]])){
        return()
      }
      draw_plots(config)
      prev_configs[["genes"]] = reactiveValuesToList(config)
      return()
    })
  })
}
    
## To be copied in the UI
# mod_plot_genes_ui("plot_genes_1")
    
## To be copied in the server
# mod_plot_genes_server("plot_genes_1")
