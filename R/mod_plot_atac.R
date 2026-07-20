#' plot_atac UI Function
#'
#' @description Draw a ATAC track plot according to user-specified configuration.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param elements Reactive list of plot elements to include from the relevent 
#'  plot configuration UI server function.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList plotOutput renderPlot
#' @importFrom shinycssloaders withSpinner
mod_plot_atac_ui <- function(id, elements) {
  ns <- NS(id)
  return(NULL)
}
    
#' plot_atac Server Functions
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
mod_plot_atac_server <- function(id, basic_config, plot_config, current_plots,
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
      width = config$end - config$start
      config$resolution = as.integer(width / plot_config$resolution())
      config$peaksets = plot_config$elements()
      config$screen_dimensions = c(session$userData$screen_width(), session$userData$screen_height())
      if(!("atac" %in% isolate(basic_config$plot_type_select()))){
        config$selected = FALSE
      }
      else { config$selected = TRUE }
      return(config)
    }

    draw_plots = function(config){
      if(length(config$peaksets) == 0){
        prev_configs[["atac"]] = config
        for(p in names(browser_data$atac_peaks)){
          current_plots[[paste0("atac-",p)]] = NULL
          session$userData$plot_heights[[paste0("atac-",p)]] = 0
        }
        return()
      }
      
      # Reset any deselected plots
      for(p in names(browser_data$atac_peaks)){
        if(!(p %in% config$peaksets)){
          current_plots[[paste0("atac-",p)]] = NULL
          session$userData$plot_heights[[paste0("atac-",p)]] = 0
        }
      }
      
      plots = plot_atac(config$chr, config$start, config$end, 
        config$resolution, config$peaksets, session)
      for(p in names(plots)){
        current_plots[[paste0("atac-",p)]] = plots[[p]]
      }
    }

    observeEvent({basic_config$draw_plots() | toolbar_config$update_plots() |
      session$userData$regionChange()}, {
      if(!shiny::isTruthy(session$userData$region())){
        return()
      }
      if(basic_config$draw_plots()==0 & toolbar_config$update_plots()==0){
        return()
      }
      if(!("atac" %in% isolate(basic_config$plot_type_select()))){
        for(p in names(browser_data$atac_peaks)){
          current_plots[[paste0("atac-",p)]] = NULL
          session$userData$plot_heights[[paste0("atac-",p)]] = 0
        }
        prev_configs[["atac"]]$selected = FALSE
        return()
      }

      config = build_config()
      if(identical(reactiveValuesToList(config), prev_configs[["atac"]])){
        return()
      }
      draw_plots(config)
      prev_configs[["atac"]] = reactiveValuesToList(config)
      return()
    })
  })
}
    
## To be copied in the UI
# mod_plot_atac_ui("plot_atac_1")
    
## To be copied in the server
# mod_plot_atac_server("plot_atac_1")
