#' plot UI Function
#'
#' @description Create a patchwork plot containing all elements as set 
#'  by the user
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom shinycssloaders withSpinner 
mod_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(plotOutput(ns('patchwork'), height='auto'))
  )
}
    
#' plot Server Functions
#'
#' @param id Internal Shiny parameter
#' @param basic_config List of reactive functions - output from 
#'  mod_necessary_setup which details overall configuration settings
#' @param current_plots reactiveValues object containing all currently active plots
#' @param plot_configs Named list of reactive functions from the plot UI which detail
#'  the user selected plot configuration settings. Used to determine the dimensions
#'  of the patchwork plot output.
#' 
#' @noRd 
#' 
#' @import shiny
#' @importFrom shinycssloaders showSpinner hideSpinner
#' @importFrom patchwork wrap_plots
#' @importFrom shinyjs addClass removeClass
mod_plot_server <- function(id, basic_config, current_plots, plot_configs){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    patchwork_plot = shiny::reactiveVal(NULL)
    plot_height = reactive({
      height = 0
      for(name in names(session$userData$plot_heights)){
        height = height + session$userData$plot_heights[[name]]
      }
      return(height)
    })

    observeEvent(basic_config$draw_plots(), {
      shinycssloaders::showSpinner("patchwork")
      shinyjs::addClass(selector="#plot_1-patchwork", class="recalculating")
    }, priority = 1)

    observeEvent(basic_config$draw_plots(), {
      plotlist = isolate(reactiveValuesToList(current_plots))
      plotlist_clean = list()
      for(p in names(plotlist)){
        if(!is.null(plotlist[[p]])){
          plotlist_clean[[p]] = plotlist[[p]]
        }
      }
      if(length(plotlist_clean) == 0){
        patchwork_plot(NULL)
        shinycssloaders::hideSpinner("patchwork")
      }
      else if(length(plotlist_clean) == 1){
        patchwork_plot(plotlist_clean[[1]])
      }
      else{
        plot = patchwork::wrap_plots(
          plotlist_clean,
          ncol = 1
        )
        patchwork_plot(plot)
      }
      rm(plotlist)
      rm(plotlist_clean)
    }, priority = -1)

    output$patchwork = renderPlot({
      patchwork_plot()
    }, 
      res = 96,
      height = function(){
        height = plot_height()
        if(height == 0 | height <= 0.5){
          return(session$clientData$"output_plot_panel_width"*0.5)
        }
        else{
          return(session$clientData$"output_plot_panel_width" * height)
        }
      }
    )
  })
}
    
## To be copied in the UI
# mod_plot_ui("plot_1")
    
## To be copied in the server
# mod_plot_server("plot_1")
