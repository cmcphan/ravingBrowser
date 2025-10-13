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
      if(any(!is.null(current_plots))){
        plotlist = isolate(reactiveValuesToList(current_plots))
        plotlist_clean = list()
        for(p in names(plotlist)){
          if(!is.null(plotlist[[p]])){
            plotlist_clean[[p]] = plotlist[[p]]
          }
        }
        plot = patchwork::wrap_plots(
          plotlist_clean,
          ncol = 1
        )
        patchwork_plot(plot)
        rm(plotlist)
        rm(plotlist_clean)
      }
      else{
        patchwork_plot(NULL)
      }
      shinycssloaders::hideSpinner("patchwork")
    }, priority = -1)

    output$patchwork = renderPlot({
      patchwork_plot()
    }, 
      res = 96,
      height = function(){
        height = plot_height()
        if(height == 0){
          return(1)
        }
        else{
          height = session$clientData$"output_plot_panel_width" * height
          return(height)
        }
      }
    )
  })
}
    
## To be copied in the UI
# mod_plot_ui("plot_1")
    
## To be copied in the server
# mod_plot_server("plot_1")
