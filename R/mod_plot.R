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
#' @importFrom patchwork wrap_plots patchworkGrob
mod_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(plotOutput(ns('patchwork'), height='auto',
      brush=brushOpts(id=ns("plot_brush"), direction="x", resetOnNew=TRUE))),
    tags$div(id=ns("alignment_bar"), class="alignment_bar",
      tags$div(id=ns("alignment_bar_padding_left"), class="alignment_bar_padding"),
      tags$div(id=ns("alignment_bar_line"), class="alignment_bar_line"),
      tags$div(id=ns("alignment_bar_padding_right"), class="alignment_bar_padding")
    )
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
#' @param toolbar_config List of reactive functions - output from mod_toolbar which
#'  contains toolbar button data
#' 
#' @noRd 
#' 
#' @import shiny
#' @importFrom shinycssloaders showSpinner hideSpinner
#' @importFrom patchwork wrap_plots
#' @importFrom shinyjs addClass removeClass
#' @importFrom ggplotify as.ggplot
mod_plot_server <- function(id, basic_config, current_plots, plot_configs, 
toolbar_config){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    plot_height = reactive({
      height = 0
      for(name in names(session$userData$plot_heights)){
        height = height + session$userData$plot_heights[[name]]
      }
      return(height)
    })

    observeEvent({basic_config$draw_plots() | toolbar_config$zoom_in() |
      toolbar_config$zoom_out() | toolbar_config$update_plots()}, {
      if(!shiny::isTruthy(session$userData$region())){
        return()
      }
      shinycssloaders::showSpinner("patchwork")
      shinyjs::addClass(selector="#plot_1-patchwork", class="recalculating")
    }, priority = 1)

    observeEvent({basic_config$draw_plots() | toolbar_config$zoom_in() |
      toolbar_config$zoom_out() | toolbar_config$update_plots()}, {
      if(!shiny::isTruthy(session$userData$region())){
        return()
      }
      plotlist = isolate(reactiveValuesToList(current_plots))
      plotlist_clean = list()
      for(p in names(plotlist)){
        if(!is.null(plotlist[[p]])){
          plotlist_clean[[p]] = plotlist[[p]]
        }
      }
      if(length(plotlist_clean) == 0){
        session$userData$patchwork_plot(NULL)
        shinycssloaders::hideSpinner("patchwork")
      }
      else if(length(plotlist_clean) == 1){
        session$userData$patchwork_plot(plotlist_clean[[1]])
      }
      else{
        plot = patchwork::wrap_plots(
          plotlist_clean,
          ncol = 1
        )
        #attr(plot, "class") = c("ggplot", "patchwork")
        session$userData$patchwork_plot(plot)
      }
      rm(plotlist)
      rm(plotlist_clean)
    }, priority = -1)

    output$patchwork = renderPlot({
      plot = session$userData$patchwork_plot()
      if("patchwork" %in% attr(plot, "class")){
        plot = patchwork::patchworkGrob(plot)
        plot = ggplotify::as.ggplot(plot)
        plot
      }
      else{
        plot
      }
    }, 
      res = 96,
      height = function(){
        height = plot_height()
        if(height <= 0.5){
          return(session$clientData$"output_plot_panel_width"*0.5)
        }
        else{
          return(session$clientData$"output_plot_panel_width" * height)
        }
      }
    )

    observeEvent(toolbar_config$toggle_brush(), {
      print("Brush toggled")
    })

    observeEvent(input$plot_brush, {
      print(paste0("Min = ",input$plot_brush$xmin, ", max = ",input$plot_brush$xmax))
    })
  })
}
    
## To be copied in the UI
# mod_plot_ui("plot_1")
    
## To be copied in the server
# mod_plot_server("plot_1")
