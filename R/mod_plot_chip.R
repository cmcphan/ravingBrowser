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
mod_plot_chip_ui <- function(id, elements) {
  ns <- NS(id)
  output = tagList()
  for(s in elements()){
    output = c(output, tagList(
        plotOutput(ns(s), height='auto')
      )
    )
  }
  return(output)
}
    
#' plot_chip Server Functions
#'
#' @param id Internal Shiny parameter
#' @param basic_config List of reactive functions - output from 
#'  mod_necessary_setup which details overall configuration settings
#' @param plot_config Reactive function from the relevant plot UI function which details
#'  the user selected plot configuration settings
#' @param current_plots reactiveValues object containing all currently active plots
#'
#' @noRd
#'
#' @importFrom cowplot align_plots ggdraw
mod_plot_chip_server <- function(id, basic_config, plot_config, current_plots){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent(basic_config$draw_plots(), {
      for(s in browser_data$chip$bw_sample_names){
        current_plots[[paste0('chip-',s)]] = NULL
      }
      if(!('chip' %in% isolate(basic_config$plot_type_select()))){
        return()
      }

      region = basic_config$region()
      if(!shiny::isTruthy(region())){
        return()
      }
      chr = region()$region_chr
      start = as.numeric(region()$region_start)
      end = as.numeric(region()$region_end)
      resolution = plot_config$resolution()
      chip_samples = plot_config$elements()
      plots = plot_chip(chr, start, end, resolution, chip_samples)
      for(s in chip_samples){
        current_plots[[paste0('chip-',s)]] = plots[[s]]
      }

      if(length(current_plots) > 1){
        plotlist = isolate(reactiveValuesToList(current_plots))
        aligned_plots = cowplot::align_plots(plotlist=plotlist,
          align='v', axis='lr')
        for(name in names(aligned_plots)){
          current_plots[[name]] = aligned_plots[[name]]
        }
      }
      return()
    })

    # Need to use lapply instead of for loop due to the way for loops are handled by
    #  Shiny's lazy evaluation
    lapply(browser_data$chip$bw_sample_names, function(s){
      output[[s]] = renderPlot({
        cowplot::ggdraw(current_plots[[paste0('chip-',s)]])
      },
        res=96,
        height=function(){session$clientData$'output_plot_panel_width'}*0.1
      )
    })
  })
}
    
## To be copied in the UI
# mod_plot_chip_ui("plot_chip_1")
    
## To be copied in the server
# mod_plot_chip_server("plot_chip_1")
