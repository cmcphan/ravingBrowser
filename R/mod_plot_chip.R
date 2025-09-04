#' plot_chip UI Function
#'
#' @description Draw a ChIP track plot according to user-specified configuration.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList plotOutput renderPlot
mod_plot_chip_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns('chip_track'), height='auto')
  )
}
    
#' plot_chip Server Functions
#'
#' @param id Internal Shiny parameter
#' @param region_config A named list of region parameters formed from user
#'  inputs. Must include the selected chromosome, start and end coordinates
#'  (in base pairs) of the requested region.
#' @param plot_config A named list of plot parameters formed from user
#'  inputs.
#'
#' @noRd
#'
#' @importFrom cowplot align_plots ggdraw plot_grid
mod_plot_chip_server <- function(id, region_config, plot_config){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$chip_track = renderPlot({
      chr = region_config$region_chr
      start = as.numeric(region_config$region_start)
      end = as.numeric(region_config$region_end)
      ### Add resolution to chip config
      chip_samples = plot_config$chip_samples
      plots = plot_chip(chr, start, end, 5000, chip_samples)
      plots = cowplot::align_plots(plotlist=plots, align='v')
      cowplot::ggdraw(cowplot::plot_grid(plotlist=plots, ncol=1, nrow=length(plots)))
    }, res=96, height=session$clientData$'output_plot_chip_1-chip_track_width'
        *(0.1*length(plot_config$chip_samples)))
  })
}
    
## To be copied in the UI
# mod_plot_chip_ui("plot_chip_1")
    
## To be copied in the server
# mod_plot_chip_server("plot_chip_1")
