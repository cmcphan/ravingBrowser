#' plot_hic UI Function
#'
#' @description Draw a Hi-C matrix plot according to user-specified configuration.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param elements List of plot elements to include
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList plotOutput renderPlot
mod_plot_hic_ui <- function(id, elements) {
  ns <- NS(id)
  output = tagList(plotOutput(ns('hic_plot'), height='auto'))
  # Can add a brushOpts() object to the plotOutput function here to enable brushing
  if('loops' %in% elements){
    output = c(output, tagList(plotOutput(ns('loops_track'), height='auto')))
  }
  if('pca' %in% elements){
    output = c(output, tagList(plotOutput(ns('pca_track'), height='auto')))
  }
  return(output)
}
    
#' plot_hic Server Functions
#'
#' @param id Internal Shiny parameter
#' @param region_config A named list of region parameters formed from user
#'  inputs. Must include the selected chromosome, start and end coordinates
#'  (in base pairs) of the requested region.
#' @param plot_config A named list of plot parameters formed from user
#'  inputs. Must include the selected
#'  resolution, normalization method and format of the requested Hi-C plot.
#'
#' @noRd 
#'
#' @importFrom cowplot align_plots ggdraw
mod_plot_hic_server <- function(id, region_config, plot_config){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    chr = region_config$region_chr
    start = as.numeric(region_config$region_start)
    end = as.numeric(region_config$region_end)
    resolution = as.numeric(plot_config$resolution)
    normalization = plot_config$normalization
    format = plot_config$format
    plots = list()
    plots[['hic']] = plot_hic(chr, start, end, resolution, normalization, format)
    # This approach for setting plot height comes from https://github.com/rstudio/
    #  shiny/issues/650, wisdom dispensed by one of the creators of R Shiny. Set
    #  the element height to 'auto' and use the height in the renderPlot call.
    # Output element width, height and visibility can be directly accessed
    #  as part of session$clientData. Output name needs to be namespaced
    output$hic_plot = renderPlot({
      if('tads' %in% plot_config$elements){
        plots[['hic']] = draw_tads(plots[['hic']], chr, start, end)
      }
      cowplot::ggdraw(plots[['hic']])
    }, res=96, height=session$clientData$'output_plot_hic_1-hic_plot_width'*0.5)

    output$loops_track = renderPlot({
      if('loops' %in% plot_config$elements){
        plots[['loops']] = plot_loops(chr, start, end)
        plots = cowplot::align_plots(plotlist=plots, align='v')
        cowplot::ggdraw(plots[['loops']])
      }
      else{ NULL }
    }, res=96, height=session$clientData$'output_plot_hic_1-loops_track_width'*0.1)

    output$pca_track = renderPlot({
      if('pca' %in% plot_config$elements){
        plots[['pca']] = plot_pca(chr, start, end)
        plots = cowplot::align_plots(plotlist=plots, align='v')
        cowplot::ggdraw(plots[['pca']])
      }
      else{ NULL }
    }, res=96, height=session$clientData$'output_plot_hic_1-pca_track_width'*0.1)
  })
}
    
## To be copied in the UI
# mod_plot_hic_ui("plot_hic_1")
    
## To be copied in the server
# mod_plot_hic_server("plot_hic_1")
