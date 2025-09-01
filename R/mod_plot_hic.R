#' plot_hic UI Function
#'
#' @description Draw a Hi-C matrix plot according to user-specified configuration.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList plotOutput renderPlot
mod_plot_hic_ui <- function(id) {
  ns <- NS(id)
  tagList(
 		plotOutput(ns('hic_plot'))
		# Can add a brushOpts() object to the plotOutput function here to enable brushing
		### Add another plotOutput for loop track?
  )
}
    
#' plot_hic Server Functions
#'
#' @param id Internal Shiny parameter
#' @param region_config A named list of region parameters formed from user
#'  inputs. Must include the selected chromosome, start and end coordinates
#'  (in base pairs) of the requested region.
#' @param plot_config A names list of plot parameters formed from user
#'  inputs. Must include the selected
#' 	resolution, normalization method and format of the requested Hi-C plot.
#'
#' @noRd 
mod_plot_hic_server <- function(id, region_config, plot_config){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$hic_plot = renderPlot({
      chr = region_config$region_chr
      start = as.numeric(region_config$region_start)
      end = as.numeric(region_config$region_end)
      resolution = as.numeric(plot_config$resolution)
      normalization = plot_config$normalization
      format = plot_config$format
      plot = plot_hic(chr, start, end, resolution, normalization, format)
      # Check for which plot elements are requested and do the necessary work
      if('tads' %in% plot_config$elements){
          # plot = draw_tads(plot)
      }
      return(plot)
    })
  })
}
    
## To be copied in the UI
# mod_plot_hic_ui("plot_hic_1")
    
## To be copied in the server
# mod_plot_hic_server("plot_hic_1")
