#' plot_chip UI Function
#'
#' @description Draw a ChIP track plot according to user-specified configuration.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param chip_samples List of samples to be included.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList plotOutput renderPlot
mod_plot_chip_ui <- function(id, chip_samples) {
  ns <- NS(id)
  output = tagList()
  for(s in chip_samples){
    output = c(output, tagList(plotOutput(ns(s), height='auto')))
  }
  return(output)
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
#' @importFrom cowplot align_plots ggdraw
mod_plot_chip_server <- function(id, region_config, plot_config){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    chr = region_config$region_chr
    start = as.numeric(region_config$region_start)
    end = as.numeric(region_config$region_end)
    ### Add resolution to chip config
    chip_samples = plot_config$chip_samples
    plots = plot_chip(chr, start, end, 5000, chip_samples)
    for(s in chip_samples){
      session$userData$plots[[paste0('chip-',s)]] = plots[[s]]
    }
    session$userData$plots = cowplot::align_plots(plotlist=session$userData$plots,
      align='v', axis='lr')
    # Need to use lapply instead of for loop due to the way for loops are handled by
    #  Shiny's lazy evaluation
    lapply(browser_data$bw$bw_sample_names, function(s){
      if(s %in% chip_samples){
        output[[s]] = renderPlot({
          cowplot::ggdraw(session$userData$plots[[paste0('chip-',s)]])
        },
          res=96,
          height=session$clientData[[paste0('output_',ns(s),'_width')]]*0.1
        )
      }
      else{
        output[[s]] = renderPlot({
          NULL
        })
        session$userData$plots[[paste0('chip-',s)]] = NULL
      }
    })
  })
}
    
## To be copied in the UI
# mod_plot_chip_ui("plot_chip_1")
    
## To be copied in the server
# mod_plot_chip_server("plot_chip_1")
