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
  output = tagList(
    plotOutput(ns('hic_track'), height='auto')
  )
  for(element in elements){
    if(element == 'tads'){
      next
    }
    output = c(output, tagList(
        plotOutput(ns(paste0(element,'_track')), height='auto')
      )
    )
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
#' @param invalidate Flag which determines whether all plots in this module should be
#'  replaced with NULL. For clearing plots when configuration has changed/plot is 
#'  deselected. Defaults to FALSE.
#' @param draw Flag which determines whether plots should be (re)drawn or not. If
#'  set to false, just realigns the existing plots with any added since drawing. 
#'  Defaults to TRUE.
#'
#' @noRd 
#'
#' @importFrom cowplot align_plots ggdraw
mod_plot_hic_server <- function(id, region_config, plot_config, 
  invalidate=FALSE, draw=TRUE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    if(invalidate){
      output$hic_track = renderPlot({ NULL }, 
        res = 96,
        height=session$clientData$'output_plot_panel_width'*0.1
      )
      output$loops_track = renderPlot({ NULL }, 
        res = 96,
        height=session$clientData$'output_plot_panel_width'*0.1
      )
      output$pca_track = renderPlot({ NULL }, 
        res = 96,
        height=session$clientData$'output_plot_panel_width'*0.1
      )
      return(NULL)
    }

    if(draw){
      chr = region_config$region_chr
      start = as.numeric(region_config$region_start)
      end = as.numeric(region_config$region_end)
      resolution = as.numeric(plot_config$resolution)
      normalization = plot_config$normalization
      format = plot_config$format

      plots = list()
      plots[['hic']] = plot_hic(chr, start, end, resolution, normalization, format)
      if('tads' %in% plot_config$elements){
        plots[['hic']] = draw_tads(plots[['hic']], chr, start, end)
      }
      if('loops' %in% plot_config$elements){
        plots[['loops']] = plot_loops(chr, start, end)
      }
      else { plots[['loops']] = NULL }
      if('pca' %in% plot_config$elements){
        plots[['pca']] = plot_pca(chr, start, end)
      }
      else { plots[['pca']] = NULL }
      session$userData$plots[['hic-hic']] = plots[['hic']]
      session$userData$plots[['hic-loops']] = plots[['loops']]
      session$userData$plots[['hic-pca']] = plots[['pca']]
    }
    if(length(session$userData$plots) > 0){
      session$userData$plots = cowplot::align_plots(plotlist=session$userData$plots,
        align='v', axis='lr')
    }
    # This approach for setting plot height comes from https://github.com/rstudio/
    #  shiny/issues/650, wisdom dispensed by one of the creators of R Shiny. Set
    #  the element height to 'auto' and set the height in the renderPlot call.
    # Output element width, height and visibility can be directly accessed
    #  as part of session$clientData. Output name needs to be namespaced
    output$hic_track = renderPlot({
      cowplot::ggdraw(session$userData$plots[['hic-hic']])
    },
      res=96,
      height=session$clientData$'output_plot_panel_width'*0.5
    )

    output$loops_track = renderPlot({
      if('loops' %in% plot_config$elements){
        cowplot::ggdraw(session$userData$plots[['hic-loops']])
      }
      else{
        NULL
      }
    },
      res=96,
      height=session$clientData$'output_plot_panel_width'*0.1
    )

    output$pca_track = renderPlot({
      if('pca' %in% plot_config$elements){
        cowplot::ggdraw(session$userData$plots[['hic-pca']])
      }
      else{
        NULL
      }
    },
      res=96,
      height=session$clientData$'output_plot_panel_width'*0.1
    )
  })
}
    
## To be copied in the UI
# mod_plot_hic_ui("plot_hic_1")
    
## To be copied in the server
# mod_plot_hic_server("plot_hic_1")
