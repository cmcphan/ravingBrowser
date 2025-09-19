#' plot_genes UI Function
#'
#' @description Draw a gene feature plot according to user-specified configuration.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param elements List of feature types to be included.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_genes_ui <- function(id, elements) {
  ns <- NS(id)
  if(!is.null(elements)){
    tagList(
      plotOutput(ns('gene_track'), height='auto')
    )
  }
}
    
#' plot_genes Server Functions
#'
#' @param region_config A named list of region parameters formed from user
#'  inputs. Must include the selected chromosome, start and end coordinates
#'  (in base pairs) of the requested region.
#' @param plot_config A named list of plot parameters formed from user
#'  inputs.
#' @param invalidate Flag which determines whether all plots in this module should be
#'  replaced with NULL. For clearing plots when configuration has changed/plot is 
#'  deselected. Defaults to FALSE.
#' @param draw Flag which determines whether plots should be (re)drawn or not. If
#'  set to false, just realigns the existing plots with any added since drawing. 
#'  Defaults to TRUE.
#' 
#' @noRd 
mod_plot_genes_server <- function(id, region_config, plot_config,
  invalidate=FALSE, draw=TRUE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    if(invalidate){
      output$gene_track = renderPlot({ NULL },
        res = 96,
        height=session$clientData$'output_plot_panel_width'*0.1
      )
      session$userData$gene_feature_colours = NULL
      return(NULL)
    }

    if(draw){
      chr = region_config$region_chr
      start = as.numeric(region_config$region_start)
      end = as.numeric(region_config$region_end)
      elements = plot_config$elements
      if(is.null(elements)){ return(NULL) }
      plot = plot_genes(chr, start, end, elements)
      session$userData$plots[['genes-gene_track']] = plot
    }
    if(length(session$userData$plots) > 0){
      session$userData$plots = cowplot::align_plots(plotlist=session$userData$plots,
        align='v', axis='lr')
    }

    output$gene_track = renderPlot({
      cowplot::ggdraw(session$userData$plots[['genes-gene_track']])
    },
      res=96,
      height=session$clientData$'output_plot_panel_width'*0.1
    )
  })
}
    
## To be copied in the UI
# mod_plot_genes_ui("plot_genes_1")
    
## To be copied in the server
# mod_plot_genes_server("plot_genes_1")
