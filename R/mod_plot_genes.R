#' plot_genes UI Function
#'
#' @description Draw a gene feature plot according to user-specified configuration.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param elements Reactive list of plot elements to include from the relevent 
#'  plot configuration UI server function.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinycssloaders withSpinner
mod_plot_genes_ui <- function(id, elements) {
  ns <- NS(id)
  if(!is.null(elements())){
    tagList(
      shinycssloaders::withSpinner(plotOutput(ns('gene_track'), height='auto'))
    )
  }
}
    
#' plot_genes Server Functions
#'
#' @param id Internal Shiny parameter
#' @param basic_config List of reactive functions - output from 
#'  mod_necessary_setup which details overall configuration settings
#' @param plot_config Reactive function from the relevant plot UI function which details
#'  the user selected plot configuration settings
#' @param current_plots reactiveValues object containing all currently active plots
#' @param prev_configs reactiveValues object containing configs for previous plots.
#'  Used to determine whether the plot needs to be redrawn or not.
#' 
#' @noRd 
mod_plot_genes_server <- function(id, basic_config, plot_config, current_plots,
  prev_configs){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent(basic_config$draw_plots(), {
      if(!('genes' %in% isolate(basic_config$plot_type_select()))){
        current_plots[['genes-gene_track']] = NULL
        return()
      }

      region = basic_config$region()
      if(!shiny::isTruthy(region())){
        return()
      }
      config = list()
      config$chr = region()$region_chr
      config$start = as.numeric(region()$region_start)
      config$end = as.numeric(region()$region_end)
      config$elements = plot_config$elements()
      if(is.null(config$elements)){ 
        prev_configs[['genes']] = config
        return() 
      }
      if(identical(config, prev_configs[['genes']])){
        return()
      }
      
      current_plots[['genes-gene_track']] = plot_genes(config$chr, config$start, 
        config$end, config$elements)
      
      prev_configs[['genes']] = config
      return()
    })

    output$gene_track = renderPlot({
      cowplot::ggdraw(current_plots[['genes-gene_track']])
    },
      res=96,
      height=function(){session$clientData$'output_plot_panel_width'}*0.1
    )
  })
}
    
## To be copied in the UI
# mod_plot_genes_ui("plot_genes_1")
    
## To be copied in the server
# mod_plot_genes_server("plot_genes_1")
