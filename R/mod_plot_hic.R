#' plot_hic UI Function
#'
#' @description Draw a Hi-C matrix plot according to user-specified configuration.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param elements Reactive list of plot elements to include from the relevent 
#'  plot configuration UI server function.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList plotOutput renderPlot
#' @importFrom shinycssloaders withSpinner
mod_plot_hic_ui <- function(id, elements){
  ns <- NS(id)
  
  output = tagList(
    shinycssloaders::withSpinner(plotOutput(ns('hic_track'), height='auto'))
  )
  for(element in elements()){
    if(element == 'tads'){
      next
    }
    output = c(output, tagList(
        shinycssloaders::withSpinner(plotOutput(ns(paste0(element,'_track')), height='auto'))
      )
    )
  }
  return(output)
}
    
#' plot_hic Server Functions
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
#'
#' @importFrom cowplot align_plots ggdraw
mod_plot_hic_server <- function(id, basic_config, plot_config, current_plots, 
  prev_configs){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent(basic_config$draw_plots(), {
      if(!('hic' %in% isolate(basic_config$plot_type_select()))){
        current_plots[['hic-hic']] = NULL
        current_plots[['hic-loops']] = NULL
        current_plots[['hic-pca']] = NULL
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
      config$resolution = as.numeric(plot_config$resolution())
      config$normalization = plot_config$normalization()
      config$format = plot_config$format()
      config$elements = plot_config$elements()
      if(identical(config, prev_configs[['hic']])){
        return()
      }
      
      current_plots[['hic-hic']] = plot_hic(config$chr, config$start, config$end, 
        config$resolution, config$normalization, config$format)
      if('tads' %in% config$elements){
        current_plots[['hic-hic']] = draw_tads(current_plots[['hic-hic']], config$chr, 
          config$start, config$end)
      }
      if('loops' %in% config$elements){
        current_plots[['hic-loops']] = plot_loops(config$chr, config$start, config$end)
      }
      else { current_plots[['hic-loops']] = NULL }
      if('pca' %in% config$elements){
        current_plots[['hic-pca']] = plot_pca(config$chr, config$start, config$end)
      }
      else { current_plots[['hic-pca']] = NULL }
      
      prev_configs[['hic']] = config
      return()
    })

    # This approach for setting plot height comes from https://github.com/rstudio/
    #  shiny/issues/650, wisdom dispensed by one of the creators of R Shiny. Set
    #  the element height to 'auto' and set the height in the renderPlot call.
    # Output element width, height and visibility can be directly accessed
    #  as part of session$clientData. Output name needs to be namespaced
    output$hic_track = renderPlot({
      cowplot::ggdraw(current_plots[['hic-hic']])
    },
      res=96,
      height=function(){session$clientData$'output_plot_panel_width'}*0.5
    )

    output$loops_track = renderPlot({
      cowplot::ggdraw(current_plots[['hic-loops']])
    },
      res=96,
      height=function(){session$clientData$'output_plot_panel_width'}*0.1
    )

    output$pca_track = renderPlot({
      cowplot::ggdraw(current_plots[['hic-pca']])
    },
      res=96,
      height=function(){session$clientData$'output_plot_panel_width'}*0.1
    )
  })  
}
    
## To be copied in the UI
# mod_plot_hic_ui("plot_hic_1")
    
## To be copied in the server
# mod_plot_hic_server("plot_hic_1")
