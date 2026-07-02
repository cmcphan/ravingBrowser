#' UI elements common to all plot types
#'
#' @description Module for the initial browser setup options. These options are required for visualizations 
#'  regardless of the desired plot, e.g. which plots to show, which region to show, and determine what elements
#'  to populate the rest of the sidebar with
#' @param id,input,output,session Internal parameters for `{shiny}`.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_necessary_setup_ui <- function(id) {
  ns <- NS(id)
  plot_types = browser_data$plot_types
  plot_selections = list()
  for(type in names(plot_types)){
    plot_selections[[ plot_types[[type]] ]] = type
  }
  tagList(
    tags$div(id='necessary_setup_controls',
      checkboxGroupInput(
        inputId = ns('plot_type_select'),
        label = 'Select any number of plot types:',
        choices = plot_selections
      ),
      tabsetPanel(
        mod_region_input_ui(ns("region_input_1")),
        mod_gene_select_ui(ns("gene_select_1")),
        id = ns('region_select'),
        selected = 'region_input',
        type = 'tabs'
      ),
      htmlOutput(
        outputId = ns("region_display")
      ),
      actionButton(
        inputId = ns('draw_plots'),
        label = 'Draw plots'
      ),
      tags$hr()
    )
  )
}
    
#' necessary_setup Server Functions
#'
#' @noRd 
mod_necessary_setup_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    inputs = list(
      plot_type_select = reactive({ input$plot_type_select }),
      draw_plots = reactive({ input$draw_plots }),
      current_tab = reactive({ input$region_select })
    )

    mod_region_input_server("region_input_1", inputs$current_tab)
    mod_gene_select_server("gene_select_1", inputs$current_tab)

    observeEvent(session$userData$activeRegion(),{
      region = session$userData$activeRegion()
      if(!shiny::isTruthy(region)){
        output$region_display = NULL
      }
      else{
        output$region_display = renderUI(HTML(paste0(
          tags$b("Currently active region:"),
          tags$p(paste0(region$chr,": ",region$start," - ",region$end))
        )))
      }
    })
    

    observeEvent(input$draw_plots,{
      if(!shiny::isTruthy(session$userData$region())){
        shiny::showNotification(
          'Region is not set. Input a region or select a gene.',
          duration = NULL,
          closeButton = FALSE,
          id = 'region_notif',
          type = 'error',
          session = session
        )
        session$userData$plot_redraw = FALSE
        return()
      }
      else{
        session$userData$activeRegion(session$userData$region())
        session$userData$plot_redraw = TRUE
      }
    }, priority = 1)

    return( inputs )
  })
}
    
## To be copied in the UI
# mod_necessary_setup_ui("necessary_setup_1")
    
## To be copied in the server
# mod_necessary_setup_server("necessary_setup_1")
