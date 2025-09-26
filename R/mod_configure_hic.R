#' Controls for configuring Hi-C plot output
#'
#' @description Provides a set of inputs to allow the user to configure plot
#' 	parameters specific to Hi-C contact matrix plots.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags checkboxGroupInput selectInput actionButton
#' @importFrom shinyjs disable
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback
mod_configure_hic_ui <- function(id, session) {
  ns <- NS(id)
  plot_types = list('tads'='TADs', 'loops'='Loops', 'pca'='A/B Compartment Scores')
  plot_selections = list()
  for(type in names(plot_types)){
    if(!is.null(browser_data[[type]])){
      plot_selections[[ plot_types[[type]] ]] = type
    }
  }
  tagList(
    tags$div(id='hic_controls',
      tags$h2('Hi-C'),
      checkboxGroupInput(
        inputId = ns('plot_elements'),
        label = 'Select any number of elements to plot:',
        choices = plot_selections
      ),
      selectInput(
        inputId = ns('plot_resolution'),
        label = 'Resolution (base pairs):',
        choices = browser_data$resolutions,
        selected = browser_data$resolutions[1],
        multiple = FALSE
      ),
      selectInput(
        inputId = ns('plot_normalization'),
        label = 'Normalization method:',
        choices = browser_data$normalizations,
        selected = 'KR',
        multiple = FALSE
      ),
      selectInput(
        inputId = ns('plot_format'),
        label = 'Plot format:',
        choices = list(Square='square', Triangular='triangular', Rectangular='rectangular'),
        selected = 'triangular',
        multiple = FALSE
      ),
      tags$hr()
    )
  )
}
    
#' configure_hic Server Functions
#'
#' @param id Internal Shiny parameter
#' @param region Reactive function from mod_necessary_setup which details region 
#'  configuration
#' @noRd 
mod_configure_hic_server <- function(id, region){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
 		return( 
      list(
        elements = reactive({ input$plot_elements }),
        resolution = reactive({ input$plot_resolution }),
        normalization = reactive({ input$plot_normalization }),
        format = reactive({ input$plot_format })
      )
 		)
  })
}
    
## To be copied in the UI
# mod_configure_hic_ui("configure_hic_1")
    
## To be copied in the server
# mod_configure_hic_server("configure_hic_1")
