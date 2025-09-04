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
mod_configure_hic_ui <- function(id) {
  ns <- NS(id)
  tagList(
  		tags$hr(),
  		tags$h2('Hi-C'),
 		checkboxGroupInput(
      inputId = ns('plot_elements'),
      label = 'Select any number of elements to plot:',
      choices = c('TADs'='tads', 'Loops'='loops', 'A/B Compartment Scores (PCA)'='pca')
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
    actionButton(
    		inputId = ns('draw_plot'),
    		label = 'Draw plot'
    )
  )
}
    
#' configure_hic Server Functions
#'
#' @param id Internal Shiny parameter
#' @param disabled Should the UI elements be disabled? Defaults to FALSE. Used to prevent user
#' 	from moving on when region inputs are invalid
#' @noRd 
mod_configure_hic_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # This check should not be necessary, but just in case a user manages to break
    #  the UI somehow, it's handled
    observeEvent(input$draw_plot, {
      message = validate_hic(input$plot_resolution, input$plot_normalization,
        input$plot_format)
      if(!is.na(message)){
        shinyFeedback::showFeedbackDanger('draw_plot', session, text=message)
      }
      else{ shinyFeedback::hideFeedback('draw_plot', session)}
    })
 		return( 
      list(
        elements = reactive({ input$plot_elements }),
        resolution = reactive({ input$plot_resolution }),
        normalization = reactive({ input$plot_normalization }),
        format = reactive({ input$plot_format }),
        draw_plot = reactive({ input$draw_plot })
      )
 		)
  })
}
    
## To be copied in the UI
# mod_configure_hic_ui("configure_hic_1")
    
## To be copied in the server
# mod_configure_hic_server("configure_hic_1")
