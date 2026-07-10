#' Controls for configuring methylation plot output
#'
#' @description Provides a set of inputs to allow the user to configure plot parameters
#'  specific to methtlation plot.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags
mod_configure_methylation_ui <- function(id, session) {
  ns <- NS(id)

  tagList(
    tags$div(id="methylation_controls",
      tags$h3("Methylation"),
      checkboxGroupInput(
        inputId = ns("statuses"),
        label = "Select any number of methylation statuses to include:",
        choices = c("methylated", "unmethylated", "variable", "high_sd"),
        selected = c("methylated", "unmethylated", "variable", "high_sd")
      ),
      tags$hr()
    )
  )
}
    
#' configure_methylation Server Functions
#'
#' @param id Internal Shiny parameter
#' 
#' @noRd 
mod_configure_methylation_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    return(
      list(
        elements = reactive({ input$statuses }),
        resolution = reactive({ input$resolution })
      )
    )
  })
}
    
## To be copied in the UI
# mod_configure_methylation_ui("configure_methylation_1")
    
## To be copied in the server
# mod_configure_methylation_server("configure_methylation_1")
