#' Controls for configuring ATAC-seq plot output
#'
#' @description Provides a set of inputs to allow the user to configure plot parameters
#'  specific to ATAC-seq plots.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags
mod_configure_atac_ui <- function(id, session) {
  ns <- NS(id)

  region = isolate(session$userData$region())
  if(!shiny::isTruthy(region)){
    region_width = browser_data$hic_info[browser_data$default_chr, "length"]-1
  }
  else{
    region_width = region$width
  }
  max = as.integer(2000)
  min = as.integer(250)
  step = as.integer(250)

  tagList(
    tags$div(id="atac_controls",
      tags$h3("ATAC-seq"),
      checkboxGroupInput(
        inputId = ns("peaksets"),
        label = "Select any number of peaksets to plot:",
        choices = names(browser_data$atac_peaks),
        selected = "callpeakBed"
      ),
      sliderInput(
        inputId = ns("resolution"),
        label = "Resolution (as proportion of region width):",
        min = min,
        max = max,
        value = min,
        step = step,
        pre = "1/"
      ),
      tags$hr()
    )
  )
}
    
#' configure_atac Server Functions
#'
#' @param id Internal Shiny parameter
#' 
#' @noRd 
mod_configure_atac_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # This will run once when the slider is created
    # Set a lower step to preserve the tick marks but allow a greater
    #  number of selections
    observeEvent(input$resolution, {
      updateSliderInput(
        session = session,
        inputId = "resolution",
        step = 25,
        value = 1000
      )
    }, once=TRUE)

    return(
      list(
        elements = reactive({ input$peaksets }),
        resolution = reactive({ input$resolution })
      )
    )
  })
}
    
## To be copied in the UI
# mod_configure_atac_ui("configure_atac_1")
    
## To be copied in the server
# mod_configure_atac_server("configure_atac_1")
