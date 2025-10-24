#' Controls for configuring ChIP-seq plot output
#'
#' @description Provides a set of inputs to allow the user to configure plot parameters
#'  specific to ChIP-seq plots.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags
mod_configure_chip_ui <- function(id, session) {
  ns <- NS(id)

  region = isolate(session$userData$region())
  if(!shiny::isTruthy(region)){
    region_width = browser_data$hic_info[browser_data$default_chr, 'length']-1
  }
  else{
    region_width = region$width
  }
  min = as.integer(region_width/2000)
  max = as.integer(region_width/250)
  step = as.integer((max-min)/100)

  tagList(
    tags$div(id='chip_controls',
      tags$h3('ChIP-seq'),
      checkboxGroupInput(
        inputId = ns('chip_samples'),
        label = 'Select any number of samples to plot:',
        choices = browser_data$chip$bw_sample_names
      ),
      sliderInput(
        inputId = ns('resolution'),
        label = 'Bin size (base pairs):',
        min = min,
        max = max,
        value = min,
        step = step
      ),
      tags$hr()
    )
  )
}
    
#' configure_chip Server Functions
#'
#' @param id Internal Shiny parameter
#' 
#' @noRd 
mod_configure_chip_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(session$userData$region(), {
      region = session$userData$region()
      if(is.na(region$start) | is.na(region$end)){
        return()
      }
      region_width = region$end - region$start
      if(region$width < 2000){
        region_width = 2000
      }
      min = as.integer(region_width/2000)
      max = as.integer(region_width/250)
      step = as.integer((max-min)/100)
      updateSliderInput(
        inputId = 'resolution',
        min = min,
        max = max,
        value = isolate(max(min(input$resolution, max), min)),
        step = step,
        session = session
      )
    }, ignoreInit=FALSE)
      

    return(
      list(
        elements = reactive({ input$chip_samples }),
        resolution = reactive({ input$resolution })
      )
    )
  })
}
    
## To be copied in the UI
# mod_configure_chip_ui("configure_chip_1")
    
## To be copied in the server
# mod_configure_chip_server("configure_chip_1")
