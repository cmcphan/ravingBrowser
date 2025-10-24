#' region_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList icon
#' @importFrom shinyjs disabled
mod_region_input_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = 'Region input',
    tagList(
      tags$div(id='region_input_controls',
        selectInput(
          inputId = ns('region_chr'),
          label = 'Chromosome:',
          choices = browser_data$hic_info$name,
          selected = browser_data$default_chr,
          multiple = FALSE
        ),
        sliderInput(
          inputId = ns('region_size_slider'),
          label = 'Region limits (in base pairs):',
          min = 1,
          max = browser_data$hic_info[browser_data$default_chr, 'length'],
          value = c(1, browser_data$default_chr_length)
        ),
        actionButton(
          inputId = ns('toggle_region_size'),
          label = 'Switch range input'
        ),
        shinyjs::disabled(
          numericInput(
            inputId = ns('region_size_direct_min'),
            label = 'Region start:',
            value = 1,
            min = 1,
            max = browser_data$default_chr_length-1
          )
        ),
        shinyjs::disabled(
          numericInput(
            inputId = ns('region_size_direct_max'),
            label = 'Region end:',
            value = browser_data$default_chr_length,
            min = 2,
            max = browser_data$default_chr_length
          )
        )
      )
    ),
    value = 'region_input',
    icon = shiny::icon('keyboard')
  )
}
    
#' region_input Server Functions
#' 
#' @importFrom shinyjs toggleState
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback
#' @noRd 
mod_region_input_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
    # Keep track of max bound of currently selected chromosome
    current_max = reactive({ browser_data$hic_info[input$region_chr, 'length'] })

    observeEvent(input$toggle_region_size, {
      shinyjs::toggleState(id='region_size_slider')
      shinyjs::toggleState(id='region_size_direct_min')
      shinyjs::toggleState(id='region_size_direct_max')
      # Slider input starts enabled (button == 0), therefore if the button is odd,
      #  the numeric input must be enabled, otherwise the slider input must be
      #  enabled
      if(input$toggle_region_size %% 2 == 1){
        # Copy currently selected values onto the other input method
        updateNumericInput(session, 'region_size_direct_min',
          value=input$region_size_slider[1])
        updateNumericInput(session, 'region_size_direct_max',
          value=input$region_size_slider[2])
        shinyFeedback::hideFeedback('region_size_slider', session)
      }
      else{
        copied_value = c(input$region_size_direct_min, input$region_size_direct_max)
        # This check is not strictly necessary but prevents a warning message being
        #  printed
        if(is.na(copied_value[1])){ copied_value[1]=1 }
        if(is.na(copied_value[2])){ copied_value[2]=current_max() }
        updateSliderInput(session, 'region_size_slider',
          value=copied_value)
        shinyFeedback::hideFeedback('region_size_direct_min', session)
        shinyFeedback::hideFeedback('region_size_direct_max', session)
      }
    })
    
    observeEvent(input$region_chr, {
      updated_max = current_max()
      updateSliderInput(session, 'region_size_slider', max=updated_max,
        value=c(1, updated_max))
      updateNumericInput(session, 'region_size_direct_min', max=updated_max-1,
        value=1)
      updateNumericInput(session, 'region_size_direct_max', max=updated_max,
        value=updated_max)
    })
    
    # Validate slider
    observeEvent(input$region_size_slider, {
      value = input$region_size_slider
      if(value[2] == value[1]){
        shinyFeedback::showFeedbackDanger('region_size_slider', session=session,
          text='Region cannot be 0 base pairs long')
      }
      else{
        shinyFeedback::hideFeedback('region_size_slider', session)
      }
    })
    
    # Validate direct numeric inputs
    observeEvent({input$region_size_direct_min | input$region_size_direct_max}, {
      shinyFeedback::hideFeedback('region_size_direct_min', session)
      shinyFeedback::hideFeedback('region_size_direct_max', session)

      min = input$region_size_direct_min
      max = input$region_size_direct_max
      # Need to check that opposing bound is not NA first, otherwise the comparisons
      #  break
      if(is.na(min)){
        shinyFeedback::showFeedbackDanger('region_size_direct_min', session=session,
        text=paste0('Invalid input: must be a number between 1 and ',
          current_max()-1))
      }
      else if(is.na(max)){
        shinyFeedback::showFeedbackDanger('region_size_direct_max', session=session,
        text=paste0('Invalid input: must be a number between 2 and ', current_max()))
      }
      else if(max <= min){
      shinyFeedback::showFeedbackDanger('region_size_direct_max', session=session,
          text='Maximum must be greater than minimum')
      }
      #	Invalid inputs, e.g. non numeric strings, get returned as NA
			#	Values beyond the allowable ranges can be directly input and be returned,
			#  so need to check
			#	Both should be treated as invalid
			if(is.na(min) | min < 1 | min > current_max()-1){
        shinyFeedback::showFeedbackDanger('region_size_direct_min', session=session,
          text=paste0('Invalid input: must be a number between 1 and ',
            current_max()-1))
			}
      if(is.na(max) | max < 2 | max > current_max()){
        shinyFeedback::showFeedbackDanger('region_size_direct_max', session=session,
          text=paste0('Invalid input: must be a number between 2 and ',
            current_max()))
			}
    }, ignoreInit = TRUE)

    region = reactive({ 
      config = list(
        chr = input$region_chr
      )
      # As per shiny.posit.co/r/reference/shiny/latest/actionbutton.html, action
      #  button return values treat 0 as falsy and therefore validations fail on 
      #  initial load. Use is.null to handle initial load.
      if(is.null(input$toggle_region_size) | input$toggle_region_size %% 2 == 0){
        config$start = input$region_size_slider[1]
        config$end = input$region_size_slider[2]
      }
      else{
        config$start = input$region_size_direct_min
        config$end = input$region_size_direct_max
      }
      config$width = config$end - config$start
      return(config)
    })

    observeEvent(region(), {
      region = region()
      session$userData$region(region)
    })

    return( region )
  })
}
    
## To be copied in the UI
# mod_region_input_ui("region_input_1")
    
## To be copied in the server
# mod_region_input_server("region_input_1")
