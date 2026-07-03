#' toolbar UI Function
#'
#' @description Toolbar for controlling interactive elements of patchwork plot
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
mod_toolbar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$span(class="toolbar",
      shiny::uiOutput(ns("download_button"), inline = TRUE),
      shiny::actionButton(
        ns("zoom_in"),
        label = "Zoom in",
        class = "toolbar-button",
        icon = shiny::icon("magnifying-glass-plus"),
        onclick = "hideBrush()"
      ),
      shiny::actionButton(
        ns("zoom_out"),
        label = "Zoom out",
        class = "toolbar-button",
        icon = shiny::icon("magnifying-glass-minus"),
        onclick = "hideBrush()"
      ),
      shiny::actionButton(
        ns("toggle_alignment_bar"),
        label = "Toggle alignment bar",
        class = "toolbar-button",
        icon = shiny::icon("sort"),
        onclick = "dragLine()"
      ),
      shiny::actionButton(
        ns("toggle_brush"),
        label = "Toggle region selector",
        class = "toolbar-button",
        icon = shiny::icon("arrows-left-right-to-line"),
        onclick = "brushElement()"
      ),
      shiny::actionButton(
        ns("update_plots"),
        label = "Update plots",
        class = "toolbar-button",
        icon = shiny::icon("arrows-rotate"),
        onclick = "hideBrush()"
      )
    )
  )
}
    
#' toolbar Server Functions
#'
#' @importFrom shinyjs disabled
#' 
#' @noRd 
mod_toolbar_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    plot_height = reactive({
      height = 0
      for(name in names(session$userData$plot_heights)){
        height = height + session$userData$plot_heights[[name]]
      }
      return(height)
    })
    
    calc_plot_height = function(){
      height = plot_height()
      if(height <= 0.5){
        return(session$clientData$"output_plot_panel_width"*0.5)
      }
      else{
        return(session$clientData$"output_plot_panel_width"*height)
      }
    }

    # Disabling a download button isn't as simple as changing its class
    #  to disabled - even disabling with shinyjs doesn't actually stop
    #  the onclick event from triggering. Instead, this approach of replacing
    #  the button with a disabled dummy button when the condition is not met
    #  and rendering the download button when the condition is met comes from
    #  https://github.com/rstudio/shiny/issues/4119#issuecomment-3197232845
    output$download_button = renderUI({
      if(!is.null(session$userData$patchwork_plot())){
        shiny::downloadButton(
          ns("plot_download"),
          label = "Download plot",
          class = "toolbar-button"
        )
      }
      else{
        shinyjs::disabled(
          shiny::actionButton(
            ns("plot_download_disabled"),
            label = "Download plot",
            icon = shiny::icon("download")
          )
        )
      }
    })
    
    output$plot_download = shiny::downloadHandler(
      filename = function(){
        "patchwork_plot.png"
      },
      content = function(file){
        ggplot2::ggsave(
          filename = file,
          plot = session$userData$patchwork_plot(),
          height = floor(calc_plot_height())*2,
          width = floor(session$clientData$"output_plot_panel_width")*2,
          units = "px",
          device = "png",
          dpi = 300
        )
      }
    )

    observeEvent(input$zoom_in, {
      region = session$userData$activeRegion()
      zoomed_region = zoom(region$chr, region$start, region$end, "in")
      if(zoomed_region$width <= 1){
        zoomed_region = NULL
      }
      session$userData$activeRegion(zoomed_region)
      session$userData$regionChange(session$userData$regionChange()+1)
    }, priority = 1)

    observeEvent(input$zoom_out, {
      region = session$userData$activeRegion()
      zoomed_region = zoom(region$chr, region$start, region$end, "out")
      if(zoomed_region$width <= 1){
        zoomed_region = NULL
      }
      session$userData$activeRegion(zoomed_region)
      session$userData$regionChange(session$userData$regionChange()+1)
    }, priority = 1)

    observeEvent(input$update_plots, {
      session$userData$activeRegion(session$userData$region())
      session$userData$regionChange(session$userData$regionChange()+1)
    }, priority = 1)

    return(
      list(
        plot_download = reactive({ input$plot_download }),
        zoom_in = reactive({ input$zoom_in }),
        zoom_out = reactive({ input$zoom_out }),
        toggle_alignment_bar = reactive({ input$toggle_alignment_bar }),
        toggle_brush = reactive({ input$toggle_brush }),
        update_plots = reactive({ input$update_plots })
      )
    )
  })
}
    
## To be copied in the UI
# mod_toolbar_ui("toolbar_1")
    
## To be copied in the server
# mod_toolbar_server("toolbar_1")
