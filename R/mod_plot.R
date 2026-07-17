#' plot UI Function
#'
#' @description Create a patchwork plot containing all elements as set 
#'  by the user
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom shinycssloaders withSpinner 
#' @importFrom patchwork wrap_plots patchworkGrob
mod_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(plotOutput(ns('patchwork'), height='auto')),
    tags$div(id=ns("alignment_bar"), class="alignment_bar",
      tags$div(id=ns("alignment_bar_padding_left"), class="alignment_bar_padding"),
      tags$div(id=ns("alignment_bar_line"), class="alignment_bar_line"),
      tags$div(id=ns("alignment_bar_padding_right"), class="alignment_bar_padding"),
      tags$p("No plot", id=ns("alignment_bar_text"), class="alignment_bar_text")
    ),
    tags$div(id=ns("brush"), class="plot_brush",
      shiny::actionButton(
        ns("brush_zoom"),
        label = "Zoom to area",
        class = "plot_brush_button",
        onclick = "hideBrush()"
      )
    )
  )
}
    
#' plot Server Functions
#'
#' @param id Internal Shiny parameter
#' @param basic_config List of reactive functions - output from 
#'  mod_necessary_setup which details overall configuration settings
#' @param current_plots reactiveValues object containing all currently active plots
#' @param plot_configs Named list of reactive functions from the plot UI which detail
#'  the user selected plot configuration settings. Used to determine the dimensions
#'  of the patchwork plot output.
#' @param toolbar_config List of reactive functions - output from mod_toolbar which
#'  contains toolbar button data
#' 
#' @noRd 
#' 
#' @import shiny
#' @importFrom shinycssloaders showSpinner hideSpinner
#' @importFrom patchwork wrap_plots patchworkGrob
#' @importFrom shinyjs addClass removeClass
mod_plot_server <- function(id, basic_config, current_plots, plot_configs, 
prev_configs, toolbar_config){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    plot_height = reactive({
      height = 0
      for(name in names(session$userData$plot_heights)){
        height = height + session$userData$plot_heights[[name]]
      }
      # I don't know why this is necessary but adding multiple of the ChIP/ATAC
      #  tracks makes the plot draw compress the x axes if this extra height isn't
      #  applied beyond the first track. This is a band-aid fix but it works well
      #  enough.
      active = reactiveValuesToList(session$userData$plot_heights) != 0
      mult = sum(unlist(lapply(names(session$userData$plot_heights)[active], 
        startsWith, prefix=c("chip", "atac"))))
      if(mult > 1){
        height = height + (0.02*mult)
      }
      return(height)
    })

    observeEvent({basic_config$draw_plots() | toolbar_config$zoom_in() |
      toolbar_config$zoom_out() | toolbar_config$update_plots() |
      input$brush_zoom}, {
      if(!shiny::isTruthy(session$userData$region())){
        return()
      }
      shinycssloaders::showSpinner("patchwork")
      shinyjs::addClass(selector="#plot_1-patchwork", class="recalculating")
    }, priority = 1)

    observeEvent({basic_config$draw_plots() | toolbar_config$zoom_in() |
      toolbar_config$zoom_out() | toolbar_config$update_plots() | 
      input$brush_zoom}, {
      if(basic_config$draw_plots()==0 & toolbar_config$zoom_in()==0 &
        toolbar_config$zoom_out()==0 & toolbar_config$update_plots()==0 &
        input$brush_zoom==0){
        shinycssloaders::hideSpinner("patchwork")
        shinyjs::removeClass(selector="#plot_1-patchwork", class="recalculating")
        return()
      }
      else if(!shiny::isTruthy(session$userData$region())){
        shinycssloaders::hideSpinner("patchwork")
        shinyjs::removeClass(selector="#plot_1-patchwork", class="recalculating")
        return()
      }
      plotlist = isolate(reactiveValuesToList(current_plots))
      plotCheck = unique(unlist(lapply(plotlist, is.null)))
      if(is.null(plotCheck)){
        shinycssloaders::hideSpinner("patchwork")
        shinyjs::removeClass(selector="#plot_1-patchwork", class="recalculating")
        return()
      }
      plotlist_clean = list()
      for(p in names(plotlist)){
        if(!is.null(plotlist[[p]])){
          plotlist_clean[[p]] = plotlist[[p]]
        }
      }
      if(length(plotlist_clean) == 0){
        session$userData$patchwork_plot(NULL)
      }
      else if(length(plotlist_clean) == 1){
        session$userData$patchwork_plot(plotlist_clean[[1]])
      }
      else{
        plot = patchwork::wrap_plots(
          plotlist_clean,
          ncol = 1
        )
        session$userData$patchwork_plot(plot)
      }
      rm(plotlist)
      rm(plotlist_clean)
    }, priority = -1)

    output$patchwork = renderPlot({
      plot = session$userData$patchwork_plot()
      if("patchwork" %in% attr(plot, "class")){
        plot_info = patchwork::patchworkGrob(plot)
        # Calculate pixel widths of plot elements
        w_px = grid::convertWidth(plot_info$width, "native", TRUE)
        # Total pixel widths before and after the actual plot panel
        post_panel = sum(w_px[which(as.character(plot_info$width) == "1null"):
          length(plot_info$width)])
        pre_panel = sum(w_px)-post_panel
        # These need to be passed through to the brush javascript onclick function
        session$sendCustomMessage("panel_widths", c(pre_panel, post_panel))
        region = session$userData$activeRegion()
        session$sendCustomMessage("plot_coords", c(region$start, region$end))
      }
      else if("ggplot" %in% attr(plot, "class")){
        plot_info = ggplot2::ggplotGrob(plot)
        w_px = grid::convertWidth(plot_info$width, "native", TRUE)
        post_panel = sum(w_px[which(as.character(plot_info$width) == "1null"):
          length(plot_info$width)])
        pre_panel = sum(w_px)-post_panel
        session$sendCustomMessage("panel_widths", c(pre_panel, post_panel))
        region = session$userData$activeRegion()
        session$sendCustomMessage("plot_coords", c(region$start, region$end))
      }
      plot
    }, 
      res = isolate(session$clientData$pixelratio*96),
      height = function(){
        height = plot_height()
        if(height <= 0.5){
          return(session$clientData$"output_plot_panel_width"*0.5)
        }
        else{
          return(session$clientData$"output_plot_panel_width" * height)
        }
      }
    )

    observeEvent(input$brush_zoom, {
      brush_coords = input$brush_coords
      if(!shiny::isTruthy(brush_coords)){
        shiny::showNotification(
          "No plot to zoom on.",
          duration = 5,
          closeButton = TRUE,
          id = 'brush_zoom_missing_plot_notif',
          type = 'error',
          session = session
        )
        return()
      }
      if(brush_coords[1] < 0){
        brush_coords[1] = 0
      }
      if(brush_coords[2] > 1){
        brush_coords[2] = 1
      }
      if(brush_coords[1] >= brush_coords[2]){
        # As is the case when the brush is not dragged out at all
        shiny::showNotification(
          "Area is too narrow.",
          duration = 5,
          closeButton = TRUE,
          id = 'brush_zoom_narrow_area_notif',
          type = 'error',
          session = session
        )
        return()
      }
      region = isolate(session$userData$activeRegion())
      start = region$start
      end = region$end
      width = end - start
      newStart = as.integer(start + (brush_coords[1] * width))
      newEnd = as.integer(start + (brush_coords[2] * width))
      zoomed_region = list(
        chr = region$chr,
        start = newStart,
        end = newEnd,
        width = newEnd - newStart
      )
      session$userData$activeRegion(zoomed_region)
      session$userData$regionChange(session$userData$regionChange()+1)
    }, priority=1)
  })
}
    
## To be copied in the UI
# mod_plot_ui("plot_1")
    
## To be copied in the server
# mod_plot_server("plot_1")
