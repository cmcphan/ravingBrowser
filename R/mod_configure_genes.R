#' Controls for configuring gene feature plot output
#'
#' @description Provides a set of inputs to allow the user to configure plot parameters
#'  specific to gene feature plots.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags HTML icon
mod_configure_genes_ui <- function(id, session) {
  ns <- NS(id)

  choices = build_gene_checkbox(format="simple")
  session$userData$gene_features_values = choices$choiceValues

  tagList(
    tags$div(id="genes_controls",
      tags$h3("Gene Features"),
      tags$div(id="genesCheckbox",
        checkboxGroupInput(
          inputId = ns("gene_features"),
          label = "Feature types to plot:",
          choiceNames = choices$choiceNames,
          choiceValues = choices$choiceValues,
          selected = choices$choiceValues[1],
        )
      ),
      actionButton(
        inputId = ns("toggle_advanced"),
        label = "Show advanced"
      ),
      actionButton(
        inputId = ns("select_all"),
        label = "Select all"
      ),
      actionButton(
        inputId = ns("deselect_all"),
        label = "Deselect all"
      ),
      # These should be put into a little ? icon and shown on hover
      # See help page for shiny::icon
      tags$p("For performance reasons a maximum of 1000 features 
        will be shown at a time. If there are more than 1000 features included
        then the largest features will be prioritized."),
      tags$p("Numbers in parentheses show the count for each type 
        over the entire dataset."),
      tags$p("Refer to https://www.gencodegenes.org/pages/biotypes.html for
        an explanation of groupings. Note that some highly specific groups have been 
        collapsed into others to simplify the list:
        all pseudogene biotypes -> pseudogene,
        immunoglobulin and T cell receptor coding genes -> protein_coding,
        Mt_rRna/Mt_tRna -> misc_RNA"),
      tags$hr()
    )
  )
}
    
#" configure_genes Server Functions
#"
#" @param id Internal Shiny parameter
#" @param region Reactive function from mod_necessary_setup which details region 
#"  configuration
#" @importFrom shinyjs addClass removeClass
#" @noRd 
mod_configure_genes_server <- function(id, region){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$toggle_advanced, {
      if(input$toggle_advanced %% 2 == 0){
        choices = build_gene_checkbox(format="simple")
        buttonLabel = "Show advanced"
        shinyjs::removeClass(selector="#genesCheckbox", class="multicol")
      }
      else{
        choices = build_gene_checkbox(format="advanced")
        buttonLabel = "Hide advanced"
        shinyjs::addClass(selector="#genesCheckbox", class="multicol")
      }

      updateCheckboxGroupInput(
        session = session,
        inputId = "gene_features",
        selected = input$gene_features,
        choiceNames = choices$choiceNames,
        choiceValues = choices$choiceValues
      )
      updateActionButton(
        session = session,
        inputId = "toggle_advanced",
        label = buttonLabel
      )

      session$userData$gene_features_values = choices$choiceValues
    }, ignoreInit=TRUE)

    observeEvent(input$select_all, {
      updateCheckboxGroupInput(
        session = session,
        inputId = "gene_features",
        selected = session$userData$gene_features_values
      )
    }, ignoreInit=TRUE)

    observeEvent(input$deselect_all, {
      updateCheckboxGroupInput(
        session = session,
        inputId = "gene_features",
        selected = NA
      )
    }, ignoreInit=TRUE)

    return(
      list(
        elements = reactive({ input$gene_features })
      )
    )
  })
}
    
## To be copied in the UI
# mod_configure_genes_ui("configure_genes_1")
    
## To be copied in the server
# mod_configure_genes_server("configure_genes_1")
