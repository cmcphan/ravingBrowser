#' gene_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tabPanel selectizeInput
mod_gene_select_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = 'Gene select',
    tagList(
      tags$div(id='gene_select_controls',
        selectizeInput(
          inputId = ns('gene_select'),
          label = 'Search for a gene:',
          choices = NULL,
          multiple = FALSE,
        )
      )
    ),
    value = 'gene_select',
    icon = shiny::icon('magnifying-glass')
  )
}
    
#' gene_select Server Functions
#'
#' @noRd 
#' 
#' @importFrom shiny updateSelectizeInput
mod_gene_select_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    updateSelectizeInput(
      session = session, 
      inputId = 'gene_select', 
      choices = browser_data$genes, 
      server = TRUE,
      options = list(
        labelField = 'symbol',
        valueField = 'rowId',
        searchField = c('symbol', 'ensembl', 'gene_name'),
        render = I(
          '{
            option: function(item, escape) {
              return "<div><strong>" + escape(item.symbol) + "</strong>: " + 
                escape(item.gene_name) + "</div>"
            }
          }'),
        maxOptions = 100,
        placeholder = 'Gene symbol, ENSEMBL ID, or gene name'
      )
    )

    region_config = reactive({
      if(input$gene_select == ""){
        return( NA )
      }
      gene = browser_data$genes[input$gene_select,]
      start = gene$gStart
      end = gene$gEnd
      width = end-start
      chr = gene$gChr
      expandedStart = start-(width*2)
      expandedEnd = end+(width*2)
      if(expandedStart < 1){
        expandedStart = 1
      }
      if(expandedEnd > browser_data$hic_info[chr, 'length']){
        expandedEnd = browser_data$hic_info[chr, 'length']
      }
      config = list(
        region_chr = chr,
        region_start = expandedStart,
        region_end = expandedEnd,
        region_width = expandedEnd-expandedStart
      )
      return(config)
    })

    return( region_config )
  })
}
    
## To be copied in the UI
# mod_gene_select_ui("gene_select_1")
    
## To be copied in the server
# mod_gene_select_server("gene_select_1")
