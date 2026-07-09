#' gene_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tabPanel selectizeInput verbatimTextOutput
#' @importFrom shinyjs hidden
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
        ),
        shinyjs::hidden(actionButton(
          inputId = ns('collapse_info'),
          label = 'Hide gene info'
        )),
        htmlOutput(ns('gene_info'))
      )
    ),
    value = 'gene_select',
    icon = shiny::icon('magnifying-glass')
  )
}
    
#' gene_select Server Functions
#' 
#' @param current_tab Reactive function that gets the currently active tab from
#'  mod_necessary_setup
#'
#' @noRd 
#' 
#' @import shiny
#' @importFrom shinyjs toggle show hide
mod_gene_select_server <- function(id, current_tab){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    updateSelectizeInput(
      session = session, 
      inputId = "gene_select", 
      choices = browser_data$genes, 
      server = TRUE,
      options = list(
        labelField = "symbol",
        valueField = "rowId",
        searchField = c("symbol", "ensembl", "gene_name", "aliases"),
        render = I(
          '{
            option: function(item, escape) {
              return "<div><strong>" + escape(item.symbol) + "</strong>: " + 
                escape(item.gene_name) + escape(" (" + item.aliases + ")") 
                + "</div>"
            }
          }'),
        maxOptions = 100,
        placeholder = "Gene symbol, ENSEMBL ID, or gene name"
      )
    )

    region = reactive({
      if(input$gene_select == ""){
        return( list(config=NA, current_tab=current_tab()) )
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
      if(expandedEnd > browser_data$hic_info[chr, "length"]){
        expandedEnd = browser_data$hic_info[chr, "length"]
      }
      config = list(
        chr = chr,
        start = expandedStart,
        end = expandedEnd,
        width = expandedEnd-expandedStart,
        current_tab = current_tab()
      )
      return(config)
    })

    observeEvent(region(), {
      region = region()
      if(region$current_tab != "gene_select"){
        return()
      }
      if(input$gene_select == ""){
        shinyjs::hide(id="collapse_info")
        output$gene_info = NULL
        session$userData$region(NULL)
        return()
      }
      shinyjs::show(id="collapse_info")
      gene = browser_data$genes[input$gene_select,]
      if(gene$strand == 0){
        strand = "-ve"
      }
      else{
        strand = "+ve"
      }
      name = paste0(toupper(substring(gene$gene_name, 1, 1)),substring(gene$gene_name, 2))
      ncbi_link = build_links(gene$entrezid, "ncbi")
      genecards_link = build_links(gene$symbol, "genecards")
      ensembl_link = build_links(gene$ensembl, "ensembl")
      uniprot_link = build_links(gene$uniprot, "uniprot")
      omim_link = build_links(gene$omim, "omim")
      hgnc_link = build_links(gene$hgnc_id, "hgnc")
      ucsc_link = build_links(gene$ucsc, "ucsc", gene$gChr, gene$gStart, gene$gEnd)
      output$gene_info = renderUI(HTML(paste0(
        tags$h3(paste0(name," (",gene$symbol,") ")),
        tags$p(paste0(gene$gChr," : ",gene$gStart," - ",gene$gEnd,
          " (",strand," strand)")),
        tags$p(paste0("GC content: ",gene$gc_content,"%")),
        tags$p(gene$summary),
        tags$b("External links"),
        tags$p("NCBI entry: ", ncbi_link),
        tags$p("GeneCards entry: ", genecards_link),
        tags$p("ENSEMBL entry: ", ensembl_link),
        tags$p("Uniprot entries: ", uniprot_link),
        tags$p("OMIM entries: ", omim_link),
        tags$p("HGNC entry: ", hgnc_link),
        tags$p("UCSC browser: ", ucsc_link)
      )))
      session$userData$region(region) 
    })

    observeEvent(input$collapse_info, {
      shinyjs::toggle(id="gene_info")
      if(input$collapse_info %% 2 == 0){
        buttonLabel = "Hide gene info"
      }
      else{
        buttonLabel = "Show gene info"
      }
      updateActionButton(
        session = session,
        inputId = "collapse_info",
        label = buttonLabel
      )
    })
  })
}
    
## To be copied in the UI
# mod_gene_select_ui("gene_select_1")
    
## To be copied in the server
# mod_gene_select_server("gene_select_1")
