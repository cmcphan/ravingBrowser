#' Plot a gene feature track
#'
#' @description Generate a gene feature track built in ggplot2
#' 
#' @param chr An integer value representing the chromosome of the region to be plotted.
#' @param start,end Integer values representing the start and end coordinates
#'  (in base pairs) of the region to be plotted.
#' @param elements List of feature types to include.
#' @param session Internal Shiny parameter containing session data.
#'
#' @return A ggplot2 object displaying gene features for the given genomic region.
#' 
#' @import ggplot2
#' @importFrom gggenes geom_gene_arrow geom_gene_label
#' 
#' @noRd
plot_genes <- function(chr, start, end, elements, session){
  included_genes = subset(browser_data$genes, gene_biotype %in% elements)
  included_genes = subset(included_genes, 
    gChr==chr & ((gEnd >= start & gStart <= start) | 
    (gStart <= end & gEnd >= end) | 
    (gStart >= start & gEnd <= end))
  )
  if(nrow(included_genes) == 0){
    session$userData$plot_heights[["genes-gene_track"]] = 0
    return(NULL)
  }
  if(nrow(included_genes) > 1000){
    included_genes = dplyr::arrange(included_genes, desc(width))[1:1000,]
  }
  genes = cascade_genes(included_genes)
  max_y = max(genes$y_offset)
  plot = ggplot2::ggplot(data = genes,
    ggplot2::aes(xmin=gStart, xmax=gEnd, y=-y_offset, label=symbol, 
      forward=strand, fill=gene_biotype)) +
    gggenes::geom_gene_arrow(arrowhead_height=unit(6, 'mm'), 
      arrowhead_width=unit(1, 'mm'), arrow_body_height=unit(6, 'mm')) +
    gggenes::geom_gene_label(align='left') + 
    ggplot2::coord_cartesian(xlim=c(start, end), ylim=c(-max_y-0.5, 0.5), 
      expand=FALSE) + 
    ggplot2::theme(aspect.ratio=0.0225+(0.02*max_y),
      axis.line.x=ggplot2::element_blank(), 
      axis.ticks.x=ggplot2::element_blank(), 
      axis.text.x=ggplot2::element_blank(), 
      axis.text.y=ggplot2::element_blank(), 
      axis.ticks.y=ggplot2::element_blank(), 
      axis.title.y=ggplot2::element_blank(),
      legend.title=ggplot2::element_blank(), 
      panel.background=ggplot2::element_blank())
  nBiotypesAspectRatio = 0.0175*length(unique(included_genes$gene_biotype))
  yOffsetAspectRatio = 0.0225+(0.02*max_y)
  if(yOffsetRatio > nBiotypesRatio){
    session$userData$plot_heights[["genes-gene_track"]] = yOffsetAspectRatio
  }
  else{
    session$userData$plot_heights[["genes-gene_track"]] = nBiotypesAspectRatio
  }
  rm(included_genes)
  rm(genes)
  return(plot)
}

#' @description Accessory function which takes a data frame of gene features 
#'  with assigned y offsets and checks whether any features with the same y offset 
#'  are overlapping. 
#' @param df Data frame containing gene feature info and assigned y offsets for each.
#'  Output from cascade_genes().
#' @returns TRUE if overlaps are resolved (i.e. all overlapping features have a 
#'  different y offset) and FALSE otherwise.
#' @importFrom dplyr arrange
#' @noRd
.check_overlaps <- function(df){
  # A y offset of -1 is an initialization value and should not be present 
  #  in the resolved data frame
  if(-1 %in% df$y_offset){
    return(FALSE)
  }
  for(i in unique(df$y_offset)){
    offset_group = dplyr::arrange(subset(df, y_offset==i), gStart)
    if(nrow(offset_group) == 1){ next }
    starts = offset_group$gStart
    ends = offset_group$gEnd
    for(x in 1:(length(ends)-1)){
      if(ends[x] > starts[x+1]){
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

#' @description Accessory function which resolves overlaps within a group 
#'  of contiguous features. Iterates through to place features in such a way 
#'  that they are given the lowest offset possible without overlapping with 
#'  any other features with the same offset.
#' 
#' @param feature Feature to place within the overlap_group. A row from
#'  the gene info data frame
#' @param overlap_group A group of features forming a contiguous region
#' 
#' @return The lowest possible y offset for the given feature as an integer
#' 
#' @noRd
.resolve_overlaps <- function(feature, overlap_group){
  offset = 0
  offset_group = subset(overlap_group, y_offset==offset)
  feature$y_offset = 0
  if(nrow(offset_group) == 0){
    return(0)
  }
  while(!.check_overlaps(rbind(feature, offset_group))){
    offset = offset+1
    feature$y_offset = offset
    offset_group = subset(overlap_group, y_offset==offset)
  }
  return(offset)
}

#' @description Function which takes a gene data frame subsetted from
#'  genekitr::genInfo() and returns the same data frame with an added 
#'  column, y_offset, to be used as a y value when plotting using gggenes, 
#'  used by .check_overlaps() to cascade gene features which overlap.
#' @param genes Data frame containing gene feature information. Output from
#'  genekitr::genInfo()
#' @return A modified version of the input data frame with an added y_offset
#'  column, to be used for plotting
#' @importFrom dplyr arrange desc
#' @noRd
cascade_genes <- function(genes){
  genes = dplyr::arrange(genes, gStart)
  genes_cascade = data.frame()
  len = nrow(genes)
  if(len < 2){
    genes$y_offset = 0
    genes_cascade = rbind(genes_cascade, genes)
  }
  overlap_edge = genes[1,]$gEnd
  overlap_group_list = list()
  overlap_group = genes[1,]
  i = 2
  while(i <= len){
    current_feature = genes[i,]
    if(current_feature$gStart < overlap_edge){
      if(current_feature$gEnd > overlap_edge){
          overlap_edge = current_feature$gEnd
      }
      overlap_group = rbind(overlap_group, current_feature)
    }
    else{
      overlap_edge = current_feature$gEnd
      overlap_group_list[[length(overlap_group_list)+1]] = overlap_group
      overlap_group = current_feature
    }
    i = i+1
  }
  # Make sure the final group is also added
  overlap_group_list[[length(overlap_group_list)+1]] = overlap_group
  # Iterate through overlap groups and resolve each of them, then add the resolved data 
  #  frame to genes_cascade
  for(i in 1:length(overlap_group_list)){
    overlap_group = dplyr::arrange(overlap_group_list[[i]], dplyr::desc(width))
    overlap_group$y_offset = -1
    for(i in 1:nrow(overlap_group)){
      current_feature = overlap_group[i,]
      overlap_group[i,]$y_offset = .resolve_overlaps(current_feature, overlap_group)
    }
    genes_cascade = rbind(genes_cascade, overlap_group)
  }
  return(genes_cascade) # Should pass .check_overlaps()
}