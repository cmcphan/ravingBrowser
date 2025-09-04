#' Plot a HiC contact matrix 
#'
#' @description Generate a HiC contact matrix built in ggplot2 in either square,
#'  triangular or rectangular format.
#' @param chr An integer value representing the chromosome of the region to be plotted
#' @param start,end Integer values representing the start and end coordinates
#'  (in base pairs) of the region to be plotted
#' @param resolution An integer value representing the bin size of the contact matrix.
#'  A smaller bin size represents a higher plot resolution and uses higher
#'  resolution data at the back end. Must be present in the underlying data.
#' @param normalization String describing the normalization method to apply to the
#'  count matrix. Default is 'KR'(Knight-Ruiz). Must be present in the
#'  underlying data.
#' @param format String describing the output format for the plot. Default is
#'  'triangular', which shows the upper triangle of the contact matrix for the
#'  specified region. May also be 'square', which plots the full symmetrical matrix,
#'  or 'rectangular', which plots the upper triangle but with additional data
#'  points filled in from surrounding areas.
#' @return A HiC contact matrix in the specified format showing interactions within
#'  the specified region, as a ggplot2 object
#'
#' import ggplot2
#' importFrom strawr straw
plot_hic <- function(chr, start, end, resolution, normalization='KR',
                format='triangular'){
  hic_path = browser_data$hic_path
  if(format == 'square'){
    strawr_query = paste(chr,start,end, sep=':')
    interactions = strawr::straw(normalization, hic_path, strawr_query,
      strawr_query, 'BP', resolution)
    plot = ggplot2::ggplot() +
      ggplot2::geom_tile(ggplot2::aes(x=y, y=x, fill=log2(counts)),
        data=interactions) +
      ggplot2::geom_tile(ggplot2::aes(x=x, y=y, fill=log2(counts)),
        data=interactions) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::scale_y_continuous(transform='reverse') +
      ggplot2::coord_cartesian(xlim=c(start, end), ylim=c(end, start),
        expand=FALSE) +
      ggplot2::theme(aspect.ratio=1, panel.background=ggplot2::element_blank(),
        plot.margin=ggplot2::margin(0, 0, 0, 0),
        axis.title=ggplot2::element_blank(),
        axis.ticks.y=ggplot2::element_blank(),
        axis.text.y=ggplot2::element_blank())
  }
  else if(format == 'triangular'){
    range = end-start
    strawr_query = paste(chr,start,end, sep=':')
    interactions = strawr::straw(normalization, hic_path, strawr_query,
      strawr_query, 'BP', resolution)
    interactions$dist = interactions$y - interactions$x
    plot = ggplot2::ggplot() +
      ggplot2::geom_rect(ggplot2::aes(xmin=x+(dist/2),
        xmax=x+(dist/2)+resolution, ymin=dist, ymax=dist+resolution,
        fill=log2(counts)), data=interactions) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::coord_cartesian(xlim=c(start, end), ylim=c(0, range),
        expand=FALSE) +
      ggplot2::theme(aspect.ratio=0.5, panel.background=ggplot2::element_blank(),
        plot.margin=ggplot2::margin(0, 0, 0, 0),
        axis.title=ggplot2::element_blank(),
        axis.ticks.y=ggplot2::element_blank(),
        axis.text.y=ggplot2::element_blank())
  }
  else if(format == 'rectangular'){
      range = end-start
      strawr_query = paste(chr,(start-range),(end+range), sep=':')
      interactions = strawr::straw(normalization, hic_path, strawr_query,
        strawr_query, 'BP', resolution)
      interactions$dist = interactions$y - interactions$x
      plot = ggplot2::ggplot() +
        ggplot2::geom_rect(ggplot2::aes(xmin=x+(dist/2),
          xmax=x+(dist/2)+resolution, ymin=dist, ymax=dist+resolution,
          fill=log2(counts)), data=interactions) +
        ggplot2::scale_fill_viridis_c() +
        ggplot2::coord_cartesian(xlim=c(start, end), ylim=c(0, range),
          expand=FALSE) +
        ggplot2::theme(aspect.ratio=0.5, panel.background=ggplot2::element_blank(),
          plot.margin=ggplot2::margin(0, 0, 0, 0),
          axis.title=ggplot2::element_blank(),
          axis.ticks.y=ggplot2::element_blank(),
          axis.text.y=ggplot2::element_blank())
  }
  attr(plot, 'format') = format
  return(plot)
}

#' Overlay TAD boundaries
#'
#' @description Overlay topologically associated domain boundaries onto an existing
#'  Hi-C contact matrix plot.
#'
#' @param plot A ggplot2 object - the Hi-C contact matrix to overlay the TADs onto
#' @param chr,start,end Region parameters describing the bounds of the plot
#'
#' @return A modified version of the input plot, with the TAD boundaries overlaid
#'
#' @import ggplot2
draw_tads <- function(plot, chr, start, end){
  format = attr(plot, 'format')
  included_tads = subset(browser_data$tads, tChr==chr &
    ((tEnd >= start & tStart <= start) |
    (tStart <= end & tEnd >= end) |
    (tStart >= start & tEnd <= end)))[c('tStart', 'tEnd')]
  included_tads$tDist = included_tads$tEnd - included_tads$tStart
  included_tads$tApex = included_tads$tStart + (included_tads$tDist/2)
  if(format == 'square'){
    plot = plot +
      ggplot2::geom_segment(ggplot2::aes(x=tStart, y=tStart, xend=tEnd,
        yend=tStart), data=clip(included_tads, start, end, 'left', format)) +
      ggplot2::geom_segment(ggplot2::aes(x=tEnd, y=tStart, xend=tEnd, yend=tEnd),
        data=clip(included_tads, start, end, 'right', format)) +
      ggplot2::geom_segment(ggplot2::aes(x=tStart, y=tStart, xend=tStart,
        yend=tEnd), data=clip(included_tads, start, end, 'left', format)) +
      ggplot2::geom_segment(ggplot2::aes(x=tStart, y=tEnd, xend=tEnd, yend=tEnd),
        data=clip(included_tads, start, end, 'right', format))
  }
  else if(format == 'triangular'){
    plot = plot +
      ggplot2::geom_segment(ggplot2::aes(x=tStart, y=0, xend=tApex, yend=tDist),
        data=clip(included_tads, start, end, 'left', format)) +
      ggplot2::geom_segment(ggplot2::aes(x=tApex, y=tDist, xend=tEnd, yend=0),
        data=clip(included_tads, start, end, 'right', format))
  }
  else if(format == 'rectangular'){
    plot = plot +
      ggplot2::geom_segment(ggplot2::aes(x=x, y=tDist-(tApex-x)*2, xend=xend,
        yend=(xend-tStart)*2), data=clip(included_tads, start, end, 'left',
        format)) +
      ggplot2::geom_segment(ggplot2::aes(x=x, y=tDist-(x-tApex)*2, xend=xend,
        yend=(tEnd-xend)*2), data=clip(included_tads, start, end, 'right',
        format))
  }
  return(plot)
}

#' Draw arc representation of loops
#'
#' @description Draw a plot which visualizes loops for the requested region as
#'  arcs. Intended to be plotted alongside a Hi-C contact matrix as an aligned track.
#'
#' @param chr,start,end Region parameters describing the bounds of the plot
#'
#' @return A ggplot2 object visualizing loops for the given region
#'
#' @import ggplot2
#' @import ggraph
#' @importFrom tidygraph tbl_graph
plot_loops <- function(chr, start, end){
  included_cis_loops = subset(browser_data$loops, (lChr1==chr & lChr2==chr) &
                        ((from>=start & to<=start) | (from<=end & to>=end) |
                        (from>=start & to<=end)))[c('from', 'to', 'lDist', 'lPval')]
  nodes = data.frame(bin = sort(unique(rbind(included_cis_loops$from,
    included_cis_loops$to))))
  edges = data.frame(included_cis_loops)
  edges$from = as.character(edges$from)
  edges$to = as.character(edges$to)
  loop_graph = tidygraph::tbl_graph(nodes = nodes, edges = edges)
  loop_layout = ggraph::create_layout(loop_graph, layout='linear', sort.by=bin,
    use.numeric=TRUE)
  plot = ggraph::ggraph(loop_layout) +
    ggraph::geom_edge_arc0(ggplot2::aes(alpha=1/lPval), strength = -1) +
    ggraph::theme_graph(plot_margin=ggplot2::margin(0, 0, 0, 0),
      base_family='sans') +
    ggplot2::theme(aspect.ratio=0.05) +
    ggplot2::coord_cartesian(xlim=c(start, end), expand=FALSE)
  return(plot)
}

#' Draw PCA ribbon
#'
#' @description Draw a plot which visualizes A/B compartmentalization scores
#'  (i.e. PCA scores) for the requested region as a ribbon. Intended to be
#'  plotted alongside a Hi-C contact matrix as an aligned track.
#'
#' @param chr,start,end Region parameters describing the bounds of the plot
#'
#' @return A ggplot2 object visualizing PCA scores for the given region
#'
#' @import ggplot2
plot_pca <- function(chr, start, end){
  included_pca = subset(browser_data$pca, pChr==chr &
    ((pEnd >= start & pStart <= start) | (pStart <= end & pEnd >= end) |
    (pStart >= start & pEnd <= end)))
  included_pca$comp = 'A'
  included_pca$comp[included_pca$pScore < 0] = 'B'
  y_max = max(included_pca$pScore)
  y_min = min(included_pca$pScore)

  included_pca_A = included_pca
  included_pca_A$pScore[included_pca_A$comp == 'B'] = 0
  included_pca_B = included_pca
  included_pca_B$pScore[included_pca_B$comp == 'A'] = 0

  plot = ggplot2::ggplot() +
    ggplot2::geom_ribbon(ggplot2::aes(x=pStart, ymin=0, ymax=pScore, fill='A'),
      data=included_pca_A) +
    ggplot2::geom_ribbon(ggplot2::aes(x=pStart, ymin=0, ymax=pScore, fill='B'),
      data=included_pca_B) +
    ggplot2::coord_cartesian(xlim=c(start, end), ylim=c(y_min, y_max),
      expand=FALSE) +
    ggplot2::theme(panel.background=ggplot2::element_blank(),
      plot.margin=ggplot2::margin(0, 0, 0, 0),
      aspect.ratio=0.1,
      axis.title=ggplot2::element_blank(),
      axis.ticks=ggplot2::element_blank(),
      axis.text=ggplot2::element_blank())
  return(plot)
}
