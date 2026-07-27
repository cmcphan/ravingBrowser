#' Plot a methylation track
#'
#' @description Generate a methylation track built in ggplot2
#' 
#' @param chr An integer value representing the chromosome of the region to be plotted.
#' @param start,end Integer values representing the start and end coordinates
#'  (in base pairs) of the region to be plotted.
#' @param statuses List of status labels to include.
#' @param session Internal Shiny parameter containing session data.
#'
#' @return A ggplot2 object displaying methylation features for the given genomic region.
#' 
#' @import ggplot2
#' 
#' @noRd
plot_methylation <- function(c, s, e, statuses, session){
  w_ratio = session$userData$screen_width()/1920
  h_ratio = session$userData$screen_height()/965
  data = subset(methylation$obs, status %in% statuses & chr == c & start >= s & start <= e)
  if(nrow(data) == 0){
    session$userData$plot_height_ratios[["methylation"]] = 0
    return(NULL)
  }
  visible_width = max(ceiling((e-s)/session$clientData$"output_plot_panel_width"), 1)
  plot = ggplot2::ggplot(data=data, aes(x=start, y=(mean-0.5)/2, width=visible_width, height=mean+0.5))+
    ggplot2::geom_tile(aes(fill=status)) +
    ggplot2::scale_fill_manual(breaks=c("methylated", "unmethylated", "variable", "high_sd"), 
                      values=c("#a2fc9f", "#fc9fa0", "#fcd849", "#e0e0e0")) + 
    ggplot2::coord_cartesian(xlim=c(s, e), ylim=c(-0.5,1), expand=FALSE) +
    ggplot2::theme(aspect.ratio=0.05, 
      panel.background=element_blank(), 
      plot.margin=margin(0,0,0,0),
      axis.ticks.y=ggplot2::element_blank(),
      axis.text.y=ggplot2::element_blank(),
      axis.title=ggplot2::element_blank(),
      text=ggplot2::element_text(size=4.5*min(w_ratio, h_ratio)),
      legend.key.size=grid::unit(10*min(w_ratio, h_ratio), "points")
    )
  session$userData$plot_height_ratios[["methylation"]] = 0.05
  rm(data)
  return(plot)
}