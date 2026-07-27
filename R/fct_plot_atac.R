#' Plot an ATAC ribbon track
#'
#' @description Generate an ATAC ribbon track built in ggplot2
#'
#' @param c An integer value representing the cosome of the region to be plotted.
#' @param s,e Integer values representing the start and end coordinates
#'  (in base pairs) of the region to be plotted.
#' @param resolution Size of the bins (in base pairs) for which summary statistics are
#'  calculated from the bigWig files. A higher resolution (smaller bin size) will
#'  result in a more detailed ribbon but will take longer to generate.
#'  Defaults to 5000.
#' @param peaksets List of ATAC-seq peaksets selected for display by the user.
#' @param session Internal Shiny parameter containing session data.
#'
#' @return A list of ggplot2 objects showing ATAC signals for the specified samples
#'  over the configured region.
#'
#' @noRd
#'
#' @import ggplot2
#' @importFrom scales hue_pal
#' @importFrom ggblend blend
#' @importFrom quickcode mix.color
plot_atac <- function(c, s, e, resolution, peaksets, session) {
  w_ratio = session$userData$screen_width()/1920
  h_ratio = session$userData$screen_height()/965
  atac_query = paste0(c,':',s,'-',e)
  bedfile = gen_windows(chr=c, start=s, end=e, window_size=resolution)
  bw = browser_data$atac_signal
  atac_signal = get_summaries(bedSimple=bedfile, bigWigs=bw$bw_files, metric="mean")
  atac_signal_names = bw$bw_sample_names
  samples = unique(bw$sample)
  #bw_input = subset(browser_data$chip_signal, sample %in% samples & mark=="INPUT")
  #input_signal = get_summaries(bedSimple=bedfile, bigWigs=bw_input$bw_files, metric="mean")
  # Remove temporary bedfile now that we have no more use for it. This is ripped from the get_summaries
  #  function, where it was originally and modified to run asynchronously
  system(command = paste0("rm ", bedfile), intern = FALSE, wait=FALSE)
  plots = sapply(peaksets, function(p){
    peaks = subset(browser_data$atac_peaks[[p]], chr==c & ((end >= s & start <= s) | 
      (start <= e & end >= e) | (start >= s & end <= e)))
    # Calculate peak block positions
    maxVal = max(unlist(lapply(atac_signal, function(df){ max(df[,"metric"]) }))) # Get max signal value
    peakBlockMax = maxVal*1.3
    base_colour = browser_data$COLOURS[[paste0("atac-",p)]]
    colours = NULL
    max_reps = browser_data$atac_max_reps[[p]]
    for(i in 1:max_reps){
      colours[as.character(i)] = alpha(base_colour, i*(1/max_reps))
    }
    plot = ggplot2::ggplot() +
      lapply(names(atac_signal), function(n){ggplot2::geom_area(ggplot2::aes(x=start, y=metric), 
        fill=base_colour, alpha=1/max_reps, data=atac_signal[[n]])}) |> ggblend::blend("add") +
      ggplot2::geom_rect(data=peaks, ggplot2::aes(xmin=start, xmax=end, ymin=maxVal, 
        ymax=peakBlockMax, fill=nReps), key_glyph=cowplot::rectangle_key_glyph(colour=fill, size=0, 
        padding=grid::unit(c(1*h_ratio, 0, 1*h_ratio, 0), "pt"))) +
      ggplot2::coord_cartesian(xlim=c(s, e), expand=FALSE) +
      ggplot2::scale_fill_manual(limits=as.factor(seq(from=1, to=max_reps, by=1)), 
        values=colours, name="Support") +
      ggplot2::labs(subtitle=p) +
      ggplot2::theme(aspect.ratio=0.05,
        panel.background=ggplot2::element_blank(),
        plot.margin=ggplot2::margin(0, 0, 0, 0),
        axis.ticks.y=ggplot2::element_blank(),
        axis.text.y=ggplot2::element_blank(),
        axis.title=ggplot2::element_blank(),
        plot.subtitle=ggplot2::element_text(vjust=-3),
        text=ggplot2::element_text(size=4.5*min(w_ratio, h_ratio)),
        legend.key.size=grid::unit(8*min(w_ratio, h_ratio), "points"),
        legend.key.spacing=grid::unit(0, "points"),
        legend.direction="horizontal",
        legend.title.position="top",
        legend.text.position="bottom"
      )
    session$userData$plot_height_ratios[[paste0("atac-",p)]] = 0.09
    return(plot)
  }, USE.NAMES=TRUE)
  rm(bw)
  rm(atac_signal)
  return(plots)
}
