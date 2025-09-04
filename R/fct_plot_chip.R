#' Plot a ChIP ribbon track
#'
#' @description Generate a ChIP ribbon track built in ggplot2
#'
#' @param chr An integer value representing the chromosome of the region to be plotted.
#' @param start,end Integer values representing the start and end coordinates
#'  (in base pairs) of the region to be plotted.
#' @param resolution Size of the bins (in base pairs) for which summary statistics are
#'  calculated from the bigWig files. A higher resolution (smaller bin size) will
#'  result in a more detailed ribbon but will take longer to generate.
#'  Defaults to 5000.
#'
#' @return A list of ggplot2 objects showing ChIP signals for the specified samples
#'  over the configured region.
#'
#' @noRd
#'
#' @import ggplot2
plot_chip <- function(chr, start, end, resolution=5000, chip_samples) {
    chip_query = paste0('chr',chr,':',start,'-',end)
    bedfile = gen_windows(chr=chr, start=start, end=end, window_size=resolution)
    bw = subset(browser_data$bw, bw_sample_names %in% chip_samples)
    chip_signal = get_summaries(bedSimple=bedfile, bigWigs=bw$bw_files)
    chip_signal_names = bw$bw_sample_names
    plots = list()
    for(i in 1:length(chip_signal_names)){
      chip_track = ggplot2::ggplot() +
        ggplot2::geom_area(ggplot2::aes(x=start, y=max), data=chip_signal[[i]]) +
        ggplot2::coord_cartesian(xlim=c(start, end), expand=FALSE) +
        ggplot2::labs(subtitle=bw$bw_sample_names[i]) +
        ggplot2::theme(panel.background=ggplot2::element_blank(),
          aspect.ratio=0.1,
          plot.margin=ggplot2::margin(0, 0, 0, 0),
          axis.title=ggplot2::element_blank(),
          axis.ticks.y=ggplot2::element_blank(),
          axis.text.y=ggplot2::element_blank())
      plots[[i]] = chip_track
    }
    return(plots)
}
