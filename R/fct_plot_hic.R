#' Plot a HiC contact matrix 
#'
#' @description Generate a HiC contact matrix built in ggplot2 in either square, triangular or rectangular format.
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
#' @return A HiC contact matrix in the specified format showing interactions within the
#'  specified region, as a ggplot2 object
#'
#' import ggplot2
#' importFrom strawr straw
plot_hic <- function(chr, start, end, resolution, normalization='KR', format='triangular'){
    hic_path = browser_data$hic_path
    if(format == 'square'){
        strawr_query = paste(chr,start,end, sep=':')
        interactions = strawr::straw(normalization, hic_path, strawr_query, strawr_query, 'BP', resolution)
        plot = ggplot2::ggplot() +
            ggplot2::geom_tile(ggplot2::aes(x=y, y=x, fill=log2(counts)), data=interactions) +
            ggplot2::geom_tile(ggplot2::aes(x=x, y=y, fill=log2(counts)), data=interactions) +
            ggplot2::scale_fill_viridis_c() +
            ggplot2::scale_y_continuous(transform='reverse') +
            ggplot2::coord_cartesian(xlim=c(start, end), ylim=c(end, start), expand=FALSE) +
            ggplot2::theme(aspect.ratio=1, panel.background=ggplot2::element_blank())
    }
    else if(format == 'triangular'){
        range = end-start
        strawr_query = paste(chr,start,end, sep=':')
        interactions = strawr::straw(normalization, hic_path, strawr_query, strawr_query, 'BP', resolution)
        interactions$dist = interactions$y - interactions$x
        plot = ggplot2::ggplot() +
            ggplot2::geom_rect(ggplot2::aes(xmin=x+(dist/2), xmax=x+(dist/2)+resolution,
                          ymin=dist, ymax=dist+resolution, fill=log2(counts)), data=interactions) +
            ggplot2::scale_fill_viridis_c() +
            ggplot2::coord_cartesian(xlim=c(start, end), ylim=c(0, range), expand=FALSE) +
            ggplot2::theme(aspect.ratio=0.5, panel.background=ggplot2::element_blank())
    }
    else if(format == 'rectangular'){
        range = end-start
        strawr_query = paste(chr,(start-range),(end+range), sep=':')
        interactions = strawr::straw(normalization, hic_path, strawr_query, strawr_query, 'BP', resolution)
        interactions$dist = interactions$y - interactions$x
        plot = ggplot2::ggplot() +
            ggplot2::geom_rect(aes(xmin=x+(dist/2), xmax=x+(dist/2)+resolution,
                          ymin=dist, ymax=dist+resolution, fill=log2(counts)), data=interactions) +
            ggplot2::scale_fill_viridis_c() +
            ggplot2::coord_cartesian(xlim=c(start, end), ylim=c(0, range), expand=FALSE) +
            ggplot2::theme(aspect.ratio=0.5, panel.background=ggplot2::element_blank())
    }
    attr(plot, 'format') = format
    return(plot)
}
