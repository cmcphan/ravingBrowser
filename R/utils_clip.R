#' Ensure TAD boundaries are drawn inside HiC plot
#'
#' @description Function which clips line segments to contain them to the data points of a HiC matrix 
#'  plot instead of drawing them outside the useful area of the plot. Necessary values are calculated here and then 
#'  passed as parameters to the geom_segment function calls in `draw_tads()`, where most of the maths is done.
#' @param included_tads Data frame detailing the TAD boundaries to be drawn. This comes from the `draw_tads()` 
#'  function and should include the chromosome, start and end coordinates, the distance between the start and end, 
#'  and the position of the apex.
#' @param regionStart An integer value describing the starting position of the region of interest (in base pairs)
#' @param regionEnd An integer value describing the ending position of the region of interest (in base pairs)
#' @param segmentSet A string describing which set of line segments to calculate for. May be either 'left' or
#'  'right'
#' @param format A string describing the format of the HiC matrix the TADs are to be drawn on. May be either 
#'  'square', 'triangular' or 'rectangular'
#' @return Returns a modified version of the `included_tads` input data frame which contains segment coordinates 
#'  fitted to the given plot region
#'
#' @noRd
clip = function(included_tads, regionStart, regionEnd, segmentSet, format){
    if(segmentSet == 'left'){
        included_tads$x = included_tads$tStart
        included_tads$xend = included_tads$tApex
        firstTad = included_tads[1,]
        finalTad = included_tads[nrow(included_tads),]
        if(format == 'triangular' | format == 'square'){
            included_tads = subset(included_tads, tStart >= regionStart)
            if(finalTad$tEnd > regionEnd){
                finalTad$tEnd = regionEnd
                finalTad$tDist = regionEnd - finalTad$tStart
                finalTad$tApex = finalTad$tStart + finalTad$tDist/2
                included_tads[nrow(included_tads),] = finalTad
            }
        }
        else if(format == 'rectangular'){
            if(firstTad$tApex > regionStart){
                # Update the start coordinate to match region limit and do the rest of the work in the geom_segment function call
                firstTad$x = regionStart
                included_tads[1,] = firstTad
            }
            else{ included_tads = included_tads[-1,] }
            if(finalTad$tApex > regionEnd){
                finalTad$xend = regionEnd
                included_tads[nrow(included_tads),] = finalTad
            }
        }
    }
    else if(segmentSet == 'right'){
        included_tads$x = included_tads$tApex
        included_tads$xend = included_tads$tEnd
        firstTad = included_tads[1,]
        finalTad = included_tads[nrow(included_tads),]
        if(format == 'triangular' | format == 'square'){
            included_tads = subset(included_tads, tEnd <= regionEnd)
            if(firstTad$tStart < regionStart){
                firstTad$tStart = regionStart
                firstTad$tDist = firstTad$tEnd - regionStart
                firstTad$tApex = firstTad$tEnd - firstTad$tDist/2
                included_tads[1,] = firstTad
            }
        }
        if(format == 'rectangular'){
            if(firstTad$tApex < regionStart){
                firstTad$x = regionStart
                included_tads[1,] = firstTad
            }
            if(finalTad$tApex < regionEnd){
                finalTad$xend = regionEnd
                included_tads[nrow(included_tads),] = finalTad
            }
            else{ included_tads = included_tads[-nrow(included_tads),] }
        }
    }
    return(included_tads)
}
