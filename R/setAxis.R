#' setAxis Sets the range of the quanitiative axis (mostly the Y-axis)
#'
#' Copied from "ms_popage" in codemgprofile, Modified by AB 12/2017
#' Creates a Chart comparing Forecast Population Growth by Age in Colorado.
#'
#' Uses the data from the State Demography Office package codemog to
#' create a graph showing projected population  changes by Age for each Colorado county from
#' 2010 to 2025.
#' The chart is modified from the original.  Now, we show three bars, one for each series.
#'
#' @param inValue is the quantitative scale to be input, typically a quantitive field from a data frame
#' @return A list consisting of3 values:  "minBrk", "maxBrk" to be used in the "limits=" statement
#'         and "yBrk" to be used in the "breaks=" statement of the scale_y_continuous command in the ggplot call
#' @export

setAxis <- function(inValue) {
  maxval <- max(inValue)
  minval <- min(inValue)
  yBrk <- pretty(minval:maxval,n=5)
  minBrk <- min(yBrk)
  maxBrk <- max(yBrk)
  outList <- list("minBrk" = minBrk, "maxBrk" = maxBrk, "yBrk" = yBrk)
 return(outList)
}
