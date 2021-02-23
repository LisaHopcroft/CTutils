#' Rejig panel dimensions
#'
#' @param p The plot to rejig
#' @param w The relative widths to use
#' @param h The relative heights to use
#' @param w.expansion A value to expand the widths by
#' @param h.expansion A value to expand the heights by
#' @param c1.w The width of the first column
#'
#' @return Corrected plot
rejig_panel_dimensions = function(p,
                                  w,
                                  h,
                                  w.expansion = 15,
                                  h.expansion = 8,
                                  c1.w = unit(4,"cm") ) {
  p.gt = ggplot_gtable(ggplot_build(p))
  i = as.character( p.gt$widths ) == "1null"
  p.gt$widths[i] = unit(w*w.expansion,"pt")
  p.gt$widths[4] = c1.w

  i = as.character( p.gt$heights ) == "1null"
  p.gt$heights[i] = unit(h*h.expansion,"pt")

  return( p.gt )
}


