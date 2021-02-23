#' Rejig panel widths
#'
#' @param p The plot to rejig
#' @param w The relative widths to use
#' @param w.expansion A value to expand the widths by
#' @param c1.w The width of the first column
#'
#' @return Corrected plot
rejig_panel_widths = function(p,
                              w,
                              w.expansion = 15,
                              c1.w = unit(4,"cm") ) {
  p.gt = ggplot_gtable(ggplot_build(p))
  i = as.character( p.gt$widths ) == "1null"
  p.gt$widths[i] = unit(w*w.expansion,"pt")
  p.gt$widths[4] = c1.w
  return( p.gt )
}
