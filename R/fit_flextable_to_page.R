#' Fit flextable object to the width of the page
#'
#' @param ft The flextable object
#' @param pgwidth The width of the page, in inches (default = 6in)
#'
#' @return The flextable with the width updated
#' @export
fit_flextable_to_page <- function(ft, pgwidth = 6){
  ft_out = ft %>% autofit()
  ft_out = width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return( ft_out )
}
