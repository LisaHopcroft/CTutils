### Taken from http://michaeljw.com/blog/post/subchunkify/

#' Print a figure at a different width/height than the chunk
#' it is in.
#'
#' @param g The plot
#' @param fig_height The desired height of the plot (inches)
#' @param fig_width The desired width of the plot (inches)
#'
#' @importFrom knitr knit_expand
#' @importFrom stats runif
#'
#' @export
subchunkify <- function(g, fig_height=7, fig_width=5) {
  g_deparsed <- paste0(deparse(
    function() {g}
  ), collapse = '')

  sub_chunk <- paste0("
`",
                      "``{r sub_chunk_", floor(runif(1) * 10000), ", fig.height=", fig_height, ", fig.width=", fig_width, ", echo=FALSE}",
                      "\n(",
                      g_deparsed
                      , ")()",
                      "\n`","``
")

  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
}

