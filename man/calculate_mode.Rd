% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_mode.R
\name{calculate_mode}
\alias{calculate_mode}
\title{Find the mode in a list of values (i.e., the most
common value).}
\usage{
calculate_mode(v)
}
\arguments{
\item{v}{A list of values}
}
\value{
The mode (most common value)
}
\description{
Find the mode in a list of values (i.e., the most
common value).
}
\examples{
calculate_mode( c( 1, 1, 1, 10, 10000) )
calculate_mode( c( "A", "B", "B", "C") )
calculate_mode( c(LETTERS[1:3], LETTERS[3:5] ) )
}
