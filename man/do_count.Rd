% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/do_count.R
\name{do_count}
\alias{do_count}
\title{Tabulate Yes/No entries in one or more columns.}
\usage{
do_count(
  this_data,
  this_var,
  missing_value = "Missing",
  these_levels = c("Yes", "No", "Missing"),
  missing_data_limit = 10,
  this_function = NA,
  key_for_variables = glossary.key
)
}
\arguments{
\item{this_data}{The tibble in which the column exists.}

\item{this_var}{The column name (if not a quosure, error will be thrown).  Also defines the order of the rows.}

\item{missing_value}{The text to use for missing (i.e., "") entries (default is Missing)}

\item{these_levels}{Which levels to display (default = Yes/No/Missing).}

\item{missing_data_limit}{Print warning for any result showing at least this percentage of missing data.}

\item{this_function}{A label for error/warning messages (by default it is the name of this function).}

\item{key_for_variables}{Glossary to use for parameters.}
}
\value{
Counts of levels in the factor.
}
\description{
This function takes a data frame and defined list of columns
(optionally one) and summarises the numbers of Yes/No in a table.
}
