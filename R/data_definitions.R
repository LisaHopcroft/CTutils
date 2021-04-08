### https://kbroman.org/pkg_primer/pages/data.html
#' Example Trial Data
#'
#' Completely random data generated (using simstudy) to represent
#' a clinical trial.  Proportions for ethnicity informed by the 
#' Scottish 2011 census (https://www.scotlandscensus.gov.uk/variables-classification/ethnic-group).
#' Surgical outcome hierarchy taken from NAXIVA (https://clinicaltrials.gov/ct2/show/NCT03494816).
#' 
#' @docType data
#'
#' @usage data(example_trial_data)
#'
#' @format An object of class \code{"tibble"}
#'
#' 
#' @keywords datasets
#'
#' @examples
#' data(example_trial_data)
#' age = example_trial.data$age
#' \donttest{boxplot(age)}
"example_trial.data"


### https://kbroman.org/pkg_primer/pages/data.html
#' Glossary for example Trial Data
#'
#' A named list containing a more readable string for each variable in the dataset
#' 
#' @docType data
#'
#' @usage data(example_trial_data)
#'
#' @format A named list
#' 
#' @keywords datasets
#'
#' @examples
#' data(example_trial_data)
#' age = example_trial.glossary$age
"example_trial.glossary"


### https://kbroman.org/pkg_primer/pages/data.html
#' Vocabulary for example Trial Data
#'
#' A named list containing a list of acceptable values for each variable in the dataset
#' 
#' @docType data
#'
#' @usage data(example_trial_data)
#'
#' @format An object of class \code{"tibble"}
#' 
#' @keywords datasets
#'
#' @examples
#' data(example_trial_data)
#' age = example_trial.vocabulary$Gender
"example_trial.vocabulary"
