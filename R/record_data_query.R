#' Record a data query
#'
#' data_queries.list = NULL

#' Add a data query to the list of queries being compiled throughout
#' the report writing process.
#'
#' @param patient_list A list of patients
#' @param query_message The query message of each patient
#' @param report_reference A reference for the DMC report
#' @param CRF_reference A reference for the CRF
#' @param list_of_data_queries A list of data queries to add to
#'
#' @importFrom dplyr %>%
#' @importFrom tibble add_row
#'
#' @return An updated list of data queries
#'
#' @export
record_data_query = function( patient_list,
                              query_message,
                              report_reference = NULL,
                              CRF_reference = NULL,
                              list_of_data_queries ) {

  list_of_data_queries = list_of_data_queries %>%
    add_row(
      label   = patient_list,
      message = query_message,
      DMC_ref = report_reference,
      CRF_ref = CRF_reference
    )

  return( list_of_data_queries )
}
