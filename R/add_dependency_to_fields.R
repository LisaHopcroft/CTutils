#' Add an additional dependency to certain columns
#'
#' This function takes a dependency map (these_dependencies), looks
#' for fields #' that match a particular string pattern (field_match)
#' and adds a new dependency (new_dependency) to the column for
#' manual dependencies (param_dependencies_manual).
#' If there is nothing there, the new dependency becomes the whole
#' dependency.  Any columns that are updated will be marked as
#' amended in the dependency map tibble.
#'
#' @param these_dependencies A dependency map
#' @param field_match A string pattern that will identify the columns to update
#' @param new_dependency The new dependency to add
#' @param verbose Whether informative messages should be printed (default = FALSE)
#'
#' @return An updated dependency map.
#' @export
add_dependency_to_fields = function( these_dependencies,
                                     field_match,
                                     new_dependency,
                                     verbose = FALSE ) {

  section_name = "ADD DEPENDENCY"

  if ( ! "param_dependencies_manual" %in% colnames( these_dependencies ) ) {
    these_dependencies = these_dependencies %>%
      mutate( param_dependencies_manual = "NA" )
  }

  if ( ! "manual_addition" %in% colnames( these_dependencies ) ) {
    these_dependencies = these_dependencies %>%
      mutate( manual_addition = FALSE )
  }


  updated_dependencies = these_dependencies

  update_count = 0
  amend_count  = 0

  if ( verbose ) print_message( sprintf( "Adding [%s] to [%s] columns",
                                         new_dependency,
                                         field_match),
                                section=section_name )


  for ( i in 1:nrow( these_dependencies ) ) {
    this.column_name = these_dependencies[ i, "column_name" ]
    this.dependency  = these_dependencies[ i, "param_dependencies_manual" ]


    if ( str_detect( this.column_name, field_match ) ) {

      this.verb = ""

      if ( ( ! this.dependency %in% c( "", "NA" ) ) & !is.na( this.dependency ) ) {
        this.dependency_updated = sprintf( "%s and %s", this.dependency, new_dependency )
        this.verb = "amended"
        amend_count = amend_count + 1
      } else {
        this.dependency_updated = new_dependency
        this.verb = "added"
      }

      if ( verbose ) print_message( sprintf( "%s %s",
                                             this.column_name,
                                             this.verb ),
                                    section=section_name,
                                    print_title = FALSE,
                                    mask_character = "." )

      updated_dependencies[ i , "manual_addition" ]           = TRUE
      updated_dependencies[ i , "param_dependencies_manual" ] = this.dependency_updated

      update_count = update_count + 1

    }

  }

  print_message( sprintf( "%d dependencies updated (%d amended):",
                          update_count,
                          amend_count ),
                 section = section_name )
  print_message( sprintf( "[%s] >> %s", field_match, new_dependency ),
                 section = section_name ,
                 print_title = FALSE )

  return( updated_dependencies )
}
