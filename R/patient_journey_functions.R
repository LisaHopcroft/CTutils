#' #' Plot patient journey by time
#' #'
#' #' @param this_data The tibble in which the column exists.
#' #' @param this_var The column name (if not a quosure, error will be thrown).  Also defines the order of the rows.
#' #' @param facet_column The column to facet patients with
#' #'
#' #' @importFrom dplyr inner_join
#' #' @importFrom dplyr rename
#' #' @importFrom dplyr .data
#' #' @importFrom forcats fct_reorder
#' #' @importFrom ggplot2 geom_line
#' #' @importFrom ggplot2 geom_point
#' #' @importFrom ggplot2 geom_segment
#' #' @importFrom ggplot2 geom_vline
#' #' @importFrom ggplot2 scale_colour_brewer
#' #' @importFrom ggplot2 scale_shape_manual
#' #' @importFrom tidyr pivot_longer
#' #'
#' #' @export
#' plot_patient_journey = function( this_data,
#'                                  id_vars = quo( Label ),
#'                                  these_vars,
#'                                  these_vars_estimated,
#'                                  first_visit,
#'                                  last_visit ) {
#'
#'   #this_data = dummy.d
#'   #these_vars = quos( recruitment_date, surgery_date, chemotherapy_date, FU1mo_date, FU3mo_date, FU6mo_date, FU12mo_date )
#'   #these_vars_estimated = c( 0, 14, 2*7, 30, 2*30, 3*30, 6*30 )
#'   #last_visit = quo( FU12mo_date )
#'   #first_visit = quo( recruitment_date )
#'
#'   estimated_timelines = tibble(
#'     visit_name = lapply( these_vars, quo_name ) %>% unlist,
#'     visit_day = cumsum( these_vars_estimated )
#'   )
#'
#'   this_plot_data = this_data %>%
#'     select( !! id_vars,
#'             !!!these_vars ) %>%
#'     group_by( !! id_vars ) %>%
#'     pivot_longer( -!!id_vars,
#'                   values_to = "date",
#'                   names_to  = "visit_name" ) %>%
#'     ### Add the start and end times
#'     inner_join( this_data %>%
#'                   select( !!id_vars,
#'                           !!first_visit,
#'                           !!last_visit ),
#'                 by = quo_name(id_vars) ) %>%
#'     rename( start_date = !!first_visit,
#'             end_date   = !!last_visit  ) %>%
#'     ### Add missing end dates using estimated_timelines
#'     inner_join( estimated_timelines,
#'                 by = "visit_name" ) %>%
#'     mutate( estimated_date = .data$start_date + .data$visit_day ) %>%
#'     mutate( estimated_end_flag  = ifelse( is.na(.data$end_date),
#'                                           TRUE,
#'                                           FALSE ) ) %>%
#'     mutate( end_date = as_date( ifelse( .data$estimated_end_flag,
#'                                         .data$estimated_date,
#'                                         .data$end_date ) )  ) %>%
#'     ### Add other missing dates using estimated_timelines
#'     mutate( estimated_date_status  = ifelse( !is.na(.data$date),
#'                                              "Present",
#'                                              "Missing" ) ) %>%
#'     mutate( date = as_date( ifelse( .data$estimated_date_status=="Missing",
#'                                     .data$estimated_date,
#'                                     .data$date ) )  ) %>%
#'     ### Dates in the future are estimated
#'     mutate( estimated_date_status = ifelse( .data$date > today(),
#'                                             "Future",
#'                                             .data$estimated_date_status ) )
#'
#'
#'   # if ( !is.null( facet_column ) ) {
#'   #   this_plot_data = this_plot_data %>%
#'   #     inner_join( this_data %>%
#'   #                   select( !!id_vars,
#'   #                           !!facet_column ),
#'   #                 by = quo_name(id_vars) )
#'   # }
#'
#'
#'   journey_plot_bydate = ggplot( this_plot_data,
#'           aes( x=date,
#'                y={{id_vars}},
#'                shape=estimated_date_status ) ) +
#'     geom_segment(aes( x   =start_date,
#'                       xend=end_date,
#'                       y   ={{id_vars}},
#'                       yend={{id_vars}}),
#'                  col="grey",
#'                  size=5
#'     ) +
#'     geom_point( aes( x=estimated_date ),
#'                 shape="|",
#'                 size=3) +
#'     geom_vline( xintercept = today() ) +
#'     geom_point(  ) +
#'     scale_shape_manual( name  ="Date status",
#'                         values=c( "Missing"= 4,
#'                                   "Present"=19,
#'                                   "Future" = 1) )+
#'     theme_bw() +
#'   ylab( "" ) +
#'   xlab( "Date" )
#'
#'   # if ( !is.null( facet_column ) ) {
#'   #   journey_plot = journey_plot + facet_wrap( ~{{facet_column}})
#'   # }
#'
#'   this_plot_data2 = this_plot_data %>%
#'     mutate( dateN = as.integer(.data$date-.data$start_date),
#'             start_dateN = 0,
#'             end_dateN = as.integer(.data$end_date-.data$start_date),
#'             estimated_dateN = as.integer(.data$estimated_date-.data$start_date) ) %>%
#'     mutate( number_missing = as.character( sum(.data$estimated_date_status=="Missing" ) ) ) %>%
#'     ungroup() %>%
#'     mutate( {{id_vars}}:= fct_reorder({{id_vars}}, .data$number_missing ) )
#'
#'
#'   journey_plot_byday = ggplot( this_plot_data2,
#'                                 aes( x=dateN,
#'                                      y={{id_vars}},
#'                                      shape=estimated_date_status ) ) +
#'     geom_segment(aes( x   =start_dateN,
#'                       xend=end_dateN,
#'                       y   ={{id_vars}},
#'                       yend={{id_vars}},
#'                       col =number_missing),
#'                  size=5
#'     ) +
#'     geom_point( aes( x=estimated_dateN ),
#'                 shape="|",
#'                 size=3 ) +
#'     geom_point( ) +
#'     scale_colour_brewer( name   ="Number dates missing",
#'                          palette="YlOrRd") +
#'     scale_shape_manual( name  ="Date status",
#'                         values=c( "Missing"= 4,
#'                                   "Present"=19,
#'                                   "Future" = 1 ) ) +
#'     theme_bw() +
#'     ylab( "" ) +
#'     xlab( "Day (normalised)" )
#'
#'   return( list( dated      = journey_plot_bydate ,
#'                 normalised = journey_plot_byday ) )
#'
#' }
