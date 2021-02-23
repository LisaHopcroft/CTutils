# calculate_basic_statistics = function( this.data,
#                                        this.var,
#                                        round.digits = 0 ) {
#
#   these.values = this.data %>% pull(!!this.var )
#
#   these.values.raw = tibble( statistic = c( "n",
#                                             "n.missing",
#                                             "min",
#                                             "median",
#                                             "mean",
#                                             "max",
#                                             "sd" ),
#                              value= c( length(these.values),
#                                        sum   (is.na(these.values))+sum(these.values==""),
#                                        min   (these.values, na.rm=TRUE),
#                                        median(these.values, na.rm=TRUE),
#                                        mean  (these.values, na.rm=TRUE ),
#                                        max   (these.values, na.rm=TRUE ),
#                                        sd    (these.values, na.rm=TRUE ) ) )
#
#   these.values.plot = ggplot( this.data,
#                               aes( x=quo_name( this.var ), y=!!this.var)) +
#     geom_boxplot(outlier.shape = NA) + geom_jitter( width=0.10 ) +
#     theme_bw() +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank()) +
#     ggtitle( sprintf( "Boxplot of %s (N=%d)",
#                       quo_name( this.var ),
#                       these.values.raw %>% filter(statistic=="n") %>% pull(value) ) )
#
#
#   these.values.rounded = these.values.raw %>%
#     mutate( value = round( value, digits=round.digits ))
#
#   return( list( table=these.values.rounded,
#                 plot=these.values.plot ) )
