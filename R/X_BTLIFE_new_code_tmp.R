# ### If the version of Pandoc is new enough, then set this flag
# ### to true.
#
# FLAG.Flextable = compareVersion( as.character(pandoc_version()),
#                                  "2.0" ) >= 0
#
# replace_these_values = function( x,
#                                  target="",
#                                  replacement="Missing") {
#   if ( x == target ) {
#     return( x )
#   } else {
#     return( replacement )
#   }
# }
#
# ### Do the plots for the psychological assessment
#
# do_psychological_plots = function( data,
#                                    this.var,
#                                    this.ylim = NULL ) {
#   data.tmp = data %>%
#     mutate( value = !!this.var ) %>%
#     group_by( HOSPITAL_SITE,
#               BASELINE_T0_RANDO_FORM_STUDY_ARM )
#
#   this.overall_plot =
#     ggplot( data.tmp,
#             aes( x=BASELINE_T0_RANDO_FORM_STUDY_ARM,
#                  y=value,
#                  fill=BASELINE_T0_RANDO_FORM_STUDY_ARM)) +
#     geom_boxplot( outlier.shape = NA,
#                   alpha=0.80) +
#     geom_jitter(width=0.10) +
#     xlab( glossary.key[["BASELINE_T0_RANDO_FORM_STUDY_ARM"]] )+
#     ylab( glossary.key[[quo_name(this.var)]] ) +
#     scale_fill_manual( values=ARM.colour_scheme ) +
#     theme_BTLIFE +
#     theme( legend.position="none" )
#
#   if ( !is.null(this.ylim) ) {
#     this.overall_plot = this.overall_plot + ylim( this.ylim )
#   }
#
#   print( this.overall_plot +
#            ggtitle( sprintf( "[OVERALL] %s",
#                              glossary.key[[quo_name(this.var)]]) ) )
#
#   print( this.overall_plot +
#            facet_grid( .~HOSPITAL_SITE ) +
#            ggtitle( sprintf( "[AT SITES] %s",
#                              glossary.key[[quo_name(this.var)]]) ) )
# }
#
#
# do_psychological_tables = function( data,
#                                     this.var,
#                                     this.ylim = NULL ) {
#   data.tmp = data %>%
#     mutate( value = !!this.var ) %>%
#     group_by( HOSPITAL_SITE,
#               BASELINE_T0_RANDO_FORM_STUDY_ARM ) %>%
#     spread( BASELINE_T0_RANDO_FORM_STUDY_ARM,
#             value )
#
#   data.summary = do_summary( data.tmp,
#                              quos( Control, HC, `HC & PA` ))
#
#   data.table = data.summary$table %>%
#     mutate( N=n-n.missing ) %>%
#     select( -n, -n.missing ) %>%
#     select( variable, N, everything() ) %>%
#     rename( `Study arm` = variable )
#
#   if ( FLAG.Flextable ) {
#     print_flextable( data.table )
#   } else {
#     kable( data.table )
#   }
# }
#
#
#
#
#
# shapiro_wilks_test_normal = function( x,
#                                       alpha=0.05) {
#   #http://www.jmp.com/support/notes/35/406.html
#   return( stats::shapiro.test(na.omit(x))$p.value > alpha )
# }
#
# print_qoutcomes_results_ANOVA = function( section_string = "X",
#                                           plot_number    = 0,
#                                           description    = "<EMPTY>",
#                                           comparison     = "<EMPTY>",
#                                           summary_table  = NULL,
#                                           normality_plot = NULL,
#                                           base_plot      = NULL,
#                                           site_plot      = NULL,
#                                           waterfall_plot = NULL,
#                                           LEVENES_table  = NULL,
#                                           ANOVA_results  = NULL,
#                                           BONFERRONI_posthoc = NULL,
#                                           DUNNETTS_posthoc   = NULL
#
#
# ) {
#
#   cat(sprintf("
# ## Figure %s.%ia: %s Shapiro-Wilks normality test (%s)
#
# The Shapiro-Wilks test has been imposed on these data to identify
# which data subsets are normal and therefore can be analysed with
# parametric methodologies.  Any data shown below in red violates
# the assumption of normality (using a threshold of 0.05 on the
# Shapiro-Wilks test).
#
# In the boxplot below, the median value is indicated by a thick
# horizontal line, while the hinge points (the top and bottom
# boundaries of the box) are defined by the 25th and 75th percentile.
# The whisker indicates 1.5 x IQR (Inter Quartile Range).
#
# ",
#               section_string,
#               plot_number,
#               description,
#               comparison
#   ))
#
#   print( normality_plot + ggtitle("") )
#
#   cat("
#
# ")
#
#   variance_string = "equal"
#   posthoc_method = "Bonferroni"
#   variance_pvalue = LEVENES_table %>% filter( df==2 ) %>% pull( p.value )
#   if ( variance_pvalue < 0.05 ) {
#     variance_string = "unequal"
#     posthoc_method = "Dunnett's C"
#   }
#
#   cat( sprintf("
# Levene's test (which is robust to non-normal data) indicates that these
# data have *%s* variance (p values is %.2f).  So, the preferred post hoc test
# is *%s*.
#
# ",
#                variance_string,
#                variance_pvalue,
#                posthoc_method
#                ))
#
#   cat(sprintf("
# ## Figure %s.%ib: %s %s difference
#
# The same data as in the previous plot, but coloured by arm.
# ",
#               section_string,
#               plot_number,
#               description,
#               comparison ))
#
#   print( base_plot + ggtitle("") )
#
#   cat("
#
#       ")
#
#
#   cat(sprintf("
# ## Table %s.%i: %s %s difference summary
#
# ",
#               section_string,
#               plot_number+2,
#               description,
#               comparison ))
#
#   print( kable( summary_table ) )
#
#   cat("
#
#       ")
#
#
#
#   cat(sprintf("
# ## Figure %s.%ic: %s %s difference (by site)
#
# These are the same data as in the previous plot, but separated by site.
#
# ",
#               section_string,
#               plot_number,
#               description,
#               comparison))
#
#   print( site_plot + ggtitle( "" ) )
#
#   cat("
#
#       ")
#
#
#   cat(sprintf("
# ## Figure %s.%id: %s %s difference
#
# A waterfall plot of the same data.
# ",
#               section_string,
#               plot_number,
#               description,
#               comparison ))
#
#   print( waterfall_plot + ggtitle("") )
#
#   cat("
#
#       ")
#
#
#   cat(sprintf("
# ## Summary: %s %s results (ANOVA+posthoc)
#
# ",
#               description,
#               comparison))
#
#   cat( sprintf( "
# ANOVA results: F(%d)=%.2f; p=%.2f
#
# ",
#                 as.list(ANOVA_results[1,])$df,
#                 as.list(ANOVA_results[1,])$statistic,
#                 as.list(ANOVA_results[1,])$p.value
#                 ) )
#
#
#
#   this.Bonferroni = BONFERRONI_posthoc %>% mutate(
#     string = sprintf( "BONFERRONI: %s v %s; p=%.2f [%.2f/%.2f]",
#                       group1,
#                       group2,
#                       p.value,
#                       conf.low,
#                       conf.high ) )
#
#   cat( sprintf( "
# %s
# ", this.Bonferroni %>% pull(string) ) )
#
#   this.Dunnett = DUNNETTS_posthoc %>% mutate(
#     string = sprintf( "DUNNETT'S C: %s v %s; p=%.2f [%.2f/%.2f]",
#                       group1,
#                       group2,
#                       p.value,
#                       conf.low,
#                       conf.high ) )
#   cat( sprintf( "
# %s
# ", this.Dunnett %>% pull(string) ) )
#
# }
#
#
