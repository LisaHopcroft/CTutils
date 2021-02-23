

#
# extract_axitinib_doses = function( this.data ) {
#
#   drug_profile.d = this.data %>%
#     ### Selecting only those columns that we need
#     select( Label,
#             Miscellaneous_Trial_treatment_TT_Drug_Restart,
#             Miscellaneous_Trial_treatment_TT_Drug_Stp_Cha,
#             Miscellaneous_Trial_treatment_TT_Drug_Dose,
#             Miscellaneous_Trial_treatment_TT_Dose_Mod ) %>%
#     ### Removing rows that don't have any dose information
#     filter( !is.na(Miscellaneous_Trial_treatment_TT_Drug_Dose) ) %>%
#     ### Renaming columns for readibility
#     rename( dose=Miscellaneous_Trial_treatment_TT_Drug_Dose,
#             mod =Miscellaneous_Trial_treatment_TT_Dose_Mod ) %>%
#     ### Using lubridate to format the dates
#     mutate( start_date = dmy(as.character(Miscellaneous_Trial_treatment_TT_Drug_Restart)),
#             stop_date  = dmy(Miscellaneous_Trial_treatment_TT_Drug_Stp_Cha)) %>%
#     ### Removing columns that we have formatted correctly in other variables
#     select( -Miscellaneous_Trial_treatment_TT_Drug_Restart,
#             -Miscellaneous_Trial_treatment_TT_Drug_Stp_Cha )
#
#   drug_profile.daily_data = drug_profile.d %>%
#     select( Label, dose, start_date, stop_date ) %>%
#     gather( date_type, date, -Label, -dose ) %>%
#     filter( date_type == "start_date" ) %>%
#     select( -date_type )
#
#   drug_profile.other = drug_profile.d %>%
#     select( Label, mod, dose, stop_date )
#
#   for ( i in 1:nrow( drug_profile.other ) ) {
#
#     new_dose = NA
#
#     if ( drug_profile.other$mod[i] == "Stopped" ) {
#       new_dose = 0
#     } else if ( drug_profile.other$mod[i] == "Escalation" ) {
#       new_dose = dose_modification(drug_profile.other$dose[i],"+")
#     } else if ( drug_profile.other$mod[i] == "Interruption" ) {
#       new_dose = -Inf
#     }
#
#     drug_profile.daily_data = drug_profile.daily_data %>%
#       add_row( Label = drug_profile.other$Label[[i]],
#                dose  = new_dose,
#                date  = drug_profile.other$stop_date[i]+1 )
#   }
#
#   drug_profile.daily_data = drug_profile.daily_data %>% arrange( Label, date ) %>%
#     group_by( Label ) %>%
#     mutate(date2 = as.Date(date)) %>%
#     complete(date2 = seq.Date(min(date2,na.rm=TRUE), max(date2,na.rm=TRUE), by="day")) %>%
#     mutate( day1  = min(date2, na.rm = TRUE),
#             dayX  = as.integer(date2-day1)+1,
#             WeekX = ( (dayX-1)%/%7)+1 ) %>%
#     fill( dose ) %>%
#     mutate(dose = na_if(dose, -Inf),
#            reduction = dose<TRIAL.starting_dose)
#
#
#
#
#   drug_profile.plot = ggplot( drug_profile.daily_data,
#                               aes( dayX,
#                                    dose,
#                                    colour=reduction )) +
#     annotate("rect",
#              xmin=seq(1,TRIAL.treatment_length_days,by=14),
#              xmax=seq(1,TRIAL.treatment_length_days,by=14)+7,
#              ymin=0,
#              ymax=Inf,
#              fill=brewer.pal(7,"Blues")[4],
#              colour=NA,
#              alpha=0.5) +
#     geom_point() + geom_step() +
#     facet_grid(Label~.) +
#     ylim( 0, max(TRIAL.dose_scale) ) +
#     scale_x_continuous(breaks = seq(1,
#                                     TRIAL.treatment_length_days,
#                                     by=7)) +
#     scale_colour_manual(values=c("TRUE"="red",
#                                  "FALSE"="black")) +
#     theme_NAXIVA
#
#   return( list( data=drug_profile.daily_data %>% distinct(),
#                 plot=drug_profile.plot ) )
#
# }
#
#
# extract_missing_doses = function( this.data ) {
#
#   missing_profile.d = this.data %>%
#
#     ### Selecting only those columns that we need
#     select( Label,
#             Week_3_Treat_Compli_Missed_Dose_Num,
#             Week_5_Treat_Compli_Missed_Dose_Num,
#             Week_7_Treat_Compli_Missed_Dose_Num,
#             Week_9_Treat_Compl_Wk9_Missed_Dose_Num ) %>%
#     gather( Week, missed_doses, -Label ) %>%
#     mutate( WeekX = as.integer( str_replace( str_replace( Week, "_Treat.*", "" ),
#                                              "^Week_", "" ) ) ) %>%
#     select( -Week ) %>%
#     filter( !is.na( missed_doses ) )
#
#
#   return( missing_profile.d )
#
# }
#
# perform_axitinib_test_battery = function( this.data,
#                                           this.visit,
#                                           this.testbattery,
#                                           axitinib_YN.var_name = "Axitin_Y_N",
#                                           axitinib_Mod.var_name = "Axitin_Dose_Mod",
#                                           axitinib_missedYN.var_name = "Missed_Dose",
#                                           axitinib_returned.var_name = "Drug_Return_Y_N",
#                                           axitinib_missedNum.var_name = "Missed_Dose_Num",
#                                           axitinib_missedMax.var_name = "Missed_Dose_Max"
# ) {
#   ### Table 1: Is the patient still taking axitinib
#   axitinib_YN.variable =  sprintf( "%s_%s_%s",
#                                    this.visit,
#                                    this.testbattery,
#                                    axitinib_YN.var_name )
#   T1.out = do_count(this.data, sym(axitinib_YN.variable))
#   print_table( T1.out )
#
#   ### Table 2: Reason not taking axitinib
#   # axitinib_reason.variable =  sprintf( "Miscellaneous_Trial_treatment_TT_Drug_Dose" )
#   # T2.out = do_count(this.data, quo(Miscellaneous_Trial_treatment_TT_Drug_Dose))#sym(axitinib_reason.variable))
#   # print_table( T2.out )
#
#   ### Table 3: Dose modifications since last assessment
#   axitinib_Mod.variable =  sprintf( "%s_%s_%s",
#                                     this.visit,
#                                     this.testbattery,
#                                     axitinib_Mod.var_name)
#
#   if ( any( colnames(this.data) == axitinib_Mod.variable ) ) {
#     T3.out = do_count(this.data, sym(axitinib_Mod.variable))
#     print_table( T3.out )
#   }
#
#   ### Table 4: Reason dose modification
#
#   # Table 5a: Missed any dose since the last assessment
#   axitinib_missed.variable = sprintf( "%s_%s_%s",
#                                       this.visit,
#                                       this.testbattery,
#                                       axitinib_missedYN.var_name )
#   T5a.out = do_count(this.data, sym(axitinib_missed.variable))
#   # print_table( T5a.out )
#
#   # Table 5b: drug returns
#   axitinib_returned.variable = sprintf( "%s_%s_%s",
#                                         this.visit,
#                                         this.testbattery,
#                                         axitinib_returned.var_name)
#   T5b.out = do_count(this.data, sym(axitinib_returned.variable))
#   # print_table( T5b.out )
#
#   # Table 5c: cross tabulation
#   T5c.out = this.data %>%
#     group_by( !!sym(axitinib_missed.variable),
#               !!sym(axitinib_returned.variable) ) %>%
#     summarise( count=n() ) %>%
#     ungroup() %>%
#     mutate( Missed_Dose = str_replace( !!sym(axitinib_missed.variable), "^$", "Missing") ) %>%
#     mutate( Drug_Return_Y_N = str_replace( !!sym(axitinib_returned.variable), "^$", "Missing") ) %>%
#     select( -starts_with(this.visit)) %>%
#     spread( Missed_Dose, count ) %>%
#     mutate( Drug_Return_Y_N = factor(Drug_Return_Y_N,
#                                      levels=c("Yes", "No", "Missing" )) ) %>%
#     arrange( Drug_Return_Y_N ) %>% select( Drug_Return_Y_N,
#                                            Yes,
#                                            No,
#                                            Missing )
#
#   print_table( T5c.out )
#
#   # Table 6: number fo doses missed and max number of consecutive doses missed
#   axitinib_num_missed.variable = sprintf( "%s_%s_%s",
#                                           this.visit,
#                                           this.testbattery,
#                                           axitinib_missedNum.var_name)
#   axitinib_max_consecutive_missed.variable =  sprintf( "%s_%s_%s",
#                                                        this.visit,
#                                                        this.testbattery,
#                                                        axitinib_missedMax.var_name)
#
#   axitinib_num_missed.summary = do_summary( this.data %>%
#                                               filter(!!sym(axitinib_missed.variable)=="Yes"),
#                                             syms(c(axitinib_num_missed.variable,
#                                                    axitinib_max_consecutive_missed.variable) ) )
#
#   print_table( axitinib_num_missed.summary$table )
#
# }
#
#
#
#
# perform_axitinib_drug_returned = function( this.data,
#                                            this.visit,
#                                            this.testbattery,
#                                            axitinib_YN.var_name = "Axitin_Y_N",
#                                            axitinib_Mod.var_name = "Axitin_Dose_Mod",
#                                            axitinib_missedYN.var_name = "Missed_Dose",
#                                            axitinib_returned.var_name = "Drug_Return_Y_N",
#                                            axitinib_missedNum.var_name = "Missed_Dose_Num",
#                                            axitinib_missedMax.var_name = "Missed_Dose_Max"
# ) {
#
#   # # Table 5a: Missed any dose since the last assessment
#   axitinib_missed.variable = sprintf( "%s_%s_%s",
#                                       this.visit,
#                                       this.testbattery,
#                                       axitinib_missedYN.var_name )
#   # T5a.out = do_count(this.data, sym(axitinib_missed.variable))
#   # # print_table( T5a.out )
#   #
#   # # Table 5b: drug returns
#   axitinib_returned.variable = sprintf( "%s_%s_%s",
#                                         this.visit,
#                                         this.testbattery,
#                                         axitinib_returned.var_name)
#   # T5b.out = do_count(this.data, sym(axitinib_returned.variable))
#   # # print_table( T5b.out )
#
#   # Table 5c: cross tabulation
#   T5c.out = this.data %>%
#     group_by( !!sym(axitinib_missed.variable),
#               !!sym(axitinib_returned.variable) ) %>%
#     summarise( count=n() ) %>%
#     ungroup() %>%
#     mutate( Missed_Dose = str_replace( !!sym(axitinib_missed.variable), "^$", "Missing") ) %>%
#     mutate( Drug_Return_Y_N = str_replace( !!sym(axitinib_returned.variable), "^$", "Missing") ) %>%
#     select( -starts_with(this.visit)) %>%
#     spread( Missed_Dose, count ) %>%
#     mutate( Drug_Return_Y_N = factor(Drug_Return_Y_N,
#                                      levels=c("Yes", "No", "Missing" )) ) %>%
#     arrange( Drug_Return_Y_N ) %>% select( Drug_Return_Y_N,
#                                            Yes,
#                                            No,
#                                            Missing )
#
#   return( T5c.out )
# }
#
#
# perform_axitinib_missed_doses = function( this.data,
#                                           this.visit,
#                                           this.testbattery,
#                                           axitinib_YN.var_name = "Axitin_Y_N",
#                                           axitinib_Mod.var_name = "Axitin_Dose_Mod",
#                                           axitinib_missedYN.var_name = "Missed_Dose",
#                                           axitinib_returned.var_name = "Drug_Return_Y_N",
#                                           axitinib_missedNum.var_name = "Missed_Dose_Num",
#                                           axitinib_missedMax.var_name = "Missed_Dose_Max"
# ) {
#
#
#
#   axitinib_missed.variable = sprintf( "%s_%s_%s",
#                                       this.visit,
#                                       this.testbattery,
#                                       axitinib_missedYN.var_name )
#
#   axitinib_returned.variable = sprintf( "%s_%s_%s",
#                                         this.visit,
#                                         this.testbattery,
#                                         axitinib_returned.var_name)
#
#   # Table 6: number fo doses missed and max number of consecutive doses missed
#   axitinib_num_missed.variable = sprintf( "%s_%s_%s",
#                                           this.visit,
#                                           this.testbattery,
#                                           axitinib_missedNum.var_name)
#   axitinib_max_consecutive_missed.variable =  sprintf( "%s_%s_%s",
#                                                        this.visit,
#                                                        this.testbattery,
#                                                        axitinib_missedMax.var_name)
#
#   axitinib_num_missed.summary = do_summary( this.data %>%
#                                               filter(!!sym(axitinib_missed.variable)=="Yes"),
#                                             syms(c(axitinib_num_missed.variable,
#                                                    axitinib_max_consecutive_missed.variable) ) )
#
#   return( axitinib_num_missed.summary$table )
#
# }
#
#
#
# perform_axitinib_dose_modifications = function( this.data,
#                                                 this.visit,
#                                                 this.testbattery,
#                                                 axitinib_YN.var_name = "Axitin_Y_N",
#                                                 axitinib_Mod.var_name = "Axitin_Dose_Mod",
#                                                 axitinib_missedYN.var_name = "Missed_Dose",
#                                                 axitinib_returned.var_name = "Drug_Return_Y_N",
#                                                 axitinib_missedNum.var_name = "Missed_Dose_Num",
#                                                 axitinib_missedMax.var_name = "Missed_Dose_Max"
# ) {
#   ### Table 1: Is the patient still taking axitinib
#   axitinib_YN.variable =  sprintf( "%s_%s_%s",
#                                    this.visit,
#                                    this.testbattery,
#                                    axitinib_YN.var_name )
#   T1.out = do_count(this.data, sym(axitinib_YN.variable))
#
#
#   ### Table 3: Dose modifications since last assessment
#   axitinib_Mod.variable =  sprintf( "%s_%s_%s",
#                                     this.visit,
#                                     this.testbattery,
#                                     axitinib_Mod.var_name)
#
#   if ( any( colnames(this.data) == axitinib_Mod.variable ) ) {
#     T3.out = do_count(this.data, sym(axitinib_Mod.variable))
#     return( T3.out )
#   }
#
#
# }
#
#
#
#
# perform_axitinib_taking_drug = function( this.data,
#                                          this.visit,
#                                          this.testbattery,
#                                          axitinib_YN.var_name = "Axitin_Y_N",
#                                          axitinib_Mod.var_name = "Axitin_Dose_Mod",
#                                          axitinib_missedYN.var_name = "Missed_Dose",
#                                          axitinib_returned.var_name = "Drug_Return_Y_N",
#                                          axitinib_missedNum.var_name = "Missed_Dose_Num",
#                                          axitinib_missedMax.var_name = "Missed_Dose_Max"
# ) {
#   ### Table 1: Is the patient still taking axitinib
#   axitinib_YN.variable =  sprintf( "%s_%s_%s",
#                                    this.visit,
#                                    this.testbattery,
#                                    axitinib_YN.var_name )
#   T1.out = do_count(this.data, sym(axitinib_YN.variable))
#   return( T1.out )
#
# }
#
#
#
#
#
#
#
# perform_patient_safety_test_battery = function( this.data,
#                                                 this.visit,
#                                                 this.testbattery ) {
#
#
#   date.variable =  sprintf( "%s_%s_Sign_Sym_Date",
#                             this.visit,
#                             this.testbattery)
#
#   ### Table 1: currently experiencing signs and symptoms
#   T1.variable = sprintf( "%s_%s_Sign_Sym_Y_N",
#                          this.visit,
#                          this.testbattery)
#   T1.d = do_count( this.data, sym(T1.variable) )
#   knitr::kable( T1.d )
#
#   cat( sprintf( "\n\n\n" ) )
#
#
#
#   ### Table 2: currently experiencing a grade 4 or 5 toxicity
#   n.baseline_symptoms = T1.d %>% filter( variable==glossary.key[[T1.variable]] ) %>% pull( "Yes" )
#
#   T2.variable = sprintf( "%s_%s_Sign_Sym_G_4_5",
#                          this.visit,
#                          this.testbattery)
#   T2.d = do_count(this.data %>% filter(!!sym(T1.variable) == "Yes"),
#                   sym(T2.variable) )
#   knitr::kable( T2.d )
#
#   cat( sprintf( "\n\n\n" ) )
#
#   ### Table 3: Adverse Event Grades
#
#   n.G45_symptoms = T2.d %>% filter( variable==glossary.key[[T2.variable]] ) %>% pull( "Yes" )
#
#   T3.variable_string = sprintf( "%s_%s", this.visit, this.testbattery )
#   T3.variables = setdiff( grep( T3.variable_string, colnames( this.data ), value=T ),
#                           c( date.variable,
#                              T1.variable,
#                              T2.variable ) )
#   T3.variables = grep( "Sign_Sym_Pain", T3.variables, invert = TRUE, value=TRUE )
#   T3.variables = grep( "Sign_Sym_Oth" , T3.variables, invert = TRUE, value=TRUE )
#
#
#   T3.d = do_count( this.data %>% filter( !!sym(T1.variable)=="Yes" ),
#                    syms( T3.variables ),
#                    these_levels=c(seq(0,5),"Missing") )
#
#   other_flag.variable =  sprintf( "%s_%s_Sign_Sym_Oth",
#                                   this.visit,
#                                   this.testbattery)
#   pain_info.variable = sprintf( "%s_%s_Sign_Sym_PainSp",
#                                 this.visit,
#                                 this.testbattery)
#   pain_grade.variable = sprintf( "%s_%s_Sign_Sym_PainGr",
#                                  this.visit,
#                                  this.testbattery)
#   other_info.variable = sprintf( "%s_%s_Sign_Sym_OthSp",
#                                  this.visit,
#                                  this.testbattery)
#   other_grade.variable = sprintf( "%s_%s_Sign_Sym_OthGr",
#                                   this.visit,
#                                   this.testbattery)
#
#
#   T3.pain = this.data  %>%
#     filter( !!sym(T1.variable)=="Yes" &
#               !!sym(other_flag.variable)=="Yes" ) %>%
#     mutate( variable = sprintf( "Pain: %s", !!sym(pain_info.variable) ) ) %>%
#     rename( value={{pain_grade.variable}} ) %>%
#     mutate( value=replace_na(value,"Missing") ) %>%
#     group_by( variable, value ) %>%
#     summarise( count=n() )
#
#   T3.other = this.data  %>%
#     filter( !!sym(T1.variable)=="Yes" &
#               !!sym(other_flag.variable)=="Yes" ) %>%
#     mutate( variable = sprintf( "Other: %s", !!sym(other_info.variable) ) ) %>%
#     rename( value={{other_grade.variable}} ) %>%
#     mutate( value=replace_na(value,"Missing") ) %>%
#     group_by( variable, value ) %>%
#     summarise( count=n() )
#
#
#   T3.extra = rbind( T3.pain,
#                     T3.other ) %>%
#     mutate( value=factor(value,levels=c(seq(0,5),"Missing") ) ) %>%
#     spread( fill=0, value, count, drop=FALSE )
#
#   count.out = bind_rows( T3.d, T3.extra )
#
#   knitr::kable( count.out )
#
#
# }
#
#
#
# perform_patient_safety_signs_and_symptoms_count = function( this.data,
#                                                             this.visit,
#                                                             this.testbattery ) {
#
#   date.variable =  sprintf( "%s_%s_Sign_Sym_Date",
#                             this.visit,
#                             this.testbattery)
#
#   ### Table 1: currently experiencing signs and symptoms
#   T1.variable = sprintf( "%s_%s_Sign_Sym_Y_N",
#                          this.visit,
#                          this.testbattery)
#   T1.d = do_count( this.data, sym(T1.variable) )
#
#   return( T1.d )
#
# }
#
# perform_patient_safety_grade_45 = function( this.data,
#                                             this.visit,
#                                             this.testbattery ) {
#
#
#   date.variable =  sprintf( "%s_%s_Sign_Sym_Date",
#                             this.visit,
#                             this.testbattery)
#
#   ### Table 1: currently experiencing signs and symptoms
#   T1.variable = sprintf( "%s_%s_Sign_Sym_Y_N",
#                          this.visit,
#                          this.testbattery)
#   T1.d = do_count( this.data, sym(T1.variable) )
#
#
#   ### Table 2: currently experiencing a grade 4 or 5 toxicity
#   n.baseline_symptoms = T1.d %>% filter( variable==glossary.key[[T1.variable]] ) %>% pull( "Yes" )
#
#   T2.variable = sprintf( "%s_%s_Sign_Sym_G_4_5",
#                          this.visit,
#                          this.testbattery)
#   T2.d = do_count(this.data %>% filter(!!sym(T1.variable) == "Yes"),
#                   sym(T2.variable) )
#
#   return( T2.d )
#
# }
#
#
# perform_patient_safety_signs_and_symptoms_detail = function( this.data,
#                                                              this.visit,
#                                                              this.testbattery ) {
#
#
#   date.variable =  sprintf( "%s_%s_Sign_Sym_Date",
#                             this.visit,
#                             this.testbattery)
#
#   ### Table 1: currently experiencing signs and symptoms
#   T1.variable = sprintf( "%s_%s_Sign_Sym_Y_N",
#                          this.visit,
#                          this.testbattery)
#   T1.d = do_count( this.data, sym(T1.variable) )
#
#
#   ### Table 2: currently experiencing a grade 4 or 5 toxicity
#   # n.baseline_symptoms = T1.d %>% filter( variable==glossary.key[[T1.variable]] ) %>% pull( "Yes" )
#
#   T2.variable = sprintf( "%s_%s_Sign_Sym_G_4_5",
#                          this.visit,
#                          this.testbattery)
#   T2.d = do_count(this.data %>% filter(!!sym(T1.variable) == "Yes"),
#                   sym(T2.variable) )
#
#
#   ### Table 3: Adverse Event Grades
#
#   n.G45_symptoms = T2.d %>% filter( variable==glossary.key[[T2.variable]] ) %>% pull( "Yes" )
#
#   T3.variable_string = sprintf( "%s_%s", this.visit, this.testbattery )
#   T3.variables = setdiff( grep( T3.variable_string, colnames( this.data ), value=T ),
#                           c( date.variable,
#                              T1.variable,
#                              T2.variable ) )
#   T3.variables = grep( "Sign_Sym_Pain", T3.variables, invert = TRUE, value=TRUE )
#   T3.variables = grep( "Sign_Sym_Oth" , T3.variables, invert = TRUE, value=TRUE )
#   T3.variables = grep( "DOB$", T3.variables, invert = TRUE, value=TRUE )
#   T3.variables = grep( "Hospital$", T3.variables, invert = TRUE, value=TRUE )
#   T3.variables = grep( "Pat_init$", T3.variables, invert = TRUE, value=TRUE )
#   T3.variables = grep( "Trial_ID$", T3.variables, invert = TRUE, value=TRUE )
#   T3.variables = grep( "Date_Form_Comp$", T3.variables, invert = TRUE, value=TRUE )
#
#   T3.d = do_count( this.data %>% filter( !!sym(T1.variable)=="Yes" ),
#                    syms( T3.variables ),
#                    these_levels=c(seq(0,5)) )
#   colnames(T3.d) = colnames(T3.d) %>% str_replace( "<NA>", "Missing")
#
#   other_flag.variable =  sprintf( "%s_%s_Sign_Sym_Oth",
#                                   this.visit,
#                                   this.testbattery)
#   pain_info.variable = sprintf( "%s_%s_Sign_Sym_PainSp",
#                                 this.visit,
#                                 this.testbattery)
#   pain_grade.variable = sprintf( "%s_%s_Sign_Sym_PainGr",
#                                  this.visit,
#                                  this.testbattery)
#   other_info.variable = sprintf( "%s_%s_Sign_Sym_OthSp",
#                                  this.visit,
#                                  this.testbattery)
#   other_grade.variable = sprintf( "%s_%s_Sign_Sym_OthGr",
#                                   this.visit,
#                                   this.testbattery)
#
#
#   T3.pain = this.data  %>%
#     filter( !!sym(T1.variable)=="Yes" &
#               !!sym(other_flag.variable)=="Yes" ) %>%
#     mutate( variable = sprintf( "Pain: %s", !!sym(pain_info.variable) ) ) %>%
#     rename( value={{pain_grade.variable}} ) %>%
#     mutate( value=replace_na(value,"Missing") ) %>%
#     group_by( variable, value ) %>%
#     summarise( count=n() )
#
#   T3.other = this.data  %>%
#     filter( !!sym(T1.variable)=="Yes" &
#               !!sym(other_flag.variable)=="Yes" ) %>%
#     mutate( variable = sprintf( "Other: %s", !!sym(other_info.variable) ) ) %>%
#     rename( value={{other_grade.variable}} ) %>%
#     mutate( value=replace_na(value,"Missing") ) %>%
#     group_by( variable, value ) %>%
#     summarise( count=n() )
#
#
#   T3.extra = rbind( T3.pain,
#                     T3.other ) %>%
#     mutate( value=factor(value,levels=c(seq(0,5),"Missing") ) ) %>%
#     spread( fill=0, value, count, drop=FALSE )
#
#   count.out = bind_rows( T3.d, T3.extra )
#
#   return( count.out )
#
#
# }
#
#
#
#
#
# perform_patient_safety_signs_and_symptoms_detail2 = function( this.data,
#                                                               this.visit,
#                                                               this.testbattery ) {
#
#
#   date.variable =  sprintf( "%s_%s_Sign_Sym_Date",
#                             this.visit,
#                             this.testbattery)
#
#   ### Table 1: currently experiencing signs and symptoms
#   T1.variable = sprintf( "%s_%s_Sign_Sym_Y_N",
#                          this.visit,
#                          this.testbattery)
#   T1.d = do_count( this.data, sym(T1.variable) )
#
#
#   ### Table 2: currently experiencing a grade 4 or 5 toxicity
#   # n.baseline_symptoms = T1.d %>% filter( variable==glossary.key[[T1.variable]] ) %>% pull( "Yes" )
#
#   T2.variable = sprintf( "%s_%s_Sign_Sym_G_4_5",
#                          this.visit,
#                          this.testbattery)
#   T2.d = do_count(this.data %>% filter(!!sym(T1.variable) == "Yes"),
#                   sym(T2.variable) )
#
#
#   ### Table 3: Adverse Event Grades
#
#   n.G45_symptoms = T2.d %>% filter( variable==glossary.key[[T2.variable]] ) %>% pull( "Yes" )
#
#   T3.variable_string = sprintf( "%s_%s", this.visit, this.testbattery )
#   T3.variables = setdiff( grep( T3.variable_string, colnames( this.data ), value=T ),
#                           c( date.variable,
#                              T1.variable,
#                              T2.variable ) )
#   T3.variables = grep( "Sign_Sym_Pain", T3.variables, invert = TRUE, value=TRUE )
#   T3.variables = grep( "Sign_Sym_Oth" , T3.variables, invert = TRUE, value=TRUE )
#   T3.variables = grep( "DOB$", T3.variables, invert = TRUE, value=TRUE )
#   T3.variables = grep( "Hospital$", T3.variables, invert = TRUE, value=TRUE )
#   T3.variables = grep( "Pat_init$", T3.variables, invert = TRUE, value=TRUE )
#   T3.variables = grep( "Trial_ID$", T3.variables, invert = TRUE, value=TRUE )
#   T3.variables = grep( "Date_Form_Comp$", T3.variables, invert = TRUE, value=TRUE )
#
#   T3.d = do_count( this.data %>% filter( !!sym(T1.variable)=="Yes" ),
#                    syms( T3.variables ),
#                    these_levels=c(seq(0,5)) )
#   colnames(T3.d) = colnames(T3.d) %>% str_replace( "<NA>", "Missing")
#
#   other_flag.variable =  sprintf( "%s_%s_Sign_Sym_Oth",
#                                   this.visit,
#                                   this.testbattery)
#   pain_info.variable = sprintf( "%s_%s_Sign_Sym_PainSp",
#                                 this.visit,
#                                 this.testbattery)
#   pain_grade.variable = sprintf( "%s_%s_Sign_Sym_PainGr",
#                                  this.visit,
#                                  this.testbattery)
#   other_info.variable = sprintf( "%s_%s_Sign_Sym_OthSp",
#                                  this.visit,
#                                  this.testbattery)
#   other_grade.variable = sprintf( "%s_%s_Sign_Sym_OthGr",
#                                   this.visit,
#                                   this.testbattery)
#
#   T3.pain.tmp = this.data  %>%
#     filter( !!sym(T1.variable)=="Yes" &
#               !!sym(other_flag.variable)=="Yes" ) %>%
#     mutate( variable = sprintf( "Pain: %s", !!sym(pain_info.variable) ) ) %>%
#     rename( value={{pain_grade.variable}} ) %>%
#     mutate( value=replace_na(value,"Missing") ) %>%
#     group_by( variable, value ) %>%
#     summarise( count=n() )
#
#   T3.pain = T3.pain.tmp %>%
#     filter( variable != "Pain: ")
#
#   n.pain = T3.pain.tmp %>%
#     filter( variable == "Pain: ") %>% pull( count )
#
#   T3.other = this.data  %>%
#     filter( !!sym(T1.variable)=="Yes" &
#               !!sym(other_flag.variable)=="Yes" ) %>%
#     mutate( variable = sprintf( "Other: %s", !!sym(other_info.variable) ) ) %>%
#     rename( value={{other_grade.variable}} ) %>%
#     mutate( value=replace_na(value,"Missing") ) %>%
#     group_by( variable, value ) %>%
#     summarise( count=n() )
#
#   # count.out.2 = symptomsDetail.other %>%
#   #   mutate( value=factor(value,levels=c(seq(0,5),"Missing") ) ) %>%
#   #   spread( fill=0, value, count, drop=FALSE )
#   #
#   # count.out = bind_rows( count.out.1, count.out.2 )
#   # count.out = count.out %>%
#   #   mutate_if( is.double, as.integer )
#   #
#   #
#   T3.extra = T3.other %>%
#     mutate( value=factor(value,levels=c(seq(0,5),"Missing") ) ) %>%
#     spread( fill=0, value, count, drop=FALSE )
#
#   main.table = bind_rows( T3.d, T3.extra )
#   pain.table = NULL
#
#   if ( n.pain > 0 ) {
#     pain.table = T3.pain %>%
#       mutate( value=factor(value,levels=c(seq(0,5),"Missing") ) ) %>%
#       spread( fill=0, value, count, drop=FALSE ) %>%
#       mutate_if( is.double, as.integer )
#
#   }
#
#
#   return( list(main=main.table,
#                pain=pain.table) )
#
#
# }
#
#
#
