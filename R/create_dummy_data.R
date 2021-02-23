#' Create dummy CT data
#'
#' @param number_of_patients The number of patients in the dataset (default=100)
#' @param perc_missing_data What percentage of data should be missing (default=0.05)
#' @param use_seed A seed to use (default=NA, i.e., no specified seed)
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr ends_with
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr mutate_if
#' @importFrom dplyr pull
#' @importFrom dplyr vars
#' @importFrom dplyr %>%
#' @importFrom lubridate as_date
#' @importFrom lubridate is.Date
#' @importFrom lubridate today
#' @importFrom lubridate years
#' @importFrom stats rnorm
#' @importFrom stats rpois
#' @importFrom tibble tibble
#' @importFrom tidyr replace_na
#' @importFrom utils as.roman
#'
#' @return Data objects for dummy dataset as a list: (1) the data, (2) the vocabulary, (3) the glossary.
#' The vocabulary tells you what values each column can take.
#' The glossary gives you a display string for each column.
#'
#' @export
create_dummy_dataset = function(number_of_patients=100,
                                perc_missing_data=0.05,
                                use_seed=NA) {


  trial_arms.ALL = c( "Control", "Drug1", "Drug2" )
  disease_characteristic.ALL = sprintf( "M%d", 0:2 )
  hospital_names.ALL = sprintf( "Hospital %s", c( "P", "Q", "R", "X", "Y", "Z" ) )
  YN_categories.ALL = c( "Yes", "No" )
  surgery_type.ALL  = sprintf( "Surgery type %s" , LETTERS[1:5] )
  chemotherapy_regimen.ALL = sprintf( "Chemo regimen %s", as.roman(1:5) )
  tumour_locations.ALL = c(
    "right renal mass",
    "left renal mass",
    "pretracheal lymphnode",
    "right adrenal nodule",
    "satellite nodule",
    "right renal lesion",
    "left renal lesion",
    "right upper pole renal mass",
    "rt retrocaval node",
    "inter-aorto caval mass",
    "left anterior solid renal mass"
  )

  trial_arms.options = trial_arms.ALL
  disease_characteristic.options = disease_characteristic.ALL[1:2]
  hospital_names.options = hospital_names.ALL[2:length(hospital_names.ALL)]
  YN_categories.options = YN_categories.ALL
  surgery_type.options  = surgery_type.ALL[2:4]
  chemotherapy_regimen.options = chemotherapy_regimen.ALL[3:5]
  tumour_locations.options = tumour_locations.ALL

  ### Some of these vocabularies will not
  dummy.vocabulary = list(
    arm                     = trial_arms.ALL,
    hospital                = hospital_names.ALL,
    has_had_surgery         = YN_categories.ALL,
    has_had_radiotherapy    = YN_categories.ALL,
    has_had_chemotherapy    = YN_categories.ALL,
    has_had_immunotherapy   = YN_categories.ALL,
    disease_characteristic  = disease_characteristic.ALL,
    surgery_details_planned = surgery_type.ALL,
    surgery_details_actual  = surgery_type.ALL,
    chemotherapy_details    = chemotherapy_regimen.ALL
  )

  dummy.glossary = list(
    arm                     = "Treatment arm",
    hospital                = "Hospital",
    recruitment_date        = "Date of recruitment",
    has_had_surgery         = "Has the patient had surgery (Y/N)",
    has_had_radiotherapy    = "Has the patient had radiotherapy (Y/N)",
    has_had_chemotherapy    = "Has the patient had chemotherapy (Y/N)",
    has_had_immunotherapy   = "Has the patient had immunotherapy (Y/N)",
    T0_toxicity_YN          = "Toxicity present (Y/N)",
    T0_toxicityG45_YN       = "Grade 4/5 toxicity present (Y/N)",
    T1_toxicity_YN          = "Toxicity present (Y/N)",
    T1_toxicityG45_YN       = "Grade 4/5 toxicity present (Y/N)",
    T2_toxicity_YN           = "Toxicity present (Y/N)",
    T2_toxicityG45_YN       = "Grade 4/5 toxicity present (Y/N)",
    disease_characteristic  = "Disease characteristic",
    surgery_details_planned = "Planned surgery",
    surgery_details_actual  = "Actual surgery",
    surgery_date            = "Date of surgery",
    chemotherapy_details    = "Chemotherapy treatment",
    chemotherapy_date       = "Date of chemotherapy",
    FU_1mo                  = "Date of follow-up appointment (1mo)",
    FU_3mo                  = "Date of follow-up appointment (3mo)",
    FU_6mo                  = "Date of follow-up appointment (6mo)",
    FU_12mo                 = "Date of follow-up appointment (12mo)",
    age                     = "Age of patient",
    height                  = "Height of patient (cm)",
    weight                  = "Weight of patient (kg)",
    BMI                     = "BMI of patient (calculated)"
  )



  ###################################################################
  ### PHASE ONE: BASIC DATA
  ### This is where one value per patient is recorded and will not
  ### run over multiple lines (e.g., the arm in which the patient is
  ### enrolled, their surgery details etc).
  ###################################################################

  if ( !is.na(use_seed) ) {
    set.seed( use_seed )
  }

  dummy.d = tibble(
    ### Patient identifier
    Label = sprintf( "N%03d", 1:number_of_patients ),

    ### Entry number
    ### ==> This increases when there are multiple lines for a patient
    entry_number = 1,

    ### Which arm each patient belongs to
    arm = sample( trial_arms.options,
                  number_of_patients,
                  replace=TRUE),

    ### Which hospital is the patient from?
    hospital = sample( hospital_names.options,
                       number_of_patients,
                       replace=TRUE ),

    ### Categorical variables (Y/N)
    has_had_surgery      = sample( YN_categories.options,
                                   number_of_patients,
                                   replace = TRUE ),
    has_had_radiotherapy = sample( YN_categories.options,
                                   number_of_patients,
                                   replace = TRUE ),
    has_had_chemotherapy = sample( YN_categories.options,
                                   number_of_patients,
                                   replace = TRUE ),
    has_had_immunotherapy = sample( YN_categories.options,
                                    number_of_patients,
                                    replace = TRUE ),

    ### Categorical variables (not Y/N)
    disease_characteristic = sample( disease_characteristic.options,
                                     number_of_patients,
                                     replace=TRUE ),
    surgery_details_planned = sample( surgery_type.options,
                              number_of_patients,
                              replace = TRUE ),
    surgery_details_actual = sample( surgery_type.options,
                                     number_of_patients,
                                     replace = TRUE ),
    chemotherapy_details = sample( chemotherapy_regimen.options,
                            number_of_patients,
                            replace = TRUE ),

    ### Toxicities
    T0_toxicity_YN = sample( YN_categories.options,
                             number_of_patients,
                             replace = TRUE ),
    T0_toxicityG45_YN = sample( YN_categories.options,
                             number_of_patients,
                             replace = TRUE ),

    T1_toxicity_YN = sample( YN_categories.options,
                             number_of_patients,
                             replace = TRUE ),
    T1_toxicityG45_YN = sample( YN_categories.options,
                             number_of_patients,
                             replace = TRUE ),

    T2_toxicity_YN = sample( YN_categories.options,
                             number_of_patients,
                             replace = TRUE ),
    T2_toxicityG45_YN = sample( YN_categories.options,
                             number_of_patients,
                             replace = TRUE ),

    ### Demographics
    age    = round( rnorm( number_of_patients, mean=60, sd=10), 0 ),
    weight = round( rnorm( number_of_patients, mean=70, sd=15), 2 ),
    height = round( rnorm( number_of_patients, mean=170, sd=15), 2 ),
    BMI    = calculate_BMI( .data$weight, .data$height ),

    ### Measurements
    T0_measurement = rnorm( number_of_patients, mean=100, sd=15),
    T1_measurement = rnorm( number_of_patients, mean=100, sd=15),
    T2_measurement = rnorm( number_of_patients, mean=100, sd=15)
  )


  ### Add a certain percentage of missing data to some columns
  n_missing = as.integer( number_of_patients * perc_missing_data )

  dummy.d$has_had_radiotherapy [ sample(number_of_patients, n_missing) ] = ""
  dummy.d$has_had_immunotherapy[ sample(number_of_patients, n_missing) ] = ""

  dummy.d$weight[ sample(number_of_patients, n_missing) ] = NA
  dummy.d$height[ sample(number_of_patients, n_missing) ] = NA

  ### Remove surgery details for patients who didn't have surgery
  dummy.d$has_had_radiotherapy [ sample(number_of_patients, n_missing) ] = ""
  ### Remove chemotherapy details for patients who didn't have chemotherapy
  dummy.d$chemotherapy_details [ dummy.d$has_had_chemotherapy!="Yes" ] = ""

  ### Add dates
  ### Chemotherapy happens approx 2 weeks after surgery
  dummy.d = dummy.d %>%
    mutate( surgery_date = get_random_dates( number_of_patients,
                                             from = as.Date( today()-years(2) ),
                                             to   = as.Date( today()-months(6) ) ) ) %>%
    mutate( recruitment_date  = .data$surgery_date - (1*30) + runif(number_of_patients, -14, 14 ) ) %>%
    mutate( chemotherapy_date = .data$surgery_date + 14 + runif(number_of_patients, -2, 4 ) ) %>%
    mutate( FU1mo_date  = .data$surgery_date +  (1*30) + runif(number_of_patients, -2, 4 ) ) %>%
    mutate( FU3mo_date  = .data$surgery_date +  (3*30) + runif(number_of_patients, -2, 4 ) ) %>%
    mutate( FU6mo_date  = .data$surgery_date +  (6*30) + runif(number_of_patients, -2, 4 ) ) %>%
    mutate( FU12mo_date = .data$surgery_date + (12*30) + runif(number_of_patients, -2, 4 ) ) %>%
    mutate_if( is.Date, as.character )

  dummy.d$surgery_date     [ dummy.d$has_had_surgery     !="Yes" ] = ""
  dummy.d$chemotherapy_date[ dummy.d$has_had_chemotherapy!="Yes" ] = ""

  dummy.d$FU1mo_date [ sample(number_of_patients, n_missing) ] = ""
  dummy.d$FU3mo_date [ sample(number_of_patients, n_missing) ] = ""
  dummy.d$FU6mo_date [ sample(number_of_patients, n_missing) ] = ""
  dummy.d$FU12mo_date[ sample(number_of_patients, n_missing) ] = ""

  dummy.d = dummy.d %>%
    mutate_at( vars(ends_with("_date")), as_date )

  ###################################################################
  ### PHASE TWO: EXTENDED DATA
  ### This is where data is added that will run over multiple lines
  ### for each patient (e.g., toxicity details or numbers of tumours)
  ###################################################################

  these.patientIDs = dummy.d %>% pull( .data$Label )

  num_tumours_T0 = tibble( Label = these.patientIDs,
                           number = rpois(number_of_patients,lambda=2)+2 )
  num_tumours_T1 = tibble( Label = these.patientIDs,
                           number = rpois(number_of_patients,lambda=1)+1 )
  num_tumours_T2 = tibble( Label = these.patientIDs,
                           number = rpois(number_of_patients,lambda=1) )

  n_tumour_sizes_to_sample = number_of_patients*number_of_patients
  tumour_size_T0 = ceiling( rnorm(n_tumour_sizes_to_sample, mean=25, sd=7) )

  tumour_size_T1 = ceiling( tumour_size_T0 - ( rnorm( n_tumour_sizes_to_sample, sd=5 )+2 ) )
  if ( any( tumour_size_T1<0 ) ) { tumour_size_T1 = tumour_size_T1 - min(tumour_size_T1) }

  tumour_size_T2 = ceiling( tumour_size_T1 - ( rnorm( n_tumour_sizes_to_sample, sd=10 ) + 5 ) )
  tumour_size_T2[ tumour_size_T2<0 ] = 0
  tumour_size_T2[ sample(n_tumour_sizes_to_sample, ceiling( 0.33*n_tumour_sizes_to_sample ) ) ] = 0


  ### Add dummy tumour size data at T0 - (1) locations
  dummy.d_tmp1a = expand_trial_data( d.0 = dummy.d,
                                    d.toexpand = num_tumours_T0,
                                    column_name = "tumour_location_T0",
                                    option_list = tumour_locations.options )
  ### Add dummy tumour size data at T0 - (2) tumour size
  dummy.d_tmp1b = expand_trial_data( d.0 = dummy.d_tmp1a,
                                    d.toexpand = num_tumours_T0,
                                    column_name = "tumour_diam_T0",
                                    option_list = tumour_size_T0 )

  ### Add dummy tumour size data at T1 - (1) locations
  dummy.d_tmp2a = expand_trial_data( d.0 = dummy.d_tmp1b,
                                    d.toexpand = num_tumours_T1,
                                    column_name = "tumour_location_T1",
                                    option_list = tumour_locations.options )
  ### Add dummy tumour size data at T1 - (2) tumour size
  dummy.d_tmp2b = expand_trial_data( d.0 = dummy.d_tmp2a,
                                     d.toexpand = num_tumours_T1,
                                     column_name = "tumour_diam_T1",
                                     option_list = tumour_size_T1 )

  ### Add dummy tumour size data at T2 - (1) locations
  dummy.d_tmp3a = expand_trial_data( d.0 = dummy.d_tmp2b,
                                    d.toexpand = num_tumours_T2,
                                    column_name = "tumour_location_T2",
                                    option_list = tumour_locations.options )
  ### Add dummy tumour size data at T2 - (2) tumour size
  dummy.d_tmp3b = expand_trial_data( d.0 = dummy.d_tmp3a,
                                    d.toexpand = num_tumours_T2,
                                    column_name = "tumour_diam_T2",
                                    option_list = tumour_size_T2 )

  dummy.d_expanded = dummy.d_tmp3b %>%
    replace_na( list(
      tumour_location_T0 = "",
      tumour_location_T1 = "",
      tumour_location_T2 = ""
    )) %>%
    mutate( tumour_diam_T0 = as.numeric( .data$tumour_diam_T0 ),
            tumour_diam_T1 = as.numeric( .data$tumour_diam_T1 ),
            tumour_diam_T2 = as.numeric( .data$tumour_diam_T2 ) )

  dummy.d_expanded = dummy.d_expanded %>%
    group_by( .data$Label ) %>%
    mutate( tumour_sum_T0 = sum( .data$tumour_diam_T0, na.rm = TRUE ),
            tumour_sum_T1 = sum( .data$tumour_diam_T1, na.rm = TRUE ),
            tumour_sum_T2 = sum( .data$tumour_diam_T2, na.rm = TRUE )
    )

  dummy.d_expanded[ dummy.d_expanded$entry_number>1, "tumour_sum_T0" ] = NA
  dummy.d_expanded[ dummy.d_expanded$entry_number>1, "tumour_sum_T1" ] = NA
  dummy.d_expanded[ dummy.d_expanded$entry_number>1, "tumour_sum_T2" ] = NA


  dummy.d_expanded = dummy.d_expanded %>%
    select(
      .data$Label,
      .data$recruitment_date,
      .data$entry_number,
      .data$arm,
      .data$age,
      .data$weight,
      .data$height,
      .data$BMI,
      .data$has_had_surgery,
      .data$surgery_date,
      .data$has_had_radiotherapy,
      .data$has_had_chemotherapy,
      .data$chemotherapy_date,
      .data$has_had_immunotherapy,
      .data$disease_characteristic,
      .data$surgery_details_planned,
      .data$surgery_details_actual,
      .data$chemotherapy_details,
      .data$T0_toxicity_YN,
      .data$T0_toxicityG45_YN,
      .data$T1_toxicity_YN,
      .data$T1_toxicityG45_YN,
      .data$T2_toxicity_YN,
      .data$T2_toxicityG45_YN,
      .data$T0_measurement,
      .data$T1_measurement,
      .data$T2_measurement,
      .data$tumour_location_T0,
      .data$tumour_diam_T0,
      .data$tumour_sum_T0,
      .data$tumour_location_T1,
      .data$tumour_diam_T1,
      .data$tumour_sum_T1,
      .data$tumour_location_T2,
      .data$tumour_diam_T2,
      .data$tumour_sum_T2,
      .data$FU1mo_date,
      .data$FU3mo_date,
      .data$FU6mo_date,
      .data$FU12mo_date
    )

  dummy.d = dummy.d_expanded %>%
    filter( .data$entry_number == 1 )

  dummy.glossary[["tumour_location_T0"]] = "Location of tumour"
  dummy.glossary[["tumour_location_T1"]] = "Location of tumour"
  dummy.glossary[["tumour_location_T2"]] = "Location of tumour"

  dummy.glossary[["tumour_diam_T0"]] = "Diameter of individual tumour"
  dummy.glossary[["tumour_diam_T1"]] = "Diameter of individual tumour"
  dummy.glossary[["tumour_diam_T2"]] = "Diameter of individual tumour"

  dummy.glossary[["tumour_sum_T0"]] = "Summed diameter of all tumours"
  dummy.glossary[["tumour_sum_T1"]] = "Summed diameter of all tumours"
  dummy.glossary[["tumour_sum_T2"]] = "Summed diameter of all tumours"

  dummy.dictionary.tmp = tibble(
    param_desc = as.character( dummy.glossary ),
    param_name = as.character( names( dummy.glossary ) ),
    column_name = as.character( names( dummy.glossary ) )
  )


  dummy.dictionary = dummy.dictionary.tmp %>%
    group_by( .data$column_name ) %>%
    mutate( param_closed_vocab = list( get_vocab( .data$column_name, dummy.vocabulary ) ) ) %>%
    mutate( param_visit = case_when(
      str_detect( column_name, "T0" ) ~ "T0",
      str_detect( column_name, "T1" ) ~ "T1",
      str_detect( column_name, "T2" ) ~ "T2",
      str_detect( column_name, "surgery_details_actual" ) ~ "Surgery",
      str_detect( column_name, "surgery_date"           ) ~ "Surgery",
      str_detect( column_name, "FU" ) ~ "Follow-up",
      TRUE ~ "Screening" ) ) %>%
    mutate( param_testbattery = case_when(
      str_detect( param_visit, "Screening" ) ~ "Medical History",
      str_detect( column_name, "tumour"    ) ~ "Tumour Assessment",
      str_detect( column_name, "toxicity"  ) ~ "Signs and Symptoms",
      str_detect( column_name, "Surgery"   ) ~ "Surgery",
      str_detect( column_name, "FU"        ) ~ "Follow-up" ,
      TRUE ~ "Other" ) )

  return( list( d          = dummy.d,
                d.expanded = dummy.d_expanded,
                vocab      = dummy.vocabulary,
                glossary   = dummy.glossary,
                dictionary = dummy.dictionary ) )

}


get_vocab = function( p, v ) {
  vocab.list = NA
  if ( p %in% names(v) ) {
    vocab.list = v[[p]]
  }
  return( vocab.list )
}



#' Generate a random sample of dates in a given range
#'
#' @param number_of_patients The number of patients in the dataset (default=100)
#' @param from Start date for random date range (default is 3 months ago)
#' @param to End date for random date range (default is 3 months in the future)
#'
#' @importFrom lubridate today
#' @importFrom lubridate ymd
#'
#' @return a list of random dates in the given range
#' @export
#'
#' @examples
#' get_random_dates(10)
#' get_random_dates(100,as.Date("2000-01-01"),as.Date( "2000-12-31"))
get_random_dates = function(number_of_patients=100,
                            from = today()-months(3),
                            to   = today()+months(3)) {
  return( from + sort(runif(number_of_patients, 0, to-from)) )
}

#' Expand trial data (internal)
#'
#' @param d.0 Data in
#' @param d.toexpand Data to expand
#' @param column_name This column
#' @param option_list Choose random data from these options
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#'
#' @return New dataset
expand_trial_data = function( d.0,
                              d.toexpand,
                              column_name = "new_column",
                              option_list ) {

  if ( FALSE ) {
    d.0 = dummy.d
    d.toexpand = num_tumours_T0
    column_name = "tumour_location_T0"
    option_list = tumour_locations.options

    d.0 = dummy.d_tmp1
    d.toexpand = num_tumours_T1
    column_name = "tumour_location_T1"
    option_list = tumour_locations.options
  }

  d.out = d.0 %>% mutate( {{column_name}}:=NA )

  for ( i in 1:nrow(d.toexpand ) ){

    this.id       = ( d.toexpand %>% pull( .data$Label  ))[i]

    ### The number of rows needed for this patient
    this.toadd    = ( d.toexpand %>% pull( .data$number ) )[i]
    ### The number of rows currently existing for this patient
    this.existing = d.out %>% filter( .data$Label == this.id ) %>% nrow

    ### Add some number of empty rows for this patient if required
    if ( this.toadd > this.existing ) {
      this.last_entry_num = d.out %>% filter( .data$Label == this.id ) %>% pull( .data$entry_number ) %>% max

      this.start_entry_num = this.last_entry_num + 1
      this.end_entry_num = this.toadd

      d.out = d.out %>% bind_rows(
        tibble( Label        = rep.int(this.id, this.toadd-this.existing),
                entry_number = this.start_entry_num:this.end_entry_num)
      )
    }

    ### What strings need to be added?
    these.strings_to_add = sample( option_list, this.toadd )

    ### If there are more rows than required already, then pad with ""
    if ( this.existing > this.toadd ) {
      these.strings_to_add = c( these.strings_to_add,
                                rep.int( "", this.existing - this.toadd ) )
    }

    ### Add these new data
    d.out[ d.out$Label==this.id, column_name ] = these.strings_to_add

  }

  ### Return the new data
  return( d.out %>% arrange( .data$Label ) )

}


