clean <- function(censor_curr_year = TRUE) {

  ########################################  
  ######## SETTING UP ENVIRONMENT ########
  ########################################
    
  ## Loading libraries (& removing start-up data frames)
  
  library(dplyr)
  library(tidyr)
  library(stringr)

  
  ## Unpacking data files
  
  list2env(data_files, .BaseNamespaceEnv)
  
#  list2env(data_files, .GlobalEnv)
  

  #############################################
  ######## CLEANING DATA - ENROLL DATA ########
  #############################################
  
  ENROLL_DATA %>%
    mutate_at(.vars = vars(matches("^NUMB_")),
              .funs = as.numeric) -> ENROLL_DATA



  ##############################################
  ######## CLEANING DATA - CENSUS DATES ########
  ##############################################
  
  ## Converting data to correct format
  
  CENSUS_DATES %>%
    mutate_at(.vars = vars(matches("^NUMB_")),
              .funs = as.numeric) %>%
    mutate_at(.vars = vars(matches("^DATE_")),
              .funs = as.Date) -> CENSUS_DATES
  
  
  ## Getting the correct reference date (may vs fall)
  
  temp_slate_export_date <- as.Date(unique(SLATE_DATA$DATE_Export_Date), "%m/%d/%Y")
  temp_slate_cutoff_date <- as.Date(paste0(unique(SLATE_DATA$NUMB_Export_Year), "-5-5"))
  
  if (temp_slate_export_date <= temp_slate_cutoff_date) {
    
    CENSUS_DATES %>%
      mutate(DATE_Reference_Date = DATE_MAY_DATE) %>%
      select(-DATE_MAY_DATE,
             -DATE_FALL_DATE) -> CENSUS_DATES
    
  } else {
    
    CENSUS_DATES %>%
      mutate(DATE_Reference_Date = DATE_FALL_DATE) %>%
      select(-DATE_MAY_DATE,
             -DATE_FALL_DATE) -> CENSUS_DATES
    
  }
  
  rm(temp_slate_export_date, temp_slate_cutoff_date)


  
  ############################################
  ######## CLEANING DATA - SLATE DATA ########
  ############################################
  
  ## Converting data to correct format
  
  SLATE_DATA %>%
    mutate_at(.vars = vars(matches("^NUMB_")),
              .funs = as.numeric) %>%
    mutate_at(.vars = vars(matches("^BIN_")),
              .funs = as.numeric) %>%
    mutate_at(.vars = vars(matches("^TEXT_")),
              .funs = as.character) %>%
    mutate_at(.vars = vars(matches("^DATE_")),
              .funs = list(~as.Date(., "%m/%d/%Y"))) -> SLATE_DATA
  
  
  ## Some students defer immediating and then cancel, so it looks like they cancell their deposit without 
  ## ever having actually deposited. This fixes those students
  
  SLATE_DATA %>%
    mutate(DATE_CancelDeposit_Date = case_when(is.na(DATE_Deposit_Date) == FALSE ~ DATE_CancelDeposit_Date)
           ) -> SLATE_DATA
  
  
  ## Removing "test" applications
  
  temp_delete <- c(748652900, 587264610, 004202623, 608783923, 716274345, 231186265,
                   140869104, 134917966, 638081107, 138447954, 638878942, 020676299,
                   400191491, 186640045, 120553510, 731531085, 357177315)
  
  SLATE_DATA %>%
    filter(!NUMB_Slate_ID %in% temp_delete) -> SLATE_DATA
  
  rm(temp_delete)
  
  
  
  ##############################
  ######## JOINING DATA ########
  ##############################
  
  ## Joining data
  
  SLATE_DATA %>%
    left_join(y  = ENROLL_DATA,
              by = c("NUMB_Entry_Year" = "NUMB_ENTRY_YEAR",
                     "NUMB_Slate_ID"   = "NUMB_SLATE_ID")) %>%
    left_join(y  = CENSUS_DATES,
              by = c("NUMB_Entry_Year" = "NUMB_ENTRY_YEAR")) -> master_data
  
  rm(SLATE_DATA, ENROLL_DATA, CENSUS_DATES)
  

  
  #############################################
  ######## CLEANING DATA - MASTER DATA ########
  #############################################
  
  ## Fixing missing/unknown values
  
  master_data %>%
    mutate_at(.vars = vars(matches("^NUMB_")),
              .funs = list(~case_when(is.na(.) == TRUE ~ 0, TRUE ~ .))) %>%
    mutate_at(.vars = vars(matches("^OUT_")),
              .funs = list(~case_when(is.na(.) == TRUE ~ 0, TRUE ~ .))) %>%
    mutate_at(.vars = vars(matches("^BIN_")),
              .funs = list(~case_when(is.na(.) == TRUE ~ 0, TRUE ~ .))) %>%
    mutate_at(.vars = vars(matches("^BIN_")),
              .funs = as.factor) %>%
    mutate_at(.vars = vars(matches("^TEXT_")),
              .funs = list(~case_when(. == "" ~ "None/Unknown", TRUE ~ trimws(.)))) -> master_data
  
  
  ## Forcing enroll and retain to zero if the applicant did nor enroll (this happens when a students submits 
  ## multiple applications or when a transfer files a FTIC application by mistake)
  
  master_data %>%
    mutate(NUMB_ENROLL = case_when(is.na(DATE_Deposit_Date) == TRUE ~ 0, 
                                   TRUE ~ NUMB_ENROLL),
           NUMB_RETAIN = case_when(is.na(DATE_Deposit_Date) == TRUE ~ 0, 
                                   TRUE ~ NUMB_RETAIN)) -> master_data
  
  
  ## Creating ordinal dates (ordinal dates are censured to prevent leakage)
  
  master_data %>%
    mutate_at(.vars = vars(matches("DATE_")),
              .funs = list(ORD = ~case_when(as.numeric(. - as.Date(paste0(NUMB_Entry_Year, "-5-5"))) < as.numeric(DATE_Export_Date - as.Date(paste0(NUMB_Export_Year, "-5-5"))) ~ as.numeric(. - DATE_Reference_Date),
                                            TRUE ~ -999999))
              ) -> master_data
  
  
  ##
  
  
  
  ## Cleaning phone poll responses
  
  master_data %>%
    mutate(TEXT_Phone_Poll_Response     = case_when(str_detect(TEXT_Phone_Poll_Response, "_A_") ~ "Highly Likely",
                                                    str_detect(TEXT_Phone_Poll_Response, "_B_") ~ "Likely",
                                                    str_detect(TEXT_Phone_Poll_Response, "_C_") ~ "Somewhat Likely'",
                                                    str_detect(TEXT_Phone_Poll_Response, "_D_") ~ "Not Likely",
                                                    str_detect(TEXT_Phone_Poll_Response, "_E_") ~ "Not Attending",
                                                    str_detect(TEXT_Phone_Poll_Response, "_F_") ~ "No Response",
                                                    TRUE ~ "N/A"),
           NUMB_Phone_Poll_Response = case_when(TEXT_Phone_Poll_Response == "Highly Likely"   ~ 4,
                                                TEXT_Phone_Poll_Response == "Likely"          ~ 3,
                                                TEXT_Phone_Poll_Response == "Somewhat Likely" ~ 2,
                                                TEXT_Phone_Poll_Response == "Not Likely"      ~ 1,
                                                TEXT_Phone_Poll_Response == "Not Attending"   ~ 0,
                                                TRUE ~ -999999)
           ) -> master_data
  
  
  ## Major
  
  master_data %>%
    mutate(BIN_Major_MAS     = case_when(TEXT_Major_1 == "Marine Science" ~ 1,
                                         TEXT_Major_2 == "Marine Science" ~ 1,
                                         TEXT_Major_3 == "Marine Science" ~ 1,
                                         TRUE ~ 0),
           BIN_Major_ENS     = case_when(TEXT_Major_1 == "Environmental Studies" ~ 1,
                                         TEXT_Major_2 == "Environmental Studies" ~ 1,
                                         TEXT_Major_3 == "Environmental Studies" ~ 1,
                                         TRUE ~ 0),
           BIN_Major_UND     = case_when(TEXT_Major_1 == "Undecided" ~ 1,
                                         TEXT_Major_2 == "Undecided" ~ 1,
                                         TEXT_Major_3 == "Undecided" ~ 1,
                                         TRUE ~ 0)
           ) -> master_data
  
  
  ## Cleaning decision codes
  
  master_data %>%
    mutate_at(.vars = vars(matches("_Decision_Bin$")),
              .funs = list(~case_when(. %in% c('DP', 'DPD', 'DW', 'DPW')                     ~ "Deposited",
                                      . %in% c('AA', 'AS')                                   ~ "Admitted",
                                      . %in% c('AW', 'WLWT')                                 ~ "Wait Listed",
                                      . %in% c('AFSGR', 'AI', 'AIF', 'AINC', 'APCA', 'APCR') ~ "More Info Needed",
                                      . %in% c('DC', 'ADC', 'CW', 'CWBD', 'CWFS')            ~ "Cancelled",
                                      . %in% c('AD')                                         ~ "Denied",
                                      . %in% c('DD', 'ADF')                                  ~ "Deferred",
                                      . %in% c("None/Unknown")                               ~ "None",
                                      TRUE ~ "Other"))
              ) -> master_data
  
  
  ## Age, Race, Residency
  
  master_data %>%
    mutate(NUMB_Age       = case_when(is.na(DATE_Birthdate) == FALSE ~ floor(as.numeric(DATE_Reference_Date - DATE_Birthdate)/365),
                                      TRUE ~ -999999),
           TEXT_Race_Bin  = case_when(BIN_Hispanic == 0 & TEXT_Race == "White"                     ~ "White",
                                      BIN_Hispanic == 0 & TEXT_Race == "Black or African-American" ~ "Black",
                                      BIN_Hispanic == 1 | TEXT_Race == "Hispanic or Latino"        ~ "Hispanic",
                                      str_detect(TEXT_Race, ",") == TRUE                           ~ "Multi-Racial",
                                      TEXT_Race == "None/Unknown"                                  ~ "Unknown",
                                      TRUE                                                         ~ "Other"),
           TEXT_Residency  = case_when(str_detect(TEXT_Applicant_Type, "International") == TRUE ~ "I",
                                       TEXT_Permanent_State == "FL"                             ~ "F",
                                       TRUE                                                     ~ "N")
           ) -> master_data
  
  
  ## Inquiry and Prospect Type
  
  temp_inquiry_codes <- read.csv("Supporting Files//Inquiry_codes.csv", stringsAsFactors = FALSE)
  
  master_data %>%
    left_join(y  = temp_inquiry_codes,
              by = "TEXT_Inquiry_Summary") %>%
    mutate(TEXT_Prospect_Bin = case_when(TEXT_Prospect_Summary == "None/Unknown" ~ "Not_Bought",
                                         TRUE ~ "Bought"),
           TEXT_Inquiry_Bin  = case_when(is.na(TEXT_Inquiry_Bin) == FALSE ~ TEXT_Inquiry_Bin,
                                         TRUE ~"Other")
           ) -> master_data
  
  rm(temp_inquiry_codes)
  
  
  ## Explore Eckerd
  
  master_data %>%
    mutate(TEXT_Most_Recent_EEC = case_when(NUMB_Most_Recent_EEC == NUMB_Entry_Year ~ "This_Year",
                                            NUMB_Most_Recent_EEC <  NUMB_Entry_Year ~ "Prior_Year",
                                            TRUE ~ "None")
           ) -> master_data
  
  
  ## Lag Periods
  
  master_data %>%
    mutate(NUMB_Apply_Lag   = case_when(DATE_App_Created_ORD   != -999999 & DATE_App_Submitted_ORD != -999999 ~ as.numeric(DATE_App_Submitted - DATE_App_Created),
                                        TRUE ~ -999999),
           NUMB_Admit_Lag   = case_when(DATE_App_Submitted_ORD != -999999 & DATE_Admit_Date_ORD    != -999999 ~ as.numeric(DATE_Admit_Date - DATE_App_Submitted),
                                        TRUE ~ -999999),
           NUMB_Deposit_Lag = case_when(DATE_Deposit_Date_ORD  != -999999 & DATE_Admit_Date_ORD    != -999999 ~ as.numeric(DATE_Deposit_Date - DATE_Admit_Date),
                                        TRUE ~ -999999)) %>%
    mutate_at(.vars = vars(matches("_Lag$")),
              .funs = list(~case_when(. < 0 & . != -999999 ~ -666666,
                                      TRUE ~ .))) -> master_data
  
  
  ## Fixing missing HS_GPA's and censoring HS_GPA for students who have not yet beeen admitted
  
  master_data %>%
    mutate(NUMB_HS_GPA = case_when(NUMB_HS_GPA > 4 ~ 4,
                                   TRUE ~ NUMB_HS_GPA),
           NUMB_HS_GPA = case_when(DATE_Admit_Date_ORD != -999999 | DATE_Deposit_Date_ORD != -999999 ~ NUMB_HS_GPA,
                                   TRUE ~ -999999)
           ) -> master_data
  
  
  ## Outcomes
  
  master_data %>%
    mutate(OUT_March_1_Deposit = case_when(TEXT_March_1_Decision_Bin == "Deposited" ~ 1, TRUE ~ 0),
           OUT_April_1_Despoit = case_when(TEXT_April_1_Decision_Bin == "Deposited" ~ 1, TRUE ~ 0),
           OUT_May_5_Deposit   = case_when(TEXT_May_5_Decision_Bin   == "Deposited" ~ 1, TRUE ~ 0),
           
           OUT_Curr_Desposit_Enroll   = case_when(NUMB_ENROLL == 1 & TEXT_Current_Decision_Bin == "Deposited" ~ 1, 
                                                  TRUE ~ 0),
           OUT_May_5_Desposit_Enroll  = case_when(NUMB_ENROLL == 1 & TEXT_May_5_Decision_Bin == "Deposited" ~ 1, 
                                                  TRUE ~ 0)
           ) -> master_data
  
  
  ## Converting text/binary variables into factors
  
  master_data %>%
    mutate_at(.vars = vars(matches("^TEXT_")),
              .funs = as.factor) %>%
    mutate_at(.vars = vars(matches("^BIN_")),
              .funs = as.factor) %>%
    mutate_at(.vars = vars(matches("^OUT_")),
              .funs = as.factor) -> master_data
  
  
  ## Censuring current year (if specified)
  
  if (censor_curr_year == TRUE) {

    master_data %>%
      mutate_at(.vars = vars(matches("^OUT_")),
                .funs = list(~case_when(NUMB_Entry_Year < max(NUMB_Entry_Year) ~ .))) -> master_data
    
  }
  
  
  #################################
  ######## FINAL FILTERING ########
  #################################

  ## getting rid on misc apps
  
  master_data %>%
    filter(is.na(NUMB_Slate_ID)      == FALSE,
           is.na(DATE_App_Submitted) == FALSE,
           NUMB_Transfer             == 0,
           TEXT_Entry_Term           %in% c('Fall', 'Autumn'),
           NUMB_Entry_Year           <= NUMB_Export_Year) %>%
    unique() -> master_data
  
  
  
  ##########################################
  ######## WRITING THE DATA TO FILE ########
  ##########################################
  
  print("Data Cleaned")
  
  master_data <<- master_data
  
}
  
  


