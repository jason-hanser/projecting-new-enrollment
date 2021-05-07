fetch <- function() {
  
  ## loading libraries
  
  library(dplyr)
  library(tidyr)
  library(RODBC)
  
  
  ###############################
  ######## LOADING DATA #########
  ###############################
  
  # Loading Slate data
  
  SLATE_DATA <- read.csv("slate_export.csv", stringsAsFactors = FALSE)
  
  
  ## Banner Query 1: Start date of fall term
  
  CENSUS_DATES <- sqlQuery(Banner_db, "
    SELECT
      SUBSTR(STVTERM.STVTERM_CODE, 1, 4) AS NUMB_ENTRY_YEAR,
      STVTERM.STVTERM_START_DATE         AS DATE_FALL_DATE,
      TO_DATE(CONCAT('5/1/', SUBSTR(STVTERM.STVTERM_CODE, 1, 4)), 'MM/DD/YYYY') AS DATE_MAY_DATE
    FROM
      STVTERM
    WHERE
      STVTERM.STVTERM_CODE >= '201310' AND
      SUBSTR(STVTERM.STVTERM_CODE, 5, 2) = '10'
  ")
  
  
  ## Banner Query 2: Enroll & Retain
  
  ENROLL_DATA <- sqlQuery(Banner_db, "
    SELECT
      SWVADMS.SLATE_ID           AS NUMB_SLATE_ID,
      SUBSTR(SWVADMS.TERM, 1, 4) AS NUMB_ENTRY_YEAR,
      SWVENRL_6YR.FA_1YR         AS NUMB_ENROLL,
      SWVENRL_6YR.FA_2YR         AS NUMB_RETAIN
    FROM
      SWVADMS
      LEFT JOIN SWVENRL_6YR ON (SWVENRL_6YR.PIDM = SWVADMS.PIDM) AND (SWVENRL_6YR.TERM = SWVADMS.TERM)
    WHERE
      SWVADMS.TERM >= '201310' AND
      SUBSTR(SWVADMS.TERM, 5, 2) = '10' AND
      SWVADMS.SLATE_ID IS NOT NULL AND
      SWVENRL_6YR.FA_1YR = 1
    ")
  
  
  
  ######################################
  ######## WRITING DATA TO FILE ########
  ######################################
  
  print("Data Imported")
  
  data_files <<- list(SLATE_DATA   = SLATE_DATA,
                      CENSUS_DATES = CENSUS_DATES,
                      ENROLL_DATA  = ENROLL_DATA)
  
}