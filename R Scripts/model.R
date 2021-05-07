model <- function(years_of_data = 5, weight_years = FALSE, Use_EEC = FALSE, ignore_year = NULL) {

  ## Loading libraries
  
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(randomForest)
  
  
  ###############################
  ######## CLEANING DATA ########
  ###############################
  
  ## Filtering out applications that have not been submitted yet
  
  master_data %>%
    filter(DATE_App_Submitted_ORD != -999999) -> ml_data
  
  
  
  ###################################
  ######## LOADING VARIABLES ########
  ###################################
  
  variables <- read.csv("Supporting Files//variables.csv", stringsAsFactors = FALSE)
  
  if (Use_EEC == FALSE) {
    
    variables %>%
      filter(!Variable_Code %in% c('NUMB_EEC_Count', 'TEXT_Most_Recent_EEC')) -> variables
    
  }
  rm(Use_EEC)
  
  
  
  ###############################
  ######## MODELING LOOP ########
  ###############################
  
  print(paste0("Modeling data."))
  var_importance <- data.frame()
  
  
  for (i in 1:5) {
    
    ## Filtering data to only include relevant years
  
    temp_years <- data.frame(NUMB_Entry_Year = sort(unique(ml_data$NUMB_Entry_Year), decreasing = TRUE)[(i):(i+years_of_data)],
                             WEIGHT          = c(1, rep(seq(to = 1/years_of_data, from = 1, length.out = years_of_data))))
    
    ml_data %>%
      inner_join(y  = temp_years,
                 by = c("NUMB_Entry_Year" = "NUMB_Entry_Year")) -> temp_ml_data
    
    rm(temp_years)
    
    
    ## Getting training data
    
    temp_ml_data %>%
      filter(NUMB_Entry_Year != max(NUMB_Entry_Year)) -> train_data
    
    
    ## Throwing out weird years
    
    if (is.null(ignore_year) == FALSE) {
      
      train_data %>%
        filter(NUMB_Entry_Year != ignore_year) -> train_data
      
    }
    
    
    ## Getting a weighted sample of training data (if specificied)
    
    if (weight_years == TRUE) {
      
      set.seed(1958)
      
      train_data <- train_data[sample(nrow(train_data), nrow(train_data), prob = train_data$WEIGHT, replace = TRUE), ]
      
    }

    
    #####################################
    ######## CONSTRUCTING MODELS ########
    #####################################

    ## March Deposit Predictions
    
    if (unique(train_data$DATE_Export_Date) <= as.Date(paste0("3/1/", unique(train_data$NUMB_Export_Year)), "%m/%d/%Y")) {
      
      ## Fitting data to model 
      
      set.seed(1958)
      
      temp_rf <- randomForest(x = train_data[, variables$Variable_Code],
                              y = train_data[, "OUT_March_1_Deposit"],
                              ntree = if(i == 1) {500} else {500})
      
      
      ## Getting predictions for test data
      
      temp_preds <- predict(object  = temp_rf,
                            newdata = ml_data[ml_data$NUMB_Entry_Year == max(temp_ml_data$NUMB_Entry_Year), variables$Variable_Code],
                            type    = "prob")[, 2]
      
      ml_data[ml_data$NUMB_Entry_Year == max(temp_ml_data$NUMB_Entry_Year), "PRED_OUT_March_1_Deposit"] <- temp_preds
      
      
      ## Variable Importance
      
      temp_var_imp <- data.frame(Entry_Year    = max(temp_ml_data$NUMB_Entry_Year),
                                 Model         = "March_1_Deposit",
                                 Variable_Code = row.names(temp_rf$importance),
                                 Importance    = as.numeric(temp_rf$importance),
                                 r_Importance  = as.numeric(temp_rf$importance)/max(as.numeric(temp_rf$importance)))
                                 
      
      var_importance <- rbind(var_importance, temp_var_imp)
      
      rm(temp_var_imp, temp_rf, temp_preds)
      
      
    } else {
      
      ml_data[ml_data$NUMB_Entry_Year == max(temp_ml_data$NUMB_Entry_Year), "PRED_OUT_March_1_Deposit"] <- NA
      
    }
    
    
    ## April Deposit Predictions
    
    if (unique(train_data$DATE_Export_Date) <= as.Date(paste0("4/1/", unique(train_data$NUMB_Export_Year)), "%m/%d/%Y")) {
      
      ## Fitting data to model 
      
      set.seed(1958)
      
      temp_rf <- randomForest(x = train_data[, variables$Variable_Code],
                              y = train_data[, "OUT_April_1_Despoit"],
                              ntree = if(i == 1) {500} else {500})
      
      
      ## Getting predictions for test data
      
      temp_preds <- predict(object  = temp_rf,
                            newdata = ml_data[ml_data$NUMB_Entry_Year == max(temp_ml_data$NUMB_Entry_Year), variables$Variable_Code],
                            type    = "prob")[, 2]
      
      ml_data[ml_data$NUMB_Entry_Year == max(temp_ml_data$NUMB_Entry_Year), "PRED_OUT_April_1_Deposit"] <- temp_preds
      
      
      ## Variable Importance
      
      temp_var_imp <- data.frame(Entry_Year    = max(temp_ml_data$NUMB_Entry_Year),
                                 Model         = "April_1_Deposit",
                                 Variable_Code = row.names(temp_rf$importance),
                                 Importance    = as.numeric(temp_rf$importance),
                                 r_Importance  = as.numeric(temp_rf$importance)/max(as.numeric(temp_rf$importance)))
      
      
      var_importance <- rbind(var_importance, temp_var_imp)
      
      rm(temp_var_imp, temp_rf, temp_preds)
      
    } else {
      
      ml_data[ml_data$NUMB_Entry_Year == max(temp_ml_data$NUMB_Entry_Year), "PRED_OUT_April_1_Deposit"] <- NA
      
    }
    
    
    ## May Deposit Predictions
    
    if (unique(train_data$DATE_Export_Date) <= as.Date(paste0("5/5/", unique(train_data$NUMB_Export_Year)), "%m/%d/%Y")) {
      
      ## Fitting data to model 
      
      set.seed(1958)
      
      temp_rf <- randomForest(x = train_data[, variables$Variable_Code],
                              y = train_data[, "OUT_May_5_Deposit"],
                              ntree = if(i == 1) {1000} else {500})
      
      
      ## Getting predictions for test data
      
      temp_preds <- predict(object  = temp_rf,
                            newdata = ml_data[ml_data$NUMB_Entry_Year == max(temp_ml_data$NUMB_Entry_Year), variables$Variable_Code],
                            type    = "prob")[, 2]
      
      ml_data[ml_data$NUMB_Entry_Year == max(temp_ml_data$NUMB_Entry_Year), "PRED_OUT_May_5_Deposit"] <- temp_preds
      
      
      ## Variable Importance
      
      temp_var_imp <- data.frame(Entry_Year    = max(temp_ml_data$NUMB_Entry_Year),
                                 Model         = "May_5_Deposit",
                                 Variable_Code = row.names(temp_rf$importance),
                                 Importance    = as.numeric(temp_rf$importance),
                                 r_Importance  = as.numeric(temp_rf$importance)/max(as.numeric(temp_rf$importance)))
      
      
      var_importance <- rbind(var_importance, temp_var_imp)
      
      rm(temp_var_imp, temp_rf, temp_preds)
      
    } else {
      
      ml_data[ml_data$NUMB_Entry_Year == max(temp_ml_data$NUMB_Entry_Year), "PRED_OUT_May_5_Deposit"] <- NA
      
    }
    
    
    ## May Deposit & Enroll Predictions
    
    if (unique(train_data$DATE_Export_Date) <= as.Date(paste0("5/5/", unique(train_data$NUMB_Export_Year)), "%m/%d/%Y")) {
      
      ## Fitting data to model 
      
      set.seed(1958)
      
      temp_rf <- randomForest(x = train_data[, variables$Variable_Code],
                              y = train_data[, "OUT_May_5_Desposit_Enroll"],
                              ntree = if(i == 1) {1000} else {500})
      
      
      ## Getting predictions for test data
      
      temp_preds <- predict(object  = temp_rf,
                            newdata = ml_data[ml_data$NUMB_Entry_Year == max(temp_ml_data$NUMB_Entry_Year), variables$Variable_Code],
                            type    = "prob")[, 2]
      
      ml_data[ml_data$NUMB_Entry_Year == max(temp_ml_data$NUMB_Entry_Year), "PRED_OUT_May_5_Desposit_Enroll"] <- temp_preds
      
      
      ## Variable Importance
      
      temp_var_imp <- data.frame(Entry_Year    = max(temp_ml_data$NUMB_Entry_Year),
                                 Model         = "May_5_Deposit_Enroll",
                                 Variable_Code = row.names(temp_rf$importance),
                                 Importance    = as.numeric(temp_rf$importance),
                                 r_Importance  = as.numeric(temp_rf$importance)/max(as.numeric(temp_rf$importance)))
      
      
      var_importance <- rbind(var_importance, temp_var_imp)
      
      rm(temp_var_imp, temp_rf, temp_preds)
      
    } else {
      
      ml_data[ml_data$NUMB_Entry_Year == max(temp_ml_data$NUMB_Entry_Year), "PRED_OUT_May_5_Desposit_Enroll"] <- NA
      
    }
    
    
    ## Current Decision & Enroll Predictions
    
    if (unique(train_data$DATE_Export_Date) > as.Date(paste0("5/5/", unique(train_data$NUMB_Export_Year)), "%m/%d/%Y")) {
      
      ## Fitting data to model 
    
      set.seed(1958)
      
      temp_rf <- randomForest(x = train_data[, variables$Variable_Code],
                              y = train_data[, "OUT_Curr_Desposit_Enroll"],
                              ntree = if(i == 1) {750} else {500})
      
      
      ## Getting predictions for test data
      
      temp_preds <- predict(object  = temp_rf,
                            newdata = ml_data[ml_data$NUMB_Entry_Year == max(temp_ml_data$NUMB_Entry_Year), variables$Variable_Code],
                            type    = "prob")[, 2]
      
      ml_data[ml_data$NUMB_Entry_Year == max(temp_ml_data$NUMB_Entry_Year), "PRED_OUT_Curr_Desposit_Enroll"] <- temp_preds
      
      
      ## Variable Importance
      
      temp_var_imp <- data.frame(Entry_Year    = max(temp_ml_data$NUMB_Entry_Year),
                                 Model         = "Curr_Deposit_Enroll",
                                 Variable_Code = row.names(temp_rf$importance),
                                 Importance    = as.numeric(temp_rf$importance),
                                 r_Importance  = as.numeric(temp_rf$importance)/max(as.numeric(temp_rf$importance)))
      
      
      var_importance <- rbind(var_importance, temp_var_imp)
      
      rm(temp_var_imp, temp_rf, temp_preds)
      
    } else {
      
      ml_data[ml_data$NUMB_Entry_Year == max(temp_ml_data$NUMB_Entry_Year), "PRED_OUT_Curr_Desposit_Enroll"] <- NA
      
    }
    
    
    ## Loop clean-up
    
    print(paste0(max(temp_ml_data$NUMB_Entry_Year), " model is complete."))
    rm(temp_ml_data, train_data)
    
  }
  rm(i)
  

  ##########################
  ######## CLEAN-UP ########
  ##########################
  
  ## Getting only relevant years
  
  ml_data %>%
    filter(NUMB_Entry_Year >= NUMB_Entry_Year-4) -> ml_data
  
  
  ## Getting the highest predicted value for each student (in cases where students submit multiple applications)
  
  if (unique(ml_data$DATE_Export_Date) > as.Date(paste0("5/5/", unique(ml_data$NUMB_Export_Year)), "%m/%d/%Y")) {
  
    ml_data %>%
      group_by(NUMB_Slate_ID,
               NUMB_Entry_Year) %>%
      filter(PRED_OUT_Curr_Desposit_Enroll == max(PRED_OUT_Curr_Desposit_Enroll)) %>%
      ungroup() -> ml_data
    
  } else {
    
    ml_data %>%
      group_by(NUMB_Slate_ID,
               NUMB_Entry_Year) %>%
      filter(PRED_OUT_May_5_Desposit_Enroll == max(PRED_OUT_May_5_Desposit_Enroll)) %>%
      ungroup() -> ml_data
    
  }
  
  
  ############################################
  ######## ADJUSTING PREDICTED VALUES ########
  ############################################

  ml_data %>%
    mutate_at(.vars = vars(matches("^PRED_")),
              .funs = as.numeric) -> ml_data  

  
  ## Forcing predicitons to zero for students who are "cancelled"  
  
  if (unique(ml_data$DATE_Export_Date) <= as.Date(paste0("3/1/", unique(ml_data$NUMB_Export_Year)), "%m/%d/%Y")) {
    
    ml_data %>%
      mutate_at(.vars = vars(matches("PRED_OUT_March_1_Deposit")),
                .funs = list(~case_when(TEXT_Current_Decision_Bin == "Cancelled" ~ 0, 
                                        TEXT_Current_Decision_Bin == "Denied"    ~ 0, 
                                        TEXT_Current_Decision_Bin == "Admitted"  ~ .,
                                        TRUE ~ .))) -> ml_data
    
  } 
  
  if (unique(ml_data$DATE_Export_Date) <= as.Date(paste0("4/1/", unique(ml_data$NUMB_Export_Year)), "%m/%d/%Y")) {
  
    ml_data %>%
      mutate_at(.vars = vars(matches("PRED_OUT_April_1_Deposit")),
                .funs = list(~case_when(TEXT_Current_Decision_Bin == "Cancelled" ~ 0, 
                                        TEXT_Current_Decision_Bin == "Denied"    ~ 0, 
                                        TEXT_Current_Decision_Bin == "Admitted"  ~ .,
                                        TRUE ~ .))) -> ml_data
    
  }
  
  if (unique(ml_data$DATE_Export_Date) <= as.Date(paste0("5/5/", unique(ml_data$NUMB_Export_Year)), "%m/%d/%Y")) {
  
    ml_data %>%
      mutate_at(.vars = vars(matches("^PRED_OUT_May")),
                .funs = list(~case_when(TEXT_Current_Decision_Bin == "Cancelled" ~ 0, 
                                        TEXT_Current_Decision_Bin == "Denied"    ~ 0, 
                                        TEXT_Current_Decision_Bin == "Admitted"  ~ .,
                                        TRUE ~ .))) %>%
      mutate(OUT_Curr_Desposit_Enroll = as.numeric(NA)) -> ml_data
  
  }
  
  if (unique(ml_data$DATE_Export_Date) > as.Date(paste0("5/5/", unique(ml_data$NUMB_Export_Year)), "%m/%d/%Y")) {
    
    ml_data %>%
      mutate_at(.vars = vars(matches("PRED_OUT_Curr_Desposit_Enroll")),
                .funs = list(~case_when(TEXT_Current_Decision_Bin == "Deposited" ~ ., 
                                        TRUE ~ 0))) -> ml_data
    
  }
  
  
  
  ######################################
  ######## WRITING DATA TO FILE ########
  ######################################
  
  ## returning out to environment
  
  ml_data        <<- ml_data
  var_importance <<- var_importance
  
  
  ## writing to physical file
  
  write.csv(ml_data, "output//ml_data.csv", row.names = FALSE)
  write.csv(var_importance, "Output//var_importance.csv", row.names = FALSE)
  
}


  


