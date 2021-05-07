var_compare <- function() {
  
  library(ggplot2)
  library(gridExtra)
  library(rlang)
  
  ###################################
  ######## LOADING VARIABLES ########
  ###################################
  
  variables      <- read.csv("Supporting Files\\variables.csv", stringsAsFactors = FALSE)
  temp_variables <- read.csv("Output\\var_importance.csv", stringsAsFactors = FALSE)
  
  variables %>%
    filter(Variable_Code %in% temp_variables$Variable) -> variables
  
  rm(temp_variables)
  
  
  
  ###########################
  ######## PLOT LOOP ########
  ###########################
  
  for (i in 1:nrow(variables)) {
    
    if (variables$Variable_Type[i] == "Continuous") {
      
      ## Missing value frequency
      
      ml_data %>%
        mutate(MISSING = case_when(!!sym(variables$Variable_Code[i]) == -999999 ~ 1, TRUE ~ 0)) %>%
        ggplot(aes(x = as.factor(NUMB_Entry_Year == max(NUMB_Entry_Year)),
                   y = MISSING,
                   fill = as.factor(NUMB_Entry_Year == max(NUMB_Entry_Year)))) +
          geom_bar(stat  = "summary",
                   fun   = "mean") + 
          ggtitle(variables$Variable_Desc[i]) + 
          scale_y_continuous(name   = "Missing Values",
                             labels = scales::percent_format(1)) +
          scale_x_discrete(name   = NULL,
                           labels = c("Past Years", max(master_data$NUMB_Entry_Year))) + 
          scale_fill_manual(values = c("#54585a", "#38939b")) + 
          theme(axis.ticks      = element_blank(),
                legend.position = "none",
                axis.text       = element_text(color = "grey20")) -> plot_1
      
      if (sum(ml_data[,variables$Variable_Code[i]] == -999999) == 0) {
        
        plot_1 +
          annotate(geom  = "text", 
                   label = "No Missing Data", 
                   size  = 6,
                   x     = 1.5,
                   y     = 0) -> plot_1
        
      }
      
      
      ## Distribution of non-missing values
      
      ml_data %>%
        filter(!!sym(variables$Variable_Code[i]) != -999999) %>%
        ggplot() +
          stat_density(aes(x     = !!sym(variables$Variable_Code[i]),
                           color = as.factor(NUMB_Entry_Year == max(NUMB_Entry_Year))),
                       geom     = "line",
                       position = "identity",
                       lwd      = 1) +
          scale_y_continuous(name   = "Density",
                             labels = NULL) +
          scale_x_continuous(name   = NULL) + 
          scale_color_manual(name   = NULL,
                             values = c("#54585a", "#38939b"),
                             labels = c("Past Terms", max(ml_data$NUMB_Entry_Year))) + 
          theme(axis.ticks      = element_blank(),
                legend.position = "bottom",
                legend.key      = element_blank(),
                axis.text       = element_text(color = "grey30")) -> plot_2
      
      
      ## Combining plots
      
      temp_plot <- grid.arrange(plot_1, plot_2, ncol = 2)
      
      
      ## Writing plot to file
      
      ggsave(plot     = temp_plot,
             filename = paste0("Output//Variable Comparison Plots//C-", variables$Variable_Code[i], ".png"),
             width    = 6.5,
             height   = 3.25,
             units    = "in")
      
      rm(temp_plot, plot_1, plot_2)
      
      
      
    } else if (variables$Variable_Type[i] == "Discrete") {
      
      ## Distribution of Values
      
      ml_data %>%
        mutate(TEMP_YEAR = NUMB_Entry_Year == max(NUMB_Entry_Year),
               TEMP_VAR  = !!sym(variables$Variable_Code[i])) %>%
        group_by(TEMP_YEAR,
                 TEMP_VAR) %>%
        summarise(CT = n()) %>%
        group_by(TEMP_YEAR) %>%
        mutate(PCT = CT/sum(CT)) %>%
        ungroup() %>%
        ggplot() +
          geom_col(aes(x = TEMP_VAR,
                       y = PCT,
                       fill = TEMP_YEAR),
                   position = "dodge") + 
          ggtitle(variables$Variable_Desc[i]) + 
          scale_y_continuous(name   = NULL,
                             labels = scales::percent_format(1),
                             limits = c(0, 1)) +
          scale_x_discrete(name   = NULL,
                           labels = abbreviate) +
          scale_fill_manual(name   = NULL,
                            values = c("#54585a", "#38939b"),
                            labels = c("Past Terms", max(ml_data$NUMB_Entry_Year))) +
          theme(axis.ticks      = element_blank(),
                legend.position = "bottom",
                axis.text       = element_text(color = "grey20"),
                axis.text.x     = element_text(angle = 45)) -> plot_1
      
      
      ## Outcomes
      
      ml_data %>%
        filter(NUMB_Entry_Year < max(NUMB_Entry_Year)) %>%
        group_by(!!sym(variables$Variable_Code[i])) %>%
        summarise(May_5_Deposit = mean(OUT_May_5_Deposit == 1),
                  Curr_Desposit_Enroll = mean(OUT_Curr_Desposit_Enroll == 1)) %>%
        ungroup() %>%
        gather("OUTCOME", "VALUE", 2:3) %>%
        ggplot() +
          geom_col(aes(x = !!sym(variables$Variable_Code[i]),
                       y = VALUE)) +
          scale_x_discrete(name   = NULL,
                           labels = abbreviate) +
          scale_y_continuous(name   = NULL,
                             labels = scales::percent_format(1)) +
          facet_wrap(.~OUTCOME, 
                     ncol   = 1,
                     nrow   = 2,
                     scales = "free_y") + 
          theme(axis.ticks  = element_blank(),
                axis.text   = element_text(color = "grey20"),
                axis.text.y = element_text(size = 8),
                axis.text.x = element_text(angle = 45)) -> plot_2
      
      
      ## Combining plots
      
      temp_plot <- grid.arrange(plot_1, plot_2, nrow = 1)
      
      
      ## Writing plot to file
      
      ggsave(plot     = temp_plot,
             filename = paste0("output//Variable Comparison Plots//D-", variables$Variable_Code[i], ".png"),
             width    = 6.5,
             height   = 3.25,
             units    = "in")
      
      rm(plot_1, plot_2, temp_plot)
      
      
    }
    
    
  } 
  
   
}


  