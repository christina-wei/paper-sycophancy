library(dplyr)
library(stringr)
library(randomForest)

create_balanced_df <- function(input_data) {
  
  min_n <- min(table(input_data$alignment))
  
  balanced_data <-
    input_data |>
    group_by(across(alignment)) |>
    slice_sample(n = min_n) |>
    ungroup()
  
  rm(min_n)
  
  return(balanced_data)
}

create_balanced_df_scenario <- function(input_data) {
  
  balanced_data <- data.frame()
  scenarios <- unique(input_data$scenario)
  
  for (s in scenarios) {
    
    tmp_data <- filter(input_data, scenario == s)
    min_n <- min(table(tmp_data$alignment))
    
    tmp_data <-
      tmp_data |>
      group_by(alignment) |>
      slice_sample(n = min_n) |>
      ungroup()
    
    balanced_data <- bind_rows(balanced_data, tmp_data)
    
  }
  
  rm(tmp_data)
  rm(min_n)
  
  return(balanced_data)
}

run_rf_model <- function(rf_data, n = 500) {
  
  rf_model <- randomForest(alignment ~ ., 
                           data = rf_data, 
                           importance = TRUE, 
                           ntree = n)
  
  # 3. View the results
  print(rf_model)
  
  #see error rate
  plot(rf_model)
  
  #variable importance df
  variable_importance <-
    importance(rf_model) |>
    as.data.frame() |>
    # Move the variable names from the "row names" to an actual column
    tibble::rownames_to_column("LIWC_Variable") |>
    arrange(desc(MeanDecreaseAccuracy))
  
  return(variable_importance)
}

## FUNCTION: create data to be used in RF call
create_rf_data <- function(input_data, turn_filter = "all") {
  
  rf_data <-
    input_data |>
    filter(!is.numeric(turn_filter) | turn_no == turn_filter) 
  
  #balanced_data <- create_balanced_df(rf_data_all)
  balanced_data <- create_balanced_df_scenario(rf_data)
  
  rf_input <-
    balanced_data |>
    select(-prompt, -question_id, -turn_no) |>
    select(alignment, matches("^[a-z]", ignore.case = FALSE), Analytic, Clout, Authentic, Tone, LSM) |>
    select(-liwc_function, -pronoun, -ppron, -cogproc, -emotion, -socbehav, -socrefs)
  
  return(rf_input)
}