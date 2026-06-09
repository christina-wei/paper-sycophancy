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
# variable_type = "state", "delta", "combined"
create_rf_data <- function(input_data, variable_type) {
  
  include_variables = c(
    "Clout", 
    "Authentic", 
    "Clout", 
    "Tone", 
    "LSM"
    )
  
  exclude_variables = c(
    "liwc_function", 
    "pronoun", 
    "ppron", 
    "cogproc", 
    "emotion", 
    "socbehav", 
    "socrefs"
    )
  
  include_variables_diff = str_c(include_variables, "_diff") 
  exclude_variables_diff = str_c(exclude_variables, "_diff") 
  
  if(variable_type == "delta") {
    include_variables = include_variables_diff
    exclude_variables = exclude_variables_diff
  } else if (variable_type == "combined") {
    include_variables = c(include_variables, include_variables_diff)
    exclude_variables = c(exclude_variables, exclude_variables_diff)
  }
  
  rf_input = select(input_data, -model, -scenario, -question_id, -prompt, -turn_no, -ToF, -NoF)
  
  if(variable_type == "delta") {
    rf_input = select(rf_input, alignment, ends_with("_diff"))
  } else if (variable_type == "state") {
    rf_input = select(rf_input, -ends_with("_diff"))
  }
    
  # rf_data <-
  #   input_data |>
  #   filter(!is.numeric(turn_filter) | turn_no == turn_filter) 
  
  #balanced_data <- create_balanced_df(rf_data_all)
  
  rf_input <-
    rf_input |>
    mutate(alignment = as.factor(alignment)) |>
    #select(-model, -scenario, -question_id, -turn_no, -ToF, -NoF) |>
    select(alignment, matches("^[a-z]", ignore.case = FALSE), all_of(include_variables)) |> #leaf nodes
    select(-all_of(exclude_variables)) |> #exclude some variables
    na.omit()
    
  return(rf_input)
}