library(dplyr)
library(readr)

read_liwc_results_all <- function(models, segmented = FALSE) {
  
  #filename <- ifelse(!segmented, "liwc_combined_results.csv", "liwc_combined_results_segmented.csv")
  
  liwc_results_allmodels <- data.frame()
  
  for (m in models) {
    
    liwc_results <- read_csv(
      paste0(
        "analysis/", 
        m, 
        "/liwc_combined_results",
        ifelse(segmented, "_segmented",""),
        ".csv"
      ),
      show_col_types = FALSE
      ) |>
      select(model, prompt, scenario, question_id, turn_no, alignment, ToF, NoF, Analytic:last_col()) |>
      rename(liwc_function = `function`) |>
      select(-contains("Person.")) |>
      #Create delta variables
      group_by(model, prompt, question_id) |>
      arrange(turn_no, .by_group = TRUE) |>
      mutate(
        across(
          Clout:last_col(),
          ~ .x - lag(.x),
          .names = "{.col}_diff"
        )
      ) |>
      ungroup()
    
    liwc_results_allmodels <- bind_rows(liwc_results_allmodels, liwc_results)
  }
  
  return(liwc_results_allmodels)
  
}