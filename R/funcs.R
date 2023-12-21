library(purrr)
library(tidyverse)
library(data.table)

# Support function
select_variable <- function(survey_list, var_names) {
  lapply(survey_list, function(year_list) {
    lapply(year_list, function(df) {
      if(all(var_names %in% names(df))) {
        dplyr::select(df, all_of(var_names))
      } else {
        df
      }
    })
  })
}

combine_surveys <- function(survey_list) {
  
  # Find the data frame with the most unique 'SEQN' IDs
  seqn_counts <- map_int(survey_list, ~n_distinct(.x$SEQN))
  base_df_index <- which.max(seqn_counts)
  
  # Check if all values are zero, which would mean no unique SEQN values were found
  if (all(seqn_counts == 0)) {
    stop("No unique SEQN values found in any data frame.")
  }
  
  # Extract the base data frame
  base_df <- as.data.frame(survey_list[[base_df_index]])
  
  # Remove the base data frame from the list
  # Perform a full join of all data frames with the base data frame based on 'SEQN'
  combined_df <- reduce(lapply(survey_list[-base_df_index], function(x){as.data.frame(x)}), full_join, by = c("SEQN", "Year"), .init = base_df)
  
  return(combined_df)
}


combine_surveys_DT <- function(survey_list) {
  
  # Find the data frame with the most unique 'SEQN' IDs
  seqn_counts <- map_int(survey_list, ~n_distinct(.x$SEQN))
  base_df_index <- which.max(seqn_counts)
  
  # Check if all values are zero, which would mean no unique SEQN values were found
  if (all(seqn_counts == 0)) {
    stop("No unique SEQN values found in any data frame.")
  }
  
  # Extract the base data frame
  base_df <- as.data.frame(survey_list[[base_df_index]])
  setDT(base_df)
  
  survey_list <- lapply(survey_list[-base_df_index], function(x){as.data.frame(x)})
  
  for (df in survey_list) {
    setDT(df) # Convert to data.table
    base_df <- base_df[df, on = "SEQN", allow.cartesian=TRUE]
  }
  
  return(base_df)
}
