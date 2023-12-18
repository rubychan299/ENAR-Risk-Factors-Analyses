library(tidyverse)
# Support functions####

combine_datasets <- function(survey_list, ids_vector, id_column_name) {
  library(dplyr)
  
  final_datasets <- list()
  column_presence_count <- list()
  
  # Helper function to coerce character columns with numeric content to numeric
  coerce_numeric <- function(df) {
    num_coerced_df <- df
    for (col_name in names(df)) {
      if (is.character(df[[col_name]])) {
        suppressed_warnings <- suppressWarnings(as.numeric(df[[col_name]]))
        if (!any(is.na(suppressed_warnings))) {
          num_coerced_df[[col_name]] <- suppressed_warnings
        }
      }
    }
    return(num_coerced_df)
  }
  
  # Loop through each year
  for(year in names(survey_list)) {
    # Loop through each survey dataframe in the year
    for(survey_name in names(survey_list[[year]])) {
      
      # Skip if the dataset is NULL or doesn't have the ID column
      if(is.null(survey_list[[year]][[survey_name]]) || !(id_column_name %in% names(survey_list[[year]][[survey_name]]))) {
        next
      }
      
      # Subset and coerce numeric columns
      survey_subset <- survey_list[[year]][[survey_name]] %>%
        filter(!!sym(id_column_name) %in% ids_vector) %>%
        coerce_numeric()
      
      # Skip this dataset if it becomes empty after subsetting
      if(nrow(survey_subset) == 0) {
        next
      }
      
      # Standardize the survey name
      standardized_name <- if (startsWith(survey_name, "P_")) {
        substring(survey_name, 3)
      } else {
        gsub(pattern = "_[A-Z]$", replacement = "", x = survey_name)
      }
      
      # Create an identifier for the year based on the survey_name
      survey_with_year <- survey_subset %>%
        mutate(Year = year)
      
      # Bind rows without checking for common columns first
      if(!standardized_name %in% names(final_datasets)) {
        final_datasets[[standardized_name]] <- survey_with_year
        column_presence_count[[standardized_name]] <- setNames(numeric(ncol(survey_with_year)), names(survey_with_year))
      } else {
        final_datasets[[standardized_name]] <- bind_rows(final_datasets[[standardized_name]], survey_with_year)
        
        # Count the presence of each column
        current_cols <- names(survey_with_year)
        column_presence_count[[standardized_name]][current_cols] <- column_presence_count[[standardized_name]][current_cols] + 1
      }
    }
  }
  
  # Filter out columns that don't have at least five years of data
  for(dataset_name in names(final_datasets)) {
    cols_to_keep <- names(which(column_presence_count[[dataset_name]] >= 5))
    final_datasets[[dataset_name]] <- final_datasets[[dataset_name]][, cols_to_keep, drop = FALSE]
  }
  
  # Remove NULL datasets from the final list
  final_datasets <- final_datasets[!sapply(final_datasets, is.null)]
  
  return(final_datasets)
}

remove_variable <- function(survey_list, var_name) {
  lapply(survey_list, function(year_list) {
    lapply(year_list, function(df) {
      if(!is.null(df) && var_name %in% names(df)) {
        dplyr::select(df, -!!sym(var_name))
      } else {
        df
      }
    })
  })
}


# Use nhanesA package####
library(nhanesA)
library(cardioStatsUSA)

years <- c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 'P')

data(nhanes_data)
id <- nhanes_data$svy_id

## Dietary ####
ditables <- vector("list", length = length(years))

for(i in seq_along(years)){
  dataset_names <- nhanesTables('DIET', year = years[i], namesonly=TRUE)
  ditables[[i]] <- lapply(dataset_names, nhanes)
  names(ditables[[i]]) <- dataset_names
}
names(ditables) <- years

di_dat <- combine_datasets(ditables, id, 'SEQN')

save(ditables, di_dat, file = "data/cleaned/Dietary_v2.RData")

## Questionnaires ####
qtables <- vector("list", length = length(years))

for(i in seq_along(years)){
  dataset_names <- nhanesTables('Q', year = years[i], namesonly=TRUE)
  qtables[[i]] <- lapply(dataset_names, nhanes)
  names(qtables[[i]]) <- dataset_names
}
names(qtables) <- years

q_dat <- combine_datasets(qtables, id, 'SEQN')

save(qtables, q_dat, file = "data/cleaned/Questionaires_2.RData")

## Examinations ####
extables <- vector("list", length = length(years))

for(i in seq_along(years)){
  dataset_names <- nhanesTables('EXAM', year = years[i], namesonly=TRUE)
  extables[[i]] <- lapply(dataset_names, nhanes)
  names(extables[[i]]) <- dataset_names
}

names(extables) <- years

ex_dat <- combine_datasets(extables, id, 'SEQN')

save(extables, ex_dat, file = "data/cleaned/Examinations_v2.RData")

## Laboratory ####
labtables <- vector("list", length = length(years))

for(i in seq_along(years)){
  dataset_names <- nhanesTables('LAB', year = years[i], namesonly=TRUE)
  labtables[[i]] <- lapply(dataset_names, nhanes)
  names(labtables[[i]]) <- dataset_names
}

names(labtables) <- years
# Problematic - changed variable type, thus dropped: KID221	How old {were you/was SP} when {you were/he was} first told that {you/he} had prostate cancer?

labtables <- remove_variable(labtables, "KID211")


lab_dat <- combine_datasets(labtables, id, 'SEQN')

save(labtables, lab_dat, file = "data/cleaned/Laboratory_v2.RData")

## Demographics ####
demotables <- vector("list", length = length(years))

for(i in seq_along(years)){
  dataset_names <- nhanesTables('DEMO', year = years[i], namesonly=TRUE)
  demotables[[i]] <- lapply(dataset_names, nhanes)
  names(demotables[[i]]) <- dataset_names
}
names(demotables) <- years

demo_dat <- combine_datasets(demotables, id, 'SEQN')

save(demotables, demo_dat, file = "data/cleaned/Demographics_2.RData")

