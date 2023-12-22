library(tidyverse)
# Support functions####

combine_datasets <- function(survey_list, ids_vector, id_column_name) {
  # This function assumes 'survey_list' is structured with years as the first layer and dataframes as the second layer.
  
  final_datasets <- list()
  
  # Loop through each year
  for(year in names(survey_list)) {
    # Loop through each survey dataframe in the year
    for(survey_name in names(survey_list[[year]])) {
      
      # Skip if the dataset is NULL
      if(is.null(survey_list[[year]][[survey_name]])) {
        next
      }
      
      # Skip if the idcolumn is NULL
      if(is.null(survey_list[[year]][[survey_name]])) {
        next
      }
      
      # Skip if the dataset does not have the ID column
      if(!(id_column_name %in% names(survey_list[[year]][[survey_name]]))) {
        next
      }
      
      # Subset the dataset based on ids_vector
      survey_subset <- survey_list[[year]][[survey_name]] %>%
        filter(!!sym(id_column_name) %in% ids_vector)
      
      # Skip this dataset if it becomes empty after subsetting
      if(nrow(survey_subset) == 0) {
        next
      }
      
      if (startsWith(survey_name, "P_")) {
        # Remove "P_" 
        standardized_name <- substring(survey_name, 3)
      } else {
        # Standardize the survey name by removing the year and any suffixes
        standardized_name <- gsub(pattern = "_[A-Z]$", replacement = "", x = survey_name)
      }

      # Create an identifier for the year based on the survey_name
      survey_with_year <- survey_subset %>%
        mutate(Year = year)
      
      # If the dataset for the survey doesn't exist in the final datasets, initialize it
      if(!standardized_name %in% names(final_datasets)) {
        final_datasets[[standardized_name]] <- survey_with_year
      } else {
        # Find common columns between existing data and new data
        common_columns <- intersect(names(final_datasets[[standardized_name]]), names(survey_with_year))
        
        # Ensure 'Year' is always included as a common column
        common_columns <- union(common_columns, "Year")
        
        # Only keep the common columns and bind the rows
        combined_data <- bind_rows(
          select(final_datasets[[standardized_name]], common_columns),
          select(survey_with_year, common_columns)
        )
        
        # Only keep the combined data if it has rows
        if(nrow(combined_data) > 0) {
          final_datasets[[standardized_name]] <- combined_data
        } else {
          final_datasets[[standardized_name]] <- NULL
        }
      }
    }
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
  dataset_names <- NULL
  dataset_names <- nhanesTables('DIET', year = years[i], namesonly=TRUE)
  ditables[[i]] <- lapply(dataset_names, nhanes)
  names(ditables[[i]]) <- dataset_names
  
  for(j in seq_along(dataset_names)){
    vars <- names(ditables[[i]][[j]])
    ditables[[i]][[j]] <- nhanesTranslate(dataset_names[j], vars, data=ditables[[i]][[j]])
  }
}
names(ditables) <- years



di_dat <- combine_datasets(ditables, id, 'SEQN')

save(ditables, di_dat, file = "data/cleaned/Dietary.RData")

## Questionnaires ####
qtables <- vector("list", length = length(years))

for(i in seq_along(years)){
  dataset_names <- NULL
  dataset_names <- nhanesTables('Q', year = years[i], namesonly=TRUE)
  qtables[[i]] <- lapply(dataset_names, nhanes)
  names(qtables[[i]]) <- dataset_names
  
  for(j in seq_along(dataset_names)){
    vars <- names(qtables[[i]][[j]])
    qtables[[i]][[j]] <- nhanesTranslate(dataset_names[j], vars, data=qtables[[i]][[j]])
  }
}
names(qtables) <- years

q_dat <- combine_datasets(qtables, id, 'SEQN')

save(qtables, q_dat, file = "data/cleaned/Questionaires.RData")

## Examinations ####
extables <- vector("list", length = length(years))

for(i in seq_along(years)){
  dataset_names <- NULL
  dataset_names <- nhanesTables('EXAM', year = years[i], namesonly=TRUE)
  extables[[i]] <- lapply(dataset_names, nhanes)
  names(extables[[i]]) <- dataset_names
  
  for(j in seq_along(dataset_names)){
    vars <- names(extables[[i]][[j]])
    extables[[i]][[j]] <- nhanesTranslate(dataset_names[j], vars, data=extables[[i]][[j]])
  }
}

names(extables) <- years

ex_dat <- combine_datasets(extables, id, 'SEQN')

save(extables, ex_dat, file = "data/cleaned/Examinations.RData")

## Laboratory ####
labtables <- vector("list", length = length(years))

for(i in seq_along(years)){
  dataset_names <- NULL
  dataset_names <- nhanesTables('LAB', year = years[i], namesonly=TRUE)
  labtables[[i]] <- lapply(dataset_names, nhanes)
  names(labtables[[i]]) <- dataset_names
  
  for(j in seq_along(dataset_names)){
    vars <- names(labtables[[i]][[j]])
    labtables[[i]][[j]] <- nhanesTranslate(dataset_names[j], vars, data=labtables[[i]][[j]])
  }
}

names(labtables) <- years

lab_dat <- combine_datasets(labtables, id, 'SEQN')

save(labtables, lab_dat, file = "data/cleaned/Laboratory.RData")

## Demographics ####
demotables <- vector("list", length = length(years))

for(i in seq_along(years)){
  dataset_names <- NULL
  dataset_names <- nhanesTables('DEMO', year = years[i], namesonly=TRUE)
  demotables[[i]] <- lapply(dataset_names, nhanes)
  names(demotables[[i]]) <- dataset_names
  
  for(j in seq_along(dataset_names)){
    vars <- names(demotables[[i]][[j]])
    demotables[[i]][[j]] <- nhanesTranslate(dataset_names[j], vars, data=demotables[[i]][[j]])
  }
}
names(demotables) <- years

# Problematic DMDHHSIZ

demotables <- remove_variable(demotables, "DMDHHSIZ")

demo_dat <- combine_datasets(demotables, id, 'SEQN')

save(demotables, demo_dat, file = "data/cleaned/Demographics.RData")

