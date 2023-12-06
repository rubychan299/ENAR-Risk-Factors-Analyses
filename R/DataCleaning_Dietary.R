library(tidyverse)
library(haven)

# Support functions####
read_and_assign <- function(file_path) {
  # Extract the base name of the file without extension
  data_name <- tools::file_path_sans_ext(basename(file_path))
  # Read the .XPT file
  data <- read_xpt(file_path)
  # Assign it to a variable in the global environment
  assign(data_name, data, envir = .GlobalEnv)
}

# Function to rename datasets
rename_datasets <- function(name) {
  # Check if the name starts with "P_"
  if (startsWith(name, "P_")) {
    # Remove "P_" and append "_2017-20"
    new_name <- paste0(substring(name, 3), "_2017-20")
  } else {
    # Find the position of the last underscore
    last_underscore <- max(regexpr("_", name))
    # Extract the suffix
    suffix <- substring(name, last_underscore + 1)
    # Find the corresponding year
    year <- suffix_to_year[[suffix]]
    if (!is.null(year)) {
      # Remove the suffix (everything after the last underscore) from the original name
      name_without_suffix <- substring(name, 1, last_underscore - 1)
      # Construct new dataset name by appending the year
      new_name <- paste0(name_without_suffix,"_", year)
    } else {
      # If no matching year is found, return the original name
      return(name)
    }
  }
  # Assign the dataset to the new name
  assign(new_name, get(name), envir = .GlobalEnv)
  # Optionally, remove the old dataset
  rm(list = name, envir = .GlobalEnv)
}


# Load the Data####
dietary.sources <-  list.files("data/raw/Dietary", 
                               pattern="*.XPT$", full.names=TRUE, 
                               ignore.case=TRUE)


sapply(dietary.sources, read_and_assign)

# Rename datasets####

# Create a mapping from suffixes to cycle years
suffix_to_year <- list(
  B = "2001-02",
  C = "2003-04",
  D = "2005-06",
  E = "2007-08",
  F = "2009-10",
  G = "2011-12",
  H = "2013-14",
  I = "2015-16",
  J = "2017-18"
)

# Get all object names in the global environment
all_objects <- ls()

# Filter only those objects that are data frames (or similar structures like tibbles)
dataset_names <- all_objects[sapply(mget(all_objects, .GlobalEnv), function(x) is.data.frame(x) || is(x, "tbl"))]

# Apply the function to each dataset name
sapply(dataset_names, rename_datasets)





