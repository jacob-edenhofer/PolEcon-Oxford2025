

##############################################
# Define Functions -- Data Import
##############################################

# Function to import data from various file types
import_data <- function(file_path, verbose = FALSE) {
  # Skip files with extensions that aren't data files
  if (grepl("\\.(R(?!Data$)|docx|pdf|txt)$", file_path, perl = TRUE)) {
    if (verbose) log_info("Skipping non-data file: {file_path}")
    return(NULL)
  }
  
  # Extract the file extension
  ext <- tools::file_ext(file_path)
  
  # Initialise data as NULL
  data <- NULL
  
  # Use tryCatch to handle errors 
  tryCatch({
    # Switch based on file extension
    data <- switch(tolower(ext),
                   "csv" = {
                     if (verbose) log_info("Attempting to read CSV file: {file_path}")
                     data <- data.table::fread(file_path, showProgress = FALSE)
                     as_tibble(data)
                   },
                   "tsv" = {
                     if (verbose) log_info("Attempting to read TSV file: {file_path}")
                     data <- data.table::fread(file_path, sep = "\t", showProgress = FALSE)
                     as_tibble(data)
                   },
                   "xlsx" = {
                     if (verbose) log_info("Attempting to read XLSX file: {file_path}")
                     sheet_names <- readxl::excel_sheets(file_path)
                     data <- lapply(sheet_names, function(sheet) {
                       sheet_data <- readxl::read_xlsx(file_path, sheet = sheet)
                       janitor::clean_names(sheet_data)
                     })
                     names(data) <- sheet_names
                     data
                   },
                   "xls" = {
                     if (verbose) log_info("Attempting to read XLS file: {file_path}")
                     sheet_names <- readxl::excel_sheets(file_path)
                     data <- lapply(sheet_names, function(sheet) {
                       sheet_data <- readxl::read_xls(file_path, sheet = sheet)
                       janitor::clean_names(sheet_data)
                     })
                     names(data) <- sheet_names
                     data
                   },
                   "json" = {
                     if (verbose) log_info("Attempting to read JSON file: {file_path}")
                     data <- jsonlite::fromJSON(file_path)
                     as_tibble(data)
                   },
                   "xml" = {
                     if (verbose) log_info("Attempting to read XML file: {file_path}")
                     xml_data <- xml2::read_xml(file_path)
                     data <- xml2::as_list(xml_data)
                     as_tibble(data)
                   },
                   "rds" = {
                     if (verbose) log_info("Attempting to read RDS file: {file_path}")
                     readRDS(file_path)
                   },
                   "rdata" = {
                     if (verbose) log_info(paste("Attempting to read RData file:", file_path))
                     env <- new.env()
                     load(file_path, envir = env)
                     objects_list <- as.list(env)
                     if (length(objects_list) == 1) {
                       return(objects_list[[1]])  # Return the single object directly
                     } else {
                       return(objects_list)  # Return a named list of all objects
                     }
                     
                     # If there's only one object, return it directly
                     if (length(objects_list) == 1) {
                       return(objects_list[[1]])
                     } else {
                       return(objects_list)  # Return a named list of objects
                     }
                   },
                   "dta" = {
                     if (verbose) log_info("Attempting to read DTA file: {file_path}")
                     haven::read_dta(file_path)
                   },
                   "sav" = {
                     if (verbose) log_info("Attempting to read SAV file: {file_path}")
                     haven::read_sav(file_path)
                   },
                   "sas7bdat" = {
                     if (verbose) log_info("Attempting to read SAS file: {file_path}")
                     haven::read_sas(file_path)
                   },
                   "feather" = {
                     if (verbose) log_info("Attempting to read Feather file: {file_path}")
                     arrow::read_feather(file_path)
                   },
                   "parquet" = {
                     if (verbose) log_info("Attempting to read Parquet file: {file_path}")
                     arrow::read_parquet(file_path)
                   },
                   "shp" = {
                     if(verbose) log_info("Attempting to read Shapefile: {file_path}")
                     sf::st_read(file_path, quiet = !verbose)
                   },
                   "gpkg" = {
                     if (verbose) log_info(paste("Attempting to read GeoPackage file:", file_path))
                     sf::st_read(file_path, quiet = !verbose)
                   },
                   {
                     if (verbose) log_warn("Unsupported file type for: {file_path}")
                     NULL
                   }
    )
    
    # Clean column names if data was successfully read
    if (!is.null(data)) {
      data <- janitor::clean_names(data)
    }
    
    return(data)
  }, error = function(e) {
    log_error("Error reading file {basename(file_path)}: {e$message}")
    return(NULL)
  })
}

# Function to create a data name from file path
create_data_name <- function(file_path) {
  file_name <- file_path_sans_ext(basename(file_path))
  file_name <- str_replace_all(file_name, " ", "_")
  str_to_lower(str_replace_all(gsub("\\..*", "", file_name), "-", "_"))
}

# Function to get all files recursively from a directory
get_files <- function(base_dir = raw_data_dir, 
                      file_types = c("csv", "xlsx", "xls", "rds", "dta", "sav", "sas7bdat", "shp", "RData", 
                                     "parquet", "arrow", "feather", "json", "xml", "rdata", "gpkg"), 
                      recursive = TRUE) {
  # Create a regular expression pattern based on the file types
  pattern <- paste0("\\.(", paste(file_types, collapse = "|"), ")$")
  
  # List all items in the base directory
  all_items <- list.files(base_dir, full.names = TRUE)
  
  # Initialize a list to hold file paths
  file_paths <- list()
  
  # Iterate through each item
  for (item in all_items) {
    if (dir.exists(item)) {
      # If the item is a directory, list its files recursively
      files <- list.files(item, full.names = TRUE, recursive = recursive, pattern = pattern)
      file_paths <- c(file_paths, files)
    } else {
      # If it's a file, check if it matches the pattern
      if (grepl(pattern, item)) {
        file_paths <- c(file_paths, item)
      }
    }
  }
  file_paths <- unlist(file_paths)
  return(file_paths)
}


# Function to load and assign data to a list
load_and_assign_data <- function(file_path, data_list, verbose = FALSE) {
  data_name <- create_data_name(file_path)
  data <- import_data(file_path, verbose = verbose)
  
  if (!is.null(data)) {
    data_list[[data_name]] <- data
  }
  
  return(data_list)
}


# Function for adding iso3 codes to all datasets in the list and ensuring that the year column is numeric
add_iso3_and_numeric_year <- function(data_list) {
  for (data_name in names(data_list)) {
    data <- data_list[[data_name]]
    
    # Check if data is a dataframe or a list of dataframes
    if (is.data.frame(data)) {
      # Proceed with the existing logic for dataframes
      data <- process_dataframe(data, data_name)
    } else if (is.list(data) && all(sapply(data, is.data.frame))) {
      # If it's a list, iterate over each dataframe in the list
      for (i in seq_along(data)) {
        sub_data_name <- paste(data_name, i, sep = "_")
        data[[i]] <- process_dataframe(data[[i]], sub_data_name)
      }
    } else {
      log_error("{data_name} is neither a dataframe nor a list of dataframes.")
      next
    }
    
    # Save the modified data back to the list
    data_list[[data_name]] <- data
  }
  
  return(data_list)
}

# Helper function to process individual dataframes
process_dataframe <- function(data, data_name) {
  # Add iso3 codes if they do not already exist
  if (!any(grepl("iso", names(data), ignore.case = TRUE))) {
    # Extract potential country columns based on regular expression
    country_columns <- names(data)[grep("country|ctry", names(data), ignore.case = TRUE)]
    
    # Prioritize columns with "name" in their name, such as "country_name"
    prioritized_country_column <- country_columns[grepl("name", country_columns, ignore.case = TRUE)]
    
    # New step: prioritize column explicitly named "country"
    if ("country" %in% country_columns) {
      country_column <- "country"
      log_info("Using 'country' column in {data_name}")
    } else if (length(prioritized_country_column) == 1) {
      country_column <- prioritized_country_column
      log_info("Using country column: {country_column} in {data_name}")
    } else if (length(country_columns) == 1) {
      country_column <- country_columns
      log_info("Using country column: {country_column} in {data_name}")
    } else if (length(country_columns) > 1) {
      log_warn("Multiple potential country columns found in {data_name}: {paste(country_columns, collapse = ', ')}. Selecting the first prioritized or matched one.")
      country_column <- prioritized_country_column[1] %||% country_columns[1]
    } else {
      log_error("No country column found in {data_name}")
      return(data)
    }
    
    # Check for NA values and the length of entries in the country column
    if (all(is.na(data[[country_column]]))) {
      log_error("The country column in {data_name} is entirely NA.")
      return(data)
    }
    
    country_max_length <- suppressWarnings(max(str_length(data[[country_column]]), na.rm = TRUE))
    
    # If-else statement for length of entries in country column
    if (!is.na(country_max_length) && country_max_length > 3) {
      data <- data %>%
        mutate(iso3 = countrycode(data[[country_column]], origin = "country.name", destination = "iso3c"))
    } else if (!is.na(country_max_length)) {
      data <- data %>%
        mutate(iso3 = countrycode(data[[country_column]], origin = "iso3c", destination = "iso3c"))
    } else {
      log_error("Unable to determine country column format in {data_name}.")
      return(data)
    }
  }
  
  # Ensure year column is numeric if it exists
  if ("year" %in% names(data)) {
    data$year <- suppressWarnings(as.numeric(as.character(data$year)))
    
    if (any(is.na(data$year))) {
      log_warn("Some year values could not be converted to numeric in {data_name}.")
    }
  }
  
  return(data)
}


# Function for saving data 
save_data <- function(merged_list, extension, cleaned_data_dir, timeout = 60, tolerance = 0.001) {
  for (data_name in names(merged_list)) {
    data <- merged_list[[data_name]]
    conversion_error <- FALSE  # Flag to track conversion success
    
    ## Check for geometry columns
    geometry_columns <- sapply(data, function(col) inherits(col, "sfc"))
    
    if (any(geometry_columns) && extension %in% c("feather", "parquet", "csv", "xlsx", "dta")) {
      tryCatch({
        withTimeout({
          # Convert geometry columns to WKT for unsupported formats
          data <- data %>%
            mutate(across(which(geometry_columns), ~ {
              tryCatch({
                if (inherits(., "sfc")) {
                  # Convert MULTIPOLYGON or other sfc types to WKT
                  st_as_text(st_simplify(., dTolerance = tolerance)) 
                } else {
                  log_warn(paste("Column", cur_column(), "is not a recognized geometry type. Skipping conversion for", data_name))
                  . # Return column unchanged if not recognized as geometry
                }
              }, error = function(e) {
                log_warn(paste("Error converting column", cur_column(), "for", data_name, ":", e$message))
                conversion_error <<- TRUE  # Set flag to indicate error
                return(NA)  # Replace with NA on error
              })
            }))
        }, timeout = timeout, onTimeout = "error")  # Use the timeout argument
        log_info(paste("Converted geometry columns to WKT for", data_name, "because", extension, "format requires text representation of geometries."))
      }, error = function(e) {
        log_warn(paste("Error converting geometry columns for", data_name, ":", e$message))
        conversion_error <- TRUE
      })
    }
    
    if (conversion_error) {
      log_warn(paste("Skipping saving for", data_name, "due to errors in geometry conversion."))
      next # Skip saving if there was an error
    }
    
    ## Sanitise column names for GeoPackage
    if (extension == "gpkg") {
      colnames(data) <- make.names(colnames(data), unique = TRUE)  # Make unique and valid column names
    }
    
    ## Use switch to save data in different formats
    switch(extension,
           "rds" = {
             saveRDS(data, paste0(here(), cleaned_data_dir, data_name, ".rds"))
           },
           "csv" = {
             readr::write_csv(data, paste0(here(), cleaned_data_dir, data_name, ".csv"))
           },
           "xlsx" = {
             openxlsx::write.xlsx(data, paste0(here(), cleaned_data_dir, data_name, ".xlsx"))
           },
           "parquet" = {
             arrow::write_parquet(data, paste0(here(), cleaned_data_dir, data_name, ".parquet"))
           },
           "dta" = {
             haven::write_dta(data, paste0(here(), cleaned_data_dir, data_name, ".dta"))
           },
           "feather" = {
             arrow::write_feather(data, paste0(here(), cleaned_data_dir, data_name, ".feather"))
           },
           "gpkg" = {
             tryCatch({
               sf::st_write(data, paste0(here(), cleaned_data_dir, data_name, ".gpkg"), layer = data_name, delete_layer = TRUE)
             }, error = function(e) {
               log_warn(paste("Error saving", data_name, "to GeoPackage:", e$message))
             })
           },
           {
             log_warn(paste("Unsupported file type for:", data_name))
           }
    )
  }
}

# Define function for shortening file names
shorten_file_name <- function(file_name, max_words = 10, max_chars = 50) {
  # Remove special characters and keep only alphanumeric, underscores, hyphens
  clean_name <- gsub("[^a-zA-Z0-9_-]", "", file_name)
  
  # Split the file name into words based on underscores or hyphens
  words <- unlist(strsplit(clean_name, "[-_]"))
  
  # Keep up to max_words words and ensure the name is no longer than max_chars
  truncated_name <- paste0(words[1:min(max_words, length(words))], collapse = "_")
  
  # Ensure the truncated name doesn't exceed the max_chars limit
  if (nchar(truncated_name) > max_chars) {
    truncated_name <- substr(truncated_name, 1, max_chars)
  }
  
  return(truncated_name)
}

# Define a function to prompt the user for CSV writing method
ask_for_csv_method <- function() {
  repeat {
    answer <- tolower(readline("Which method would you like to use for writing CSV? (type '1' for readr::write_csv, '2' for arrow::write_csv_arrow): "))
    if (answer %in% c("1", "2")) {
      return(answer)
    } else {
      message("Invalid input. Please type '1' for readr::write_csv or '2' for arrow::write_csv_arrow.")
    }
  }
}

# Define the save_data_simple function
save_data_simple <- function(data, file_name, file_type) {
  # # Apply the shortening function to the file name
  # file_name <- shorten_file_name(file_name)
  # log_info("Saving data using the shortened file name: {file_name}")
  
  switch(tolower(file_type),  # Use tolower to make file type case-insensitive
         "rds" = {
           saveRDS(data, paste0(file_name, ".rds"))
         },
         "rdata" = {
           # Save as .RData (can save multiple objects, so wrap in list)
           save(data, file = paste0(file_name, ".RData"))
         },
         "csv" = {
           # Ask user which CSV method to use
           if (interactive()) {
             method <- ask_for_csv_method()
             if (method == "1") {
               readr::write_csv(data, paste0(file_name, ".csv"))
             } else if (method == "2") {
               arrow::write_csv_arrow(data, paste0(file_name, ".csv"))
             }
           } else {
             # Default to readr::write_csv if not interactive
             readr::write_csv(data, paste0(file_name, ".csv"))
           }
         },
         "xlsx" = {
           writexl::write_xlsx(data, paste0(file_name, ".xlsx"))
         },
         "parquet" = {
           arrow::write_parquet(data, paste0(file_name, ".parquet"))
         },
         "dta" = {
           haven::write_dta(data, paste0(file_name, ".dta"))
         },
         "sav" = {
           haven::write_sav(data, paste0(file_name, ".sav"))  # SPSS format
         },
         "feather" = {
           arrow::write_feather(data, paste0(file_name, ".feather"))
         },
         "gpkg" = {
           sf::st_write(data, paste0(file_name, ".gpkg"), 
                        layer = sub("\\..*$", "", basename(file_name)), 
                        delete_layer = TRUE)
         },
         "json" = {
           jsonlite::write_json(data, paste0(file_name, ".json"), pretty = TRUE)
         },
         "txt" = {
           write.table(data, paste0(file_name, ".txt"), sep = "\t", row.names = FALSE)
         },
         {
           log_warn(paste("Unsupported file type:", file_type, "for file:", file_name))
         }
  )
}

# Define function for saving plots with error handling
save_plot <- function(plot, path, width = 9, height = 6, dpi = 300) {
  tryCatch(
    {
      ggsave(filename = path, plot = plot, width = width, height = height, dpi = dpi)
      message("Plot saved successfully at ", path)
    },
    error = function(e) {
      message("Failed to save plot at ", path, ": ", e$message)
    }
  )
}



#############################################
# Data wrangling functions
#############################################

clean_numeric_column <- function(x) {
  x %>%
    str_trim("both") %>%                # Trim whitespace
    str_replace_all("\\.", "") %>%      # Remove dots
    str_replace_all(",", ".") %>%       # Replace commas with dots
    as.numeric()                        # Convert to numeric
}


##############################################
# Define Function for Importing Eurostat Data
##############################################

# Main function
eurostat_custom_import <- function(code, eurostat_list, directory, file_type, apply_labels = TRUE, width = 40) {
  
  # Ensure the main directory exists
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
    log_info(paste("Created directory:", directory))
  } else {
    log_info(paste("Directory already exists:", directory))
  }
  
  # Logging: Print the input pattern
  log_info(paste("Input code:", code))
  
  # Load eurostat_toc if it is not already loaded
  if (!exists("eurostat_toc")) {
    eurostat_toc <- eurostat::get_eurostat_toc()
  }
  
  # Extract code from the list using exact matching
  code <- eurostat_toc$code[code == eurostat_toc$code]
  
  # Logging: Check matched codes
  log_info(paste("Matched codes:", paste(code, collapse = ", ")))
  
  ## Ensure that if there are more than two non-unique elements, one is selected
  if (length(code) > 1 & all(code == code[1])) {
    code <- code[1]
  } else if (length(code) == 1) {
    code <- code[1]
  } else {
    log_info("No valid code found. Returning unchanged list.")
    return(eurostat_list)
  }
  
  log_info(paste("Selected code:", code))
  
  # Extract long name 
  long_name <- eurostat_toc$title[eurostat_toc$code == code][1]
  data_name_long <- long_name %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9 ]", "") %>% # Remove all characters that are not alphanumeric or spaces
    str_replace_all(" +", "_") %>%
    str_wrap(width = 40)
  
  # Determine whether the code corresponds to a table, folder, or dataset
  type <- eurostat_toc$type[eurostat_toc$code == code]
  
  # Logging: Check matched types
  log_info(paste("Matched types:", paste(type, collapse = ", ")))
  
  if (length(type) > 1 & all(type == type[1])) {
    type <- type[1]
  } else if (length(type) == 1) {
    type <- type[1]
  } else {
    log_info("No valid type found. Returning unchanged list.")
    return(eurostat_list)
  }
  
  log_info(paste("Selected type:", type))
  
  # Initialize data as NULL
  data <- NULL
  
  # Use switch() function to handle different types
  data <- switch(type,
                 "table" = {
                   log_info("Retrieving table data...")
                   data <- get_eurostat(id = code, type = "code", time_format = "raw", use.data.table = T)
                   
                   if (is.null(data)) {
                     log_info("No table data retrieved.")
                   } else {
                     log_info("Table data retrieved.")
                     if (apply_labels) {
                       data <- label_eurostat_tables(data)
                     }
                   }
                   data
                 },
                 "folder" = {
                   log_info("Retrieving folder data...")
                   
                   folder_data <- get_eurostat_folder(code = code, env = .GlobalEnv)
                   if (is.null(folder_data)) {
                     log_info("No data retrieved from folder. Returning unchanged list.")
                     return(eurostat_list)
                   }
                   
                   # Save each item in the folder directly to the main directory
                   for (item in names(folder_data)) {
                     item_data <- folder_data[[item]]
                     
                     # Debugging: Check if item_data is not NULL
                     log_info(paste("Processing item:", item))
                     if (is.null(item_data)) {
                       log_info(paste("Item data is NULL for item:", item))
                       next
                     } else {
                       log_info(paste("Item data is valid for item:", item))
                     }
                     
                     # Save item to file
                     item_file_name <- file.path(directory, paste0(data_name_long, "_", item, ".", file_type))
                     log_info(paste("Saving item to path:", item_file_name))
                     
                     # Use tryCatch to handle any errors during saving
                     tryCatch({
                       save_data_simple(item_data, item_file_name, file_type)
                       log_info(paste("Saved folder item:", item_file_name))
                     }, error = function(e) {
                       log_info(paste("Error saving item:", item, "Error:", e$message))
                     })
                   }
                   folder_data
                 },
                 "dataset" = {
                   log_info("Retrieving dataset data...")
                   data <- get_eurostat(id = code, time_format = "raw", type = "code", use.data.table = T)
                   
                   if (is.null(data)) {
                     log_info("No dataset data retrieved.")
                   } else {
                     log_info("Dataset data retrieved.")
                     if (apply_labels) {
                       data <- label_eurostat(data, fix_duplicated = TRUE)
                     }
                   }
                   data
                 },
                 {
                   log_info("Type did not match 'table', 'folder', or 'dataset'. No data retrieved.")
                   NULL
                 }
  )
  
  # Logging: Check if data retrieval was successful
  if (is.null(data) & type != "folder") {
    log_info("No data, folder, or table retrieved. Returning unchanged list.")
  } else {
    log_info("Data retrieved successfully.")
    
    # Save the dataset to the specified directory
    if (type != "folder" && !is.null(data)) {
      # file_name <- file.path(directory, paste0(data_name_long, ".", file_type))
      file_name <- file.path(directory, data_name_long)
      log_info(paste("Saving dataset to:", file_name))
      
      # Use tryCatch to handle any errors during saving
      tryCatch({
        save_data_simple(data, file_name, file_type)
        log_info(paste("Data saved to:", file_name))
      }, error = function(e) {
        log_info(paste("Error saving dataset. Error:", e$message))
      })
    }
  }
  
  # Only add to the list if data is not NULL
  if (!is.null(data)) {
    eurostat_list[[data_name_long]] <- data
    log_info(paste("List updated with code:", data_name_long))
  }
  
  # Add a log to confirm the size of the updated list
  length_list <- length(eurostat_list)
  log_info(paste("Updated list size:", length_list))
  
  return(eurostat_list)  # Return the updated list
}



##############################################
# Define Function for Descriptive Statistics
##############################################

# Function to prune data by column limit
prune_data <- function(data, column_limit = 250, seed = 123) {
  # the column limit is set to 250 by default because datasummary_skim doesn't display more than 250 columns
  if (ncol(data) > column_limit) {
    set.seed(seed)
    columns_included <- sample(names(data), column_limit, replace = FALSE)
    data <- data %>% select(columns_included)
  } else {
    # Log message before returning the data
    message("No columns were pruned.")
  }
  return(data)
}

# Enhanced helper function for extracting and converting value labels
convert_labels_replace <- function(data) {
  # Step 1: Process labelled columns
  is_labelled_column <- function(x) {
    inherits(x, "haven_labelled") || labelled::is.labelled(x)
  }
  
  lbl_columns <- names(data)[purrr::map_lgl(data, is_labelled_column)]
  
  if (length(lbl_columns) > 0) {
    message("Processing labelled columns:")
    # print(lbl_columns)
    for (col_name in lbl_columns) {
      # Check if all non-NA values in the column are numeric-like
      non_na_values <- as.character(data[[col_name]][!is.na(data[[col_name]])])
      if (inherits(data[[col_name]], "double") && all(grepl("^[0-9]+$", non_na_values))) {
        message(paste("Converting labelled column to numeric:", col_name))
        data[[col_name]] <- as.numeric(data[[col_name]])
      } else {
        message(paste("Converting labelled column to factor:", col_name))
        data[[col_name]] <- haven::as_factor(data[[col_name]], levels = "label")
      }
    }
  } else {
    message("No labelled columns found to process.")
  }
  
  # Step 2: Process character columns
  char_columns <- names(data)[purrr::map_lgl(data, is.character)]
  
  if (length(char_columns) > 0) {
    message("Processing character columns:")
    # print(char_columns)
    for (col_name in char_columns) {
      # Check if all non-NA values in the column are numeric-like
      non_na_values <- data[[col_name]][!is.na(data[[col_name]])]
      if (all(grepl("^[0-9]+$", non_na_values))) {
        message(paste("Converting character column to numeric:", col_name))
        data[[col_name]] <- as.numeric(data[[col_name]])
      } else {
        message(paste("Converting character column to factor:", col_name))
        data[[col_name]] <- as.factor(data[[col_name]])
      }
    }
  } else {
    message("No character columns found to process.")
  }
  
  # Return the processed data
  return(data)
}



# Function for summarising data with more flexibility
summarise_data <- function(data_list, 
                           column_limit = 250, 
                           output_format = "html", 
                           save_to_file = TRUE, 
                           output_directory, 
                           filter_pattern = NULL, 
                           selected_columns = NULL, 
                           perl = TRUE, 
                           render_to_viewer = FALSE, 
                           include_all_data = FALSE, 
                           var_dict = NULL) {
  
  # Ensure exactly one of filter_pattern or selected_columns is specified
  if (!xor(is.null(filter_pattern), is.null(selected_columns))) {
    stop("You must provide either a filter_pattern or selected_columns, but not both.")
  }
  
  # Check if output_directory is provided when save_to_file is TRUE
  if (save_to_file && missing(output_directory)) {
    stop("You must provide an output_directory when save_to_file is TRUE.")
  }
  
  summary_list <- list()
  
  # Ensure output directory exists if saving to file
  if (save_to_file && !dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }
  
  for (data_name in names(data_list)) {
    # Extract data from list
    data <- data_list[[data_name]]
    
    # Select columns based on selected_columns or filter_pattern
    if (!is.null(selected_columns)) {
      # Ensure selected_columns is treated as a regex
      column_filter <- function(names) grepl(selected_columns, names, perl = perl)
      column_names_interest <- names(data)[column_filter(names(data))]
      
      # Skip if no matching columns found
      if (length(column_names_interest) == 0) {
        warning(paste("No matching columns found in", data_name, "for the specified selected_columns. Skipping."))
        next
      }
    } else {
      # Use filter_pattern to select columns
      column_filter <- function(names) !grepl(filter_pattern, names, perl = perl)
      column_names_interest <- names(data)[column_filter(names(data))]
      
      if (length(column_names_interest) == 0) {
        warning(paste("No matching columns found in", data_name, "for the specified filter_pattern. Skipping."))
        next
      }
    }
    
    # Subset data to include only the relevant columns
    data <- data %>% select(all_of(column_names_interest))
    
    # Convert dbl+lbl columns to factors with labels
    data <- convert_labels_replace(data)
    
    # If include_all_data is FALSE, retain only numeric columns
    if (!include_all_data) {
      data <- data %>% select(where(is.numeric))
      
      # Skip datasets with no numeric variables
      if (nrow(data) == 0 || ncol(data) == 0) {
        warning(paste("No numeric variables found in", data_name, ". Skipping."))
        next
      }
    }
    
    # Prune data if necessary
    data <- prune_data(data, column_limit = column_limit)
    
    # Attach long variable names from var_dict
    if (!is.null(var_dict)) {
      message("Attaching long variable names from var_dict...")
      for (col_name in names(data)) {
        # Debug: Check if col_name exists in var_dict
        if (col_name %in% names(var_dict)) {
          message(paste("Attaching label for column:", col_name))
          # Attach the long name as an attribute
          attr(data[[col_name]], "label") <- var_dict[[col_name]]
        } else {
          message(paste("Column not found in var_dict:", col_name))
        }
      }
      message("Long variable names attached for dataset.")
    } else {
      message("No var_dict provided. Skipping reattachment of long variable names.")
    }
    
    # Generate summary stats table in the specified format
    summary_output <- modelsummary::datasummary_skim(data = data, 
                                                     output = output_format, 
                                                     title = data_name)
    
    # Check that object is not empty
    if (is.null(summary_output)) {
      warning(paste("No summary output generated for", data_name, ". Skipping."))
      next
    }
    
    # Render to Viewer explicitly if specified
    if (render_to_viewer && output_format == "html") {
      if (interactive()) {
        print(summary_output)  # Ensure the object is rendered interactively
      }
    }
    
    # Store the summary in the list
    summary_list[[data_name]] <- summary_output
    
    # Save to file if required
    if (save_to_file) {
      summary_file <- file.path(output_directory, paste0(data_name, "_summary.", output_format))
      
      if (output_format == "html") {
        # Convert the output to HTML string and write to file
        html_output <- as.character(summary_output)
        writeLines(html_output, summary_file)
      } else if (output_format %in% c("markdown", "latex", "text")) {
        # For other formats, convert to character and save
        text_output <- capture.output(print(summary_output))
        writeLines(text_output, summary_file)
      } else {
        stop("Unsupported output format")
      }
    }
  }
  
  return(summary_list)
}


##############################################
# Webscraping Functions 
##############################################

# Function to create safe file names by replacing or removing invalid characters
normalise_filename <- function(filename, download_dir) {
  # Convert non-ASCII characters to their closest ASCII equivalents
  safe_filename <- stringi::stri_trans_general(filename, "latin-ascii")
  
  # Separate the filename into the base name and the extension
  file_base <- tools::file_path_sans_ext(safe_filename)  # Base filename without extension
  file_ext <- tools::file_ext(safe_filename)  # File extension
  
  # Replace or remove any remaining invalid characters in the base name (e.g., slashes, colons, etc.)
  file_base <- gsub("[^a-zA-Z0-9_-]", "_", file_base)  # Keep only alphanumeric characters, dashes, and underscores
  
  # Rebuild the safe filename with the extension preserved
  if (file_ext != "") {
    safe_filename <- paste0(file_base, ".", file_ext)
  } else {
    safe_filename <- file_base
  }
  
  # Ensure filename uniqueness by appending a number if necessary
  dest_file <- file.path(download_dir, safe_filename)
  original_filename <- safe_filename
  i <- 1
  while (file.exists(dest_file)) {
    # If file exists, append a number to make it unique
    safe_filename <- paste0(file_base, "_", i, if (file_ext != "") paste0(".", file_ext) else "")
    dest_file <- file.path(download_dir, safe_filename)
    i <- i + 1
  }
  
  return(safe_filename)
}


# Function to normalize and standardize URLs (adding separator if necessary)
normalise_url <- function(url) {
  
  # Parse the URL using httr::parse_url
  parsed_url <- tryCatch({
    httr::parse_url(url)
  }, error = function(e) {
    message("Error parsing URL: ", e$message)
    return(NULL)
  })
  
  if (is.null(parsed_url)) {
    return(NULL)  # Return NULL if the URL couldn't be parsed
  }
  
  # Force HTTPS
  if (!is.null(parsed_url$scheme)) {
    parsed_url$scheme <- "https"
  }
  
  # Remove www subdomain
  parsed_url$hostname <- gsub("^www\\.", "", parsed_url$hostname)
  
  # Remove query parameters that are irrelevant (optional: can be more selective)
  if (!is.null(parsed_url$query)) {
    parsed_url$query <- NULL  # Optional: adjust this to remove only tracking params if needed
  }
  
  # Remove fragment
  parsed_url$fragment <- NULL
  
  # Rebuild the normalized URL
  normalised_url <- httr::build_url(parsed_url)
  
  # Remove trailing slashes
  normalised_url <- gsub("/$", "", normalised_url)
  
  return(normalised_url)
}


# Function for hasing URLs
hash_url <- function(url) {
  # Normalize the URL first
  normalised_url <- normalise_url(url)
  
  # Generate MD5 hash of the normalized URL
  url_hash <- digest::digest(normalised_url, algo = "md5")
  
  return(url_hash)
}



# Function to make a URL absolute given a base URL
make_absolute_url <- function(link, base_url) {
  parsed_base <- httr::parse_url(base_url)
  
  # If the link is already absolute (starts with http), return the normalized version
  if (grepl("^http", link)) {
    return(normalise_url(link))
  }
  
  # Handle base URL scheme
  if (is.null(parsed_base$scheme)) {
    parsed_base$scheme <- "https"
  }
  
  # If the link starts with '/', append it to the domain (root URL)
  if (grepl("^/", link)) {
    root_url <- paste0(parsed_base$scheme, "://", gsub("^www\\.", "", parsed_base$hostname))
    return(normalise_url(paste0(root_url, link)))
  }
  
  # For relative links that don't start with '/', append to base URL path
  return(normalise_url(httr::modify_url(base_url, path = link)))
}



# Improved function for downloading files with retries
attempt_download <- function(download_link, dest_file, max_retries = 3) {
  for (i in 1:max_retries) {
    tryCatch({
      download.file(download_link, destfile = dest_file, mode = "wb")
      return(TRUE)
    }, error = function(e) {
      message(paste0("Attempt ", i, " failed for: ", download_link, " - ", e$message))
      Sys.sleep(2^i)  # Exponential backoff
    })
  }
  return(FALSE)
}


# Function to check if the content type is valid by inspecting headers before download
is_valid_content_type <- function(content_type) {
  # List of valid content types to support a wide range of file formats
  valid_content_types <- c(
    "application/pdf", 
    "application/msword", 
    "application/vnd.openxmlformats-officedocument.wordprocessingml.document", 
    "application/vnd.ms-excel", 
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", 
    "application/zip", 
    "text/csv",
    "text/html",
    "image/jpeg",     # Added image support
    "image/png",
    "video/mp4",      # Added video support
    "application/octet-stream"  # Generic binary data
  )
  
  # Check if the content type is in the list of valid content types
  return(content_type %in% valid_content_types)
}


# Function to download a file from a URL
handle_download <- function(download_link, base_url, download_dir, failed_downloads, downloaded_basenames, download_unknown = FALSE, timeout_seconds = 10, log_file) {
  # Normalise and standardise the URL
  download_link <- make_absolute_url(download_link, base_url)
  
  # Check if download_link is valid after normalisation
  if (is.null(download_link) || download_link == "") {
    message("Skipping malformed URL: ", download_link)
    return()
  }
  
  # Parse the URL to check for validity
  parsed_url <- tryCatch({
    httr::parse_url(download_link)
  }, error = function(e) {
    message("Error parsing URL: ", e$message)
    failed_downloads <<- append(failed_downloads, list(list(link = download_link, error = "Malformed URL")))
    return(NULL)
  })
  
  if (is.null(parsed_url)) {
    return()  # Exit if URL parsing failed
  }
  
  # Extract the basename from the URL
  basename <- URLdecode(basename(download_link))
  
  # Check if this basename has already been downloaded
  if (basename %in% downloaded_basenames) {
    message("Skipping already downloaded file with basename: ", basename)
    return()
  }
  
  # Create safe file names by handling special characters
  normalised_filename <- normalise_filename(basename, download_dir)
  dest_file <- file.path(download_dir, normalised_filename)
  
  # Check if the file already exists locally
  if (file.exists(dest_file)) {
    message("File already exists locally: ", dest_file)
    return()
  }
  
  # Send a HEAD request to check the content type and status, adding a timeout
  response_head <- tryCatch({
    httr::HEAD(download_link, timeout(timeout_seconds))
  }, error = function(e) {
    message("Error in HEAD request or timed out: ", e$message)
    failed_downloads <<- append(failed_downloads, list(list(link = download_link, error = "Timeout exceeded or SSL issue")))
    write_lines(paste("[FAILED] Timeout or SSL error for URL:", download_link), log_file, append = TRUE)
    return(NULL)
  })
  
  if (is.null(response_head)) {
    return()
  }
  
  # Check for HTTP errors (e.g., 404)
  if (httr::status_code(response_head) == 404) {
    message("404 Not Found: ", download_link)
    failed_downloads <<- append(failed_downloads, list(list(link = download_link, error = "404 Not Found")))
    write_lines(paste("[FAILED] 404 Not Found for URL:", download_link), log_file, append = TRUE)
    return()
  }
  
  # Proceed if no error and valid content type
  content_type <- httr::http_type(response_head)
  ext <- switch(content_type,
                "application/pdf" = ".pdf",
                "application/zip" = ".zip",
                "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = ".docx",
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = ".xlsx",
                "application/msword" = ".doc",
                "application/vnd.ms-excel" = ".xls",
                "text/csv" = ".csv",
                "text/html" = ".html",
                "image/jpeg" = ".jpg",  # Added image content types
                "image/png" = ".png",
                "video/mp4" = ".mp4",   # Added video content types
                "application/octet-stream" = ".bin",  # Generic binary data
                NULL)
  
  # If extension is still NULL, try to infer it from the URL
  if (is.null(ext)) {
    ext_from_url <- tools::file_ext(parsed_url$path)
    
    # Check if the URL contains a valid extension
    if (ext_from_url %in% c("pdf", "docx", "zip", "jpg", "png", "mp4", "xls", "xlsx", "csv", "html")) {
      ext <- paste0(".", ext_from_url)
    }
  }
  
  # If extension is still NULL, skip the file unless downloading unknown types is allowed
  if (is.null(ext)) {
    if (!download_unknown) {
      message("Skipping unsupported content type: ", content_type, " for URL: ", download_link)
      return()
    } else {
      ext = ".unknown"
    }
  }
  
  # Append the correct extension to the destination file
  dest_file_with_ext <- paste0(dest_file, ext)
  
  # Handle text/html differently
  if (content_type == "text/html") {
    message("Downloading and processing HTML content: ", download_link)
    
    # Download the HTML content with a timeout
    response <- tryCatch({
      httr::GET(download_link, timeout(timeout_seconds))
    }, error = function(e) {
      message("Error downloading HTML content or timed out: ", e$message)
      failed_downloads <<- append(failed_downloads, list(list(link = download_link, error = "Timeout exceeded or SSL issue")))
      write_lines(paste("[FAILED] Timeout or SSL error for URL:", download_link), log_file, append = TRUE)
      return(NULL)
    })
    
    if (is.null(response)) {
      return()
    }
    
    # Check for 404 status in GET request
    if (httr::status_code(response) == 404) {
      message("404 Not Found: ", download_link)
      failed_downloads <<- append(failed_downloads, list(list(link = download_link, error = "404 Not Found")))
      write_lines(paste("[FAILED] 404 Not Found for URL:", download_link), log_file, append = TRUE)
      return()
    }
    
    # Check for 429 status in GET request
    if (httr::status_code(response_head) == 429) {
      message("429 Too Many Requests - retrying after delay...")
      Sys.sleep(10)  # Wait for 10 seconds before retrying
      return(handle_download(download_link, base_url, download_dir, failed_downloads, downloaded_basenames, download_unknown, timeout_seconds, log_file))  # Retry after delay
    }
    
    # Parse the HTML content
    html_content <- httr::content(response, as = "text", encoding = "UTF-8")
    html_document <- rvest::read_html(html_content)
    
    # Extract html and save as md file
    tryCatch({
      # Extract structured text preserving headings and lists
      html_text <- html_document %>% 
        html_nodes("h1, h2, h3, p, li") %>% 
        map_chr(~ {
          node <- .x
          if (html_name(node) == "h1") {
            return(paste("\n# ", html_text(node), "\n"))  # Add # for major headings
          } else if (html_name(node) == "h2") {
            return(paste("\n## ", html_text(node), "\n"))  # Add ## for sub-headings
          } else if (html_name(node) == "li") {
            return(paste("- ", html_text(node)))  # Add bullet points for list items
          } else {
            return(html_text(node))
          }
        }) %>%
        paste(collapse = "\n")
      
      # Save the structured text to a file
      writeLines(html_text, paste0(dest_file, ".md"))  # Save as markdown (.md) file for better formatting
      message("HTML content saved as structured text: ", dest_file, ".md")
      write_lines(paste("[SUCCESS] HTML saved for URL:", download_link), log_file, append = TRUE)
      
    }, error = function(e) {
      message("Error extracting and saving HTML content: ", e$message)
      write_lines(paste("[FAILED] Error extracting HTML for URL:", download_link), log_file, append = TRUE)
    })
    
    # Add to downloaded basenames
    downloaded_basenames <<- c(downloaded_basenames, basename)
    
  } else {
    # Attempt to download other types of files with retries
    if (attempt_download(download_link, dest_file_with_ext, max_retries = 1)) {
      message("Successfully downloaded and renamed to: ", dest_file_with_ext)
      downloaded_basenames <<- c(downloaded_basenames, basename)
      write_lines(paste("[SUCCESS] File downloaded for URL:", download_link), log_file, append = TRUE)
    } else {
      failed_downloads <<- append(failed_downloads, list(list(link = download_link, error = "Download failed after retries")))
      write_lines(paste("[FAILED] Download failed for URL:", download_link), log_file, append = TRUE)
    }
  }
}

# Function to scrape and download files
scrape_and_download_files <- function(page_url, base_url, download_dir, failed_downloads, downloaded_basenames, extensions, log_file, ssl_verify = TRUE) {
  
  # Fetch the HTML page using httr::GET with optional SSL verification
  message("Fetching page: ", page_url)
  ssl_config <- if (ssl_verify) httr::config(ssl_verifypeer = TRUE) else httr::config(ssl_verifypeer = FALSE)
  
  page <- tryCatch({
    response <- httr::GET(page_url, ssl_config)
    
    # Handle 429 Too Many Requests explicitly
    if (httr::status_code(response) == 429) {
      message("429 Too Many Requests - retrying after delay...")
      Sys.sleep(10)  # Wait for 10 seconds before retrying
      return(scrape_and_download_files(page_url, base_url, download_dir, failed_downloads, downloaded_basenames, extensions, log_file, ssl_verify))
    }
    
    if (httr::status_code(response) == 200) {
      rvest::read_html(httr::content(response, as = "text"))
    } else {
      stop("Non-200 status code: ", httr::status_code(response))
    }
  }, error = function(e) {
    message("Error reading page: ", e$message)
    failed_downloads <<- append(failed_downloads, list(list(link = page_url, error = e$message)))
    return(NULL)
  })
  
  if (is.null(page)) {
    return(failed_downloads)
  }
  
  # Get all links from the page
  links <- page %>% html_nodes("a") %>% html_attr("href")
  links <- links[!is.na(links)]
  message("Found ", length(links), " links on page: ", page_url)
  # print(links)
  
  # Make the URLs absolute
  full_links <- tryCatch({
    map_chr(links, ~ make_absolute_url(.x, base_url))
  }, error = function(e) {
    message("Error converting relative to absolute URLs: ", e$message)
    failed_downloads <<- append(failed_downloads, list(list(link = page_url, error = "Failed to convert relative URLs")))
    return(NULL)  # Return NULL if there's an issue with the URL conversion
  })
  
  if (is.null(full_links)) {
    return(failed_downloads)  # Exit if URL conversion failed
  }
  
  # Modify the filtering logic to pick up directory or HTML links without extensions
  filtered_links <- full_links[grepl(paste0("\\.(", paste(extensions, collapse = "|"), ")$"), full_links) | 
                                 !grepl("\\.[a-zA-Z0-9]{2,5}$", full_links)]
  
  # Download the relevant links
  walk(filtered_links, function(link) {
    Sys.sleep(1)  # Add a 1-second delay between each download to prevent rate limiting
    
    # Check if the 429 error is being thrown and skip file-related operations
    if (grepl("429", link)) {
      message("Skipping file-related operations for 429 response.")
      return()
    }
    
    # Proceed with downloading the relevant links
    handle_download(download_link = link, base_url = base_url, download_dir = download_dir, 
                    failed_downloads = failed_downloads, downloaded_basenames = downloaded_basenames, 
                    download_unknown = TRUE, log_file = log_file)
  })
  
  return(failed_downloads)
}


# Recursive scraping function with cycle detection (no depth limiting)
scrape_recursively <- function(start_url, base_url, download_dir, failed_downloads, visited_links, downloaded_basenames = NULL, start_time, timeout_seconds, retry_delay = 5, extensions, log_file, ssl_verify = TRUE) {
  # Calculate the elapsed time
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  # If the timeout has been reached, exit the function
  if (elapsed_time > timeout_seconds) {
    failed_downloads <<- append(failed_downloads, list(list(link = start_url, error = "Timeout exceeded")))
    message("Timeout reached during recursion. Exiting.")
    return(failed_downloads)
  }
  
  # Hash the normalised start URL
  url_hash <- hash_url(start_url)
  
  # Check if the page has already been visited
  if (exists(url_hash, envir = visited_links)) {
    message("Skipping already visited page: ", start_url)
    return(failed_downloads)
  }
  
  # Mark the page as visited by storing the hash
  visited_links[[url_hash]] <- TRUE
  
  # Set the current page URL
  page_url <- start_url
  
  # Try to scrape and download files from the current page
  new_links <- tryCatch({
    scrape_and_download_files(page_url, base_url, download_dir, failed_downloads, downloaded_basenames, extensions, log_file, ssl_verify = ssl_verify)  # Pass ssl_verify here
  }, error = function(e) {
    # Handle real errors, such as HTTP errors or connection failures
    if (grepl("404", e$message)) {
      failed_downloads <<- append(failed_downloads, list(list(link = page_url, error = "404 Not Found")))
    } else if (grepl("429", e$message)) {
      message("429 Too Many Requests - retrying after delay...")
      Sys.sleep(retry_delay)  # Delay for retry on rate limit
      return(scrape_recursively(start_url, base_url, download_dir, failed_downloads, visited_links, downloaded_basenames, start_time, timeout_seconds, retry_delay * 2, extensions, log_file, ssl_verify))  # Pass ssl_verify here
    } else {
      failed_downloads <<- append(failed_downloads, list(list(link = page_url, error = e$message)))
    }
    return(NULL)  # Ensure new_links is not used if there's an error
  })
  
  # Recursively scrape internal links, but check timeout before proceeding
  if (!is.null(new_links)) {
    walk(new_links, function(link) {
      # Add delay between recursive calls to avoid rate-limiting
      Sys.sleep(retry_delay)  # Delay between requests to reduce load on the server
      scrape_recursively(link, base_url, download_dir, failed_downloads, visited_links, downloaded_basenames, start_time, timeout_seconds, retry_delay, extensions, log_file, ssl_verify)  # Pass ssl_verify here
    })
  }
  
  return(failed_downloads)
}


# Wrapper to apply a timeout to the entire process
start_scraping_with_timeout <- function(start_url, base_url, download_dir, timeout_seconds = 300, extensions, log_file, ssl_verify = TRUE) {
  failed_downloads <- list()
  visited_links <- new.env(hash = TRUE, parent = emptyenv())  # Using a hash environment for fast lookups
  
  # Track the start time for manual timeout handling
  start_time <- Sys.time()
  
  tryCatch({
    scrape_recursively(start_url, base_url, download_dir, failed_downloads, visited_links, 
                       downloaded_basenames = NULL, start_time, timeout_seconds, retry_delay = 5, extensions, log_file, ssl_verify)  # Pass ssl_verify here
  }, TimeoutException = function(ex) {
    message("Timeout reached for the entire scraping process after ", timeout_seconds, " seconds.")
    failed_downloads <<- append(failed_downloads, list(list(link = start_url, error = "Timeout exceeded")))
  })
  
  return(failed_downloads)
}
