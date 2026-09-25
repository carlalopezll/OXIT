# Script to download DO data directly from the GDrive

# load libraries
library(googledrive)
library(purrr)
library(dplyr)
library(readr)
library(stringr)

# Google Driver folder structure
# for most of them, some are a little different but will work with these for now
# download_date/site/sensor_id/Cat.TXT 


# define Google Driver folder ID
top_folder_id <- "1lhkllY10PrWuFD7KIUWh4tiz83ix-YOb"

# create local directory to download data, if it doesn't exist yet
dir.create("data/raw", showWarnings = FALSE)

# 1. list date folders
# these should probably be clean dates but OK for now
# this is where access will be requested
date_folders <- drive_ls(path = as_id(top_folder_id), type = "folder") %>%
  mutate(date_clean = str_extract(name, "^[0-9]{8}"))  # grabs leading 8-digit date

# 2. for each date folder, list site folders within

# function to get site folders
get_site_folders <- function(date_id, date_name) {
  sites <- drive_ls(path = as_id(date_id), type = "folder")
  if (nrow(sites) == 0) return(NULL)
  sites %>%
    mutate(date_name = date_name)
}

site_folders <- map2_dfr(date_folders$id, date_folders$date_clean, get_site_folders)

# 3. get a list of the folders within each date/site folder that have the concatenated text file
get_sensor_folders <- function(site_id, site_name, date_name) {
  sensors <- drive_ls(path = as_id(site_id), type = "folder")
  if (nrow(sensors) == 0) return(NULL)
  sensors %>% mutate(site_name = site_name, date_name = date_name)
}

sensor_folders <- pmap_dfr(
  list(site_folders$id, site_folders$name, site_folders$date_name),
  get_sensor_folders
)

# 4. for each of the previous folders, get the CAT.txt

find_cat_file <- function(sensor_id, sensor_name, site_name, date_name) {
  files_in_sensor <- drive_ls(path = as_id(sensor_id))
  cat_file <- files_in_sensor %>%
    filter(str_to_lower(str_trim(name)) == "cat.txt")
  
  if (nrow(cat_file) == 0) {
    warning(paste("No Cat.txt found for", date_name, "/", site_name, "/", sensor_name,
                  "- files present:", paste(files_in_sensor$name, collapse = ", ")))
    return(NULL)
  }
  
  if (nrow(cat_file) > 1) {
    warning(paste("Multiple Cat.txt matches for", date_name, "/", site_name, "/", sensor_name,
                  "- files:", paste(cat_file$name, collapse = ", "),
                  "- using:", cat_file$name[1]))
    cat_file <- cat_file[1, ]
  }
  
  tibble(
    date = date_name,
    site = site_name,
    sensor_id = sensor_name,
    file_id = cat_file$id[1],
    file_name = cat_file$name[1]
  )
}

cat_files <- pmap_dfr(
  list(sensor_folders$id, sensor_folders$name, sensor_folders$site_name, sensor_folders$date_name),
  find_cat_file
)

# 5. download each CAT.txt with a unique local name
# following the structure: date_site_DO.txt

cat_files <- cat_files %>%
  mutate(local_path = file.path("data/raw", paste0(date, "_", site, "_DO.txt")))

walk2(
  cat_files$file_id, cat_files$local_path,
  ~ drive_download(file = as_id(.x), path = .y, overwrite = TRUE)
)

# 6. MERGE!

read_cat_file <- function(path, date, site, sensor) {
  read_csv(
    path,
    
    # skip title, metadata, and the two header rows
    skip = 9,
    
    # rename column names
    col_names = c(
      "unix_timestamp",
      "utc_datetime",
      "local_datetime",
      "battery_v",
      "temp_c",
      "do_mgL",
      "do_sat_pct",
      "q"
    ),
    col_types = cols(
      unix_timestamp = col_double(),
      utc_datetime   = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      local_datetime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
      battery_v      = col_double(),
      temp_c         = col_double(),
      do_mgL         = col_double(),
      do_sat_pct     = col_double(),
      q              = col_double()
    ),
    trim_ws = TRUE
  ) %>%
    mutate(
      date_download = date,
      site = site,
      sensor_id = sensor
    )
}

merged_data <- pmap_dfr(
  list(cat_files$local_path, cat_files$date, cat_files$site, cat_files$sensor_id),
  read_cat_file
)

# save csv
write_csv(merged_data, "data/processed/merged DO data.csv")