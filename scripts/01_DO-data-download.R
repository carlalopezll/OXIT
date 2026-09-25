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
# this is where access will be requested
date_folders <- drive_ls(path = as_id(top_folder_id), type = "folder")

# 2. for each date folder, list site folders
get_site_folders <- function(date_id, date_name) {
  sites <- drive_ls(path = as_id(date_id), type = "folder")
  if (nrow(sites) == 0) return(NULL)
  sites %>%
    mutate(date_name = date_name)
}

site_folders <- map2_dfr(date_folders$id, date_folders$name, get_site_folders)

# 3. For each site folder, find CAT.txt directly

find_cat_txt <- function(site_id, site_name, date_name) {
  files_in_site <- drive_ls(path = as_id(site_id))
  cat_file <- files_in_site %>% 
    filter(str_detect(name, "_DO_"))
  
  if (nrow(cat_file) == 0) {
    warning(paste("No DO file found for", date_name, "/", site_name, 
                  "- files present:", paste(files_in_site$name, collapse = ", ")))
    return(NULL)
  }
  
  if (nrow(cat_file) > 1) {
    warning(paste("Multiple DO files found for", date_name, "/", site_name,
                  "- using first:", cat_file$name[2]))
    cat_file <- cat_file[2, ]
  }
  
  tibble(
    date = date_name,
    site = site_name,
    file_id = cat_file$id[1],
    file_name = cat_file$name[1]
  )
}

cat_files <- pmap_dfr(
  list(site_folders$id, site_folders$name, site_folders$date_name),
  find_cat_txt
)

cat_files <- cat_files %>%
  mutate(local_path = file.path("drive_downloads", paste0(date, "_", site, "_DO.txt")))


# 4. Download each CAT.txt with a unique local name

walk2(
  cat_files$file_id, cat_files$local_path,
  ~ drive_download(file = as_id(.x), path = .y, overwrite = TRUE)
)



# 5. Read and merge, tagging each with date/site
read_and_tag <- function(path, date, site) {
  read_delim(path, delim = "\t", show_col_types = FALSE) %>%  # adjust delim if needed
    mutate(date = date, site = site)
}

merged_data <- pmap_dfr(
  list(cat_files$local_path, cat_files$date, cat_files$site),
  read_and_tag
)
