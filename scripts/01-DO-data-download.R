download_fun <- function(file_path){
  #Read the files
  temp <- read_delim(paste0(file_path),
                     delim = ",",
                     col_names = TRUE,
                     skip = 1) %>% 
    as_tibble() %>% 
    mutate(file = str_sub(file_path, 10)) %>% 
    mutate(Site_ID = str_sub(file, 1, 2))
  temp
}




library(googledrive)
library(purrr)

folder_id <- "1lhkllY10PrWuFD7KIUWh4tiz83ix-YOb"

file_info <- drive_ls(path = as_id(folder_id))


dir.create("drive_downloads", showWarnings = FALSE)

walk2(
  file_info$id, file_info$name,
  ~ drive_download(
    file = as_id(.x),
    path = file.path("drive_downloads", paste0(.y, ".csv")),
    overwrite = TRUE
  )
)
