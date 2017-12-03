library(httr); library(purrr)

check_url <- function(url) http_status(HEAD(url))$message
safe_check_url <- safely(check_url)

link_status <- vector(mode = "list", length = nrow(new_dataset))
for(i in 1:nrow(new_dataset)){
  cat(i, "; ")
  if(is.na(new_dataset$distribution_accessURL[i])){
    link_status[[i]] <- safe_check_url(new_dataset$distribution_downloadURL[i])
  } else {
    link_status[[i]] <- safe_check_url(new_dataset$distribution_accessURL[i])
  }
}

# This next section is incredibly messy but it seems to work.  Converts the above link status check
# to character and adds a new column to the "new_dataset"
new_dataset <- bind_cols(
  new_dataset, 
  data_frame(link_status_20171202_result = 
               unlist(lapply(link_status, function(x) if_else(is.null(x$result), NA_character_, x$result))),
             link_status_20171202_error = unlist(lapply(link_status, function(x){
    if(is.null(x$error)){
      return(NA_character_)
    } else {
      return(as.character(x$error))
    }
  })) %>%
    str_replace("Error in curl\\:\\:curl_fetch_memory\\(url, handle \\= handle\\)\\: ", ""))
  ) %>%
  mutate(link_status_20171202 = 
           if_else(is.na(new_dataset$distribution_accessURL) & is.na(new_dataset$distribution_downloadURL),
                 NA_character_,
                 if_else(is.na(link_status_20171202_result), link_status_20171202_error,
                         link_status_20171202_result))) %>%
  select(-link_status_20171202_result, -link_status_20171202_error)

write_csv(new_dataset, "va_data_inventory.csv", na = "")