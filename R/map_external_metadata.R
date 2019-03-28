#' map_external_metadata
#'
#' Map External Metadata values from the EEX to DDH
#'
#' @param metadata_list list: output of extract_eex_metadata()
#' @param output list: output of map_eex_metadata_dataset()
#' @import dplyr
#' @return list
#' @export
#'

map_external_metadata <- function(metadata_list, output) {
  
  external_data <- list()
  
  # Check if license is Custom License
  if(output[["field_license_wbddh"]] == "Custom License"){
      external_data[["Custom License Information"]] <- c(metadata_list$license_title, metadata_list$license_url) 
  }
  
  # Add Topic
  external_data[["Topic"]] <- unlist(metadata_list$topic)
  
  # Add Group
  if(metadata_list$group != ""){
    external_data[["Group"]] <- metadata_list$group
  }
  
  # Create Vector of characters
  temp <- c()
  for(i in seq_along(external_data)){

    temp <- c(temp, 
                paste(names(external_data[i]), paste0(external_data[[i]], collapse = ", "), sep = ": ")
              )
  }
  
  output[["field_external_metadata"]] <- paste(temp, collapse = ". ")
  
  return(output)
}
