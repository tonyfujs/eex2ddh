#' map_license
#'
#' Map License values from the EEX to DDH
#'
#' @param metadata_list list: output of extract_eex_metadata()
#' @param output list: output of map_eex_metadata_dataset()
#' @import dplyr
#' @return list
#' @export
#'

map_license <- function(metadata_list, output) {
  
  external_data <- list()
  
  # Check if license is Custom License
  if(output[["field_license_wbddh"]] == "Custom License"){
      external_data[["Custom License Information"]] <- c(metadata_list$license_title, metadata_list$license_url) 
  }
  
  # Add Topic
  external_data[["Topics"]] <- unlist(metadata_list$topic)
  
  # Add Group
  if(metadata_list$group != ""){
    external_data[["Group"]] <- metadata_list$group
  }
  
  output[["field_external_metadata"]] <- external_data
  
  return(output)
}
