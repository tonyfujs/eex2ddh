#' map_external_metadata
#'
#' Map External Metadata values from the EEX to DDH
#'
#' @param metadata_list list: output of extract_eex_metadata()
#' @param output list: output of map_eex_metadata_dataset()
#' @return list
#' @export
#'

map_external_metadata <- function(metadata_list, output) {
  
  metadata_list <- metadata_list$result 
  external_data <- list()
  
  # Check if license is Custom License
  if(output$field_license_wbddh == "Custom License"){
      external_data$`Custom License Information` <- c(metadata_list$license_title, metadata_list$license_url) 
  }
  
  # Add Topic
  external_data$Topic <- unlist(metadata_list$topic)
  
  # Add Group
  if(length(metadata_list$group) > 0){
    if(metadata_list$group != ""){
      
      external_data$Group <- metadata_list$group
      
    }
  }
  
  # Check if Country mapped 
  if(length(output$field_wbddh_country) == 1){
    if(output$field_wbddh_country == "Region/Country not specified"){
    
    external_data$`Country Code` <- c(metadata_list$country_code, metadata_list$region)

    }
  }
  
  if(!is.null(output$invalid_country_codes)){
    
    external_data$`Country Code` <- unlist(output$invalid_country_codes)
    output$invalid_country_codes <- NULL
  }
  
  # Create Vector of characters
  temp <- c()
  for(i in seq_along(external_data)){
    temp <- c(temp, 
                paste(names(external_data[i]), paste0(external_data[[i]], collapse = ", "), sep = ": ")
              )
  }
  
  output$field_external_metadata <- paste(temp, collapse = ". ")
  
  return(output)
}
