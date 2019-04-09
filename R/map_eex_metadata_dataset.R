#' map_eex_metadata_dataset
#'
#' Map simple values from the EEX to DDH
#'
#' @param metadata_list list: output of extract_eex_metadata()
#' @import dplyr
#' @return list
#' @export
#'

map_eex_metadata_dataset <- function(metadata_list) {
  
  metadata_list       <- metadata_list$result 
  output              <- list()
  eex_fields          <- names(metadata_list)
  
  # Filter out free text fields
  free_text_lkup      <- dataset_master_lookup %>% 
    filter(is.na(list_value_name) & is.na(eex_value) & eex_field_JSON != "region")
  
  free_text_variables <- eex_fields[eex_fields %in% free_text_lkup$eex_field_JSON]
  free_text           <- metadata_list[names(metadata_list) %in% free_text_variables]
  
  # Map values to DDH free text field
  for(i in seq_along(free_text)){
    eex_field   <- free_text[i]
    if(eex_field != ""){
      machine_name            <-  free_text_lkup %>% filter(eex_field_JSON == names(eex_field)) %>%
        select(machine_name)
      
      if(nrow(machine_name) > 0){
        output[[as.character(machine_name)]]  <- as.character(eex_field)
      }
    }
  }
  
  # Format Dates
  if(!is.null(output$field_wbddh_start_date)){
    output$field_wbddh_start_date <- clean_date(output$field_wbddh_start_date)
  }
  if(!is.null(output$field_wbddh_end_date)){
    output$field_wbddh_end_date <- clean_date(output$field_wbddh_end_date)
  }
  output$field_wbddh_release_date   <- clean_date(output$field_wbddh_release_date)
  output$field_wbddh_modified_date  <- clean_date(output$field_wbddh_modified_date)
  
  # Format Description 
  output$body <- gsub("[\n\r]", "", output$body)
  
  # Add constant metadata
  constant_metadata <- dataset_master_lookup %>% filter(is.na(eex_value) & is.na(eex_field_JSON))
  for (i in 1:nrow(constant_metadata)){
    # Map multiple TTL UPIs
    if(constant_metadata[i,]$machine_name == "field_wbddh_collaborator_upi"){
      output[[constant_metadata[i,]$machine_name]] <- unlist(strsplit(constant_metadata[i,]$list_value_name,","))
    } else{
      output[[constant_metadata[i,]$machine_name]] <- constant_metadata[i,]$list_value_name
    }
  }
  
  # Add Country and License
  output <- map_country_values(metadata_list, output)
  output <- map_license_values(metadata_list, output)
  
  # Add Default values for empty fields
  if(!("field_wbddh_country" %in% names(output))){
    output$field_wbddh_country <- "Region/Country not specified"
  }
  
  if(!("field_license_wbddh" %in% names(output))){
    output$field_license_wbddh <- "Custom License"
  }
  
  return(output)
}
