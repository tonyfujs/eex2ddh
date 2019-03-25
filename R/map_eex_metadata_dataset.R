#' map_eex_metadata_dataset
#'
#' Extract specific metadata from the Finance API JSON response
#'
#' @param metadata_list list: output of extract_eex_metadata()
#'
#' @import dplyr
#' @return list
#' @export
#'

map_eex_metadata_dataset <- function(metadata_list) {
  
  metadata_list <- metadata_list$result 
  lkup_values   <- dataset_master_lookup
  output        <- list()
  
  # Ignore Country and Region
  ignore <- c(
    "region",
    "country_code"
  )
  
  # Map values to DDH controlled vocabulary ---------------------------------
  for(i in seq_along(metadata_list)){
     eex_field <- metadata_list[i]
     if((names(eex_field) %in% lkup_values$eex_field_JSON) & (eex_field != "") & !(names(eex_field) %in% ignore)){
       machine_name <- lkup_values %>% filter(eex_field_JSON == names(eex_field)) %>% select(machine_name) %>% as.character()
       output[[machine_name]] <- as.character(eex_field)
     }
  }
  
  # Format Dates
  if(!is.null(output[["field_wbddh_start_date"]])){
    output[["field_wbddh_start_date"]] <- clean_date(output[["field_wbddh_start_date"]])
  }
  output[["field_wbddh_release_date"]] <- clean_date(output[["field_wbddh_release_date"]])
  output[["field_wbddh_modified_date"]] <- clean_date(output[["field_wbddh_modified_date"]])
  
  # Format Description 
  output[["body"]] <- gsub("[\n\r]", "", output[["body"]])
  
  # Add constant metadata
  constant_metadata <-   lkup_values %>% filter(is.na(eex_value), is.na(eex_field_JSON))
  for (i in 1:nrow(constant_metadata)){
    # Map multiple TTL UPIs
    if(constant_metadata[i,]$machine_name == "field_wbddh_collaborator_upi"){
      output[[constant_metadata[i,]$machine_name]] <- unlist(strsplit(constant_metadata[i,]$list_value_name,","))
    }
    else{
      output[[constant_metadata[i,]$machine_name]] <- constant_metadata[i,]$list_value_name
    }
  }
  
  return(output)
}
