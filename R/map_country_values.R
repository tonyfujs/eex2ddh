#' map_country_values
#'
#' Map Country values from the EEX to DDH
#'
#' @param metadata_list list: output of extract_eex_metadata()
#' @param output list: output of map_eex_metadata_dataset()
#' 
#' @return list
#' @export
#'

map_country_values <- function(metadata_list, output) {

  # Merge Country and Region from EEX
  metadata_list$country_code <- c(metadata_list$country_code, metadata_list$region)
  lkup_values <- dataset_master_lookup
  temp <- unlist(lapply(metadata_list$country_code, function(x){
    lkup_values[lkup_values$eex_field_JSON == "country_code" & lkup_values$eex_value == x, "list_value_name"]
  }))
  
  # Account for Afric
  if("AFR" %in% metadata_list$country_code){
    temp <- c(temp,c("list_value_name" = "Middle East & North Africa",
                     "list_value_name" = "Sub-Saharan Africa"
                     )
              )
  }
    
  output$field_wbddh_country <- as.character(unique(temp[!is.na(temp)]))
  
  # Make account of Country Codes not mapped to DDH
  country_codes         <- lkup_values[lkup_values$eex_field_JSON == "country_code", "eex_value"]
  invalid_country_code  <- unique(metadata_list$country_code[!metadata_list$country_code %in% unlist(country_codes)])
  
  # Remove AFR
  if(length(invalid_country_code) > 0){
    output$invalid_country_codes <- invalid_country_code[invalid_country_code != "AFR"]
  }
  
  return(output)
}
