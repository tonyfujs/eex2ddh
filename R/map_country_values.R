#' map_country_values
#'
#' Map Country values from the EEX to DDH
#'
#' @param metadata_list list: output of extract_eex_metadata()
#' @param output list: output of map_eex_metadata_dataset()
#' @import dplyr
#' @return list
#' @export
#'

map_country_values <- function(metadata_list, output) {

  # Merge Country and Region from EEX
  metadata_list$country_code <- c(metadata_list$country_code, metadata_list$region)
  lkup_values <- dataset_master_lookup
  temp <- lapply(metadata_list$country_code, function(x){
    lkup_values %>%
      filter(eex_field_JSON == "country_code" & eex_value == x) %>%
      select(list_value_name)
  }) %>% unlist()
  
  # Account for Afric
  if("AFR" %in% metadata_list$country_code){
    temp <- c(temp,c("list_value_name" = "Middle East & North Africa",
                     "list_value_name" = "Sub-Saharan Africa"
                     )
              )
    
    # Remove AFR
    metadata_list$country_code <-  metadata_list$country_code[metadata_list$country_code!="AFR"]
  }
    
  if(length(temp) > 0){
    output[["field_wbddh_country"]] <- unique(unlist(as.character(temp)))
  }
  
  # Make account of Country Codes not mapped to DDH
  country_codes           <- lkup_values %>% filter(eex_field_JSON == "country_code") %>%
    select(eex_value)
  invalid_country_codes   <- unique(metadata_list$country_code[!metadata_list$country_code %in% unlist(country_codes)])
  
  if(length(invalid_country_codes) > 0){
    output$invalid_country_codes <- invalid_country_codes
  }
  return(output)
}
