#' map_country_values
#'
#' Map Country values from the EEX to DDH
#'
#' @param metadata_list list: output of extract_eex_metadata()
#' @param output list: output of map_eex_metadata_dataset()
#' 
#' @import dplyr
#' @return list
#' @export
#'

map_country_values <- function(metadata_list, output) {
  
  
  # Merge Country and Region from EEX
  metadata_list[["country_code"]] <- c(metadata_list[["country_code"]], metadata_list[["region"]])
  
  lkup_values <- dataset_master_lookup
  if(length(metadata_list[["country_code"]]) > 0){
    temp <- lapply(metadata_list[["country_code"]], function(x){
      lkup_values %>%
        filter(eex_field_JSON == "country_code" & eex_value == x) %>%
        select(list_value_name)
    })  %>% unlist()
    
    # Account for Afric
    if("AFR" %in% metadata_list[["country_code"]]){
      temp <- c(temp,c("list_value_name" = "Middle East & North Africa",
                       "list_value_name" = "Sub-Saharan Africa"
                       )
                )
    }
    
    if(length(temp) > 0){
      output[["field_wbddh_country"]] <- unique(unlist(as.character(temp)))
    }
  }
  return(output)
}