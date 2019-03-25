#' map_country_region
#'
#' Extract specific metadata from the Finance API JSON response
#'
#' @param metadata_list list: output of extract_eex_metadata()
#' @param output list: output of map_eex_metadata_dataset()
#' @import dplyr
#' @return list
#' @export
#'

map_country_region <- function(metadata_list, output) {

  lkup_values   <- dataset_master_lookup
  for(i in seq_along(metadata_list)){
    eex_field <- metadata_list[i]
    
    if(names(eex_field) == "country_code"){
      if(length(eex_field[["country_code"]]) > 0){
        output[["field_wbddh_country"]]  <- unlist(lapply(eex_field[["country_code"]], function(x){
          lkup_values %>% 
            filter(eex_field_JSON == "country_code" & eex_value == x) %>% 
            select(list_value_name) %>%
            as.character
        }))
      }

      else{
        output[["field_wbddh_country"]] <- "Region/Country not specified"
      }
    }
    
    else if(names(eex_field) == "region"){
      if(length(eex_field[["region"]]) > 0){
        
      output[["field_wbddh_region"]]  <- unlist(lapply(eex_field[["region"]], function(x){
          lkup_values %>% 
            filter(eex_field_JSON == "region" & eex_value == x) %>% 
            select(list_value_name) %>%
            as.character
        }))
      }
      
      else{
        output[["field_wbddh_region"]] <- "Region not specified"
      }
    }
    
  }

  return(output)
}
