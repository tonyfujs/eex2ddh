#' map_license_values
#'
#' Map License values from the EEX to DDH
#'
#' @param metadata_list list: output of extract_eex_metadata()
#' @param output list: output of map_eex_metadata_dataset()
#' 
#' @import dplyr
#' @return list
#' @export
#'

map_license_values <- function(metadata_list, output) {
  
  lkup_values <- dataset_master_lookup
  if(length(metadata_list[["license_title"]]) > 0){
      temp <- lapply(metadata_list[["license_title"]], function(x){
        lkup_values %>%
          filter(eex_field_JSON == "license_title" & eex_value == x) %>%
          select(list_value_name)
        })  %>% unlist()
    
    if(length(temp) > 0){
      output[["field_license_wbddh"]] <- unlist(as.character(temp))
    }
  }
  return(output)
}