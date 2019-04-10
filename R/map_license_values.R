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
  if(length(metadata_list$license_title) > 0){
      temp <- lapply(metadata_list$license_title, function(x){
        lkup_values %>%
          filter(eex_field_JSON == "license_title" & eex_value == x) %>%
          .$list_value_name
        })
    
    if(length(temp) > 0){
      output$field_license_wbddh <- temp
    }
  }
  return(output)
}
