#' map_multiple_values
#'
#' Map Country and Region values from the EEX to DDH
#'
#' @param metadata_list list: output of extract_eex_metadata()
#' @param output list: output of map_eex_metadata_dataset()
#' @param eex_input character: Energy Data API field
#' @param machine_name character: DDH Machine Name
#' @import dplyr
#' @return list
#' @export
#'

map_multiple_values <- function(metadata_list, output, eex_input, machine_name) {

  lkup_values <- dataset_master_lookup
  if(length(metadata_list[[eex_input]]) > 0){
      temp <- lapply(metadata_list[[eex_input]], function(x){
        lkup_values %>%
          filter(eex_field_JSON == eex_input & eex_value == x) %>%
          select(list_value_name)
        })  %>% unlist()
    
    if(length(temp) > 0){
      output[[machine_name]] <- unlist(as.character(temp))
    }
  }
  return(output)
}
