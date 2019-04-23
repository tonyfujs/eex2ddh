#' map_license_values
#'
#' Map License values from the EEX to DDH
#'
#' @param metadata_list list: output of extract_eex_metadata()
#' @param output list: output of map_eex_metadata_dataset()
#'
#' @import dplyr
#' @importFrom rlang .data
#' @return list
#' @export
#'

map_license_values <- function(metadata_list, output) {
  lkup_values <- eex2ddh::dataset_master_lookup
  temp <- dplyr::filter(lkup_values, .data$eex_field_JSON == "license_title" & .data$eex_value == metadata_list$license_title)
  if(length(temp) > 0){
    output$field_license_wbddh <- temp$list_value_name
  }
  
  return(output)
}
