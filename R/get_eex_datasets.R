#' get_eex_datasets
#'
#' @param token character: Energy.Info API authentication token
#'
#' @return data.frame
#' @export
#'

get_eex_datasets <- function(token) {
  
  # Retreive World Bank datasets that were created in the Energy Info portal
  resp <- connect_eex(path = paste0("/api/3/action/package_search?fq=eex_user_origin:True%20organization:world-bank-grou&rows=1000"),
                      root = "https://energydata.info")
  
  # Retrieve ids for datasets
  eex_internal_id <- purrr::map_chr(resp, 'id')
  
  # Retrieve updated dates
  eex_internal_updated <- purrr::map_chr(resp, 'metadata_modified')
  
  # Clean date format
  eex_internal_updated <- purrr::map_chr(eex_internal_updated, clean_date)
  
  # Create data frame
  out <- data.frame(eex_internal_id, eex_internal_updated, stringsAsFactors = FALSE)
  
  return(out)
}
