#' get_eex_datasets_list
#'
#' @param token character: Energy.Info API authentication token
#'
#' @return data.frame
#' @export
#'

get_eex_datasets_list <- function(token) {
  #TODO
  # Need to follow-up with Derlinix about setting eex_user_origin flag
  
  # Retreive World Bank datasets that were created in the Energy Info portal
  # resp <- connect_eex(path = "/api/action/package_search?fq=eex_user_origin:True%20organization:world-bank-grou")
  resp <- connect_eex(path = "/api/action/package_search?fq=organization:world-bank-grou")
  eex_internal_id <- purrr::map_chr(resp, 'id')
  eex_internal_updated <- purrr::map_chr(resp, 'metadata_modified')
  
  # Clean date format
  eex_internal_updated <- purrr::map_chr(eex_internal_updated, clean_date)

  out <- data.frame(eex_internal_id, eex_internal_updated, stringsAsFactors = FALSE)
  
  return(out)
}
