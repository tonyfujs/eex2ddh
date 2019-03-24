#' extract_eex_metadata
#'
#' Extract specific metadata from the Energy Portal API JSON response
#' @param eex_internal_id character: Energy Portal dataset ID
#'
#' @return list
#' @export
#'

extract_eex_metadata <- function(eex_internal_id) {
  
  url      <- paste0("https://energydata.info/api/3/action/package_show?id=", eex_internal_id)
  eex_data <- jsonlite::fromJSON(url, simplifyVector = FALSE)
  
  return(eex_data)
}