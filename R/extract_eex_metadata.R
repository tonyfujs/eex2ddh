#' extract_eex_metadata
#'
#' Extract specific metadata from the Energy Portal API JSON response
#' @param eex_internal_id character: Energy Portal dataset ID
#'
#' @return list
#' @export
#'

extract_eex_metadata <- function(eex_internal_id) {
  
  #TODO Revert back to logic used for PROD
  # url      <- paste0("https://energydata.info/api/3/action/package_show?id=", eex_internal_id)
  # eex_data <- jsonlite::fromJSON(url, simplifyVector = FALSE)
  
  url   <- paste0("https://energydata.staging.derilinx.com/api/3/action/package_show?id=", eex_internal_id)
  
  # Send request to API
  resp  <- httr::GET(url,
                     httr::add_headers(.headers = c('X-API-KEY' = "2d451b91-95d0-4177-98fc-a4e3b0a4fd12",
                                                    'charset' = "utf-8")),
                     httr::accept_json())
  
  eex_data <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  
  return(eex_data$result)
}
