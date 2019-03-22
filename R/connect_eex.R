#' connect_eex
#'
#' @param path character: path to send request
#' @param query character
#' @param root character: the root url that hosts the API
#' @param token character: access token
#' 
#' @return object of class eex_api
#' @export
#' @importFrom utils str
#'

connect_eex <- function(path, query = NULL, root = "https://energydata.info", token = "") {
  
  # Build request URL
  url <- httr::modify_url(root, path = path, query = query)
  
  # Send request to API
  resp <- httr::GET(url,
                    httr::add_headers(.headers = c('X-API-KEY' = token,
                                                   'charset' = "utf-8")),
                    httr::accept_json())
  # Return useful message on error
  httr::stop_for_status(resp, task = 'complete request to Microdata library API\n')
  
  # CHECK: datatype is .JSON
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  # Parse response
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  
  # Organize information
  out <- structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "eex_api"
  )
  
  out <- out$content$result$results
  
  return(out)
}