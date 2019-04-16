#' get_eex_from_ddh_datasets
#'
#' Retrieve Energy info datasets on DDH
#'
#' @param root_url character: API root URL
#' @param credentials list: object returned by the get_credentials() function
#'
#' @return data frame
#' @export
#'


get_eex_from_ddh_datasets <- function(root_url = dkanr::get_url(),
                                 credentials = list(cookie = dkanr::get_cookie(),
                                                    token = dkanr::get_token())) {
  eex_datasets <- ddhconnect::search_catalog(
    fields = c(
      "nid",
      "field_ddh_harvest_src",
      "field_ddh_harvest_sys_id",
      "field_wbddh_modified_date",
      "created"
    ),
    filters = c(
      "field_ddh_harvest_src" = "1014",
      "type" = "dataset"
    ),
    credentials = credentials,
    root_url = root_url
  )
  
  ddh_nids <- as.character(purrr::map(eex_datasets, "nid"))
  ddh_created <- as.character(purrr::map(eex_datasets, "created"))
  ddh_updated <- as.character(purrr::map(eex_datasets, function(x) x[["field_wbddh_modified_date"]][["und"]][[1]][["value"]]))
  eex_internal_id <- as.character(purrr::map(eex_datasets, function(x) x[["field_ddh_harvest_sys_id"]][["und"]][[1]][["value"]]))
  
  out <- data.frame(ddh_nids, ddh_created, ddh_updated, eex_internal_id, stringsAsFactors = FALSE)
  return(out)
}
