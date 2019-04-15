#' get_ddh_records_status
#'
#' Compare DDH and Energy Portal records
#'
#' @param root_url character: API root URL
#' @param credentials list: object returned by the get_credentials() function
#' @import dplyr
#' @return data frame
#' @export
#'

get_ddh_records_status <- function(root_url = dkanr::get_url(),
                                   credentials = list(cookie = dkanr::get_cookie(),
                                                      token = dkanr::get_token())) {

  # Subset the DDH catalog for the eex datasets
  ddh_df              <- get_eex_from_ddh_datasets(root_url = root_url, credentials = credentials)
  ddh_df$ddh_updated  <- as.numeric(lubridate::ymd_hms(ddh_df$ddh_updated))

  # EEX harvest
  eex_df                      <- get_eex_datasets()
  eex_df$eex_internal_updated <- as.numeric(lubridate::ymd_hms(eex_df$eex_internal_updated))
  
  # Ignore SE4ALL & ENERGYDATA.INFO - User Manual
  eex_df <- eex_df[eex_df$eex_internal_id != "538a3ba2-f218-42b2-a79c-3a5b7603556e", ]
  eex_df <- eex_df[eex_df$eex_internal_id != "6b7c56a5-f15d-4cb8-b9f2-cf8814a6d271", ]
  
  # Combine datasets
  full_list         <- dplyr::left_join(eex_df, ddh_df, by = "eex_internal_id")
  full_list$status  <- NA
  full_list$status[is.na(full_list$ddh_nids)] <- "new"
  full_list$status[!is.na(full_list$ddh_nids) & !is.na(full_list$eex_internal_updated)] <- "current"

  # Identify Current / New / Old datasets based on timestamps
  full_list$time_diff   <- abs(as.numeric(full_list$eex_internal_updated) - as.numeric(full_list$ddh_updated)) - 14400
  full_list$sync_status <- NA
  full_list$sync_status[full_list$status == "current" & full_list$time_diff <= 3600]  <- "in sync"
  full_list$sync_status[full_list$status == "current" & full_list$time_diff > 3600]   <- "out of sync"
  full_list$time_diff <- NULL
  
  # Identify duplicates
  full_list$oldest_timestamp <- ave(full_list$ddh_created, full_list$eex_internal_id, FUN = min)
  full_list$duplicate_status <- ifelse(full_list$ddh_created == full_list$oldest_timestamp, "original", "duplicate")
  full_list$oldest_timestamp <- NULL
  
  return(full_list)
}
