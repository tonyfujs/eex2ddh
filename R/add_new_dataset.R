#' add_new_dataset
#'
#' Add new EEX Dataset to DDH
#'
#' @param metadata_list list: list with one finance dataset's metadata, from extract_eex_metadata()
#' @param ddh_fields dataframe: table of all the data catalog fields by node type
#' @param lovs dataframe: lookup table of the data catalog tids and values
#' @param root_url character: API root URL
#' @param credentials list: object returned by the dkanr::get_credentials() function
#'
#' @import jsonlite
#' @return list
#' @export
#'

add_new_dataset <- function(metadata_list,
                            ddh_fields = ddhconnect::get_fields(),
                            lovs = ddhconnect::get_lovs(),
                            root_url = dkanr::get_url(),
                            credentials = list(cookie = dkanr::get_cookie(),
                                               token = dkanr::get_token())) {

  # Format raw metadata for Dataset
  metadata_dataset  <- map_eex_metadata_dataset(metadata_list)

  # Map external metadata for Dataset
  metadata_dataset  <- map_external_metadata(metadata_list, metadata_dataset)

  # Format raw metadata for Resources
  metadata_resources <- map_eex_metadata_resource(metadata_list, lovs)

  # Add Data Type to Dataset
  metadata_dataset$field_wbddh_data_type   <- metadata_resources$field_wbddh_data_type
  metadata_resources$field_wbddh_data_type <- NULL

  # Create Dataset
  json_dat <- ddhconnect::create_json_dataset(values = metadata_dataset,
                                              publication_status = "published",
                                              ddh_fields = ddh_fields,
                                              lovs = lovs,
                                              root_url = root_url)

  resp_dat <- ddhconnect::create_dataset(body = json_dat,
                                         root_url = root_url,
                                         credentials = credentials)

  # Create Resources
  tryCatch({

    for (i in seq_along(metadata_resources)){
      json_res <- ddhconnect::create_json_resource(values = metadata_resources[[i]],
                                                   dataset_nid = resp_dat$nid,
                                                   publication_status = "published",
                                                   ddh_fields = ddh_fields,
                                                   lovs = lovs,
                                                   root_url = root_url)

      resp_res <- ddhconnect::create_resource(body = json_res,
                                              root_url = root_url,
                                              credentials = credentials)

    }

    # Test created dataset
    metadata_dataset_test <- ddhconnect::get_metadata(nid = resp_dat$nid,
                                                      root_url = root_url,
                                                      credentials = credentials)

    test_created_dataset(dataset_metadata = metadata_dataset_test,
                         metadata_list = metadata_dataset,
                         root_url = root_url,
                         credentials = credentials)


    return(resp_dat$uri)

  }, error = function(e){

      return(paste("Error:",e,"; with creating resources for", resp_dat$uri))
  })

}
