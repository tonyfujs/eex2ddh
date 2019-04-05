#' update_existing_dataset
#'
#' Update a full Energy.info record in DDH (metadata + resources)
#'
#' @param metadata_list list: list of finance metadata, from extract_eex_metadata()
#' @param master dataframe: Output of fin2ddh::get_ddh_records_status()
#' @param ddh_fields dataframe: table of all the data catalog fields by node type
#' @param lovs dataframe: lookup table of the data catalog tids and values
#' @param root_url character: API root URL
#' @param credentials list: object returned by the dkanr::get_credentials() function
#'
#' @return character
#' @export
#'

update_existing_dataset <- function(metadata_list,
                                    master = eex2ddh::get_ddh_records_status(),
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
  metadata_dataset[["field_wbddh_data_type"]]   <- metadata_resources[["field_wbddh_data_type"]]
  metadata_resources[["field_wbddh_data_type"]] <- NULL
  
  # Create Dataset
  json_dat <- ddhconnect::create_json_dataset(values = metadata_dataset,
                                              publication_status = "published",
                                              ddh_fields = ddh_fields,
                                              lovs = lovs,
                                              root_url = root_url)
  
  dataset_nid <- master[master$eex_internal_id == metadata_list$result$id, "ddh_nids"]
  resp_dat <- ddhconnect::update_dataset(nid = dataset_nid,
                                         body = json_dat,
                                         root_url = root_url,
                                         credentials = credentials)
  
  # Create Resources
  metadata_dataset_ddh <- ddhconnect::get_metadata(nid = resp_dat$nid,
                                               root_url = root_url,
                                               credentials = credentials)
  
  resource_nid <- ddhconnect::get_resource_nids(metadata_dataset_ddh)
    
  update_resources_2(resp_dat$nid, resource_nid, metadata_resources)
  
  # test created dataset
  metadata_dataset_test <- ddhconnect::get_metadata(nid = resp_dat$nid,
                                                    root_url = root_url,
                                                    credentials = credentials)
  
  test_created_dataset(dataset_metadata = metadata_dataset_test,
                       metadata_list = metadata_dataset,
                       root_url = root_url,
                       credentials = credentials)
  print(resp_dat)
}
