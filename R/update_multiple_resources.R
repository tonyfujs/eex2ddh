#' update_multiple_resources
#'
#' Update multiple resources for a given dataset
#'
#' @param dataset_nid string: dataset to attach resource to
#' @param resource_nid list: list of resources to attach
#' @param metadata_resources list: list of resource metadata
#' @param lovs dataframe: lookup table of the data catalog tids and values
#' @param root_url character: API root URL
#' @param credentials list: object returned by the dkanr::get_credentials() function
#'
#' @import jsonlite
#' @return list
#' @export
#'

update_multiple_resources <- function(dataset_nid,
                            resource_nid, metadata_resources,
                            ddh_fields = ddhconnect::get_fields(),
                            lovs = ddhconnect::get_lovs(),
                            root_url = dkanr::get_url(),
                            credentials = list(cookie = dkanr::get_cookie(),
                                               token = dkanr::get_token())) {
  
  updated <- list()
  # Check whether there are outdated resources present in DDH (i.e Resources in DDH that are not in EEX)
  if(length(resource_nid) > length(metadata_resources)){
    # Update resources in DDH  
    for(i in seq_along(metadata_resources)){
      json_res <- ddhconnect::create_json_resource(values = metadata_resources[[i]],
                                                   dataset_nid = dataset_nid,
                                                   publication_status = "published",
                                                   ddh_fields = ddh_fields,
                                                   lovs = lovs,
                                                   root_url = root_url)
      
      ddhconnect::update_resource(nid = resource_nid[i],
                                  body = json_res,
                                  root_url = root_url,
                                  credentials = credentials)
      updated[i] <- resource_nid[i]
    }
    
    # Filter out resources to remove
    to_remove <- resource_nid[!(resource_nid %in% updated)]
    
    # Delete excess resources in DDH
    lapply(to_remove, function(x){
      ddhconnect::delete_dataset(x)
    })
  }
  
  # Check whether there are more resources present in EEX than DDH
  if(length(resource_nid) < length(metadata_resources)){
    # Update resources in DDH
    for (i in seq_along(resource_nid)) {
      json_res <- ddhconnect::create_json_resource(values = metadata_resources[[i]],
                                                   dataset_nid = dataset_nid,
                                                   publication_status = "published",
                                                   ddh_fields = ddh_fields,
                                                   lovs = lovs,
                                                   root_url = root_url)
      ddhconnect::update_resource(nid = resource_nid[i],
                                  body = json_res,
                                  root_url = root_url,
                                  credentials = credentials)
      
      updated[i] <- metadata_resources[i]
    }
    
    # Filter out resources to add
    to_add <- metadata_resources[!(metadata_resources %in% updated)]
    
    # Add new resources to DDH
    for(j in seq_along(to_add)){
      json_res <- ddhconnect::create_json_resource(values = to_add[[j]],
                                                   dataset_nid = dataset_nid,
                                                   publication_status = "published",
                                                   ddh_fields = ddh_fields,
                                                   lovs = lovs,
                                                   root_url = root_url)
      
      ddhconnect::create_resource(body = json_res,
                                  root_url = root_url,
                                  credentials = credentials)
    }
  }
}