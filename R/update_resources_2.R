#' update_resources_2
#'
#' Update resources for a given dataset
#'
#' @param dataset_nid string: dataset to attach resource to
#' @param resource_nid vector: vector of resources to attach
#' @param metadata_resources list: list of resource metadata
#' @param lovs dataframe: lookup table of the data catalog tids and values
#' @param root_url character: API root URL
#' @param credentials list: object returned by the dkanr::get_credentials() function
#'
#' @import jsonlite
#' @return list
#' @export
#'

update_resources_2 <- function(dataset_nid,
                             resource_nid, metadata_resources,
                             ddh_fields = ddhconnect::get_fields(),
                             lovs = ddhconnect::get_lovs(),
                             root_url = dkanr::get_url(),
                             credentials = list(cookie = dkanr::get_cookie(),
                                                token = dkanr::get_token())) {
  
  # Create list of titles with resource_nids as keys
  harv_titles <- list()
  for(i in seq_along(resource_nid)){
    res_meta <- ddhconnect::get_metadata(resource_nid[[i]])
    harv_titles[resource_nid[[i]]] <- res_meta$title
  } 
  
  # Classify current and new resources
  current  <- list()
  new      <- list()
  count    <- 0
  for(j in seq_along(metadata_resources)){
    if(metadata_resources[[j]]$title %in% harv_titles){
      res_nid             <- names(harv_titles[(harv_titles %in% metadata_resources[[j]])])
      current[[res_nid]]  <- metadata_resources[[j]]$title
    }
    else{
      count        <- count + 1
      new[[count]] <- metadata_resources[[j]]$title
    }
  }
  
  # Classify outdated resources (i.e resources not present in Energy Portal Dataset)
  old    <- list()
  for(k in seq_along(harv_ids)){
    if(!(harv_titles[[k]] %in% current) & !(harv_titles[[k]] %in% new)){
      old[[names(harv_titles[k])]] <- harv_titles[[k]] 
    }
  }
  
  # Remove outdated resources
  lapply(names(old), function(x){
    ddhconnect::delete_dataset(x)
  })
  
  if(length(old) > 0 ){
    print("Old resources removed")
  }
  # Update current resources
  update_current_resources_2(dataset_nid, metadata_resources,
                           current, ddh_fields, lovs,
                           root_url, credentials)
  
  # Add new resources
  add_new_resources_2(dataset_nid, metadata_resources,
                    new, ddh_fields, lovs,
                    root_url, credentials)
  
}
