#' update_resources
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

update_resources <- function(dataset_nid,
                            resource_nid, metadata_resources,
                            ddh_fields = ddhconnect::get_fields(),
                            lovs = ddhconnect::get_lovs(),
                            root_url = dkanr::get_url(),
                            credentials = list(cookie = dkanr::get_cookie(),
                                               token = dkanr::get_token())) {
  
 # Create list of field_ddh_harvest_sys_id with resource_nids as keys
 harv_ids <- list()
 for(i in seq_along(resource_nid)){
   res_meta <- ddhconnect::get_metadata(resource_nid[[i]])
   harv_ids[resource_nid[[i]]] <- res_meta$field_ddh_harvest_sys_id$und[[1]]$value
 } 
 
 # Classify current and new resources
 current  <- list()
 new      <- list()
 count    <- 0
 for(j in seq_along(metadata_resources)){
   if(metadata_resources[[j]]$field_ddh_harvest_sys_id %in% harv_ids){
     res_nid          <- names(harv_ids[(harv_ids %in% metadata_resources[[j]])])
     current[[res_nid]] <- metadata_resources[[j]]$field_ddh_harvest_sys_id
   }
   else{
     count        <- count + 1
     new[[count]] <- metadata_resources[[j]]$field_ddh_harvest_sys_id
   }
 }
 
 # Classify outdated resources (i.e resources not present in Energy Portal Dataset)
 old    <- list()
 for(k in seq_along(harv_ids)){
   if(!(harv_ids[[k]] %in% current) & !(harv_ids[[k]] %in% new)){
     count        <- count + 1
     old[[names(harv_ids[k])]] <- harv_ids[[k]] 
   }
 }
 
 # Remove outdated resources
 lapply(names(old), function(x){
   ddhconnect::delete_dataset(x)
 })
 
 # Update current resources
 update_current_resources(metadata_resources, current)
 
 # Add new resources
 add_new_resources(metadata_resources, new)
 
 print("Resources updated successfully")
 
}
