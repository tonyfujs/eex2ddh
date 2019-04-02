#' map_eex_metadata_resource
#'
#' Map simple values from the EEX to DDH
#'
#' @param metadata_list list: output of extract_eex_metadata()
#'
#' @import dplyr
#' @return list
#' @export
#'

map_eex_metadata_resource <- function(metadata_list) {
  
  metadata_list <- metadata_list$result 
  lkup_values   <- resource_master_lookup
  output        <- list()
  resource_type <- list()
  
  # Vector of Geospatial Extensions
  geo_ext <- c(
    "SHP",
    "GeoJSON",
    "KML",
    "shapefiles",
    "geotiff",
    "Esri REST",
    "geopackage"
  )
  
  # Loop over resources -----------------------------------------------------
  for(i in seq_along(metadata_list$resources)){
    temp                <- list()
    resource_meta       <- metadata_list$resources[[i]]
    resource_type[[i]]  <-  tolower(resource_meta$format)
    
    # Map values to DDH controlled vocabulary ---------------------------------
    resource_meta <- resource_meta[lkup_values$eex_field_JSON]
    for (j in seq_along(resource_meta)) {
      machine_name <- lkup_values %>%
        filter(eex_field_JSON == names(resource_meta[j])) %>%
        select(machine_name) %>%
        as.character()
        temp[[machine_name]]<- resource_meta[[j]]
    }
    
    # Add constant metadata
    temp[["field_wbddh_resource_type"]] <- "Download"
    temp[["field_wbddh_data_class"]]    <- "Public"
    
    # Format Description 
    temp[["body"]] <- gsub("[\n\r]", "", temp[["body"]])
    
    # Order resources
    temp[["field_resource_weight"]] <- i
    
    # Add constant metadata
    constant_metadata <-   lkup_values %>% filter(is.na(eex_value), is.na(eex_field_JSON))
    for (k in 1:nrow(constant_metadata)){
      temp[[constant_metadata[k,]$machine_name]] <- constant_metadata[k,]$list_value_name
    }
    
    output[[i]] <- temp
    
  }
  
  # Check if Geospatial Data Type
  if(length(intersect(unlist(resource_type), tolower(geo_ext))) > 0){
    output[["field_wbddh_data_type"]] <- "Geospatial"
  } else{
    output[["field_wbddh_data_type"]] <- "Other"
  }
  
  
  return(output)
}
