#' map_eex_metadata_resource
#'
#' Map simple values from the EEX to DDH
#'
#' @param metadata_list list: output of extract_eex_metadata()
#' @param lovs dataframe: lookup table of the data catalog tids and values
#' @import dplyr
#' @importFrom rlang .data
#' @return list
#' @export
#'

map_eex_metadata_resource <- function(metadata_list, lovs) {

  lkup_values   <- eex2ddh::resource_master_lookup
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
    resource_type[[i]]  <- tolower(resource_meta$format)

    # Map values to DDH controlled vocabulary ---------------------------------
    for (j in seq_along(resource_meta)) {
      machine_name <- dplyr::filter(lkup_values, .data$eex_field_JSON == names(resource_meta[j])) %>%
        dplyr::select("machine_name")

      if(nrow(machine_name) > 0){
        temp[[as.character(machine_name)]]  <- resource_meta[[j]]
      }
    }

    # Add constant metadata
    temp$field_wbddh_resource_type <- "Download"
    temp$field_wbddh_data_class    <- "Public"

    # Format Description
    temp$body <- gsub("[\n\r]", "", temp$body)

    # Order resources
    temp$field_resource_weight <- i

    # Add constant metadata
    constant_metadata <- dplyr::filter(lkup_values, is.na(.data$eex_value) & is.na(.data$eex_field_JSON))
    for (k in 1:nrow(constant_metadata)){
      temp[[constant_metadata[k,]$machine_name]] <- constant_metadata[k,]$list_value_name
    }

    # Map resource extensions
    temp$field_format <- map_resource_formats(resource_metadata = resource_meta, lovs = lovs)
    output[[i]]       <- temp
  }

  # Check if Geospatial Data Type
  if(length(dplyr::intersect(unlist(resource_type), tolower(geo_ext))) > 0){
    output$field_wbddh_data_type <- "Geospatial"
  } else{
    output$field_wbddh_data_type <- "Other"
  }

  return(output)
}
