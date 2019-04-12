library(stringr)

#cleaning date
clean_date <- function(dt) {
  if(nchar(dt) == 4){
    dt <- gsub("FY", "20", dt)
  }
  if(nchar(dt) == 6){
    dt <- gsub("FY", "", dt)
  }
  if(suppressWarnings(is_year(dt))) {
    dt <- paste(dt, "01", "01", sep="-")
  }
  if(nchar(dt) == 11) {
    months <- list(
      "jan" = "1",
      "feb" = "2",
      "mar" = "3",
      "apr" = "4",
      "may" = "5",
      "jun" = "6",
      "jul" = "7",
      "aug" = "8",
      "sep" = "9",
      "oct" = "10",
      "nov" = "11",
      "dec" = "12"
    )
    
    to_replace  <- tolower(str_extract(dt, "(?!\\-)\\D{3}"))
    dt          <- gsub(to_replace, months[[to_replace]], tolower(dt))
    dt          <- strptime(dt, "%d-%m-%Y")
    dt          <- format(dt, "%Y-%m-%d")
  }
  else{
    dt <- gsub("T", " ", dt)
    dt <- strftime(dt, format = "%Y-%m-%d %H:%M:%S")
  }
  

  return(dt)
}

is_year <- function(dt) {
  return(!is.na(as.numeric(dt)) && as.numeric(dt) >= 1900 && as.numeric(dt) <= 2100)
}

safe_see_if <- function(file_value, orig_value, field_name) {
  assert_result <- assertthat::see_if(is.same(file_value, orig_value, field_name))
  if(!assert_result){
    warning(paste0(field_name, ": The updated value is not equal to the passed value."))
  }
}

is.same <- function(file_value, orig_value, field_name) {
  is.empty(file_value) && is.empty(orig_value) ||
    is.character(file_value) && is.character(orig_value) && (gsub("[\n]", "", file_value) == gsub("[\n]", "", orig_value))
  
}

is.empty <- function(s) {
  is.null(s) || s == ""
}

is_blank <- function(input){
  return(gtools::invalid(input) || all(input == ""))
}

# Update current resources in DDH
update_current_resources <- function(dataset_nid, metadata_resources,
                                     current, ddh_fields, lovs,
                                     root_url, credentials){
   for(i in seq_along(metadata_resources)){
     if(metadata_resources[[i]]$field_ddh_harvest_sys_id %in% current){
       json_res <- ddhconnect::create_json_resource(values = metadata_resources[[i]],
                                                    dataset_nid = dataset_nid,
                                                    publication_status = "published",
                                                    ddh_fields = ddh_fields,
                                                    lovs = lovs,
                                                    root_url = root_url)
  
       resp_res <- ddhconnect::update_resource(nid = names(current[current %in% metadata_resources[[i]]$field_ddh_harvest_sys_id]),
                                               body = json_res,
                                               root_url = root_url,
                                               credentials = credentials)
     }
   }
   print(paste0(length(current), " current resources updated."))
}

# Add new resources to DDH
add_new_resources <- function(dataset_nid, metadata_resources,
                              new, ddh_fields, lovs,
                              root_url, credentials){
  for(i in seq_along(metadata_resources)){
    if(metadata_resources[[i]]$field_ddh_harvest_sys_id %in% new){
      json_res <- ddhconnect::create_json_resource(values = metadata_resources[[i]],
                                                   dataset_nid = dataset_nid,
                                                   publication_status = "published",
                                                   ddh_fields = ddh_fields,
                                                   lovs = lovs,
                                                   root_url = root_url)

      resp_res <- ddhconnect::create_resource(body = json_res,
                                              root_url = root_url,
                                              credentials = credentials)
    }
  }
  print(paste0(length(new), " new resources added."))
}

map_resource_formats <- function(resource_metadata, lovs){
  
  # Filter out DDH extensions
  ddh_formats         <- lovs[lovs$machine_name == "field_format","list_value_name"]

  # Seperate lower case and upper case formats for DDH
  lower_case_formats  <- c("docx","data","xlsx","html","txt")
  upper_case_formats  <- ddh_formats[!ddh_formats %in% lower_case_formats]
  
  # Account for Geospatial formats
  geo_formats         <- c("GeoJSON", "SHP ZIP", "KML", "GeoTIFF")
  upper_case_formats  <- upper_case_formats[!upper_case_formats %in% geo_formats]
  
  # Map field_format
  resource_metadata$format <- gsub("^\\.", "", resource_metadata$format)
  if(toupper(resource_metadata$format) %in%  upper_case_formats){
    output <- upper_case_formats[upper_case_formats %in% toupper(resource_metadata$format)]
  } else if(tolower(resource_metadata$format) %in% lower_case_formats){
    output <- lower_case_formats[lower_case_formats %in% resource_metadata$format]
  } else if (tolower(resource_metadata$format) == "xls"){
    output <- "EXCEL"
  } else if (tolower(resource_metadata$format) %in% tolower(geo_formats)){
    if(tolower(resource_metadata$format) == "geojson"){
      output <- "GeoJSON"
    } else if(tolower(resource_metadata$format) == "geotiff"){
      output <- "GeoTIFF"
    } else if(tolower(resource_metadata$format) == "SHP" | tolower(resource_metadata$format) == "SHP ZIP"){
      output <- "SHP ZIP"
    } else{
      output <- "KML"
    }
  }else{
    output <- "OTHER"
  }
  
  return(output)
}

