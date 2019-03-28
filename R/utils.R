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

