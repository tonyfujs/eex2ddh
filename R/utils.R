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
  else{
    dt <- gsub("T", " ", dt)
  }
  return(dt)
}

is_year <- function(dt) {
  return(!is.na(as.numeric(dt)) && as.numeric(dt) >= 1900 && as.numeric(dt) <= 2100)
}
