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
  }
  return(dt)
}

is_year <- function(dt) {
  return(!is.na(as.numeric(dt)) && as.numeric(dt) >= 1900 && as.numeric(dt) <= 2100)
}
