library(dplyr)
library(jsonlite)

# Metadata from flat files
ddh_lovs_df           <- ddhconnect::get_lovs(root_url = dkanr::get_url())

eex_ddh_vocab_df      <- readxl::read_excel("./data-raw/controlled_vocab_mapping.xlsx")
basic_json_mapping    <- readxl::read_excel("./data-raw/eex_ddh_JSON_lookup_basic.xlsx")
complex_lookup        <- readxl::read_excel("./data-raw/eex_ddh_JSON_lookup_complex.xlsx")

# Check for invalid values (i.e values in machine_names that are not in ddh_lovs for non-free-text fields)
invalid_controlled_vocab  <- eex_ddh_vocab_df %>% anti_join(ddh_lovs_df, by = "machine_name") 
invalid_complex           <- complex_lookup %>% filter(machine_name != "field_external_metadata") %>%
  anti_join(ddh_lovs_df, by = "machine_name")

assertthat::assert_that(nrow(invalid_controlled_vocab) == 0, msg = 'Invalid values present in eex_ddh_vocab_df')
assertthat::assert_that(nrow(invalid_complex) == 0, msg = 'Invalid values present in complex_json_mapping')

# Add tids
eex_ddh_vocab_df <- eex_ddh_vocab_df %>% left_join(ddh_lovs_df, by = c("machine_name","list_value_name"))

# Check for NA values
na_tid <- eex_ddh_vocab_df %>% filter(is.na(tid))
assertthat::assert_that(nrow(na_tid) == 0, msg = 'Invalid values present in complex_json_mapping')

# Add JSON fields
basic_lookup <- basic_json_mapping %>% full_join(eex_ddh_vocab_df, by = "machine_name")

devtools::use_data(basic_lookup,
                   complex_lookup,
                   overwrite = TRUE)
