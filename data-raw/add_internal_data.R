library(dplyr)
library(jsonlite)

# Metadata from flat files
ddh_lovs_df           <- ddhconnect::get_lovs(root_url = dkanr::get_url())

eex_ddh_vocab_df      <- readxl::read_excel("./data-raw/controlled_vocab_mapping.xlsx")
basic_json_mapping    <- readxl::read_excel("./data-raw/eex_ddh_JSON_lookup_basic.xlsx")
complex_lookup        <- readxl::read_excel("./data-raw/eex_ddh_JSON_lookup_complex.xlsx")
constant_lookup       <- readxl::read_excel("./data-raw/constant_vocab_mapping.xlsx")

# Check for invalid values (i.e values in machine_names that are not in ddh_lovs for non-free-text fields)
invalid_controlled_vocab  <- eex_ddh_vocab_df %>% anti_join(ddh_lovs_df, by = "machine_name") 
invalid_complex           <- complex_lookup %>% filter(machine_name != "field_external_metadata") %>%
  anti_join(ddh_lovs_df, by = "machine_name")

invalid_constant  <- constant_lookup %>% 
  filter(machine_name != "field_wbddh_dsttl_upi" & machine_name != "field_wbddh_collaborator_upi") %>% 
  anti_join(ddh_lovs_df, by = "machine_name") 


assertthat::assert_that(nrow(invalid_controlled_vocab) == 0, msg = 'Invalid values present in eex_ddh_vocab_df')
assertthat::assert_that(nrow(invalid_complex) == 0, msg = 'Invalid values present in complex_lookup')
assertthat::assert_that(nrow(invalid_constant) == 0, msg = 'Invalid values present in constant_lookup')

# Add tids
eex_ddh_vocab_df     <- eex_ddh_vocab_df %>% left_join(ddh_lovs_df, by = c("machine_name","list_value_name"))
constant_lookup      <- constant_lookup %>%  left_join(ddh_lovs_df, by = c("machine_name","list_value_name"))
master_basic_lookup  <- rbind(eex_ddh_vocab_df, constant_lookup)


# Check for NA values
na_tid <- master_basic_lookup %>% filter((machine_name != "field_wbddh_dsttl_upi" & machine_name != "field_wbddh_collaborator_upi") & is.na(tid))
assertthat::assert_that(nrow(na_tid) == 0, msg = 'Invalid values present in complex_json_mapping')

# Add JSON fields
basic_json_mapping   <- basic_json_mapping %>% select(machine_name, eex_field_JSON)
master_basic_lookup  <- master_basic_lookup %>% left_join(basic_json_mapping, by = "machine_name")

# Create lookup for both resource and dataset
dataset_master_lookup  <- master_basic_lookup %>% filter(is_dataset == TRUE) %>% select(-is_dataset)
resource_master_lookup <- master_basic_lookup %>% filter(is_dataset == FALSE) %>% select(-is_dataset)

devtools::use_data(dataset_master_lookup,
                   resource_master_lookup,
                   complex_lookup,
                   overwrite = TRUE)
