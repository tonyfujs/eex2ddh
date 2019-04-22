library(dplyr)
library(jsonlite)

# Set production URL
production_root_url <- 'https://datacatalog.worldbank.org'

# Get ddh lov fields
ddh_lovs_df           <- ddhconnect::get_lovs(root_url = production_root_url)

# Metadata from flat files
eex_ddh_vocab_df      <- readxl::read_excel("./data-raw/controlled_vocab_mapping.xlsx")
basic_json_mapping    <- readxl::read_excel("./data-raw/eex_ddh_JSON_lookup_basic.xlsx")
constant_lookup       <- readxl::read_excel("./data-raw/constant_vocab_mapping.xlsx")

# Following fields are not present in ddhconnect::get_lovs()
ignore <- c(
  "field_ddh_harvest_sys_id",
  "title",
  "body",
  "field_wbddh_release_date",
  "field_wbddh_end_date",
  "field_ddh_external_contact_email",
  "field_wbddh_start_date",
  "field_wbddh_publisher_name",
  "field_wbddh_source",
  "field_wbddh_modified_date",
  "body",
  "title",
  "field_upload"
)

# Check for invalid values (i.e values in machine_names that are not in ddh_lovs for non-free-text fields)
invalid_controlled_vocab  <- eex_ddh_vocab_df %>%
  filter(!(machine_name %in% ignore)) %>%
  anti_join(ddh_lovs_df, by = "machine_name")

invalid_constant  <- constant_lookup %>%
  filter(machine_name != "field_wbddh_dsttl_upi" & machine_name != "field_wbddh_collaborator_upi" & machine_name !="og_group_ref") %>%
  anti_join(ddh_lovs_df, by = "machine_name")

assertthat::assert_that(nrow(invalid_controlled_vocab) == 0, msg = 'Invalid values present in eex_ddh_vocab_df')
assertthat::assert_that(nrow(invalid_constant) == 0, msg = 'Invalid values present in constant_lookup')

# Merge controlled_vocab and constant lookups
master_basic_lookup  <- rbind(eex_ddh_vocab_df, constant_lookup)

# Add JSON fields
# The rows with NA as JSON fields have the constant metadata values
basic_json_mapping   <- basic_json_mapping %>% select(-Notes)
master_basic_lookup  <- master_basic_lookup %>% full_join(basic_json_mapping, by = c("machine_name","is_dataset"))

# Create lookup for both resource and dataset
dataset_master_lookup  <- master_basic_lookup %>% filter(is_dataset == TRUE) %>% select(-is_dataset)
resource_master_lookup <- master_basic_lookup %>% filter(is_dataset == FALSE) %>% select(-is_dataset)

devtools::use_data(dataset_master_lookup,
                   resource_master_lookup,
                   overwrite = TRUE)
