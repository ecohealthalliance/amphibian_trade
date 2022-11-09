library(tidyverse)
library(readxl)
library(lubridate)

# Import LEMIS metadata
lemis_codes <- lemis::lemis_codes()
lemis_metadata <- lemis::lemis_metadata()

#==============================================================================


# Import the LEMIS data for 1999 and 2015 from USFWS
f.99 <- read_csv("data/raw/1999_All.csv") %>%
  # eliminate extraneous columns
  select(1:21)
f.15.1 <- read_csv("data/raw/2015 Q1&2_1.csv") %>%
  # eliminate extraneous columns
  select(1:23) %>%
  select(-ESA, -Carrier)
f.15.2 <- read_csv("data/raw/2015-Q3&4_Cont_2.csv") %>%
  # eliminate extraneous columns
  select(1:23) %>%
  select(-ESA, -Carrier)

# Combine all data frames
f <- bind_rows(f.99, f.15.1, f.15.2)

# Clean column names to match with existing LEMIS format
colnames(f) <- c("species_code", "genus", "species", "subspecies",
                 "specific_name", "generic_name", "description",
                 "wildlife_category", "quantity", "unit", "country_origin",
                 "country_imp_exp", "trans_mode", "purpose", "source", 
                 "action", "disposition", "disposition_date", "shipment_date",
                 "import_export", "port")

# Import AmphibiaWeb taxonomy
a.taxonomy <- 
  jsonlite::read_json("data/taxonomy/amphib_names.json", simplifyVector = T)

# Get all unique amphibian generic names
a.genera <- unique(a.taxonomy$genus)

# Pull generic names from synonymous taxa names
synonym.genera <- a.taxonomy %>%
  select(genus, species, synonymies) %>%
  # separate synonyms out
  separate(
    synonymies, 
    into = c("synonym_1", "synonym_2", "synonym_3", 
             "synonym_4", "synonym_5", "synonym_6"), 
    sep = ", "
  ) %>%
  # pivot to get a single synonym column
  tidyr::pivot_longer(cols = starts_with("synonym"), values_to = "synonym") %>%
  select(-name) %>%
  # remove cases with no synonyms at all
  filter(!is.na(synonym), synonym != "") %>%
  # trim leading whitespace from synonym names
  mutate(synonym = str_trim(synonym, side = "left")) %>%
  # get synonym genera
  select(synonym) %>%
  separate(
    synonym,
    into = c("genus", "species", "subspecies"),
    sep = " "
  ) %>%
  # remove "Lacerta" from this list since it mistakenly captures lizards
  filter(genus != "Lacerta") %>%
  pull(genus) %>%
  unique()

# Add these in to the unique amphibian genera
a.genera <- c(a.genera, synonym.genera) %>%
  unique() %>%
  sort() %>%
  toupper()

# Filter the LEMIS data frame to only amphibian data, capturing rows 
# not necessarily listed as amphibians in the "wildlife_category" field
amphibian.indices <- which(f$wildlife_category %in% c("Amp", "AMP"))
other.indices <- which(is.na(f$wildlife_category) & f$genus %in% a.genera)
f.a <- f[sort(c(amphibian.indices, other.indices)), ]

# Eliminate the "wildlife_category" variable, replacing it with "taxa" and
# "class" information
table(f.a$wildlife_category, useNA = "ifany")
f.a <- select(f.a, -wildlife_category)
f.a$taxa <- rep("amphibian", nrow(f.a))
f.a$class <- rep("Amphibia", nrow(f.a))

# Generate (blank) "control_number", "value", "us_co", "foreign_co", and
# "cleaning_notes" columns
f.a$control_number <- rep(NA, nrow(f.a))
f.a$value <- rep(NA, nrow(f.a))
f.a$us_co <- rep(NA, nrow(f.a))
f.a$foreign_co <- rep(NA, nrow(f.a))
f.a$cleaning_notes <- rep(NA, nrow(f.a))

# Add on year variables and rearrange columns
f.a <- f.a %>%
  mutate(
    disposition_date = as.Date(disposition_date, format = "%m/%d/%y"),
    disposition_year = lubridate::year(disposition_date),
    shipment_date = as.Date(shipment_date, format = "%m/%d/%y"),
    shipment_year = lubridate::year(shipment_date)
  ) %>%
  select(control_number, species_code, taxa, class, genus, 
         species, subspecies, specific_name, generic_name,
         description, quantity, unit, value, country_origin,
         country_imp_exp, purpose, source, action, disposition,
         disposition_date, disposition_year, shipment_date, 
         shipment_year, import_export, port, us_co, foreign_co,
         cleaning_notes
  )

#==============================================================================


# Basic data cleaning

f.a <- f.a %>%
  # filter to only imports since exports are also present in this dataset
  filter(import_export == "I") %>%
  mutate(
    # convert "genus", "species", and "country_origin" columns to 
    # standardized format
    genus = str_to_sentence(genus),
    species = tolower(species),
    country_origin = toupper(country_origin),
    # replace non-CITES entries
    genus = str_replace_all(
      genus, "Noncites entry|Noncites", "Non-CITES entry"
    ),
    # replace generic "species" entries
    species = ifelse(
      species %in% c("species", "amphibians", "sp", "spp.", "sp."),
      "sp.", species
    ),
    # replace generic "description" entries with missing values
    description = ifelse(description == "***", NA_character_, description),
    # replace generic "unit" entries with missing values
    unit = ifelse(unit == "**", NA_character_, unit),
    # convert pounds to kilograms by multiplying the pound units by 0.453592
    quantity = case_when(
      unit == "LB" ~ quantity * 0.453592,
      TRUE ~ quantity
    ),
    unit = ifelse(unit == "LB", "KG", unit),
    # convert "N0" and "no" units to "NO"
    unit = ifelse(unit %in% c("N0", "no"), "NO", unit),
    # replace country codes as needed
    country_origin = case_when(
      country_origin ==  "MI" ~ "non-standard value",
      country_origin == "**" ~ NA_character_,
      TRUE ~ country_origin
    ),
    country_imp_exp = case_when(
      country_imp_exp %in% c("**") ~ NA_character_,
      TRUE ~ country_imp_exp
    ),
    # replace purpose codes as needed
    purpose = case_when(
      purpose %in% c("VA", "N") ~ "non-standard value",
      purpose == "*" ~ NA_character_,
      TRUE ~ purpose
    ),
    # replace source codes as needed
    source = case_when(
      source %in% c("*") ~ NA_character_,
      TRUE ~ source
    )
  )

#==============================================================================


# Verify good field values throughout the dataset

table(f.a$taxa, useNA = "ifany")
table(f.a$class, useNA = "ifany")

# Check "description" codes
description.codes <- lemis_codes %>%
  filter(field == "description") %>%
  pull(code)
unique(f.a$description) %in% description.codes
# there will be missing values
unique(f.a$description)[which(!(unique(f.a$description) %in% description.codes))]

# Check "unit" codes
unit.codes <- lemis_codes %>%
  filter(field == "unit") %>%
  pull(code)
unique(f.a$unit) %in% unit.codes
# there will be missing values
unique(f.a$unit)[which(!(unique(f.a$unit) %in% unit.codes))]

# Check "country_origin" codes
country.codes <- lemis_codes %>%
  filter(field == "country") %>%
  pull(code)
unique(f.a$country_origin) %in% country.codes
# there will be missing values and other non-standard entries
unique(f.a$country_origin)[which(!(unique(f.a$country_origin) %in% country.codes))]

# Check "country_imp_exp" codes
unique(f.a$country_imp_exp) %in% country.codes
unique(f.a$country_imp_exp)[which(!(unique(f.a$country_imp_exp) %in% country.codes))]

# Check "purpose" codes
purpose.codes <- lemis_codes %>%
  filter(field == "purpose") %>%
  pull(code)
unique(f.a$purpose) %in% purpose.codes
# there will be missing values
unique(f.a$purpose)[which(!(unique(f.a$purpose) %in% purpose.codes))]

# Check "source" codes
source.codes <- lemis_codes %>%
  filter(field == "source") %>%
  pull(code)
unique(f.a$source) %in% source.codes
# Some discrepancy here is ok since O and X codes have been added
unique(f.a$source)[which(!(unique(f.a$source) %in% source.codes))]

# Check "action" codes
action.codes <- lemis_codes %>%
  filter(field == "action") %>%
  pull(code)
unique(f.a$action) %in% action.codes

# Check "disposition" codes
disposition.codes <- lemis_codes %>%
  filter(field == "disposition") %>%
  pull(code)
unique(f.a$disposition) %in% disposition.codes

#==============================================================================


# Save the cleaned data
write_csv(f.a, "data/cleaned/amphibian_LEMIS_1999_and_2015.csv")
