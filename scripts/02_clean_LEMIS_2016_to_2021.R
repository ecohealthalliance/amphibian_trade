library(tidyverse)
library(readxl)
library(lubridate)

# Import LEMIS metadata
lemis_codes <- lemis::lemis_codes()
lemis_metadata <- lemis::lemis_metadata()

#==============================================================================


# Import the LEMIS data from 2016 to 2021 obtained via FOIA request
f <- read_xlsx(
  "data/raw/DecDetail.FWS-FOIA-Connelly-2022-000126.04.26.2022.xlsx", 
  sheet = 1
)

# Clean column names to match with existing LEMIS format
colnames(f) <- c("control_number", "species_code", "genus", "species",
                 "subspecies", "specific_name", "generic_name", "description",
                 "wildlife_category", "quantity", "unit", "country_origin",
                 "country_imp_exp", "purpose", "source", "action",
                 "disposition", "disposition_date", "shipment_date",
                 "import_export", "port")

# Eliminate the "wildlife_category" variable, replacing it with "taxa" and
# "class" information
table(f$wildlife_category, useNA = "ifany")
f <- select(f, -wildlife_category)
f$taxa <- rep("amphibian", nrow(f))
f$class <- rep("Amphibia", nrow(f))

# Generate (blank) "value", "us_co", "foreign_co", and "cleaning_notes" columns
f$value <- rep(NA, nrow(f))
f$us_co <- rep(NA, nrow(f))
f$foreign_co <- rep(NA, nrow(f))
f$cleaning_notes <- rep(NA, nrow(f))

# Add on year variables and rearrange columns
f <- f %>%
  mutate(
    disposition_year = year(disposition_date),
    shipment_year = year(shipment_date)
  ) %>%
  select(
    control_number, species_code, taxa, class, genus, 
    species, subspecies, specific_name, generic_name,
    description, quantity, unit, value, country_origin,
    country_imp_exp, purpose, source, action, disposition,
    disposition_date, disposition_year, shipment_date, 
    shipment_year, import_export, port, us_co, foreign_co,
    cleaning_notes
  )

#==============================================================================


# Basic data cleaning

f <- f %>%
  mutate(
    # convert "genus" and "species" columns to standardized format
    genus = str_to_sentence(genus),
    species = tolower(species),
    # replace non-CITES entries
    genus = str_replace_all(
      genus, "Noncites entry|Noncites", "Non-CITES entry"
    ),
    # replace generic "species" entries
    species = str_replace_all(species, "species|amphibians", "sp."),
    # convert pounds to kilograms by multiplying the pound units by 0.453592
    quantity = case_when(
      unit == "LB" ~ quantity * 0.453592,
      TRUE ~ quantity
    ),
    unit = ifelse(unit == "LB", "KG", unit),
    # convert "N0" units to "NO"
    unit = ifelse(unit == "N0", "NO", unit),
    # convert the invalid "N" disposition code into the "Other" code
    disposition = ifelse(disposition == "N", "O", disposition)
  )

#==============================================================================


# Verify good field values throughout the dataset

table(f$taxa, useNA = "ifany")
table(f$class, useNA = "ifany")

# Check "description" codes
description.codes <- lemis_codes %>%
  filter(field == "description") %>%
  pull(code)
unique(f$description) %in% description.codes

# Check "unit" codes
unit.codes <- lemis_codes %>%
  filter(field == "unit") %>%
  pull(code)
unique(f$unit) %in% unit.codes

# Check "country_origin" codes
country.codes <- lemis_codes %>%
  filter(field == "country") %>%
  pull(code)
unique(f$country_origin) %in% country.codes
# Some discrepancy here is ok since country codes have been added over time
unique(f$country_origin)[which(!(unique(f$country_origin) %in% country.codes))]

# Check "country_imp_exp" codes
unique(f$country_imp_exp) %in% country.codes
unique(f$country_imp_exp)[which(!(unique(f$country_imp_exp) %in% country.codes))]

# Check "purpose" codes
purpose.codes <- lemis_codes %>%
  filter(field == "purpose") %>%
  pull(code)
unique(f$purpose) %in% purpose.codes

# Check "source" codes
source.codes <- lemis_codes %>%
  filter(field == "source") %>%
  pull(code)
unique(f$source) %in% source.codes
# Some discrepancy here is ok since O and X codes have been added
unique(f$source)[which(!(unique(f$source) %in% source.codes))]

# Check "action" codes
action.codes <- lemis_codes %>%
  filter(field == "action") %>%
  pull(code)
unique(f$action) %in% action.codes

# Check "disposition" codes
disposition.codes <- lemis_codes %>%
  filter(field == "disposition") %>%
  pull(code)
unique(f$disposition) %in% disposition.codes
# Some discrepancy here is ok since O code has been added
unique(f$disposition)[which(!(unique(f$disposition) %in% disposition.codes))]

#==============================================================================


# Save the cleaned data
write_csv(f, "data/cleaned/amphibian_LEMIS_2016_to_2021.csv")
