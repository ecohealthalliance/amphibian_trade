library(tidyverse)
library(stringr)
library(assertthat)

# Import LEMIS metadata
lemis_codes <- lemis::lemis_codes()
lemis_metadata <- lemis::lemis_metadata()

#==============================================================================


# Import and combine cleaned LEMIS data sources for downstream use
a <- read_csv("data/cleaned/amphibian_LEMIS_1999_and_2015.csv")
b <- read_csv("data/cleaned/amphibian_LEMIS_2000_to_2014.csv")
c <- read_csv("data/cleaned/amphibian_LEMIS_2016_to_2021.csv")
d <- bind_rows(a, b, c) %>%
  # remove any extra whitespace from genus and species names
  mutate(
    genus = str_trim(genus, side = "both"),
    species = str_trim(species, side = "both"),
    subspecies = str_trim(subspecies, side = "both")
  ) %>%
  arrange(shipment_date, control_number)

assert_that(nrow(d) == nrow(a) + nrow(b) + nrow(c))

#==============================================================================


# Add a column to the data frame giving the full country of origin name
d <- lemis_codes %>%
  filter(field == "country") %>%
  select(code, value) %>%
  rename(country_origin_full = value) %>%
  left_join(d, ., by = c("country_origin" = "code"))

# Give full names for new country codes that don't appear in "lemis_codes"
d <- d %>%
  mutate(
    country_origin_full = case_when(
      country_origin == "BQ" ~ "Bonaire, Saint Eustatius, & Saba",
      country_origin == "CW" ~ "Curacao",
      country_origin == "MF" ~ "Saint Martin",
      country_origin == "PR" ~ "Puerto Rico",
      country_origin == "SS" ~ "South Sudan",
      country_origin == "SX" ~ "Sint Maarten",
      TRUE ~ country_origin_full
    )
  )
      
#==============================================================================


# Taxonomic harmonization

# Import AmphibiaWeb taxonomy
a.taxonomy <- 
  jsonlite::read_json("data/taxonomy/amphib_names.json", simplifyVector = T)

# Generate a table with taxonomic synonyms from the AmphibiaWeb data
synonymy.table <- a.taxonomy %>%
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
  # remove any examples of synonyms with subspecies names as these can lead to
  # ambiguity
  filter(str_count(synonym, " ") == 1)

# Which synonyms are used for more than one taxa and thus are ambiguous?
ambiguous.synonyms <- synonymy.table %>%
  group_by(synonym) %>%
  count() %>%
  filter(n > 1) %>%
  pull(synonym)

# Modify synonymy table to eliminate ambiguous synonyms 
synonymy.table <- synonymy.table %>%
  filter(!(synonym %in% ambiguous.synonyms)) %>%
  separate(
    synonym, into = c("genus_synonym", "species_synonym"), sep = " "
  ) %>%
  rename(
    genus_aw = genus,
    species_aw = species
  )

# Generate new generic and species names to synonymize with AmphibiaWeb
d <- d %>%
  left_join(
    ., 
    synonymy.table, 
    by = c("genus" = "genus_synonym", "species" = "species_synonym")
  ) %>%
  mutate(
    genus_aw = ifelse(is.na(genus_aw), genus, genus_aw),
    species_aw = ifelse(is.na(species_aw), species, species_aw),
    # remove "cf. " from the species column if it occurs
    species_aw = str_replace_all(species_aw, "cf. ", ""),
    # remove any instances of "logi logi"
    species_aw = str_replace_all(species_aw, " logi logi", ""),
    # match anything that reads "sp. nov*" and replace it with "sp."
    species_aw = str_replace_all(species_aw, regex("sp\\. nov.*"), "sp."),
    # match anything that reads "sp." and a number and replace it with "sp."
    species_aw = str_replace_all(species_aw, regex("sp\\. [1-9].*"), "sp."),
    # replace specific species codes with "sp."
    species_aw = ifelse(
      species_code == "NONA" & is.na(species), "sp.", species_aw),
    species_aw = ifelse(
      species_code == "BUF#" & is.na(species), "sp.", species_aw),
    species_aw = ifelse(
      species_code == "CRY#" & is.na(species), "sp.", species_aw)
  )

# Which genera values in the data do not match AmphibiaWeb genera?
sort(unique(d$genus_aw)[which(!(unique(d$genus_aw) %in% unique(a.taxonomy$genus)))])

# Change genus names in cases where all species within a genera should be
# different
genera.to.change <- read_csv("data/taxonomy/genera_to_change.csv")

d <- d %>%
  left_join(., genera.to.change, by = "genus") %>%
  mutate(
    genus_aw.x = ifelse(is.na(genus_aw.y), genus_aw.x, genus_aw.y)
  ) %>%
  select(-genus_aw.y) %>%
  rename(genus_aw = genus_aw.x)

# Which genera values in the data do not match AmphibiaWeb genera?
sort(unique(d$genus_aw)[which(!(unique(d$genus_aw) %in% unique(a.taxonomy$genus)))])

# Change species names in cases where specific species need changing
species.to.change <- read_csv("data/taxonomy/species_to_change.csv")

d <- d %>%
  left_join(., species.to.change, by = c("genus", "species")) %>%
  mutate(
    genus_aw = ifelse(is.na(genus_aw.y), genus_aw.x, genus_aw.y),
    species_aw = ifelse(is.na(species_aw.y), species_aw.x, species_aw.y)
  ) %>%
  select(-genus_aw.x, -species_aw.x, -genus_aw.y, -species_aw.y)

# Now which genera values do not appear in AmphibiaWeb genera?
sort(unique(d$genus_aw)[which(!(unique(d$genus_aw) %in% unique(a.taxonomy$genus)))])

#==============================================================================


# Adding higher level taxonomy

# Add family-level information using AmphibiaWeb taxonomy
a.taxonomy.family <- a.taxonomy %>%
  distinct(family, genus, species)

# Make sure "sp." species are captured in the family-level taxonomy table
a.taxonomy.family.full <- a.taxonomy.family %>%
  bind_rows(
    .,
    distinct(a.taxonomy.family, family, genus) %>%
      mutate(species = rep("sp.", nrow(.)))
  ) %>%
  arrange(family, genus, species)

d <- left_join(d,
               a.taxonomy.family.full,
               by = c("genus_aw" = "genus", "species_aw" = "species")) %>%
  # In cases where the LEMIS data assigned a family name for the genus
  # field or a genus name could not be unambiguously assigned to the
  # AmphibiaWeb taxonomy, manually assign the correct family
  mutate(
    family = case_when(
      genus_aw == "Bufonidae" ~ "Bufonidae",
      genus_aw == "Caeciliidae" ~ "Caeciliidae",
      genus_aw == "Discoglossidae" ~ "Alytidae",
      genus_aw == "Paa" ~ "Dicroglossidae",
      TRUE ~ family
    )
  )

# Which genus/species combinations were we unable to assign a family?
# (should be only non-CITES entries and other ambiguous records)
filter(d, is.na(family)) %>%
  distinct(genus_aw, species_aw)

# Add order-level information using AmphibiaWeb taxonomy
a.taxonomy.order <- a.taxonomy %>%
  distinct(order, family)

d <- left_join(d,
               a.taxonomy.order,
               by = "family") %>%
  # In cases where the LEMIS data assigned an ambiguous name for the genus,
  # manually assign the correct order
  mutate(order = case_when(
    genus_aw == "Anura" ~ "Anura",
    genus_aw == "Schoutedenella" ~ "Anura",
    TRUE ~ order
  )
)

#==============================================================================


# Add on information about species' Lacey Act designation

l <- read_csv("data/reference/Lacey_Act_species.csv")
colnames(l) <- tolower(colnames(l))
l <- l %>%
  mutate(
    genus = str_trim(genus, side = "both"),
    species = str_trim(species, side = "both"),
    scientific_name = paste(genus, species, sep = " ")
  ) %>%
  # exclude particular species as needed
  # AmphibiaWeb considers Hynobius yatsui to be synonymous with Hynobius
  # stejnegeri, which is already in the dataset
  filter(scientific_name != "Hynobius yatsui") %>%
  # Triturus hongkongensis is a synonym for Paramesotriton hongkongensis,
  # which is already in the dataset
  filter(scientific_name != "Triturus hongkongensis") %>%
  # Tylototriton daweishanensis is a synonym for Tylototriton yangi, which is
  # already in the dataset
  filter(scientific_name != "Tylototriton daweishanensis") %>%
  mutate(
    genus = ifelse(
      genus == "Triturus" & species == "vittatus", "Ommatotriton", genus
    ),
    scientific_name = paste(genus, species, sep = " "),
    lacey_act = rep(1, nrow(.))
  )

# Confirm all species are in the AmphibiaWeb taxonomy
assert_that(
  sum(l$scientific_name %in% 
    paste(a.taxonomy$genus, a.taxonomy$species, sep = " ")) == nrow(l)
)

# Join the Lacey Act information in to the larger data frame
d <- left_join(
  d, select(l, genus, species, lacey_act), 
  by = c("genus_aw" = "genus", "species_aw" = "species")
) %>%
  mutate(lacey_act = ifelse(is.na(lacey_act), 0, lacey_act))
  
#==============================================================================


# Rearrange columns
d <- d %>%
  select(
    control_number, species_code, taxa, class, order, family,
    genus, genus_aw, species, species_aw, subspecies, 
    specific_name, generic_name, lacey_act,
    description, quantity, unit, value, 
    country_origin, country_origin_full, country_imp_exp, 
    purpose, source, action, disposition,
    disposition_date, disposition_year, shipment_date, shipment_year,
    import_export, port, us_co, foreign_co,
    cleaning_notes
  )

# Save the final dataset
write_csv(d, "data/cleaned/harmonized_amphibian_LEMIS_1999_to_2021.csv")
