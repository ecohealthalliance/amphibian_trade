

# Load packages

library(tidyverse)
library(lemis)
library(zoo)

options(scipen = 10000)


# Check lemis version (should be 2.0.0)

lemis_version_current()

lemis_codes <- lemis_codes()
lemis_metadata <- lemis_metadata()

#==============================================================================


# Identify genera of records in LEMIS that were (at least sometimes) 
# classified without a taxa

genera.of.taxa.NAs <- lemis_data() %>% 
  filter(is.na(taxa)) %>%
  pull(genus) %>%
  unique()

# Get amphibian data from LEMIS

a.strict <- lemis_data() %>%
  filter(taxa == "amphibian")

nrow(a.strict)

# Which genera in genera.of.taxa.NAs are from amphibians?

genera.to.keep <- 
  genera.of.taxa.NAs[which(genera.of.taxa.NAs %in% unique(a.strict$genus))]
genera.to.keep <- genera.to.keep[!is.na(genera.to.keep)]

# Pull in new amphibian data for use that is picking up even those amphibian
# records for which taxa data is missing

a <- lemis_data() %>%
  filter(taxa == "amphibian" | (is.na(taxa) & genus %in% genera.to.keep),
         !(is.na(taxa) & genus == "noncites entry")) %>%
  # Add in shipment data in various summarized formats
  mutate(
    shipment_year = lubridate::year(shipment_date),
    shipment_month = lubridate::month(shipment_date),
    shipment_yearmon = as.yearmon(shipment_date),
    shipment_yearqtr = as.yearqtr(shipment_date)
  )

nrow(a)

# Add a column to the dataframe giving the full country of origin name

a <- filter(lemis_codes, field == "country") %>%
  select(code, value) %>%
  rename(country_origin_full = value) %>%
  left_join(a, ., by = c("country_origin" = "code"))

nrow(a)

# Correct spelling errors in LEMIS taxonomic information

a <- a %>%
  mutate(
    genus = case_when(
      # correct misspelling of "afrixalus" 
      # ("afrilaxus" is not a valid amphibian genus)
      genus == "afrilaxus" ~ "afrixalus",
      # correct misspelling of "tomopterna" 
      # ("tompterna" is not a valid amphibian genus)
      genus == "tompterna" ~ "tomopterna",
      TRUE ~ genus
    ),
    species = case_when(
      # "hyla marmurata" seems to be a misspelling of 
      # "hyla marmorata"
      genus == "hyla" & species == "marmurata" ~ "marmorata",
      # "hyperolius puncturatus" seems to be a misspelling of 
      # "hyperolius punctulatus"
      genus == "hyperolius" & species == "puncturatus" ~ "punctulatus",
      # "leptolalax peloclytoicles" seems to be a misspelling of
      # "leptolalax pelodytoides"
      genus == "leptolalax" & species == "peloclytoicles" ~ "pelodytoides",
      # "rana catesbeinana" seems to be a misspelling of
      # "rana catesbeiana"
      genus == "rana" & species == "catesbeinana" ~ "catesbeiana",
      # "sp" or "sp." should be synonymized to "spp."
      species == "sp" | species == "sp." ~ "spp.",
      TRUE ~ species
    )
  )

# Import AmphibiaWeb taxonomy

a.taxonomy <- read_csv("data/amphib_names.csv") %>%
  mutate(genus_mod = tolower(genus),
         species_mod = tolower(species))

# Which LEMIS genera values do not appear in AmphibiaWeb genera?

unique(a$genus)[which(!(unique(a$genus) %in% unique(a.taxonomy$genus_mod)))]

# Generate new generic and species names to synonymize with AmphibiaWeb

a <- a %>%
  mutate(
    genus_mod = case_when(
      # rename "batrachophrynus" to "telmatobius"
      # (according to AmphibiaWeb, all Batrachophrynus should be Telmatobius)
      genus == "batrachophrynus" ~ "telmatobius",
      # rename "caudiverbera caudiverbera" to "calyptocephalella gayi"
      genus == "caudiverbera" & species == "caudiverbera" ~ 
        "calyptocephalella",
      # rename "ceratobatrachus guentheri" to "cornufer guentheri"
      genus == "ceratobatrachus" & species == "guentheri" ~ "cornufer",
      # rename "chirixalus nongkhorensis" to "chiromantis nongkhorensis"
      genus == "chirixalus" & species == "nongkhorensis" ~ "chiromantis",
      # rename "chirixalus vittatus" to "feihyla vittatus"
      genus == "chirixalus" & species == "vittatus" ~ "feihyla",
      # rename "cryptophyllobates" to "hyloxalus"
      # (according to AmphibiaWeb, all Cryptophyllobates should be Hyloxalus)
      genus == "cryptophyllobates" ~ "hyloxalus",
      # rename "leptolalax" to "leptobrachella"
      # (according to AmphibiaWeb, all Leptolalax should be Leptobrachella)
      genus == "leptolalax" ~ "leptobrachella",
      # rename "lithobates" to "rana"
      # (according to AmphibiaWeb, all Lithobates should be Rana)
      genus == "lithobates" ~ "rana",
      # rename "ooedozyga" to "occidozyga"
      # (I believe Ooedozyga is a synonym or old name for Occidozyga)
      genus == "ooedozyga" ~ "occidozyga",
      # rename "paa spinosa" to "quasipaa spinosa"
      genus == "paa" & species == "spinosa" ~ "quasipaa",
      # rename "pachymedusa dacnicolor" to "agalychnis dacnicolor"
      genus == "pachymedusa" & species == "dacnicolor" ~ "agalychnis",
      # rename "phrynohyas" to "trachycephalus"
      # (according to AmphibiaWeb, all Phrynohyas should be Trachycephalus)
      genus == "phrynohyas" ~ "trachycephalus",
      # rename "phrynomerus" to "phrynomantis"
      # (I believe Phrynomerus is a synonym or old name for Phrynomantis)
      genus == "phrynomerus" ~ "phrynomantis",
      # rename "silurana" to "xenopus"
      # (according to AmphibiaWeb, all Silurana should be Xenopus)
      genus == "silurana" ~ "xenopus",
      # rename "speleomantes" to "hydromantes"
      # (according to AmphibiaWeb, all Speleomantes should be Hydromantes)
      genus == "speleomantes" ~ "hydromantes",
      # rename "typhlomolge" to "eurycea"
      # (according to AmphibiaWeb, all Typhlomolge should be Eurycea)
      genus == "typhlomolge" ~ "eurycea",
      # rename "vibrissaphora" to "leptobrachium"
      # (according to AmphibiaWeb, all Vibrissaphora should be Leptobrachium)
      genus == "vibrissaphora" ~ "leptobrachium",
      TRUE ~ genus
    ),
    species_mod = case_when(
      # change "caudiverbera caudiverbera" to "calyptocephalella gayi"
      genus == "caudiverbera" & species == "caudiverbera" ~ "gayi",
      TRUE ~ species
    )
  )

# Now which LEMIS genera values do not appear in AmphibiaWeb genera?

unique(a$genus_mod)[which(!(unique(a$genus_mod) %in% unique(a.taxonomy$genus_mod)))]

# Add Order-level information using AmphibiaWeb taxonomy

a <- left_join(a,
               distinct(a.taxonomy, order, genus_mod),
               by = "genus_mod") %>%
  # In cases where the LEMIS data assigned a family name for the genus
  # field or a old genus name could not be unambiguously assigned to the
  # AmphibiaWeb taxonomy, manually assign the correct Order
  mutate(order = case_when(
    genus_mod == "bufonidae" ~ "Anura",
    genus_mod == "caeciliidae" ~ "Gymnophiona",
    genus_mod == "discoglossidae" ~ "Anura",
    genus_mod == "chirixalus" ~ "Anura",
    genus_mod == "paa" ~ "Anura",
    TRUE ~ order
  ))

nrow(a)

# Which genus/species combinations were we unable to assign an Order?
# (should be only non-CITES entries and records with an unknown genus)

filter(a, is.na(order)) %>%
  distinct(genus_mod, species_mod)

#==============================================================================


# Generate a dataset representing live amphibian imports, recorded in numbers
# of individuals

a.live <- a %>%
  filter(description == "LIV",
         unit == "NO")

# How many live amphibian individuals are reported in the data?

sum(a.live$quantity)


# How many live animal imports are reported in total in the LEMIS data?

lemis.live <- lemis_data() %>%
  filter(description == "LIV",
         unit == "NO")

sum(as.numeric(lemis.live$quantity))


# Subset live amphibian data down to caudates

a.live.caudates <- a.live %>%
  filter(order == "Caudata")

# How many live caudate individuals are reported in the data?

sum(a.live.caudates$quantity)


# Generate a vector of amphibian genera restricted under the Lacey Act

lacey.act.genera <- 
  c("chioglossa", "cynops", "euproctus", "hydromantes", "hynobius", 
    "ichthyosaura", "lissotriton", "neurergus", "notophthalmus", "onychodactylus",
    "paramesotriton", "plethodon", "pleurodeles", "salamandra", "salamandrella",
    "salamandrina", "siren", "taricha", "triturus", "tylototriton")

# Generate a vector of amphibian genera known to carry Bsal

bsal.carrier.genera <-
  c("alytes", "bombina", "chioglossa", "cynops", "euproctus", 
    "hydromantes", "hynobius", "ichthyosaura", "lissotriton", "neurergus",
    "notophthalmus", "onychodactylus", "pachytriton", "paramesotriton", 
    "plethodon", "pleurodeles", "salamandra", "salamandrella", "salamandrina",
    "siren", "taricha", "triturus", "tylototriton")

# Generate a vector of highly traded Bsal carrier genera
# Pachytriton is a salamander genera not listed under Lacey Act 
# (neither is Bombina)

genera.of.interest <- c("bombina", "cynops", "pachytriton", "triturus") 

# Generate a vector of countries of interest where Bsal has been detected:
# Japan, Thailand, Vietnam, China, Germany, Belgium, Netherlands

countries.of.interest <- c("JP", "TH", "VN", "CN", "DE", "BE", "NL") 

#==============================================================================


# Bsal summary counts


# Quantify the number of imported amphibians from countries with Bsal

a.live.bsal.countries <- 
  filter(a.live, country_origin %in% countries.of.interest)
sum(a.live.bsal.countries$quantity)

a.live.bsal.countries.wild <- filter(a.live.bsal.countries, source == "W")
sum(a.live.bsal.countries.wild$quantity)


# Quantify the number of imported caudates from countries with Bsal

a.live.caudates.bsal.countries <- 
  filter(a.live.caudates, country_origin %in% countries.of.interest)
sum(a.live.caudates.bsal.countries$quantity)


# Quantify the number of imported amphibians belonging to Bsal carrier genera 

a.live.bsal.carrier.genera <- filter(a.live, genus %in% bsal.carrier.genera)
sum(a.live.bsal.carrier.genera$quantity)

#==============================================================================


# Bsal summary tables


# Summarize all live amphibian imports by port of entry

a.live %>%
  group_by(port) %>%
  summarize(n_shipments = n(),
            total_individuals = sum(quantity)) %>%
  arrange(desc(total_individuals))

# Summarize all live amphibian imports by country of origin

a.live %>%
  group_by(country_origin, country_origin_full) %>%
  summarize(n_shipments = n(), 
            total_individuals = sum(quantity)) %>%
  arrange(desc(total_individuals))


# Summarize live amphibian imports of Bsal carrier genera by port of entry

a.live.bsal.carrier.genera %>%
  group_by(port) %>%
  summarize(n_shipments = n(), 
            total_individuals = sum(quantity)) %>%
  arrange(desc(total_individuals))

# Summarize live amphibian imports of Bsal carrier genera by country of origin

a.live.bsal.carrier.genera %>%
  group_by(country_origin, country_origin_full) %>%
  summarize(n_shipments = n(), 
            total_individuals = sum(quantity)) %>%
  arrange(desc(total_individuals))


# Summarize live amphibian imports from Bsal endemic countries by port of entry

a.live.bsal.countries %>%
  group_by(port) %>%
  summarize(n_shipments = n(), 
            total_individuals = sum(quantity)) %>%
  arrange(desc(total_individuals))

# Summarize live amphibian imports from Bsal endemic countries by country of
# origin

a.live.bsal.countries %>%
  group_by(country_origin) %>%
  summarize(n_shipments = n(), 
            total_individuals = sum(quantity)) %>%
  arrange(desc(total_individuals))


# Summarize live caudate imports from Bsal endemic countries by port of entry

a.live.caudates.bsal.countries %>%
  group_by(port) %>%
  summarize(n_shipments = n(), 
            total_individuals = sum(quantity)) %>%
  arrange(desc(total_individuals))

# Summarize live caudate imports from Bsal endemic countries by country of
# origin

a.live.caudates.bsal.countries %>%
  group_by(country_origin) %>%
  summarize(n_shipments = n(), 
            total_individuals = sum(quantity)) %>%
  arrange(desc(total_individuals))

#==============================================================================


# Question 1
# What percentage of amphibians imported to the US are coming from countries 
# with Bsal and how has this changed over time?  

a.live %>%
  mutate(
    country_origin_mod = case_when(
      country_origin %in% countries.of.interest ~ country_origin_full,
      TRUE ~ "Non-Bsal Country"
    ),
    country_origin_mod = as.factor(country_origin_mod),
    country_origin_mod = forcats::fct_relevel(country_origin_mod, 
                                              "Non-Bsal Country", after = Inf)
  ) %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = country_origin_mod)) +
  geom_col() + 
  ggtitle("Trends in Live Amphibian Imports from All Countries") +
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Country") +
  # scale_x_continuous(limits = c(1999, 2016), breaks = 2000:2015) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold")) 

ggsave("outputs/live_amphibian_imports_by_country.png", 
       width = 10, height = 6)


my.main.title <- 
  expression(bold(paste("Trends in Live Amphibian Imports from ", 
                        bolditalic("Bsal"), 
                        " Positive Countries")))

a.live %>%
  filter(country_origin %in% countries.of.interest) %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = country_origin_full)) +
  geom_col() + 
  ggtitle(my.main.title) +
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Country") +
  # scale_x_continuous(limits = c(1999, 2016), breaks = 2000:2015) +
  theme_minimal() + 
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold")) 

ggsave("outputs/live_amphibian_imports_by_country_only_Bsal_positive.png", 
       width = 10, height = 6)


# Question 2
# How many individuals of known Bsal carrier species were imported to the US?

my.main.title <- 
  expression(bold(paste("Trends in Live Amphibian Imports of Heavily Traded ", 
                        bolditalic("Bsal"), 
                        " Carrier Genera")))

a.live %>%
  filter(genus %in% genera.of.interest) %>% 
  mutate(genus = Hmisc::capitalize(genus)) %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = genus)) +
  geom_col() + 
  ggtitle(my.main.title) +
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Genus") +
  theme_minimal() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "italic"))

ggsave("outputs/live_amphibian_imports_Bsal_carrier_genera.png",
       width = 10, height = 6)
