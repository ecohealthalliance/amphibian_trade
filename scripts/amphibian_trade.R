

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


# Identify genera of records in lemis that were (at least sometimes) 
# classified without a taxa

genera.of.taxa.NAs <- lemis_data() %>% 
  filter(is.na(taxa)) %>%
  pull(genus) %>%
  unique()

# Get amphibian data from lemis

a.strict <- lemis_data() %>%
  filter(taxa == "amphibian")

# Which genera in genera.of.taxa.NAs are from amphibians?

genera.to.keep <- 
  genera.of.taxa.NAs[which(genera.of.taxa.NAs %in% unique(a.strict$genus))]
genera.to.keep <- 
  genera.to.keep[!is.na(genera.to.keep)]

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
  filter(generic_name %in% c("AMPHIUMA", "AXOLOTL", "MUDPUPPY", 
                             "NEWT", "OLM", "SALAMANDER", 
                             "SIREN", "WATERDOG"))

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
  group_by(country_origin) %>%
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
  group_by(country_origin) %>%
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


# Add a column to the dataframe giving the full country of origin name

a.live <- filter(lemis_codes, field == "country") %>%
  select(code, value) %>%
  rename(country_origin_full = value) %>%
  left_join(a.live, ., by = c("country_origin" = "code"))


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
