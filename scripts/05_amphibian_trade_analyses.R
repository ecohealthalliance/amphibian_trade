library(tidyverse)
library(assertthat)

options(scipen = 10000)

#==============================================================================


# Import the full cleaned LEMIS amphibian dataset

a <- read_csv("data/cleaned/harmonized_amphibian_LEMIS_1999_to_2021.csv") %>%
  mutate(
    country_origin_full = 
      ifelse(country_origin == "TW", "Taiwan", country_origin_full),
    scientific_name = paste(genus_aw, species_aw)
  ) %>%
  left_join(
    .,
    lemis::lemis_codes() %>%
      filter(field == "port") %>%
      select(-field, -post_feb_2013) %>%
      rename(port = code, port_full = value),
    by = "port"
  ) 

#==============================================================================


# How many records overall?

nrow(a)

# How many shipments overall?

n_distinct(a$control_number)

# How many records are only reported at the level of "sp."?

a %>%
  filter(species_aw == "sp.") %>%
  nrow()/nrow(a)

# Generate a dataset representing live amphibian imports, recorded in numbers
# of individuals

a.live <- a %>%
  filter(description == "LIV", unit == "NO")

# How many live amphibian individuals are reported in the data?

sum(a.live$quantity)


# Generate a dataset representing amphibian leg and meat imports, 
# recorded in kg

a.leg.meat <- a %>%
  filter(description %in% c("LEG", "MEA"), unit == "KG")

# These are exclusively anurans

table(a.leg.meat$order, useNA = "ifany")


# Subset live amphibian data down to caudates

a.live.caudates <- a.live %>%
  filter(order == "Caudata")

# How many live caudate individuals are reported in the data?

sum(a.live.caudates$quantity)

#==============================================================================


# Import AmphibiaWeb taxonomy

aw.taxonomy <- 
  jsonlite::read_json("data/taxonomy/amphib_names.json", simplifyVector = T)

# Generate a vector of unique AmphibiaWeb genera

aw.genera <- aw.taxonomy %>%
  pull(genus) %>%
  unique() %>%
  sort()

# Generate a vector of unique AmphibiaWeb species

aw.species <- aw.taxonomy %>%
  select(genus, species) %>%
  mutate(scientific_name = paste(genus, species)) %>%
  pull(scientific_name) %>%
  unique() %>%
  sort()

# How many AmphibiaWeb species are in the full dataset?

sum(unique(a$scientific_name) %in% aw.species)

# What percentage of the dataset has good scientific names and good generic
# names?

sum(a$scientific_name %in% aw.species)/nrow(a)
nrow(filter(a, species_aw == "sp."))/nrow(a)
sum(a$genus_aw %in% aw.genera)/nrow(a)
nrow(filter(a, genus_aw == "Non-CITES entry"))/nrow(a)

#==============================================================================


# Generate a vector of amphibian genera restricted under the Lacey Act

lacey.act.genera <- read_csv("data/lacey_act/lacey_act_species.csv") %>%
  mutate(genus = str_trim(Genus, side = "both")) %>%
  pull(genus) %>%
  unique() %>%
  sort()

# Verify all of these genera match with AmphibiaWeb taxonomy

assert_that(sum(lacey.act.genera %in% aw.genera) == length(lacey.act.genera))


# Generate a vector of amphibian species restricted under the Lacey Act

lacey.act.species <- read_csv("data/lacey_act/lacey_act_species.csv") %>%
  mutate(
    genus = str_trim(Genus, side = "both"),
    species = str_trim(Species, side = "both"),
    scientific_name = paste(genus, species)
  ) %>%
  pull(scientific_name) %>%
  unique() %>%
  sort()

# Four of these species do not appear in AmphibiaWeb taxonomy, but their
# generic names are captured in "lacey.act.genera" (with the exception of
# Triturus vittatus, which is reclassified as Ommatotriton vittatus)

lacey.act.species[!(lacey.act.species %in% aw.species)]

# However, Ommatotriton vittatus is documented as a Lacey Act species in the
# LEMIS dataset

lacey.act.from.lemis <- a.live %>%
  filter(lacey_act == 1) %>%
  pull(scientific_name) %>%
  unique() %>%
  sort()

lacey.act.from.lemis[!(lacey.act.from.lemis %in% lacey.act.species)]

#==============================================================================


# Generate a vector of amphibian genera known to carry Bsal

bsal.carrier.genera <-
  c("Alytes", "Bombina", "Chioglossa", "Cynops", "Euproctus", 
    "Hydromantes", "Hynobius", "Ichthyosaura", "Lissotriton", "Neurergus",
    "Notophthalmus", "Onychodactylus", "Pachytriton", "Paramesotriton", 
    "Plethodon", "Pleurodeles", "Salamandra", "Salamandrella", "Salamandrina",
    "Siren", "Taricha", "Triturus", "Tylototriton")

assert_that(sum(bsal.carrier.genera %in% aw.genera) == length(bsal.carrier.genera))

bsal.summary.data <- 
  read_csv("~/Desktop/bsal species_countries list - Sheet1.csv")

bsal.carrier.genera <- bsal.summary.data %>%
  distinct(genus_aw) %>% 
  pull(genus_aw) %>%
  sort()

assert_that(sum(bsal.carrier.genera %in% aw.genera) == length(bsal.carrier.genera))

bsal.carrier.species <- bsal.summary.data %>%
  select(genus_aw, species_aw) %>%
  filter(species_aw != "spp.") %>%
  mutate(scientific_name = paste(genus_aw, species_aw)) %>%
  pull(scientific_name) %>%
  unique() %>%
  sort()

assert_that(sum(bsal.carrier.species %in% aw.species) == length(bsal.carrier.species))

# Generate a vector of highly traded Bsal carrier genera
# Pachytriton is a salamander genera not listed under Lacey Act 
# (neither is Bombina)

genera.of.interest <- c("Bombina", "Cynops", "Pachytriton", "Triturus") 

#==============================================================================


# Generate a vector of countries of interest where Bsal has been detected:
# Japan, Thailand, Vietnam, China, Taiwan, 
# Germany, Belgium, Netherlands, Spain

# May also want to include Hong Kong because Bsal has been detected in 
# Guangdong Province

countries.of.interest <- c("JP", "TH", "VN", "CN", "TW",
                           "DE", "BE", "NL", "ES",
                           "HK")

#==============================================================================


# Bsal summary counts


# Quantify the number of imported amphibians from countries with Bsal

a.live.bsal.countries <- a.live %>%
  filter(country_origin %in% countries.of.interest)
sum(a.live.bsal.countries$quantity)

a.live.bsal.countries.wild <- a.live.bsal.countries %>%
  filter(source == "W")
sum(a.live.bsal.countries.wild$quantity)


# Quantify the number of imported caudates from countries with Bsal

a.live.caudates.bsal.countries <- a.live.caudates %>%
  filter(country_origin %in% countries.of.interest)
sum(a.live.caudates.bsal.countries$quantity)


# Quantify the number of imported amphibians belonging to Bsal carrier genera 

a.live.bsal.carrier.genera <- a.live %>%
  filter(genus %in% bsal.carrier.genera)
sum(a.live.bsal.carrier.genera$quantity)

# Quantify the number of imported amphibians belonging to Bsal carrier species 

a.live.bsal.carrier.species <- a.live %>%
  filter(scientific_name %in% bsal.carrier.species)
sum(a.live.bsal.carrier.species$quantity)

#==============================================================================


# Bsal summary tables

# How many unique ports of entry for live amphibians?

unique(a.live$port_full) %>% sort()

# Summarize all live amphibian imports by port of entry

a.live %>%
  group_by(port, port_full) %>%
  summarize(n_shipments = n(),
            total_individuals = sum(quantity)) %>%
  arrange(desc(total_individuals))

# Summarize all live amphibian imports by country of origin

a.live %>%
  group_by(country_origin, country_origin_full) %>%
  summarize(n_shipments = n(), 
            total_individuals = sum(quantity)) %>%
  arrange(desc(total_individuals))

# Summarize all live amphibian imports by source

a.live %>%
  group_by(source) %>%
  summarize(n_shipments = n(), 
            total_individuals = sum(quantity)) %>%
  arrange(desc(total_individuals))

# What percentage of specimens are wild?

a.live %>%
  group_by(source) %>%
  summarize(n_shipments = n(), 
            total_individuals = sum(quantity)) %>%
  filter(source == "W") %>%
  pull(total_individuals)/sum(a.live$quantity)


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

# How has the live amphibian trade changed over time?

a.live %>%
  group_by(shipment_year, order) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  mutate(order = ifelse(is.na(order), "Unknown", order)) %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = order)) +
  geom_col() + 
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Order") +
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 16),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    legend.title = element_text(face = "bold", size = 16),
    plot.background = element_rect(color = "white")
  ) 

ggsave("outputs/live_amphibian_imports_over_time.png", 
       width = 10, height = 6)

a.live.table <- a.live %>%
  group_by(shipment_year) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup()

# What's the average number of live amphibians imported per year?

mean(a.live.table$quantity)

# What's the trend over time?

summary(lm(quantity ~ shipment_year, data = a.live.table))

# What's the average number of live amphibians imported per year in the most
# recent 5 years?

a.live.table %>%
  filter(shipment_year > 2016) %>%
  pull(quantity) %>%
  mean()

# Table giving percentages for the plot above

a.live %>%
  group_by(shipment_year, order) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  left_join(., a.live.table, by = "shipment_year") %>%
  mutate(yearly_percent = quantity.x/quantity.y*100)

a.leg.meat.table <- a.leg.meat %>%
  group_by(shipment_year) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup()
  
# What's the average kg of amphibian legs/meat imported per year?

mean(a.leg.meat.table$quantity)

#==============================================================================


# Question 2

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
  group_by(shipment_year, country_origin_mod) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = country_origin_mod)) +
  geom_col() + 
  # ggtitle("Trends in Live Amphibian Imports from All Countries") +
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Country") +
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 16),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    legend.title = element_text(face = "bold", size = 16),
    plot.background = element_rect(color = "white")
  ) 

ggsave("outputs/live_amphibian_imports_by_country.png", 
       width = 10, height = 6)

# Table giving percentages for the plot above

a.live %>%
  mutate(
    country_origin_mod = case_when(
      country_origin %in% countries.of.interest ~ country_origin_full,
      TRUE ~ "Non-Bsal Country"
    ) 
  ) %>%
  group_by(shipment_year, country_origin_mod) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  left_join(., a.live.table, by = "shipment_year") %>%
  mutate(yearly_percent = quantity.x/quantity.y*100)

a.live %>%
  mutate(
    country_origin_mod = case_when(
      country_origin %in% countries.of.interest ~ "Bsal Country",
      TRUE ~ "Non-Bsal Country"
    ) 
  ) %>%
  group_by(shipment_year, country_origin_mod) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  left_join(., a.live.table, by = "shipment_year") %>%
  mutate(yearly_percent = quantity.x/quantity.y*100)

my.main.title <- 
  expression(bold(paste("Trends in Live Amphibian Imports from ", 
                        bolditalic("Bsal"), 
                        " Positive Countries")))

a.live %>%
  filter(country_origin %in% countries.of.interest) %>%
  group_by(shipment_year, country_origin_full) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = country_origin_full)) +
  geom_col() + 
  # ggtitle(my.main.title) +
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Country") +
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 16),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    legend.title = element_text(face = "bold", size = 16),
    plot.background = element_rect(color = "white")
  ) 

ggsave("outputs/live_amphibian_imports_by_country_only_Bsal_positive.png", 
       width = 10, height = 6)

#==============================================================================


# Question 3

# Ports

ports.of.interest <- c("LA", "NY", "SF", "BV", "MI")

a.live %>%
  mutate(
    port_full_mod = case_when(
      port %in% ports.of.interest ~ port_full,
      TRUE ~ "Other"
    ),
    port_full_mod = as.factor(port_full_mod),
    port_full_mod = forcats::fct_relevel(port_full_mod, 
                                              "Other", after = Inf)
  ) %>%
  group_by(shipment_year, port_full_mod) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = port_full_mod)) +
  geom_col() + 
  # ggtitle("Trends in Live Amphibian Imports from All Countries") +
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Port") +
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 16),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    legend.title = element_text(face = "bold", size = 16),
    plot.background = element_rect(color = "white")
  ) 

ggsave("outputs/live_amphibian_imports_by_port.png", 
       width = 10, height = 6)

# Table giving percentages for the plot above

a.live %>%
  mutate(
    port_full_mod = case_when(
      port %in% ports.of.interest ~ port_full,
      TRUE ~ "Other"
    )
  ) %>%
  group_by(shipment_year, port_full_mod) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  left_join(., a.live.table, by = "shipment_year") %>%
  mutate(yearly_percent = quantity.x/quantity.y*100)

#==============================================================================


# Question 4

# How has the Lacey Act affected amphibian trade?

a.live %>%
  filter(genus_aw %in% lacey.act.genera) %>% 
  group_by(shipment_year, genus_aw) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = genus_aw)) +
  geom_col() + 
  # ggtitle(my.main.title) +
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Genus") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 16),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(face = "italic"),
    plot.background = element_rect(color = "white")
  ) 

ggsave("outputs/live_amphibian_imports_only_Lacey_Act.png",
       width = 10, height = 6)

a.live.lacey.table <- a.live %>%
  filter(genus_aw %in% lacey.act.genera) %>% 
  group_by(shipment_year) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup()

# No Lacey Act genera in 2018 at all, so add that data
a.live.lacey.table <- bind_rows(
  a.live.lacey.table, 
  data.frame(shipment_year = 2018, quantity = 0)
) %>%
  arrange(shipment_year)

# What's the average number of live amphibians imported per year?
mean(a.live.lacey.table$quantity)

# What's the trend over time?
summary(lm(quantity ~ shipment_year, data = a.live.lacey.table))

a.live %>%
  filter(
    genus_aw %in% lacey.act.genera,
    shipment_year > 2016
  ) %>%
  group_by(purpose, action) %>%
  summarize(quantity = sum(quantity))

a.live %>%
  mutate(
    genus_mod = case_when(
      genus_aw %in% lacey.act.genera ~ "Lacey Act Genera",
      TRUE ~ "Non-Lacey Act Genera"
    )
  ) %>%
  group_by(shipment_year, genus_mod) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = genus_mod)) +
  geom_col() + 
  #ggtitle(my.main.title) +
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Lacey Act Status") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 16),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    legend.title = element_text(face = "bold", size = 16),
    plot.background = element_rect(color = "white")
  ) 

ggsave("outputs/live_amphibian_imports_Lacey_Act_status.png",
       width = 10, height = 6)

#==============================================================================


# Question 5

# How many individuals of known Bsal carrier species were imported to the US?

a.live %>%
  filter(scientific_name %in% bsal.carrier.species) %>% 
  group_by(shipment_year, genus_aw) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = genus_aw)) +
  geom_col() + 
  # ggtitle(my.main.title) +
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Genus") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 16),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(face = "italic"),
    plot.background = element_rect(color = "white")
  ) 

ggsave("outputs/live_amphibian_imports_Bsal_carrier_species.png",
       width = 10, height = 6)

a.live.bsal.carrier.species.table <- a.live %>%
  filter(scientific_name %in% bsal.carrier.species) %>% 
  group_by(shipment_year, genus_aw, species_aw) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup()

n_distinct(a.live.bsal.carrier.species.table$genus_aw)
n_distinct(a.live.bsal.carrier.species.table$species_aw)

a.live.bsal.carrier.species.table %>%
  filter(shipment_year > 2016) %>%
  group_by(shipment_year) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup()

a.live %>%
  filter(genus_aw %in% bsal.carrier.genera) %>% 
  group_by(shipment_year, family, genus_aw) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = family)) +
  geom_col() + 
  # ggtitle(my.main.title) +
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Family") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 16),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    legend.title = element_text(face = "bold", size = 16),
    # legend.text = element_text(face = "italic"),
    plot.background = element_rect(color = "white")
  ) 

ggsave("outputs/live_amphibian_imports_Bsal_carrier_genera.png",
       width = 10, height = 6)

a.live.bsal.carrier.genera.table <- a.live %>%
  filter(genus_aw %in% bsal.carrier.genera) %>% 
  group_by(shipment_year, family, genus_aw) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup()

yearly.summary <- a.live.bsal.carrier.genera.table %>%
  group_by(shipment_year) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup()

yearly.summary %>%
  filter(shipment_year > 2016) %>%
  pull(quantity) %>%
  mean()

yearly.summary %>%
  left_join(., a.live.table, by = "shipment_year") %>%
  mutate(percentage = quantity.x/quantity.y*100)

a.live.bsal.carrier.genera.table %>%
  left_join(., yearly.summary, by = "shipment_year") %>%
  mutate(percentage = quantity.x/quantity.y*100)
