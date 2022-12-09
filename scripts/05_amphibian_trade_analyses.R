library(tidyverse)
library(assertthat)

options(scipen = 10000)

#==============================================================================


# Import the full cleaned LEMIS amphibian dataset

a <- read_csv("data/cleaned/harmonized_amphibian_LEMIS_1999_to_2021.csv") %>%
  # add variables for clarity
  mutate(
    country_origin_full = 
      ifelse(country_origin == "TW", "Taiwan", country_origin_full),
    scientific_name = paste(genus_aw, species_aw)
  ) %>%
  # import full port names
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
# But note that some years don't have "control_number" at all, so this would be
# an underestimate of shipment numbers

n_distinct(a$control_number)


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

# Good names down to species
sum(a$scientific_name %in% aw.species)/nrow(a)
# Proportion of records only reported to "sp."
nrow(filter(a, species_aw == "sp."))/nrow(a)
# Good names down to genus
sum(a$genus_aw %in% aw.genera)/nrow(a)
# Proportion of records recorded as "Non-CITES entry"
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

# How many live individuals from various genera have been imported from 1999-
# 2021 for stated commercial purposes?

a.live %>%
  filter(purpose == "T") %>%
  group_by(genus_aw) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  arrange(desc(quantity))


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


# Define palette for plotting
# https://zenodo.org/record/3381072#.Y4afhuzMJO0

Tol_bright <- c(
  "#EE6677", "#228833", "#4477AA", "#CCBB44", "#66CCEE", 
  "#AA3377", "#BBBBBB"
)

Tol_light <- c(
  "#BBCC33", "#AAAA00", "#77AADD", "#EE8866", "#EEDD88", 
  "#FFAABB", "#99DDFF", "#44BB99", "#DDDDDD"
)

Okabe_Ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
  "#D55E00", "#CC79A7", "#000000"
)

palette <- c(
  Okabe_Ito[-c(7)], 
  Tol_bright[-c(3, 5)]
)

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
  scale_fill_manual(values = palette) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 20),
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    legend.title = element_blank(),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(),
    plot.background = element_rect(color = "white")
  ) 

ggsave("outputs/live_amphibian_imports_over_time.png", 
       width = 10, height = 8)

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

key.bsal.countries <- c("CN", "HK", "TW")

a.live %>%
  mutate(
    country_origin_mod = case_when(
      country_origin %in% key.bsal.countries ~ country_origin_full,
      country_origin %in% countries.of.interest ~ "Other Bsal Country",
      TRUE ~ "Non-Bsal Country"
    ),
    country_origin_mod = as.factor(country_origin_mod),
    country_origin_mod = forcats::fct_relevel(country_origin_mod,
      "China", "Hong Kong", "Taiwan", "Other Bsal Country", "Non-Bsal Country")
  ) %>%
  group_by(shipment_year, country_origin_mod) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = country_origin_mod)) +
  geom_col() + 
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Country") +
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = palette) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 20),
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    legend.title = element_blank(),
    legend.position = c(0.8, 0.85),
    legend.background = element_rect(),
    plot.background = element_rect(color = "white")
  ) 

ggsave("outputs/live_amphibian_imports_by_country.png", 
       width = 10, height = 8)

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


# Plot with only Bsal countries

a.live %>%
  filter(country_origin %in% countries.of.interest) %>%
  group_by(shipment_year, country_origin_full) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = country_origin_full)) +
  geom_col() + 
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Country") +
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = palette) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 20),
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    legend.title = element_blank(),
    plot.background = element_rect(color = "white")
  ) 

ggsave("outputs/live_amphibian_imports_by_country_only_Bsal_positive.png", 
       width = 10, height = 8)

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
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Port") +
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = palette) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 20),
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    legend.title = element_blank(),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(),
    plot.background = element_rect(color = "white")
  ) 

ggsave("outputs/live_amphibian_imports_by_port.png", 
       width = 10, height = 8)

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

key.lacey.act.genera <- c(
  "Cynops", "Paramesotriton", "Pleurodeles", 
  "Salamandra", "Triturus", "Other Lacey Act Genera"
)

panel.a <- a.live %>%
  filter(genus_aw %in% lacey.act.genera) %>% 
  mutate(
    genus_aw = ifelse(
      genus_aw %in% key.lacey.act.genera,
      genus_aw,
      "Other Lacey Act Genera"
    ),
    genus_aw = as.factor(genus_aw),
    genus_aw = forcats::fct_relevel(genus_aw, key.lacey.act.genera)
  ) %>%
  group_by(shipment_year, genus_aw) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = genus_aw)) +
  geom_col() + 
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Genus") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = palette) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 20),
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    legend.title = element_blank(),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(),
    plot.background = element_rect(color = "white")
  ) 

panel.b <- a.live %>%
  filter(genus_aw %in% lacey.act.genera) %>% 
  filter(shipment_year > 2015) %>%
  mutate(
    genus_aw = ifelse(
      genus_aw %in% key.lacey.act.genera,
      genus_aw,
      "Other Lacey Act Genera"
    ),
    genus_aw = as.factor(genus_aw),
    genus_aw = forcats::fct_relevel(genus_aw, key.lacey.act.genera)
  ) %>%
  group_by(shipment_year, genus_aw) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = genus_aw)) +
  geom_col() + 
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Genus") +
  theme_minimal() +
  scale_x_continuous(breaks = 2016:2022) +
  scale_y_continuous(labels = scales::comma, n.breaks = 6) +
  scale_fill_manual(values = palette) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 16),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    legend.title = element_blank(),
    legend.position = "none",
    plot.background = element_rect(color = "white")
  ) 

panel.b <- cowplot::plot_grid(
  NULL, panel.b, NULL,
  nrow = 1, rel_widths = c(2, 6, 2),
  labels = c("", "b", ""), label_size = 20)

cowplot::plot_grid(
  panel.a, panel.b, 
  labels = c("a", ""), label_size = 20, 
  ncol = 1, rel_heights = c(6, 4)
)

ggsave("outputs/live_amphibian_imports_only_Lacey_Act.png",
       width = 10, height = 10)

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

# What's the stated purpose and clearance status of recent Lacey Act 
# genera shipments?
a.live %>%
  filter(
    genus_aw %in% lacey.act.genera,
    shipment_year > 2016
  ) %>%
  group_by(purpose, action) %>%
  summarize(quantity = sum(quantity))


# Plot of Lacey Act vs. non-Lacey Act status

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
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Lacey Act Status") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = palette) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 20),
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    legend.title = element_blank(),
    legend.position = c(0.8, 0.9),
    legend.background = element_rect(),
    plot.background = element_rect(color = "white")
  ) 

ggsave("outputs/live_amphibian_imports_Lacey_Act_status.png",
       width = 10, height = 8)

#==============================================================================


# Question 5

# How many individuals of known Bsal carrier species were imported to the US?


# Species-level analyses

key.bsal.carrier.genera <- 
  c("Ambystoma", "Anaxyrus", "Andrias", "Cynops", 
    "Paramesotriton", "Pleurodeles", "Salamandra", "Scaphiopus",
    "Triturus", "Other Genera")

panel.a <- a.live %>%
  filter(scientific_name %in% bsal.carrier.species) %>% 
  mutate(
    genus_aw = ifelse(
      genus_aw %in% key.bsal.carrier.genera,
      genus_aw,
      "Other Genera"
    ),
    genus_aw = as.factor(genus_aw),
    genus_aw = forcats::fct_relevel(genus_aw, key.bsal.carrier.genera)
  ) %>%
  group_by(shipment_year, genus_aw) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = genus_aw)) +
  geom_col() + 
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Genus") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = palette) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 20),
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    legend.title = element_blank(),
    legend.position = c(0.8, 0.75),
    legend.background = element_rect(),
    plot.background = element_rect(color = "white")
  ) 

panel.b <- a.live %>%
  filter(scientific_name %in% bsal.carrier.species) %>% 
  filter(shipment_year > 2015) %>%
  group_by(shipment_year, genus_aw) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = genus_aw)) +
  geom_col() + 
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Genus") +
  theme_minimal() +
  scale_x_continuous(breaks = 2016:2022) +
  scale_y_continuous(labels = scales::comma, n.breaks = 6) +
  scale_fill_manual(values = palette) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 16),
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    legend.title = element_blank(),
    legend.position = "none",
    plot.background = element_rect(color = "white")
  ) 

panel.b <- cowplot::plot_grid(
  NULL, panel.b, NULL,
  nrow = 1, rel_widths = c(2, 6, 2),
  labels = c("", "b", ""), label_size = 20)

cowplot::plot_grid(
  panel.a, panel.b, 
  labels = c("a", ""), label_size = 20, 
  ncol = 1, rel_heights = c(6, 4)
)

ggsave("outputs/live_amphibian_imports_Bsal_carrier_species.png",
       width = 10, height = 10)

a.live.bsal.carrier.species.table <- a.live %>%
  filter(scientific_name %in% bsal.carrier.species) %>% 
  group_by(shipment_year, scientific_name, genus_aw, species_aw) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup()

n_distinct(a.live.bsal.carrier.species.table$genus_aw)
n_distinct(a.live.bsal.carrier.species.table$species_aw)
n_distinct(a.live.bsal.carrier.species.table$scientific_name)

# How many of these Bsal carrier species were imported per year recently?
a.live.bsal.carrier.species.table %>%
  filter(shipment_year > 2016) %>%
  group_by(shipment_year) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup()


# Genera-level analyses

key.bsal.carrier.families <- 
  c("Bombinatoridae", "Ranidae", "Salamandridae", "Other Families")

panel.a <- a.live %>%
  filter(genus_aw %in% bsal.carrier.genera) %>% 
  mutate(
    family = ifelse(
      family %in% key.bsal.carrier.families,
      family,
      "Other Families"
    ),
    family = as.factor(family),
    family = forcats::fct_relevel(family, key.bsal.carrier.families)
  ) %>%
  group_by(shipment_year, family) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = family)) +
  geom_col() + 
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Family") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 4500000)) +
  scale_fill_manual(values = palette) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 20),
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    legend.title = element_blank(),
    legend.position = c(0.85, 0.9),
    legend.background = element_rect(),
    plot.background = element_rect(color = "white")
  ) 

panel.b <- a.live %>%
  filter(genus_aw %in% bsal.carrier.genera) %>% 
  filter(shipment_year > 2016) %>%
  mutate(
    country_origin_mod = case_when(
      country_origin %in% key.bsal.countries ~ country_origin_full,
      country_origin %in% countries.of.interest ~ "Other Bsal Country",
      TRUE ~ "Non-Bsal Country"
    ),
    country_origin_mod = as.factor(country_origin_mod),
    country_origin_mod = forcats::fct_relevel(country_origin_mod,
                                              "China", "Hong Kong", "Taiwan", "Other Bsal Country", "Non-Bsal Country")
  ) %>%
  group_by(shipment_year, country_origin_mod) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity, 
             fill = country_origin_mod)) +
  geom_col() + 
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Country") +
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma, limits = c(0, 4000000)) +
  scale_fill_manual(values = palette) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 20),
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    legend.title = element_blank(),
    legend.position = c(0.8, 0.85),
    legend.background = element_rect(),
    plot.background = element_rect(color = "white")
  )

panel.c <- a.live %>%
  filter(genus_aw %in% bsal.carrier.genera) %>% 
  filter(shipment_year > 2016) %>%
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
  labs(x = "Shipment Year", y = "Number of Individuals", fill = "Port") +
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma, limits = c(0, 4000000)) +
  scale_fill_manual(values = palette) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 20),
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    legend.title = element_blank(),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(),
    plot.background = element_rect(color = "white")
  ) 

panel.b <- cowplot::plot_grid(
  panel.b, panel.c,
  nrow = 1,
  labels = c("b", "c"), label_size = 20)

cowplot::plot_grid(
  panel.a, panel.b, 
  labels = c("a", ""), label_size = 20, 
  ncol = 1
)

ggsave("outputs/live_amphibian_imports_Bsal_carrier_genera.png",
       width = 16, height = 16)

a.live.bsal.carrier.genera.table <- a.live %>%
  filter(genus_aw %in% bsal.carrier.genera) %>% 
  group_by(shipment_year, family, genus_aw, country_origin, port_full) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup()

n_distinct(a.live.bsal.carrier.genera.table$genus_aw)

# How many of these were imported per year?
yearly.summary <- a.live.bsal.carrier.genera.table %>%
  group_by(shipment_year) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup()

# What's the average number in recent years?
yearly.summary %>%
  filter(shipment_year > 2016) %>%
  pull(quantity) %>%
  mean()

# Each year expressed as a percentage of overall live amphibian trade
yearly.summary %>%
  left_join(., a.live.table, by = "shipment_year") %>%
  mutate(percentage = quantity.x/quantity.y*100)

# Each genera expressed as a percentage of Bsal carrier trade
a.live.bsal.carrier.genera.table %>%
  left_join(., yearly.summary, by = "shipment_year") %>%
  mutate(percentage = quantity.x/quantity.y*100)

# Percentage of recent trade coming from countries
a.live.bsal.carrier.genera.table %>%
  filter(shipment_year > 2016) %>%
  group_by(shipment_year, country_origin) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  left_join(., yearly.summary, by = "shipment_year") %>%
  mutate(percentage = quantity.x/quantity.y*100)

# Percentage of recent trade coming to ports
a.live.bsal.carrier.genera.table %>%
  filter(shipment_year > 2016) %>%
  group_by(shipment_year, port_full) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  left_join(., yearly.summary, by = "shipment_year") %>%
  mutate(percentage = quantity.x/quantity.y*100)
