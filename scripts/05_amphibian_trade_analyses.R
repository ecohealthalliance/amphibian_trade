library(tidyverse)
library(ggtext)
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


# How many amphibian records are there overall?

nrow(a)

# How many amphibian shipments overall?
# But note that some years don't have "control_number" at all, so this is
# an underestimate of shipment numbers

n_distinct(a$control_number)


# Generate a dataset representing live amphibian imports, recorded in numbers
# of individuals

a.live <- a %>%
  filter(description == "LIV", unit == "NO")

# How many live amphibian individuals are reported in the data?

a.live.individuals <- sum(a.live$quantity)
a.live.individuals


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
  mutate(scientific_name = paste(genus, species)) %>%
  pull(scientific_name) %>%
  unique() %>%
  sort()

# How many AmphibiaWeb species are in the full LEMIS amphibian dataset?

sum(unique(a$scientific_name) %in% aw.species)

# What percentage of the dataset has good scientific names and good generic
# names?

# Good names down to species
sum(a$scientific_name %in% aw.species)/nrow(a)
# Proportion of records only reported to "sp." for species
nrow(filter(a, species_aw == "sp."))/nrow(a)
# Good names down to genus
sum(a$genus_aw %in% aw.genera)/nrow(a)
# Proportion of records recorded as "Non-CITES entry" for genus
nrow(filter(a, genus_aw == "Non-CITES entry"))/nrow(a)

#==============================================================================


# Generate a vector of amphibian genera restricted under the Lacey Act

lacey.act.genera <- read_csv("data/reference/Lacey_Act_species.csv") %>%
  mutate(genus = str_trim(Genus, side = "both")) %>%
  pull(genus) %>%
  unique() %>%
  sort()

# Verify all of these genera match with AmphibiaWeb taxonomy

assert_that(sum(lacey.act.genera %in% aw.genera) == length(lacey.act.genera))


# Generate a vector of amphibian species restricted under the Lacey Act

lacey.act.species <- read_csv("data/reference/Lacey_Act_species.csv") %>%
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

# Import the data table

bsal.summary.data <- 
  read_csv("data/reference/Bsal_infection_summary.csv")

nrow(bsal.summary.data)

# Which types of infection observations are present?

table(bsal.summary.data$type, useNA = "ifany")

# Verify that the species name never changed when updating from raw 
# to AW taxonomy

assert_that(sum(bsal.summary.data$species == bsal.summary.data$species_aw) ==
              nrow(bsal.summary.data))

# Verify that each taxa is only assigned a single Lacey Act status

bsal.summary.data %>%
  group_by(genus_aw, species_aw) %>%
  summarize(n_categories = n_distinct(`Lacey Act listed?`)) %>%
  pull(n_categories) %>%
  max()

# Which amphibian orders are represented in the Bsal carrier data?

sort(unique(bsal.summary.data$order))

# Which amphibian families are represented in the Bsal carrier data?

sort(unique(bsal.summary.data$family))

# Create a vector of Bsal carrier genera

bsal.carrier.genera <- bsal.summary.data %>%
  pull(genus_aw) %>%
  unique() %>%
  sort()

assert_that(sum(bsal.carrier.genera %in% aw.genera) == length(bsal.carrier.genera))

length(bsal.carrier.genera)

# Create a vector of Bsal carrier species

bsal.carrier.species <- bsal.summary.data %>%
  filter(species_aw != "sp.") %>%
  mutate(scientific_name = paste(genus_aw, species_aw)) %>%
  pull(scientific_name) %>%
  unique() %>%
  sort()

assert_that(sum(bsal.carrier.species %in% aw.species) == length(bsal.carrier.species))

length(bsal.carrier.species)

# Investigate which of these species was Lacey Act Listed

bsal.summary.data %>%
  distinct(genus_aw, species_aw, `Lacey Act listed?`) %>%
  filter(species_aw != "sp.") %>%
  mutate(
    scientific_name = paste(genus_aw, species_aw),
    lacey_act_check = ifelse(scientific_name %in% lacey.act.species, 1, 0)
  )

#==============================================================================


# In which countries has Bsal been found in the wild?

bsal.summary.data %>%
  filter(type == "wild") %>%
  pull(country) %>%
  unique() %>%
  sort()

# Generate a vector of countries of interest where Bsal has been detected in
# the wild:
# Belgium, China, Germany, Japan, Spain, 
# Taiwan, Thailand, The Netherlands, Vietnam

# May also want to include Hong Kong because Bsal has been detected in 
# Guangdong Province

countries.of.interest <- c("BE", "CN", "DE", "JP", "ES",
                           "TW", "TH", "NL", "VN", "HK")

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


# Summary tables


# How many unique ports of entry for live amphibians?

a.live %>% 
  pull(port_full) %>%
  unique() %>%
  sort()

# Summarize all live amphibian imports by port of entry

a.live %>%
  group_by(port, port_full) %>%
  summarize(
    n_shipments = n(),
    total_individuals = sum(quantity),
    percent_individuals = total_individuals/a.live.individuals*100
  ) %>%
  arrange(desc(total_individuals))

# Summarize all live amphibian imports by country of origin

a.live %>%
  group_by(country_origin, country_origin_full) %>%
  summarize(
    n_shipments = n(), 
    total_individuals = sum(quantity),
    percent_individuals = total_individuals/a.live.individuals*100
  ) %>%
  arrange(desc(total_individuals))

# Summarize all live amphibian imports by source

a.live %>%
  group_by(source) %>%
  summarize(
    n_shipments = n(), 
    total_individuals = sum(quantity),
    percent_individuals = total_individuals/a.live.individuals*100
  ) %>%
  arrange(desc(total_individuals))

# How many live individuals from various genera have been imported from 1999-
# 2021 for stated commercial purposes?

a.live %>%
  filter(purpose == "T") %>%
  group_by(genus_aw) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  arrange(desc(quantity))

# Summarize information about reported re-exports of amphibians

a.live %>%
  mutate(
    matching_countries = ifelse(country_origin == country_imp_exp, 1, 0)
  ) %>%
  group_by(matching_countries) %>%
  summarize(
    n_shipments = n(),
    n_individuals = sum(quantity)
  ) %>%
  ungroup() %>%
  mutate(
    total_shipments = sum(n_shipments),
    total_individuals = sum(n_individuals),
    percent_shipments = n_shipments/total_shipments,
    percent_individuals = n_individuals/total_individuals
  )
  

# Summarize live amphibian imports of Bsal carrier genera by port of entry

a.live.bsal.carrier.genera %>%
  group_by(port, port_full) %>%
  summarize(
    n_shipments = n(), 
    total_individuals = sum(quantity)) %>%
  arrange(desc(total_individuals))

# Summarize live amphibian imports of Bsal carrier genera by country of origin

a.live.bsal.carrier.genera %>%
  group_by(country_origin, country_origin_full) %>%
  summarize(
    n_shipments = n(), 
    total_individuals = sum(quantity)) %>%
  arrange(desc(total_individuals))


# Summarize live amphibian imports from Bsal endemic countries by port of entry

a.live.bsal.countries %>%
  group_by(port, port_full) %>%
  summarize(
    n_shipments = n(), 
    total_individuals = sum(quantity)) %>%
  arrange(desc(total_individuals))

# Summarize live amphibian imports from Bsal endemic countries by country of
# origin

a.live.bsal.countries %>%
  group_by(country_origin, country_origin_full) %>%
  summarize(
    n_shipments = n(), 
    total_individuals = sum(quantity)) %>%
  arrange(desc(total_individuals))


# Summarize live caudate imports from Bsal endemic countries by port of entry

a.live.caudates.bsal.countries %>%
  group_by(port, port_full) %>%
  summarize(
    n_shipments = n(), 
    total_individuals = sum(quantity)) %>%
  arrange(desc(total_individuals))

# Summarize live caudate imports from Bsal endemic countries by country of
# origin

a.live.caudates.bsal.countries %>%
  group_by(country_origin, country_origin_full) %>%
  summarize(
    n_shipments = n(), 
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
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 22),
    legend.position = c(0.8, 0.85),
    legend.background = element_rect(),
    legend.spacing.y = unit(1, "mm"),
    plot.background = element_rect(color = "white")
  ) 

ggsave("outputs/Fig1.png", width = 10, height = 8)

a.live.table <- a.live %>%
  group_by(shipment_year) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  mutate(
    shipment_year_s = shipment_year - 2016,
    before_after = ifelse(shipment_year >= 2016, 1, 0)
  )

# What's the average number of live amphibians imported per year?

mean(a.live.table$quantity)


# What's the trend over time?

# Fit model
out <- glm(
  quantity ~ shipment_year_s, 
  data = a.live.table, 
  family = poisson
)

# Summarize model
summary(out)
fitted <- fitted.values(out)
diffs <- sapply(2:length(fitted), function(x) fitted[x] - fitted[x-1])
mean(diffs)

# Visualize model
plot(quantity ~ shipment_year, data = a.live.table, ylim = c(0, 1e7))
lines(a.live.table$shipment_year, fitted)


# What's the average number of live amphibians imported per year since 2016?

a.live.table %>%
  filter(shipment_year >= 2016) %>%
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

# Break the leg and meat trade down by species

a.leg.meat %>%
  group_by(genus_aw, species_aw) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  arrange(desc(quantity)) %>%
  mutate(
    tot_quantity = sum(a.leg.meat$quantity),
    percent = quantity/tot_quantity
  )

#==============================================================================


# Question 2

# What percentage of live amphibians imported to the US are coming from
# countries with Bsal and how has this changed over time?  

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
  scale_fill_manual(
    values = palette,
    breaks = c("China", "Hong Kong", "Taiwan",
               "Other Bsal Country", "Non-Bsal Country"),
    labels = c("China", "Hong Kong", "Taiwan",
               "Other *Bsal* Country", "Non-*Bsal* Country"),
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 20),
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank(),
    legend.text = element_markdown(size = 22),
    legend.position = c(0.75, 0.85),
    legend.background = element_rect(),
    legend.spacing.y = unit(1, "mm"),
    plot.background = element_rect(color = "white")
  ) 

ggsave("outputs/Fig2.png", width = 10, height = 8)

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


# Compare with exporting countries for wildlife trade more generally

l <- lemis::lemis_data() %>%
  filter(description == "LIV") %>%
  collect()

total.live.individuals <- sum(l$quantity)

l %>%
  group_by(country_origin) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  mutate(percent = quantity/total.live.individuals*100) %>%
  arrange(desc(quantity))


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
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 22),
    legend.position = c(0.8, 0.85),
    legend.background = element_rect(),
    legend.spacing.y = unit(1, "mm"),
    plot.background = element_rect(color = "white")
  ) 

ggsave("outputs/Fig3.png", width = 10, height = 8)

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


# Compare with ports of entry for wildlife trade more generally

l %>%
  group_by(port) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  mutate(percent = quantity/total.live.individuals*100) %>%
  arrange(desc(quantity))

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
  scale_fill_manual(
    values = palette,
    breaks = c("Cynops", "Paramesotriton", "Pleurodeles",
               "Salamandra", "Triturus", "Other Lacey Act Genera"),
    labels = c("*Cynops*", "*Paramesotriton*", "*Pleurodeles*",
               "*Salamandra*", "*Triturus*", "Other Lacey Act Genera")
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 20),
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank(),
    legend.text = element_markdown(size = 22),
    legend.position = c(0.75, 0.8),
    legend.background = element_rect(),
    legend.spacing.y = unit(1, "mm"),
    plot.background = element_rect(color = "white")
  ) 

panel.b <- a.live %>%
  filter(genus_aw %in% lacey.act.genera) %>% 
  filter(shipment_year >= 2016) %>%
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
    panel.grid.minor.x = element_blank(),
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

ggsave("outputs/Fig4.png", width = 10, height = 10)

a.live.lacey.table <- a.live %>%
  filter(genus_aw %in% lacey.act.genera) %>% 
  group_by(shipment_year) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup()

nrow(a.live.lacey.table)

# No Lacey Act genera in 2018 at all, so add that data
a.live.lacey.table <- bind_rows(
  a.live.lacey.table, 
  data.frame(shipment_year = 2018, quantity = 0)
) %>%
  arrange(shipment_year) %>%
  mutate(
    shipment_year_s = shipment_year - 2016,
    before_after = ifelse(shipment_year >= 2016, 1, 0)
  )

nrow(a.live.lacey.table)

# What's the average number of live amphibians imported per year?
mean(a.live.lacey.table$quantity)

# What's the trend over time?

# Fit model
out <- glm(
  quantity ~ shipment_year_s, 
  data = a.live.lacey.table, 
  family = poisson
)

# Summarize model
summary(out)
fitted <- fitted.values(out)
diffs <- sapply(2:length(fitted), function(x) fitted[x] - fitted[x-1])
mean(diffs)

# Visualize model
plot(quantity ~ shipment_year, data = a.live.lacey.table, ylim = c(0, 1e6))
lines(a.live.table$shipment_year, fitted)


# Impact evaluation analysis

# Model-fitting for the intervention group (Lacey Act listed species)
out.i <- glm(
  quantity ~ shipment_year_s * before_after, 
  data = a.live.lacey.table, 
  family = poisson
)
summary(out.i)
fitted.i <- fitted.values(out.i)

# Create panel for plot
fitted.df.i <- data.frame(
  shipment_year = a.live.lacey.table$shipment_year, 
  pred = fitted.i
)

panel.a <- a.live.lacey.table %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity)) +
  geom_col(fill = alpha("black", 0.4)) + 
  geom_line(
    aes(x = shipment_year + 0.5, y = pred), 
    col = "black", size = 1.5,
    data = fitted.df.i[1:17,]
  ) +
  geom_line(
    aes(x = shipment_year + 0.5, y = pred), 
    col = "black", size = 1.5,
    data = fitted.df.i[18:23,]
  ) +
  geom_vline(xintercept = 2016, size = 1, lty = 2) +
  labs(x = "", y = "Number of Individuals") +
  xlim(1999, 2022) +
  theme_minimal() +
  scale_y_continuous(
    labels = scales::comma, 
    limits = c(0, 1e6)
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 20),
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(color = "white")
  ) 

# Repeat the same analysis for the control group (non-listed species)
a.live.non.lacey.table <- a.live %>%
  filter(!(genus_aw %in% lacey.act.genera)) %>% 
  group_by(shipment_year) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  mutate(
    shipment_year_s = shipment_year - 2016,
    before_after = ifelse(shipment_year >= 2016, 1, 0)
  )

nrow(a.live.non.lacey.table)

# Verify that "a.live.non.lacey.act.table" represents all live trade
# not represented by "a.live.lacey.act.table"
sum(a.live.lacey.table$quantity) + sum(a.live.non.lacey.table$quantity) ==
  sum(a.live$quantity)

# What's the average number of live amphibians imported per year?
mean(a.live.non.lacey.table$quantity)

# Model-fitting
out.c <- glm(
  quantity ~ shipment_year_s * before_after, 
  data = a.live.non.lacey.table, 
  family = poisson
)
summary(out.c)
fitted.c <- fitted.values(out.c)

# Create a panel for plot
fitted.df.c <- data.frame(
  shipment_year = a.live.non.lacey.table$shipment_year, 
  pred = fitted.c
)

panel.b <- a.live.non.lacey.table %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity)) +
  geom_col(fill = alpha("forestgreen", 0.4)) + 
  geom_line(
    aes(x = shipment_year + 0.5, y = pred), 
    col = "forestgreen", size = 1.5,
    data = fitted.df.c[1:17,]
  ) +
  geom_line(
    aes(x = shipment_year + 0.5, y = pred), 
    col = "forestgreen", size = 1.5,
    data = fitted.df.c[18:23,]
  ) +
  geom_vline(xintercept = 2016, size = 1, lty = 2) +
  labs(x = "Shipment Year", y = "Number of Individuals") +
  xlim(1999, 2022) +
  theme_minimal() +
  scale_y_continuous(
    labels = scales::comma, 
    limits = c(0, 6e6)
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 20),
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(color = "white")
  ) 

# Create a vertically-oriented plot
cowplot::plot_grid(
  panel.a, panel.b, 
  labels = c("a", "b"), label_size = 20, 
  ncol = 1
)

ggsave("outputs/Fig5.png", width = 10, height = 10)

# Can verify that a full before-after control-intervention model will
# give identical results (it's just harder to interpret)
baci.data <- bind_rows(
  a.live.lacey.table %>%
    mutate(
      intervention = rep(1, n()),
      intervention_name = rep("Listed", n())
    ),
  a.live.non.lacey.table %>%
    mutate(
      intervention = rep(0, n()),
      intervention_name = rep("Not Listed", n())
    )
)

# Model-fitting for full dataset
out.full <- glm(
  quantity ~ shipment_year_s * before_after * intervention, 
  data = baci.data, 
  family = poisson
)
summary(out.full)
fitted.full <- fitted.values(out.full)

# An alternative plot style with log10 axis and points instead of bars
fitted.df.full <- data.frame(
  shipment_year = a.live.non.lacey.table$shipment_year, 
  pred = fitted.full,
  intervention_name = rep(c("Listed", "Not Listed"), each = 23)
)

baci.data %>%
  filter(quantity > 0) %>%
  ggplot(aes(x = shipment_year + 0.5, y = quantity + 1, color = intervention_name)) +
  geom_vline(xintercept = 2016, size = 1, lty = 2) +
  geom_line(
    aes(x = shipment_year + 0.5, y = pred), 
    col = alpha("darkgrey", 1),
    size = 2,
    data = fitted.df.full[1:17,]
  ) +
  geom_line(
    aes(x = shipment_year + 0.5, y = pred), 
    col = alpha("darkgrey", 1),
    size = 2,
    data = fitted.df.full[18:23,]
  ) +
  geom_line(
    aes(x = shipment_year + 0.5, y = pred), 
    col = alpha("forestgreen", 1),
    size = 2,
    data = fitted.df.full[1:17 + 23,]
  ) +
  geom_line(
    aes(x = shipment_year + 0.5, y = pred), 
    col = alpha("forestgreen", 1),
    size = 2,
    data = fitted.df.full[18:23 + 23,]
  ) +
  geom_point(size = 7) +
  labs(x = "Shipment Year", y = "Number of Individuals") +
  theme_minimal() +
  scale_y_log10(labels = scales::comma, limits = c(1, 1e7)) +
  scale_color_manual(
    values = c(
      alpha("darkgrey", 0.5),
      alpha("forestgreen", 0.5)
    )
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 20),
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    legend.title = element_blank(),
    legend.spacing.y = unit(1, "mm"),
    plot.background = element_rect(color = "white")
  ) 


# What's the stated purpose and clearance status of recent Lacey Act 
# genera shipments post-2016?
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
  c("Ambystoma", "Cynops", "Paramesotriton", "Pleurodeles", "Salamandra",
    "Other Genera")

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
  scale_fill_manual(
    values = palette,
    breaks = c("Ambystoma", "Cynops", "Paramesotriton", "Pleurodeles", 
               "Salamandra", "Other Genera"),
    labels = c("*Ambystoma*", "*Cynops*", "*Paramesotriton*", "*Pleurodeles*", 
               "*Salamandra*", "Other Genera")
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 20),
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank(),
    legend.text = element_markdown(size = 22),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(),
    legend.spacing.y = unit(1, "mm"),
    plot.background = element_rect(color = "white")
  ) 

panel.b <- a.live %>%
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
  filter(shipment_year >= 2016) %>%
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
    panel.grid.minor.x = element_blank(),
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

ggsave("outputs/Fig6.png", width = 10, height = 10)

a.live.bsal.carrier.species.table <- a.live %>%
  filter(scientific_name %in% bsal.carrier.species) %>% 
  group_by(shipment_year, scientific_name, genus_aw, species_aw) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup()

n_distinct(a.live.bsal.carrier.species.table$genus_aw)
n_distinct(a.live.bsal.carrier.species.table$species_aw)
n_distinct(a.live.bsal.carrier.species.table$scientific_name)

# How many of these Bsal carrier species were imported per year since 2016?
a.live.bsal.carrier.species.table %>%
  filter(shipment_year >= 2016) %>%
  group_by(shipment_year) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup()


# Genera-level analyses

key.bsal.carrier.genera <- 
  c("Bombina", "Cynops", "Rana", "Triturus", "Other Genera")

panel.a <- a.live %>%
  filter(genus_aw %in% bsal.carrier.genera) %>% 
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
  scale_y_continuous(labels = scales::comma, limits = c(0, 4500000)) +
  scale_fill_manual(
    values = palette,
    breaks = c("Bombina", "Cynops", "Rana", "Triturus", "Other Genera"),
    labels = c("*Bombina*", "*Cynops*", "*Rana*", "*Triturus*", "Other Genera")
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 20),
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank(),
    legend.text = element_markdown(),
    legend.position = c(0.85, 0.87),
    legend.background = element_rect(),
    legend.spacing.y = unit(1, "mm"),
    plot.background = element_rect(color = "white")
  ) 

panel.b <- a.live %>%
  filter(genus_aw %in% bsal.carrier.genera) %>% 
  filter(shipment_year >= 2016) %>%
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
  scale_fill_manual(
    values = palette,
    breaks = c("China", "Hong Kong", "Taiwan", 
               "Other Bsal Country", "Non-Bsal Country"),
    labels = c("China", "Hong Kong", "Taiwan", 
               "Other *Bsal* Country", "Non-*Bsal* Country")
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    text = element_text(size = 20),
    axis.title.x = element_text(face = "bold", size = 22),
    axis.title.y = element_text(face = "bold", size = 22),
    legend.title = element_blank(),
    legend.text = element_markdown(),
    legend.position = c(0.25, 0.85),
    legend.background = element_rect(),
    legend.spacing.y = unit(1, "mm"),
    plot.background = element_rect(color = "white")
  )

panel.c <- a.live %>%
  filter(genus_aw %in% bsal.carrier.genera) %>% 
  filter(shipment_year >= 2016) %>%
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
    legend.position = c(0.25, 0.85),
    legend.background = element_rect(),
    legend.spacing.y = unit(1, "mm"),
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

ggsave("outputs/Fig7.png", width = 16, height = 16)

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

# What's the average number annually since 2016?
yearly.summary %>%
  filter(shipment_year >= 2016) %>%
  pull(quantity) %>%
  mean()

# Each year expressed as a percentage of overall live amphibian trade
yearly.summary %>%
  left_join(., a.live.table, by = "shipment_year") %>%
  mutate(percentage = quantity.x/quantity.y*100)

# Each genera expressed as a percentage of Bsal carrier trade
a.live.bsal.carrier.genera.table %>%
  filter(shipment_year >= 2016) %>%
  group_by(shipment_year, genus_aw) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  left_join(., yearly.summary, by = "shipment_year") %>%
  mutate(percentage = quantity.x/quantity.y*100)

# Percentage of recent trade coming from countries
a.live.bsal.carrier.genera.table %>%
  filter(shipment_year >= 2016) %>%
  group_by(shipment_year, country_origin) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  left_join(., yearly.summary, by = "shipment_year") %>%
  mutate(percentage = quantity.x/quantity.y*100)

# Percentage of recent trade coming to ports
a.live.bsal.carrier.genera.table %>%
  filter(shipment_year >= 2016) %>%
  group_by(shipment_year, port_full) %>%
  summarize(quantity = sum(quantity)) %>%
  ungroup() %>%
  left_join(., yearly.summary, by = "shipment_year") %>%
  mutate(percentage = quantity.x/quantity.y*100)
