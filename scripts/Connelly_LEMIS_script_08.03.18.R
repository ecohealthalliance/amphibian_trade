library(tidyverse)
library(lemis)
library(dplyr)
library(utils)
library(ggplot2)

lemis_version_current()
lemis_codes %>% View()

lemis.alive.amphibians <- lemis_data() %>%
  filter(taxa == "amphibian") %>%
  filter(unit == "NO") %>%
  filter(description == "LIV") %>%
  filter(import_export == "I") #not necessary to filter for imports, only contains imports
sum(lemis.alive.amphibians$quantity) #62,331,843 live amphibians imported 

lemis.alive.caudates <- lemis.alive.amphibians %>%
  filter(generic_name %in% c("NEWT", "SALAMANDER", "SIREN"))
sum(lemis.alive.caudates$quantity) #5,225,844 live caudates imported (~8.4% of live amphibian imports)

lacey.act.genera <- c("chioglossa", "cynops", "euproctus", "hydromantes", "hynobius", "ichthyosaura", "lissotriton", "neurergus", "notophthalmus", "onychodactylus", "paramesotriton", "plethodon", "pleurodeles", "salamandra", "salamandrella", "salamandrina", "siren", "taricha", "triturus", "tylototriton")

genera.of.interest <- c("bombina","cynops", "pachytriton", "triturus") #highly traded bsal carrier genera, pachytriton is a salamander genera not listed under lacey act (or bombina)

countries.of.interest <- c("JP", "TH", "VN", "CN", "DE", "BE", "NL") #Japan, Thailand, Vietnam, China, Germany, Belgium, Netherlands....Bsal has been detected in these countries

#Quantifies the number of imported amphibians from countries with Bsal
amphibians.from.bsal.countries <- filter(lemis.alive.amphibians, country_origin %in% c(countries.of.interest))
sum(amphibians.from.bsal.countries$quantity) #7,053,598

wild.amphibians.from.bsal.countries <- filter(amphibians.from.bsal.countries, source == "W")
sum(wild.amphibians.from.bsal.countries$quantity) #4,751,138

#Quantifies the number of imported caudates from countries with Bsal
caudates.from.bsal.countries <- filter(amphibians.from.bsal.countries, generic_name %in% c("NEWT", "SALAMANDER", "SIREN"))
sum(caudates.from.bsal.countries$quantity) #2,148,497

bsal.carrier.genera <- lemis.alive.amphibians %>%
  filter(genus %in% c("chioglossa", "cynops", "euproctus", "hydromantes", "hynobius", "ichthyosaura", "lissotriton", "neurergus", "notophthalmus", "onychodactylus", "paramesotriton", "plethodon", "pleurodeles", "salamandra", "salamandrella", "salamandrina", "siren", "taricha", "triturus", "tylototriton", "alytes", "bombina", "pachytriton"))

#Quantifies the number of imported individuals belonging to a bsal carrier genera 
sum(bsal.carrier.genera$quantity) #9,988,949 individuals 

lemis <- lemis_data() %>% collect()

#summarize by port
lemis.alive.amphibians %>%
  group_by(port) %>%
  summarize(n_shipments = n(),
            total_individuals = sum(quantity)) %>%
  arrange(desc(n_shipments))

#summarize bsal genera by port
bsal.carrier.genera %>%
  group_by(port) %>%
  summarize(n_shipments = n(), total_individuals = sum(quantity)) %>%
  arrange(desc(n_shipments))

#summarize imports from bsal endemic countries by port
lemis.alive.amphibians %>%
  filter(country_origin %in% c(countries.of.interest)) %>%
  group_by(port) %>%
  summarize(n_shipments = n(), total_individuals = sum(quantity)) %>%
  arrange(desc(n_shipments))

seized.amphibians <- lemis.alive.amphibians %>% 
  filter(disposition == "S")    
sum(seized.amphibians$quantity) #6,438 seized amphibians 

seized.amphibians %>%
  group_by(country_origin) %>%
  summarize(n_shipments = n(), total_individuals = sum(quantity)) %>%
  arrange(desc(n_shipments)) #Germany is the only country of interest in top 10 with 2 seized shipments totalling 5 individuals

lemis.live.animals <- lemis_data() %>%
  filter(description == "LIV") %>% 
  filter(unit == "NO") 

sum(lemis.live.animals$quantity) #3,264,627,918 live animals (1.9% of live animal trade is in amphibians)

lemis.alive.amphibians.by.country <- lemis.alive.amphibians %>%
  group_by(country_origin) %>%
  summarize(n_shipments = n(), total_individuals = sum(quantity)) %>%
  arrange(desc(total_individuals))

#Bsal endemic countries arranged by total individual amphibians imported: 
 amphibians.from.bsal.countries %>%
  group_by(country_origin) %>%
  summarize(n_shipments = n(), total_individuals = sum(quantity)) %>%
  arrange(desc(total_individuals))

#Bsal endemic countries arranged by total salamanders imported:
caudates.from.bsal.countries %>%
  group_by(country_origin)

#1. What percentage of amphibians imported to the US are coming from countries with Bsal and how has this changed over time?  
Imports.from.Bsal.Positive.Countries.Plot <- lemis.amphibians.alive %>%
  filter(country_origin %in% countries.of.interest) %>% 
  mutate(shipment_month = lubridate::month(shipment_date),
         shipment_year = lubridate::year(shipment_date)
  ) %>%
  ggplot(aes(x = shipment_year, y = quantity, fill = country_origin)) +
  geom_col() + labs(x = "Shipment Year", y = "Number of Individuals", color = "Country", title = "Trends in Imports from Bsal Positive Countries", fill = "Country")

Imports.from.Bsal.Positive.Countries.Plotb <- Imports.from.Bsal.Positive.Countries.Plot + theme(plot.title = element_text(face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.title.y = element_text(face="bold")) + theme(legend.title = element_text(face="bold")) 


#2. How many individuals of known Bsal carrier species were imported to the US? 
Trends.in.Heavily.Traded.Bsal.Carrier.Genera.Plot <- lemis.alive %>%
  filter(genus %in% genera.of.interest) %>% 
  mutate(shipment_month = lubridate::month(shipment_date),
         shipment_year = lubridate::year(shipment_date)
  ) %>%
  ggplot(aes(x = shipment_year, y = quantity, fill = genus)) +
  geom_col() + labs(x = "Shipment Year", y = "Number of Individuals", title = "Trends in Heavily Traded Bsal Carrier Genera")

Trends.in.Heavily.Traded.Bsal.Carrier.Genera.Plotb <- Q2.Plot + theme(plot.title = element_text(face="bold")) + theme(axis.title.x = element_text(face="bold")) + theme(axis.title.y = element_text(face="bold")) + theme(legend.title = element_text(face="bold")) + theme(legend.text = element_text(face="italic")) 
options(scipen=10000)




