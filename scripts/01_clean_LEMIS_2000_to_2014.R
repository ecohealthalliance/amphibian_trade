library(tidyverse)
library(lemis)

#==============================================================================


# Check LEMIS version (should be 1.1.0)
lemis_version_current()

# Get amphibian data from the 2000-2014 LEMIS dataset
a <- lemis_data() %>%
  filter(taxa == "amphibian") %>%
  collect()

nrow(a)

# Save the cleaned data
write_csv(a, "data/cleaned/amphibian_LEMIS_2000_to_2014.csv")
