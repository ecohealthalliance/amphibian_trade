# United States amphibian imports from 1999 to 2021

This repository contains code, data, and figures that support:

Connelly, P.J., N. Ross, O.C. Stringham, and E.A. Eskew. 2023. Ongoing amphibian trade into the United States threatens salamander biodiversity.

The global amphibian trade is closely linked to wildlife disease issues because of potential for spread of either of the two fungi responsible for amphibian chytridiomycosis, *Batrachochytrium dendrobatidis* (*Bd*) or *Batrachochytrium salamandrivorans* (*Bsal*). A primary aim of this project was evaluate the efficacy of a [2016 wildlife trade policy](https://www.federalregister.gov/documents/2016/01/13/2016-00452/injurious-wildlife-species-listing-salamanders-due-to-risk-of-salamander-chytrid-fungus) that sought to reduce the likelihood of *Bsal* introduction to the United States by banning the import of 20 salamander genera. To address this question and describe patterns in amphibian imports more generally, we collated and cleaned a dataset of United States amphibian imports from 1999 to 2021, building off [previous EcoHealth Alliance efforts to curate the United States Fish and Wildlife Service's Law Enforcement Management Information System (LEMIS) data](https://doi.org/10.1038/s41597-020-0354-5). The full, cleaned dataset, with taxonomy reconciled to the [AmphibiaWeb nomenclature](https://amphibiaweb.org/taxonomy/AWtaxonomy.html) is [available in this repository](/data/cleaned/harmonized_amphibian_LEMIS_1999_to_2021.csv).

--- 

### Repository Structure

- [`/data`](/data) contains all raw and cleaned data files
	- [`/cleaned`](/data/cleaned) contains cleaned versions of all three LEMIS data subsets that are combined in this analysis as well as [the full, cleaned dataset](/data/cleaned/harmonized_amphibian_LEMIS_1999_to_2021.csv) 
	- [`/lacey_act`](/data/lacey_act) contains a list of the taxa listed under the 2016 Lacey Act interim ruling
	- [`/raw`](/data/raw) contains the raw LEMIS data that is cleaned and combined in this analysis
	- [`/reference`](/data/reference) contains a table with information on potential *Bsal* carrier taxa, as gathered from the literature
	- [`/taxonomy`](/data/taxonomy) contains AmphibiaWeb taxonomic information as well as tables used to clean the LEMIS taxonomic data
- [`/misc`](/misc) contains the footer image used in this README page
- [`/outputs`](/outputs) contains all figures output from the [`05_amphibian_trade_analyses.R`](/scripts/05_amphibian_trade_analyses.R) script
- [`/scripts`](/scripts) contains the primary analysis scripts, numbered in order of execution

---

[![http://www.ecohealthalliance.org/](misc/eha-footer.png)](http://www.ecohealthalliance.org/)