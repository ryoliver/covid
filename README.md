# MPYC COVID-19 project

Code to support MPYC project exploring animal responses to the COVID-19 pandemic

## goals:
Develop code for to support annotating animal locations:
* **reformat-mobility-data.r** 
  * reformat SafeGraph data from wide to long format
* **intersect-events-cbg.r**
  * spatially intersect animal locations with census block geometries
  * export event table with census block group, census tract, county, and state ID
* **annotate-events-safegraph.r**
  * annotate animal location event tables with SafeGraph data based on census block group ID
  * export event table with SafeGraph daily and hourly device count
* **annotate-events-census.r**
  * annotate animal location event tables with census data based on census block group ID
  * export event table with 2019 ACS population density
* **summarize-events.r**
  * summarize n locations, indviduals, species by census block group, county, state

## completed
* procces 2019/2020 safegraph data
* spatially intersect event table + cbg geometries
* annotate event table with 2019/2020 safegraph daily data
* annotate event table with 2019 population density

## to do
* debug hourly safegraph annotation
* get geometries for 2020 census block groups
* repeat intersection with 2020 census geometries
* repeat annotation with 2020 population density
