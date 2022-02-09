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
* **summarize-events.r**
  * summarize n locations, indviduals, species by census block group, county, state

## to do
* run intersection/annotation
* annotate with 2019 population density
* get geometries for 2020 census block groups
* repeat intersection with 2020 census geometries
* repeat annotation with 2020 population density
