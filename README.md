# MPYC COVID-19 project

Code to support MPYC project exploring animal responses to the COVID-19 pandemic

## workflow overview:
annotation workflow-
* step 1: intersect event table with census block group (cbg) geometries
* step 2: compute cbg area
* step 3: annotate events with cbg info
* step 4: process safegraph data
* step 5: annotate events with safegraph data
* step 6: annotate events with TNC global human modification layer

SSF workflow-
* step 1: generate background points