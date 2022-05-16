#!/bin/bash
chmod +x /gpfs/ysm/project/jetz/ryo3/projects/covid/src/workflow/workflow_farnam.sh

#-- parameters
src=/gpfs/ysm/project/jetz/ryo3/projects/covid/src

#make executable
chmod +x $src/workflow/create_intersection_joblist.R
chmod +x $src/workflow/run_annotate_events_cbg.sh
chmod +x $src/workflow/run_annotate_events_census.sh
chmod +x $src/workflow/run_annotate_events_ghm.sh
chmod +x $src/workflow/run_annotate_events_safegraph.sh
chmod +x $src/workflow/run_cbg_intersection.sh
chmod +x $src/workflow/run_compute_cbg_area.sh
chmod +x $src/workflow/run_event_summary.sh
chmod +x $src/workflow/run_extract_gHM_cbg.sh
chmod +x $src/workflow/run_process_safegraph_data.sh
chmod +x $src/workflow/run_test_sg.sh


# ---- data wrangling workflow -----


###
# step 1: intersect events with cbg geometries
#   inputs - event table + cbg shp file
#   outputs - csv per job (event_id + cbg info)

#module load R/4.1.0-foss-2020b
#Rscript $src/workflow/create_intersection_joblist.R

#module load dSQ
#dsq --job-file $src/workflow/joblist.txt --mem-per-cpu 40g -t 2- 

# UPDATE WITH DATE
#sbatch dsq-joblist-2022-05-12.sh
###


###
# step 2: compute cbg area
#   inputs - cbg shp files
#   outputs - csv (cbg info + area)

#sbatch $src/workflow/run_compute_cbg_area.sh
###

###
# step 3: annotate events with cbg info
#   inputs - event table + cbg intersection csv + cbg area csv
#   outputs - csv (event_id + cbg info + cbg area)

#sbatch $src/workflow/run_annotate_events_cbg.sh
###

###
# step 4: process safegraph data
#   inputs - sg txt files (one file per cbg/week)
#   outputs - csv (one file per cbg/week)

#sbatch $src/workflow/run_process_safegraph_data.sh
###

###
# step 5: annotate events with safegraph data
#   inputs - event table + cbg info csv + sg data csv
#   outputs - csv (event_id + timestamp + cbg info = cbg area + sg count)

sbatch $src/workflow/run_annotate_events_safegraph.sh
###

###
# step 6: annotate events with TNC global human modification layer
#   inputs - event table + ghm raster
#   ouputs - csv (event_id + ghm)

#sbatch $src/workflow/run_annotate_events_ghm.sh
###

# ---- optional steps ----
###
# step 7: extract TNC global human modification layer to census geometries
#   inputs - cbg shp file + ghm raster
#   outputs - shp file (cbg info + ghm)

#sbatch $src/workflow/run_extract_gHM_cbg.sh
###

###
# step 8: annotate events with census population density
#   inputs - event table + safegraph open census data csv
#   outputs - csv (event_id + total_population_2019)

#sbatch $src/workflow/run_annotate_events_census.sh
###



# summarize event data
#sbatch $src/workflow/run_event_summary.sh

#sbatch $src/workflow/run_test_sg.sh

#sbatch $src/workflow/run_pull_example_data.sh

#sbatch $src/workflow/run_test.sh

