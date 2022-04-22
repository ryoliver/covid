#!/bin/bash
chmod +x /gpfs/ysm/project/jetz/ryo3/projects/covid/src/workflow/workflow_farnam.sh

#-- parameters
src=/gpfs/ysm/project/jetz/ryo3/projects/covid/src

#make executable
chmod +x $src/workflow/create_intersection_joblist.R
chmod +x $src/workflow/run_annotate_events_cbg.sh
chmod +x $src/workflow/run_annotate_events_census.sh
chmod +x $src/workflow/run_annotate_events_ghm.sh
chmod +x $src/workflow/run_cbg_intersection.sh
chmod +x $src/workflow/run_compute_cbg_area.sh
chmod +x $src/workflow/run_event_summary.sh
chmod +x $src/workflow/run_extract_gHM_cbg.sh
chmod +x $src/workflow/run_safegraph_annotation.sh
chmod +x $src/workflow/run_safegraph_processing.sh


# data wrangling workflow

# step 1: intersect events with cbg geometries
#   inputs - event table + cbg shp file
#   outputs - csv per job (event_id + cbg info)

# step 2: compute cbg area
#   inputs - cbg shp files
#   outputs - csv (cbg info + area)

# step 3: annotate events with cbg info
#   inputs - event table + cbg intersection csv + cbg area csv
#   outputs - csv (event_id + cbg info + cbg area)

# step 4: process safegraph data
#   inputs - 


# intersect events with census geometries
#module load R/4.1.0-foss-2020b
#Rscript $src/workflow/create_intersection_joblist.R

#module load dSQ
#dsq --job-file $src/workflow/joblist.txt --mem-per-cpu 40g -t 2- 

# UPDATE WITH DATE
#sbatch dsq-joblist-2022-04-13.sh

# compute area of census geometries
#sbatch $src/workflow/run_compute_cbg_area.sh

# annotate events with cbg info + area
#sbatch $src/workflow/run_annotate_events_cbg.sh

# process safegraph data
#sbatch $src/workflow/run_safegraph_processing.sh

# annotate events with safegraph data
sbatch $src/workflow/run_annotate_events_safegraph.sh

# annotate events with census data
#sbatch $src/workflow/run_annotate_events_census.sh
  
# annotate events with census data
#sbatch $src/workflow/run_annotate_events_ghm.sh

# extract gHM from census geometries
#sbatch $src/workflow/run_extract_gHM_cbg.sh

# summarize event data
#sbatch $src/workflow/run_event_summary.sh

# summarize event data
# deprecated?
#sbatch $src/workflow/run_dbbmm_test.sh
