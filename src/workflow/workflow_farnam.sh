#!/bin/bash
chmod +x /gpfs/ysm/project/jetz/ryo3/projects/covid/src/workflow/workflow_farnam.sh

#-- parameters
src=/gpfs/ysm/project/jetz/ryo3/projects/covid/src

#make executable
chmod +x $src/workflow/run_safegraph_processing.sh
chmod +x $src/workflow/run_cbg_intersection.sh
chmod +x $src/workflow/run_safegraph_annotation.sh
chmod +x $src/workflow/run_census_annotation.sh
chmod +x $src/workflow/run_ghm_annotation.sh
chmod +x $src/workflow/run_event_summary.sh
chmod +x $src/workflow/run_dbbmm_test.sh

#run

# process safegraph data
#sbatch $src/workflow/run_safegraph_processing.sh

# intersect events with census geometries
#sbatch $src/workflow/run_cbg_intersection.sh

# annotate events with safegraph data
#sbatch $src/workflow/run_safegraph_annotation.sh

# annotate events with census data
#sbatch $src/workflow/run_census_annotation.sh

# annotate events with census data
sbatch $src/workflow/run_ghm_annotation.sh

# summarize event data
#sbatch $src/workflow/run_event_summary.sh

# summarize event data
#sbatch $src/workflow/run_dbbmm_test.sh