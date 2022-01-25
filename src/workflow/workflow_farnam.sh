#!/bin/bash
chmod +x /gpfs/ysm/project/jetz/ryo3/projects/covid/src/workflow/workflow.sh

#-- parameters
src=/gpfs/ysm/project/jetz/ryo3/projects/covid/src

#make executable
chmod +x $src/workflow/run_safegraph_processing.sh

#run
sbatch $src/workflow/run_safegraph_processing.sh