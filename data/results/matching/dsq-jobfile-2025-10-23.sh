#!/bin/bash
#SBATCH --output cluster_logs/dsq-jobfile-%A_%a-%N.out
#SBATCH --array 0-1
#SBATCH --job-name dsq-job_list
#SBATCH --mem-per-cpu 2g -t 02:45:00 --cpus-per-task 1 --partition day

# DO NOT EDIT LINE BELOW
/apps/software/2024a/software/dSQ/1.05/dSQBatch.py --job-file /home/ma984/REDD_specification_curve/data/results/matching/job_list.txt --status-dir /home/ma984/REDD_specification_curve/cluster_logs

