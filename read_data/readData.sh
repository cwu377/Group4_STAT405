#!/bin/bash

module load R/R-3.6.1

n=$SLURM_ARRAY_TASK_ID
name=$(some operation of n such that name satisfies the actual file name)

Rscript readData.r $name
