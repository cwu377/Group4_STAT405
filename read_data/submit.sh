n_files = $20 # number of files

getData=$(sbatch --array=1-$n_files \
	         --output="slurm_out/slurm-%A_%a.out" \
                 --error="slurm_out/slurm-%A_%a.err" \
                 getData.sh)

getData=$(echo $getData | sed 's/Submitted batch job //')

readData=$(sbatch --array=1-$n_files \
                --output="slurm_out/slurm-%A_%a.out" \
                --error="slurm_out/slurm-%A_%a.err" \
                --dependency=afterok:$getData \
                readData.sh)
