# in terminal: 

ssh francine@fir.computecanada.ca
# enter password
# 2FA

# change into your directory (portal)
cd def-sjsmith/francine

# to transfer files between HPC & computer
sftp francine@fir.computecanada.ca
# l needs to go in front of computer (local) commands e.g., lcd 
# commands on HPC are the same e.g., cd 

# `put <file>` copies file from computer to HPC
# `get <file>` copies file from HPC to computer

# create environment with vcf tool in it 
conda create -n vcftools -c bioconda vcftools

# output:
# To activate this environment, use
#
#     $ conda activate vcftools
#
# To deactivate an active environment, use
#
#     $ conda deactivate

# Note the difference base vs vcftools
(base) [francine@login2 ~]$ conda activate vcftools
(vcftools) [francine@login2 ~]$
