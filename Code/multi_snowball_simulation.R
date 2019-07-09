#############################
### Multi-core Simulation ###
#############################
library(parallel)

# define settings
n_genes <- 1000
n_cores <- 4
n_total_core_genes <- 200

gene_ids <- 1:n_genes
core_id_list <- list(0)
for(l in 1:n_cores)
{
  n_core_l <- n_total_core_genes/n_cores # even split among cores
  core_id_list[[l]] <- sample(gene_ids,n_core_l,replace = FALSE) 
}

multi_ball <- multi_snowball(core_id_list,snowball,cos_sims = gene_pairs)