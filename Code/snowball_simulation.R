# Simulation study for snowball algorithm
# Carter Allen
# May 1st, 2019

library(tidyverse)
library(magrittr)

#####################
## Data Generation ##
#####################

n_core_genes <- 15 # number of genes in core network
n_total_genes <- 100 # number of total genes with available data (includes core)
n_cand_genes <- 10 # number of true candidate genes
# parameters for background beta distribution. MOM estimates 0.57, 5.92 from GAIL data
a <- 0.57 # alpha parameter for beta distribution of background cosine similarities
b <- 5.92 # beta parameter for beta distribution of background cosine similarities
# parameters for candidates beta distribution
c <- 5*b # alpha parameter for beta distribution of candidate cosine similarities
d <- b # beta parameter for beta distribution of candidate cosine similarities

gene_ids <- 1:n_total_genes # generate the unique gene ids
core_ids <- sort(sample(gene_ids,n_core_genes,replace = FALSE)) # choose the core genes 
cand_ids <- sort(sample(setdiff(gene_ids,core_ids),n_cand_genes,replace = FALSE)) # choose candidate genes

gene_pairs <- t(combn(gene_ids,2)) # all unique gene-gene pairs 
colnames(gene_pairs) <- c("gene1","gene2") # name columns appropriately
gene_pairs <- as.data.frame(gene_pairs) # convert matrix to data frame
n_combn <- choose(n_total_genes,2) # save the total number of unique combinations
gene_pairs$cos <- rbeta(n_combn,a,b) # sample background similarity scores
gene_pairs[union(core_ids,cand_ids),]$cos <- rbeta(n_core_genes + n_cand_genes,c,d) # sample core & cand. similarity scores

# define indicator variables for whether or not genes are core members
gene_pairs %<>% 
  mutate(gene1_is_core = ifelse(gene1 %in% core_ids, 1, 0),
         gene2_is_core = ifelse(gene2 %in% core_ids, 1, 0))


########################
## Snowball Algorithm ##
########################

n_iter = 5
critical_quantile = 0.90
# Run the snowball algorithm on the core set
network <- snowball(core_ids,
                    gene_pairs,
                    n.iter = 100,
                    crit.quantile = 0.80,
                    min_avg_sim_perc = 0.99,
                    verbose = T)

# save the genes that were added to the network
added_genes <- network$candidates
# save the true candidate genes that were added to the network
true_cands <- intersect(added_genes,cand_ids)
# compute true postitive rate
true_pos <- length(true_cands)/length(added_genes)
# save the false candidate genes that were added to the network
false_cands <- setdiff(added_genes,cand_ids)
# compute the false positive rate
false_pos <- length(false_cands)/length(added_genes)


# Summarize results
cat("------------------- \n")
cat("Snowball Simulation \n")
cat("------------------- \n")
cat(paste("Total gene pool size:",n_total_genes,"\n"))
cat(paste("Core network size:",n_core_genes,"\n"))
cat(paste("Critical percentile:",critical_quantile,"\n"))
cat(paste("Number of true candidates:",n_cand_genes,"\n"))
cat(paste("Number of added candidates:",length(added_genes),"\n"))
cat(paste("Number of true candidates added:",length(true_cands),"\n"))




