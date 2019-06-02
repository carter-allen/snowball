# Simulation study for snowball algorithm
# Carter Allen
# May 1st, 2019

library(tidyverse)
library(magrittr)

#####################
## DATA GENERATION ##
#####################

n_core_genes <- 15 # number of genes in core network
n_total_genes <- 100 # number of total genes with available data (includes core)
n_cand_genes <- 10 # number of true candidate genes
# parameters for background beta distribution. MOM estimates 0.57, 5.92 from GAIL data
a <- 0.57 # alpha parameter for beta distribution of background cosine similarities
b <- 5.92 # beta parameter for beta distribution of background cosine similarities
# parameters for candidates beta distribution
c <- 2*b # alpha parameter for beta distribution of candidate cosine similarities
d <- b # beta parameter for beta distribution of candidate cosine similarities

gene_ids <- 1:n_total_genes # generate the unique gene ids
core_ids <- sort(sample(gene_ids,n_core_genes,replace = FALSE)) # choose the core genes 
cand_ids <- sort(sample(setdiff(gene_ids,core_ids),n_cand_genes,replace = FALSE)) # choose candidate genes

gene_pairs <- t(combn(gene_ids,2)) # all unique gene-gene pairs 
colnames(gene_pairs) <- c("gene1","gene2") # name columns appropriately
gene_pairs <- as.data.frame(gene_pairs) # convert matrix to data frame
n_combn <- choose(n_total_genes,2) # save the totla number of unique combinations

gene_pairs$cos <- rbeta(n_combn,a,b) # sample background similarity scores
gene_pairs[union(core_ids,cand_ids),]$cos <- rbeta(n_core_genes + n_cand_genes,c,d) # sample core & cand. similarity scores

# define indicator variables for whether or not genes are core members
gene_pairs %<>% 
  mutate(gene1_is_core = ifelse(gene1 %in% core_ids, 1, 0),
         gene2_is_core = ifelse(gene2 %in% core_ids, 1, 0))

network <- snowball(core_ids,gene_pairs)




