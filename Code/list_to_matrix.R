list_to_matrix <- function(sim_list)
{
  gene1 = as.character(sim_list[,1])
  gene2 = as.character(sim_list[,2])
  genes = as.character(unique(dplyr::union(gene1,gene2)))
  n_genes = length(genes)
  mat = diag(0,n_genes) 
  rownames(mat) = genes
  colnames(mat) = genes
  n_combn = nrow(sim_list)
  
  for(i in 1:n_combn)
  {
    gene1_i = gene1[i]
    gene2_i = gene2[i]
    cos_i = sim_list[i,3]
    mat[gene1_i,gene2_i] = mat[gene2_i,gene1_i] = cos_i
  }
  if(isSymmetric(mat))
  {
    return(mat)
  }
  else
  {
    print("Error not symmetric")
    return(NULL)
  }
}