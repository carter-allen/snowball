matrix_to_list <- function(mat)
{
  if(!isSymmetric(mat))
  {
    print("Non-symmetric")
    return(NULL)
  }
  else{
    genes = rownames(mat)
    list = t(combn(genes,2))
    n_combn = nrow(list)
    cos = rep(0,n_combn)
    for(i in 1:n_combn)
    {
      g1 = list[i,1]
      g2 = list[i,2]
      cos[i] = mat[g1,g2]
    }
    list = cbind(list,cos)
    colnames(list) = c("gene1","gene2","cos")
    list = as.data.frame(list,
                         stringsAsFactors = FALSE)
    list$cos <- as.numeric(list$cos)
  }
  return(list)
}