path_combos <- t(combn(names(plist),2))
connect <- rep(0,dim(path_combos)[1])
threshold <- 0.5
pb <- txtProgressBar(min = 0, max = dim(path_combos)[1], style = 3)
for(i in 1:dim(path_combos)[1])
{
  path1 <- path_combos[i,1]
  path2 <- path_combos[i,2]
  genes1 <- plist[[path1]]
  genes2 <- plist[[path2]]
  gene_combos <- expand.grid(genes1,genes2)
  edges <- rep(0,dim(gene_combos)[1])
  for(j in 1:dim(gene_combos)[1])
  {
    g1 <- as.character(gene_combos[j,1])
    g2 <- as.character(gene_combos[j,2])
    sim <- runif(1)
    if(sim > threshold)
    {
      edges[j] = 1
    }
  }
  connect[i] <- sum(edges)/length(edges)
  setTxtProgressBar(pb, i)
}
close(pb)