snowball <- function(core, cos_sims, n.iter = 5, crit.quantile = 0.99) 
{
  crit.val = quantile(cos_sims$cos, probs = crit.quantile)
  network = core
  for(i in 1:n.iter)
  {
    edgecounts = cos_sims %>%
      filter(gene1 %in% network & gene2 %in% network == FALSE) %>%
      mutate(edge = ifelse(cos > crit.val,1,0)) %>%
      group_by(gene2) %>%
      summarize(n_edges = sum(edge), avg_cos = mean(cos)) %>%
      arrange(desc(n_edges))
    
    top_candidate = edgecounts %>%
      top_n(1,wt = n_edges) %>%
      pull(gene2) %>%
      as.character() %>%
      unname()
    
    network = c(network,top_candidate)
    print(paste("Iter:",i,"Added",top_candidate,"to network"))
  }
  return(as.numeric(network))
}