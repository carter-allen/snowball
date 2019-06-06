snowball <- function(core, 
                     cos_sims, 
                     n.iter = 5, 
                     crit.quantile = 0.99,
                     verbose = T) 
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
    if(verbose){
      print(paste("Iteration:",i,"Added",top_candidate,"to network"))
    }
  }
  
  sim_pairs = cos_sims %>%
    filter(gene1 %in% network & gene2 %in% network) %>%
    select(gene1,gene2,cos)
  
  sim_mat <- list_to_matrix(sim_pairs)

  added_genes = setdiff(network,core)
  ball = list(
    core = core,
    candidates = added_genes,
    network = network,
    sim_pairs = sim_pairs,
    sim_mat = sim_mat,
    n_iter = n.iter
  )
  
  attr(ball,"class") =  "snowball"
  
  return(ball)
  
}