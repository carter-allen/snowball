snowball <- function(core, 
                     cos_sims, 
                     n.iter = 5, 
                     crit.quantile = 0.99,
                     min_avg_sim_perc = 0.95,
                     verbose = T) 
{
  crit.val = quantile(cos_sims$cos, probs = crit.quantile)
  network = core
  avg_sim_0 = cos_sims %>%
    filter(gene1 %in% network & gene2 %in% network) %>%
    pull(cos) %>%
    mean()
  print(avg_sim_0)
  avg_sim = avg_sim_0
  i = 1
  for(i in 1:n.iter)
  {
    edgecounts = cos_sims %>%
      filter(gene1 %in% network & gene2 %in% network == FALSE) %>%
      mutate(edge = ifelse(cos > crit.val,1,0)) %>%
      group_by(gene2) %>%
      summarize(n_edges = sum(edge), avg_cos = mean(cos)) %>%
      arrange(desc(n_edges))
    
    top_edges = edgecounts[1,] %>%
      pull(n_edges)
    
    if(top_edges < 1)
    {
      print("No more edges to add")
      break
    }
      
    top_candidate = edgecounts %>%
      top_n(1,wt = n_edges) %>%
      pull(gene2) %>%
      as.character() %>%
      unname()
      
    if(length(top_candidate) > 1)
    {
      n_tops = length(top_candidate)
      chosen_top = sample(1:n_tops,size = 1)
      top_candidate = top_candidate[chosen_top]
    }
      
    network = c(network,top_candidate)
    if(verbose){
      print(paste("Iteration",paste(i,":",sep = ""),"Added node",top_candidate,"to network"))
    }
    i = i + 1
    
    avg_sim = cos_sims %>%
      filter(gene1 %in% network & gene2 %in% network) %>%
      pull(cos) %>%
      mean()
    print(paste("Average similarity:",avg_sim))
    if(avg_sim <= avg_sim_0*min_avg_sim_perc)
    {
      print("Minimum average similarity reached")
      break
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