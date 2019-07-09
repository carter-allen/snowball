multi_snowball <- function(core_list, 
                           cos_sims, 
                           n.iter = 5, 
                           crit.quantile = 0.99,
                           min_avg_sim_perc = 0.95,
                           verbose = T,
                           ...)
{
  multi_ball =  mclapply(core_list,
                         snowball,
                         cos_sims = cos_sims,
                         n.iter = n.iter,
                         crit.quantile = crit.quantile,
                         min_avg_sim_perc = min_avg_sim_perc,
                         verbose = verbose,
                         ...)
}