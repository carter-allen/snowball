print("Loading packages")
library(dplyr) # for data manipulation
library(parallel) # for mclapply

print("Reading data")
cos_sims <- read.table("GG.txt",header = TRUE) # cosine similarity
load("KEGG.RDATA") # pathways
path_combos <- t(combn(names(plist),2))[1:100,] # all combinations of pathways

print("Defining distance function")
# function to compute pathway distances
calculate_distances <- function(path_combos)
{
  n_pos_edges <- rep(0,dim(path_combos)[1])
  n_obs_edges <- rep(0,dim(path_combos)[1])
  connect <- rep(0,dim(path_combos)[1])
  mean_cos <- rep(0,dim(path_combos)[1])
  sd_cos <- rep(0,dim(path_combos)[1])
  med_cos <- rep(0,dim(path_combos)[1])
  threshold <- 0.288 ## approx. 95th percentile
  start.time <- proc.time()
  for(i in 1:dim(path_combos)[1])
  {
    path1 <- path_combos[i,1]
    path2 <- path_combos[i,2]
    genes1 <- plist[[path1]]
    genes2 <- plist[[path2]]
    gene_combos <- expand.grid(genes1,genes2)
    edges <- rep(0,dim(gene_combos)[1])
    sims <- rep(0,dim(gene_combos)[1])
    pb2 <- txtProgressBar(min = 0, max = dim(gene_combos)[1], style = 3)
    for(j in 1:dim(gene_combos)[1])
    {
      g1 <- as.character(gene_combos[j,1])
      g2 <- as.character(gene_combos[j,2])
      row <- cos_sims %>%
        filter((gene1 == g1 & gene2 == g2) | (gene2 == g1 & gene1 == g2))
       # the following line was used for testing
       # row <- cbind("test","row")
      if(dim(row)[1] == 1)
      {
        sim = row[3]
        # the following line was used for testing 
        #sim = runif(1)
        
        sims[j] = sim
        if(sim > threshold)
        {
          edges[j] = 1
        }
      }
      else
      {
        sims[j] = NA
        edges[j] = NA
      }
      setTxtProgressBar(pb2, j)
    }
    close(pb2)
    print(paste("Finished pathway pair",i))
    n_pos_edges[i] <- sum(is.na(edges) == FALSE)
    n_obs_edges[i] <- sum(edges,na.rm = TRUE)
    connect[i] <- sum(edges,na.rm = TRUE)/sum(is.na(edges) == FALSE)
    mean_cos[i] <- mean(sims,na.rm = TRUE)
    sd_cos[i] <- sd(sims,na.rm = TRUE)
    med_cos[i] <- median(sims,na.rm = TRUE)
    results_df <- as.data.frame(cbind(path_combos,n_pos_edges,n_obs_edges,connect,mean_cos,sd_cos,med_cos))
    write.csv(results_df,"pathway_distances.csv",row.names = FALSE,quote = FALSE)
  }
  end.time <- proc.time()
  elapsed.time <- end.time - start.time
  print(paste("Finished the job in",elapsed.time[3],"seconds"))
  return(results_df)
}

print("Starting distances job")
n_cores <- detectCores()
result_list <- mclapply(split.data.frame(path_combos,1:nrow(path_combos)),
                        calculate_distances,
                        mc.cores = n_cores)
#result_list <- calculate_distances(path_combos[1:10,])
save(result_list,file = "pathway_distances_list.rdata")

print("Done. Check current directory for pathway_distances_list.rdata")