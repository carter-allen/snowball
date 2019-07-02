library(tidygraph)
library(igraph)
library(ggraph)
plot.snowball <- function(ball)
{
  ball_mat = ball$sim_mat
  core = ball$core
  g = igraph::graph_from_adjacency_matrix(ball_mat,
                                          mode = "undirected",
                                          weighted = TRUE,
                                          diag = FALSE)
  g = tidygraph::as_tbl_graph(g) %>%
    dplyr::mutate(is_core = ifelse(name %in% core_ids,1,0),
           label = factor(is_core,
                           levels = c(0,1),
                           labels = c("Cand.",
                                      "Core")))
  ggraph(g,
         layout = "kk",
         circular = F) + 
    geom_edge_arc(aes(color = weight,
                      alpha = weight,
                      width = weight),
                  show.legend = F) + 
    geom_node_point(color = "grey30",
                    size = 8) + 
    geom_node_point(aes(color = label),
                    size = 6) + 
    theme_void() +
    theme(legend.title = element_blank(),
          legend.text = element_text(face = "bold",
                                     size = 12,
                                     family = "serif"))
}