summary.snowball <- function(ball)
{
  cat("------------------ \n")
  cat("Snowball Algorithm \n")
  cat("------------------ \n")
  n_core = length(ball$core)
  cat(paste("Core network size:",n_core,"\n"))
  n_cad = length(ball$candidates)
  cat(paste("Number of candidates:",n_cad,"\n"))
  n_iter = ball$n_iter
  cat(paste("Number of iterations:",n_iter,"\n"))
}