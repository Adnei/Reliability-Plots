library(dplyr)
library(plotly)

# -----------------------------------------------------------------------------#
#                                Equations                                     #
# -----------------------------------------------------------------------------#

serie_parallel <- function(r_key, coverage, n_subsystem ){
  r_component <- 0.9
  (r_component + (1 - r_component) * coverage * r_key * r_component) ^ n_subsystem
}

parallel_serie <- function(r_key, coverage, n_subsystem ){
  r_component <- 0.9
  r_component ^ n_subsystem + (1 - r_component ^ n_subsystem) * coverage * r_key * r_component ^ n_subsystem
}


# -----------------------------------------------------------------------------#
#                         Setting up parameters                                #
# -----------------------------------------------------------------------------#

combinations.df <- data.frame(
  r_key = seq(from = 0.8, to = 1, by = 0.02),
  coverage = seq(from = 0.8, to = 1, by = 0.02),
  n_subsystem = seq(from = 10, to = 1000, by = 99),
  stringsAsFactors=FALSE
)

# -----------------------------------------------------------------------------#
#                                Serie-Parallel                                #
# -----------------------------------------------------------------------------#

# creates a combination matrix
args_serie_parallel.df <- expand.grid(r_key = combinations.df$r_key,
  coverage = combinations.df$coverage,
  n_subsystem = combinations.df$n_subsystem)


result_serie_parallel.df <- mapply(serie_parallel,
  args_serie_parallel.df[,1],
  args_serie_parallel.df[,2],
  args_serie_parallel.df[,3]
)

# -----------------------------------------------------------------------------#
#                                Parallel-Serie                                #
# -----------------------------------------------------------------------------#

# creates a combination matrix
args_parallel_serie.df <- expand.grid(r_key = combinations.df$r_key,
  coverage = combinations.df$coverage,
  n_subsystem = combinations.df$n_subsystem)


result_parallel_serie.df <- mapply(parallel_serie,
  args_parallel_serie.df[,1],
  args_parallel_serie.df[,2],
  args_parallel_serie.df[,3]
)

# -----------------------------------------------------------------------------#
#                          Plotting all the data                               #
# -----------------------------------------------------------------------------#

my_plot <- plot_ly(x = args_serie_parallel.df$r_key * args_serie_parallel.df$coverage,
  y = args_serie_parallel.df$n_subsystem,
  z = result_serie_parallel.df,
  type = 'scatter3d',
  mode = 'lines',
  opacity = 1,
  name = 'Série-Paralelo',
  line = list(color = 'red', width = 4)
)
my_plot <- my_plot %>% add_trace(z = result_parallel_serie.df, name = 'Paralelo-Série',
  line = list(color = 'blue', width = 4))
my_plot <- my_plot %>% layout(title = "Rs X C*Rkey X N",
         scene = list(
           xaxis = list(title = "Índice de Cobertura * Rkey(t)"),
           yaxis = list (title = "Número de subsistemas"),
           zaxis = list (title = "Confiabilidade Rs(t)")
         ))
my_plot
