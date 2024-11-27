define_generation_interval <- function(mean, sd, days) {
  s = 0:days
  var = sd ^ 2
  g_shape = mean ^ 2 / var
  g_scale = var / mean
  g =   s <- 0:16
  g <- pgamma(s[-1], shape = g_shape, scale = g_scale) - pgamma(s[-length(s)], shape = g_shape, scale = g_scale)
  return(g / sum(g))
}
