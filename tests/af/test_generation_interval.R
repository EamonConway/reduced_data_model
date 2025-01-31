g_mean <- 3.6
g_var <- 3.1 ^ 2
g_shape <- g_mean ^ 2 / g_var
g_scale <- g_var / g_mean
s <- 0:16
g <- pgamma(s[-1], shape = g_shape, scale = g_scale) - pgamma(s[-length(s)], shape = g_shape, scale = g_scale)
g <- g / sum(g)

g2 <- define_generation_interval(3.6,3.1,16)
