calculate_mean_skill <- function(base,comparison){
  base = base[seq(1,length(comparison))]
  mean((base - comparison)/base)
}
