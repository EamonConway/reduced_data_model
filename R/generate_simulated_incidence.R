generate_simulated_incidence <- function(R_t,generation_interval,initial_incidence){
  n_days <- length(R_t)
  I_t <- rep(initial_incidence, n_days + 1)
  n_generation_interval <- length(generation_interval)
  for(t in 2:(n_days+1)){
    current_foi = 0.0;
    for(s in 1:n_generation_interval){
      if((t-s)>0){
        current_foi = current_foi + generation_interval[s]*I_t[t-s];
      }
    }
    I_t[t] <- rpois(1,R_t[t-1]*current_foi)
  }
  return(I_t[-1])
}
