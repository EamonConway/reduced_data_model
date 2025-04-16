calculate_crps <- function(fit,data){
  dq = 0.001
  quantiles = seq(0,1,by = dq)
  results <- rstan::extract(fit$model_fit)
  I_T = apply(results$I_T,2,quantile, probs = quantiles)
  y = data$cases[seq(1,ncol(I_T))]

  Score = 0*I_T
  for(i in 1:length(quantiles)){
    for(j in 1:length(y)){
      if(y[j] < I_T[i,j] ){
        Score[i,j] = 2*(1-quantiles[i])*(I_T[i,j]-y[j])
      }else{
        Score[i,j] = 2*quantiles[i]*(y[j] - I_T[i,j])
      }
    }
  }
  return((apply(Score,2,mean)))
}
