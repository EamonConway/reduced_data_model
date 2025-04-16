define_simulated_generation_interval<-function(){
  s <- 0:14
  g <- plnorm(s[-1],log(2.0),1.0) - plnorm(s[-length(s)],log(2.0),1.0)
  g/sum(g)
}
