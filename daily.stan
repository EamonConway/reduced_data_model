// The input data is a vector 'D' of length 'N' containing daily data.
functions {
  real pc_phi_lpdf(real x, real lambda){
    return -1.5*log(x) + log(lambda/2.0) - lambda/sqrt(x);
  }
}


data {
  int<lower=0> N_days;
  int<lower=0> D[N_days];
  int<lower=0> N_generation;
  vector<lower=0>[N_generation] g;
  real R0;
  real I0;
  real alpha;
  real beta;
  real lambda;
}

transformed data {
}

parameters {
  vector<lower=0>[N_days] R_T;
  real<lower=0> sigma_R;
  real<lower=0> R_0;
  real<lower = 0> phi;
  real<lower=0> I_negT;
}

transformed parameters {
}

model {
  vector[N_days] mu;
  real generation_value = 0.0;

  // Set the priors of interest.
  phi ~ pc_phi(lambda);
  I_negT ~ normal(I0,sqrt(I0)) T[0,];
  sigma_R ~ inv_gamma(alpha,beta);
  R_0 ~ normal(R0,4.0) T[0,];
  R_T[1] ~  normal(R_0,sigma_R) T[0,];
  R_T[2:N_days] ~ normal(R_T[1:N_days-1],sigma_R);


  for(t in 1:N_days){
      // Calculate the value of mu
      generation_value = 0.0;
      for(s in 1:N_generation){
        if((t-s)>0){
        generation_value+=g[s]*D[t-s];
        }else{
        generation_value+=g[s]*I_negT;
      }
      }
     mu[t] = R_T[t]*generation_value;
  }


  // Data
  D ~ neg_binomial_2(mu,phi);
}

generated quantities {
}
