// The input data is a vector 'D' of length 'N' containing aggregate data. 
functions {
  real pc_phi_lpdf(real x, real lambda){
    return -1.5*log(x) + log(lambda/2.0) - lambda/sqrt(x);
  }
}

data {
  int<lower=0> N_data;
  int<lower=0> D[N_data];
  int<lower=0> D_time[N_data];
  int<lower=0> D_window_size[N_data];
  int<lower=0> N_generation;
  vector<lower=0>[N_generation] g;
  int forecast_length;
  real I0;
  real R0;
  real alpha;
  real beta;
  real lambda;
}

transformed data {
  int<lower=0> N_days = max(D_time);
}

parameters {
  real<lower=0> R_0;
  real<lower=0> I_negT;
  vector<lower=0>[N_days] I_T;
  vector<lower=0>[N_days] R_T;
  real<lower=0> sigma_R;
  real<lower=0> phi;
}

transformed parameters {
}

model {
  vector[N_data] infections_in_window;
  // Set the priors of interest. 
  phi ~ pc_phi(lambda);
  sigma_R ~ inv_gamma(alpha,beta);
  // sigma_R ~normal(0,1);
  R_0 ~ normal(R0,4.0) T[0,]; 
  R_T[1] ~ normal(R_0,sigma_R) T[0,];
  R_T[2:N_days] ~ normal(R_T[1:N_days-1],sigma_R) T[0,];
  I_negT ~ normal(I0,sqrt(I0)) T[0,];

  for(t in 1:N_days){
    real g_v = 0.0;
    // Calculate the mean for the prior.
    for(s in 1:N_generation){
      if((t-s)>0){
        g_v+=g[s]*I_T[t-s];
      }else{
        g_v+=g[s]*I_negT;
      }
    }
    // Normal approximation to poisson.
    I_T[t] ~ normal(R_T[t]*g_v,sqrt(R_T[t]*g_v));
    // real mu_cases = R_T[t]*g_v;
    // real sd_cases  = sqrt(mu_cases + mu_cases^2/phi);
    // I_T[t] ~ normal(mu_cases,sd_cases);
  }
  
  for(i in 1:N_data){
    // Required as otherwise initialised to nan.
    infections_in_window[i] = 0;
    int time = D_time[i];
    int N_window = D_window_size[i];
    for(t in (time-(N_window-1)):time){
      // Calculate the total number of infections in time window.
      infections_in_window[i] += I_T[t];
    }
  }
  // Likelihood
  D ~ neg_binomial_2(infections_in_window,phi);
  // D ~ poisson(infections_in_window);
}

generated quantities {
  real forecast_IT[forecast_length];
  real forecast_RT[forecast_length];
  
  forecast_RT[1] = normal_rng(R_T[N_days],sigma_R);
  // forecast_RT[1] = R_T[N_days];
  for(day in 2:forecast_length){
    forecast_RT[day] = normal_rng(forecast_RT[day-1], sigma_R);
  }
  
  for(t in 1:forecast_length){
     // Generate I_T
    real g_v = 0.0;
    // Calculate the mean for the prior.
    for(s in 1:N_generation){
      if((t-s)>0){
        g_v+=g[s]*forecast_IT[t-s];
      }else{
        g_v+=g[s]*I_T[N_days + (t-s)];
      }
    }
    // Normal approximation to poisson.
    if(forecast_RT[t] > 0.0 && g_v > 0){
      forecast_IT[t] = normal_rng(forecast_RT[t]*g_v,sqrt(forecast_RT[t]*g_v));
    } else {
      forecast_IT[t] = 0.0;
    }
  }
}
