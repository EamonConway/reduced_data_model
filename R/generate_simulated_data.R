generate_simulated_data <- function(R_t,g){
  set.seed(0x1234)
  n_days <- length(R_t)
  tibble(diagnosis_date = 1:n_days,
        R_t = R_t,
        Total_case_count = generate_simulated_incidence(R_t,g,100))
}
