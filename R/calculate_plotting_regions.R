calculate_plotting_regions <- function(model_data){
  reporting_days <- model_data$D_time
  start_reporting <- model_data$D_time - model_data$D_window_size + 1

  reporting_windows <- tibble::tibble(start = start_reporting, finish = reporting_days)
  missing_windows <- reporting_windows |>
    dplyr::mutate(next_window = dplyr::lead(start,1,default = max(reporting_days)+1), start = finish+1, finish = next_window) |>
    dplyr::filter(start!=finish) |>
    dplyr::select(-next_window)
  if(reporting_windows$start[1]!=1){
    missing_windows <- missing_windows |> dplyr::add_row(start = 1,finish = reporting_windows$start[1]-1)
  }
  return(list(reported = reporting_windows,missing = missing_windows))
}
