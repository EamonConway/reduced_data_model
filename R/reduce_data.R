reduce_data <- function(data, window_size, keep){
  real_data <- data %>% mutate(date_group = cut(diagnosis_date,
                          breaks = min(diagnosis_date) + windows) %>%
           as_date())

aggregated_data_full <- real_data %>%
  group_by(date_group) %>%
  summarise(AggregatedIncidence = sum(cases),window = n()) %>%
  filter(!is.na(date_group)) %>%
  mutate(day = as.integer(date_group) + window,
         week = as_date(day),
         day = day - min(as.integer(date_group)))

rows_to_keep <- sample(1:nrow(real_data),floor(nrow(real_data)/filter_size))

aggregated_data <- aggregated_data_full %>%
  filter(row_number() %in% rows_to_keep)

missing_data <- aggregated_data_full %>%
  filter(!(row_number() %in% rows_to_keep))

xmax <- missing_data$week-1
xmin <- xmax - missing_data$window
}
