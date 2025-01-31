data <- read_csv("data/victoria_data.csv") %>%
  select(diagnosis_date, Total_case_count) %>%
  group_by(diagnosis_date) %>%
  summarise(cases = sum(Total_case_count)) %>%
  mutate(diagnosis_date = ymd(diagnosis_date)) %>%
  filter(diagnosis_date < dmy("01-09-2021") + 7 * 15,
         diagnosis_date >= dmy("01-09-2021"))

data2 <- load_data()
