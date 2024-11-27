load_and_filter_real_data<- function(filename){
  return(read_csv(filename) %>%
           select(diagnosis_date,Total_case_count) %>%
           group_by(diagnosis_date) %>%
           summarise(cases = sum(Total_case_count)) %>%
           mutate(diagnosis_date = ymd(diagnosis_date)) %>%
           filter(diagnosis_date < dmy("01-09-2021") + 7*15,
                  diagnosis_date >= dmy("01-09-2021")))
}
