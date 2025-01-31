plot_consistency <-function(summ){
  ggplot(summ,aes(x = day,color = Day)) + geom_ribbon(aes(ymin = `2.5%`,ymax = `97.5%`, fill = Day),alpha = 0.2) + scale_fill_brewer(palette = "YlGnBu") + scale_colour_brewer(palette = "YlGnBu") + scale_x_date("Day") +
    theme_classic() +
    theme(text = element_text(size = 16))+
    theme(
      text = element_text(size = 16),
      legend.text = element_text(size = 10, ),
      legend.position = "inside",
      legend.justification = c(0.90, 0.95)
    )
}

plot_consistency_incidence <-function(summ){
  plot_consistency(summ) +scale_y_continuous("Daily Incidence")
}

plot_consistency_rt <-function(summ){
  plot_consistency(summ) +scale_y_continuous("R Effective")
}
