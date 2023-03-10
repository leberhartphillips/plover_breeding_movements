luke_theme <- 
  theme_bw() +
  theme(
    text = element_text(family = "Franklin Gothic Book"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.text.x  = element_text(size = 8), 
    axis.title.y = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(size = 0.25, lineend = "round", colour = "grey60"),
    axis.ticks.length = unit(0.1, "cm"),
    panel.border = element_rect(linetype = "solid", colour = "grey")
  )

mapviewOptions(basemaps = c("Esri.WorldImagery"))

sex_pal <- 
  c(pull(ggthemes_data$wsj$palettes$colors6[3,2]), 
    pull(ggthemes_data$wsj$palettes$colors6[2,2]))
