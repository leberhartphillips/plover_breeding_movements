# Circle plot of diel nesting movements of male and females
# SNPL
{
  sectors = letters[1]
  circos.par("gap.degree" = 0, "cell.padding" = c(0, 0, 0, 0), "start.degree" = 90)
  circos.initialize(sectors, xlim = c(0, 60*60*24))
  SNPL_circle_plot <-
  circos.trackPlotRegion(ylim = c(0, log(20000)), 
                         track.height = 0.8, bg.border = NA, 
                         panel.fun = function(x, y) {
                           draw.sector(start.degree = hms2rad(h = "6h 21m 18s") * 180/pi + 57,
                                       end.degree = hms2rad(h = "19h 56m 53s") * 180/pi + 57,
                                       col = alpha("grey70", 0.75), border = NA, rou2 = 0.99, rou1 = 0.15)
                           draw.sector(0, 360, rou1 = 0.2, lwd = 1, lty = 1, border = alpha("grey70", 0.75))
                           draw.sector(0, 360, rou1 = 0.38, lwd = 1, lty = 1, border = alpha("grey70", 0.75))
                           draw.sector(0, 360, rou1 = 0.56, lwd = 1, lty = 1, border = alpha("grey70", 0.75))
                           draw.sector(0, 360, rou1 = 0.75,  lwd = 1, lty = 1, border = alpha("grey70", 0.75))
                           draw.sector(0, 360, rou1 = 0.935, lwd = 1, lty = 1, border = alpha("grey70", 0.75))
                           F_d1 = c(filter(dist_to_nest_mod_predicts, sex == "F" & species == "SNPL") %>% pull(seconds_of_day),
                                  filter(dist_to_nest_mod_predicts, sex == "F" & species == "SNPL") %>% pull(seconds_of_day) %>% rev())
                           F_d2 = c(filter(dist_to_nest_mod_predicts, sex == "F" & species == "SNPL") %>% pull(upper),
                                  filter(dist_to_nest_mod_predicts, sex == "F" & species == "SNPL") %>% pull(lower) %>% rev())
                           M_d1 = c(filter(dist_to_nest_mod_predicts, sex == "M" & species == "SNPL") %>% pull(seconds_of_day),
                                    filter(dist_to_nest_mod_predicts, sex == "M" & species == "SNPL") %>% pull(seconds_of_day) %>% rev())
                           M_d2 = c(filter(dist_to_nest_mod_predicts, sex == "M" & species == "SNPL") %>% pull(upper),
                                    filter(dist_to_nest_mod_predicts, sex == "M" & species == "SNPL") %>% pull(lower) %>% rev())
                           circos.polygon(F_d1, F_d2, col = alpha("#be9c2e", 0.5), border = NA)
                           circos.lines(filter(dist_to_nest_mod_predicts, sex == "F" & species == "SNPL") %>% pull(seconds_of_day),
                                        filter(dist_to_nest_mod_predicts, sex == "F" & species == "SNPL") %>% pull(fit),
                                        col = "white")
                           circos.polygon(M_d1, M_d2, col = alpha("#016392", 0.5), border = NA)
                           circos.lines(filter(dist_to_nest_mod_predicts, sex == "M" & species == "SNPL") %>% pull(seconds_of_day),
                                        filter(dist_to_nest_mod_predicts, sex == "M" & species == "SNPL") %>% pull(fit),
                                        col = "white")
                           circos.points(filter(all_data, sex == "F" & species == "SNPL") %>%
                                           pull(time_of_day) %>%
                                           as_hms(),
                                         filter(all_data, sex == "F" & species == "SNPL") %>%
                                           mutate(dist_from_nest = log(dist_from_nest) + 0.0001) %>%
                                                    pull(dist_from_nest),
                                         cex = 1,
                                         col = alpha("#be9c2e", 0.85),
                                         pch = 19)
                           circos.points(filter(all_data, sex == "M" & species == "SNPL") %>%
                                           pull(time_of_day) %>%
                                           as_hms(),
                                         filter(all_data, sex == "M" & species == "SNPL") %>%
                                           mutate(dist_from_nest = log(dist_from_nest) + 0.0001) %>%
                                           pull(dist_from_nest),
                                         cex = 1,
                                         col = alpha("#016392", 0.85),
                                         pch = 19)
                           circos.xaxis(major.at = c(0, 60*60*1, 60*60*2, 60*60*3, 60*60*4, 60*60*5, 60*60*6,
                                                     60*60*7, 60*60*8, 60*60*9, 60*60*10, 60*60*11, 60*60*12,
                                                     60*60*13, 60*60*14, 60*60*15, 60*60*16, 60*60*17,
                                                     60*60*18, 60*60*19, 60*60*20, 60*60*21, 60*60*22,
                                                     60*60*23, 60*60*24),
                                        labels = c("Midnight", as.character(c(1:11)), "Noon", as.character(c(13:23))),
                                        major.tick.length = 0.5, minor.ticks = 0)
                           circos.yaxis(at = c(0, log(10), log(100), log(1000), log(10000)),
                                        labels = c("0 m", "10 m", "100 m", "1000 m", "10000 m"))
                           
                         })
  circos.clear()
}

{
  sectors = letters[1]
  circos.par("gap.degree" = 0, "cell.padding" = c(0, 0, 0, 0), "start.degree" = 90)
  circos.initialize(sectors, xlim = c(0, 60*60*24))
  KEPL_circle_plot <-
    circos.trackPlotRegion(ylim = c(0, log(20000)), 
                           track.height = 0.8, bg.border = NA, 
                           panel.fun = function(x, y) {
                             draw.sector(start.degree = hms2rad(h = "6h 34m 31s") * 180/pi + 40,
                                         end.degree = hms2rad(h = "20h 51m 06s") * 180/pi + 40,
                                         col = alpha("grey70", 0.75), border = NA, rou2 = 0.99, rou1 = 0.15)
                             draw.sector(0, 360, rou1 = 0.2, lwd = 1, lty = 1, border = alpha("grey70", 0.75))
                             draw.sector(0, 360, rou1 = 0.38, lwd = 1, lty = 1, border = alpha("grey70", 0.75))
                             draw.sector(0, 360, rou1 = 0.56, lwd = 1, lty = 1, border = alpha("grey70", 0.75))
                             draw.sector(0, 360, rou1 = 0.75,  lwd = 1, lty = 1, border = alpha("grey70", 0.75))
                             draw.sector(0, 360, rou1 = 0.935, lwd = 1, lty = 1, border = alpha("grey70", 0.75))
                             F_d1 = c(filter(dist_to_nest_mod_predicts, sex == "F" & species == "KEPL") %>% pull(seconds_of_day),
                                      filter(dist_to_nest_mod_predicts, sex == "F" & species == "KEPL") %>% pull(seconds_of_day) %>% rev())
                             F_d2 = c(filter(dist_to_nest_mod_predicts, sex == "F" & species == "KEPL") %>% pull(upper),
                                      filter(dist_to_nest_mod_predicts, sex == "F" & species == "KEPL") %>% pull(lower) %>% rev())
                             M_d1 = c(filter(dist_to_nest_mod_predicts, sex == "M" & species == "KEPL") %>% pull(seconds_of_day),
                                      filter(dist_to_nest_mod_predicts, sex == "M" & species == "KEPL") %>% pull(seconds_of_day) %>% rev())
                             M_d2 = c(filter(dist_to_nest_mod_predicts, sex == "M" & species == "KEPL") %>% pull(upper),
                                      filter(dist_to_nest_mod_predicts, sex == "M" & species == "KEPL") %>% pull(lower) %>% rev())
                             circos.polygon(F_d1, F_d2, col = alpha("#be9c2e", 0.5), border = NA)
                             circos.lines(filter(dist_to_nest_mod_predicts, sex == "F" & species == "KEPL") %>% pull(seconds_of_day),
                                          filter(dist_to_nest_mod_predicts, sex == "F" & species == "KEPL") %>% pull(fit),
                                          col = "white")
                             circos.polygon(M_d1, M_d2, col = alpha("#016392", 0.5), border = NA)
                             circos.lines(filter(dist_to_nest_mod_predicts, sex == "M" & species == "KEPL") %>% pull(seconds_of_day),
                                          filter(dist_to_nest_mod_predicts, sex == "M" & species == "KEPL") %>% pull(fit),
                                          col = "white")
                             circos.points(filter(all_data, sex == "F" & species == "KEPL") %>%
                                             pull(time_of_day) %>%
                                             as_hms(),
                                           filter(all_data, sex == "F" & species == "KEPL") %>%
                                             mutate(dist_from_nest = log(dist_from_nest) + 0.0001) %>%
                                             pull(dist_from_nest),
                                           cex = 1,
                                           col = alpha("#be9c2e", 0.85),
                                           pch = 19)
                             circos.points(filter(all_data, sex == "M" & species == "KEPL") %>%
                                             pull(time_of_day) %>%
                                             as_hms(),
                                           filter(all_data, sex == "M" & species == "KEPL") %>%
                                             mutate(dist_from_nest = log(dist_from_nest) + 0.0001) %>%
                                             pull(dist_from_nest),
                                           cex = 1,
                                           col = alpha("#016392", 0.85),
                                           pch = 19)
                             circos.xaxis(major.at = c(0, 60*60*1, 60*60*2, 60*60*3, 60*60*4, 60*60*5, 60*60*6,
                                                       60*60*7, 60*60*8, 60*60*9, 60*60*10, 60*60*11, 60*60*12,
                                                       60*60*13, 60*60*14, 60*60*15, 60*60*16, 60*60*17,
                                                       60*60*18, 60*60*19, 60*60*20, 60*60*21, 60*60*22,
                                                       60*60*23, 60*60*24),
                                          labels = c("Midnight", as.character(c(1:11)), "Noon", as.character(c(13:23))),
                                          major.tick.length = 0.5, minor.ticks = 0)
                             circos.yaxis(at = c(0, log(10), log(100), log(1000), log(10000)),
                                          labels = c("0 m", "10 m", "100 m", "1000 m", "10000 m"))
                             
                           })
  circos.clear()
}

