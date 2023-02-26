# prepare R environment
source("R/project/project_libraries.R")
source("R/project/project_plotting_misc.R")
source("R/project/project_load_movement_data.R")
source("R/project/project_load_breeding_data.R")
source("R/wrangle/wrangle_tag_nest_brood_resight_data.R")

# load functions
function.sources = list.files(path = "R/functions/", 
                              pattern = "*\\().R$", full.names = TRUE, 
                              ignore.case = TRUE)
sapply(function.sources, source, .GlobalEnv)

# run a linear model testing how the distance from nest depends on time of day 
# and sex

# sample (4 males, 4 females / 5 SNPL, 3 KEPL):
# Male SNPL CA3340 from 2022 (PinPoint 20-min then 12-hour @ 1000/2200) perfect circadian incubation pattern
# Male SNPL CN0066 from 2022 (PinPoint 20-min) decent circadian incubation pattern, but reduced sample
# Male KEPL D59946 great circadian incubation pattern
# Male KEPL P01903 decent circadian incubation pattern
# Female KEPL P01902 great circadian incubation pattern
# Female SNPL CM1858 from 2022 (PinPoint 20-min) perfect circadian incubation pattern
# Female SNPL CA3224 from 2022 (PinPoint 20-min) perfect circadian incubation pattern
# Female SNPL CN0937 from 2022 (PinPoint 20-min then 12-hour @ 1000/2200) perfect circadian incubation pattern

#### calculate distances from nest ----
# Female KEPL P01902
P01902_dist_to_nest <- 
  bird_to_nest_dist(bird_ring = "P01902", 
                    tag_and_breeding_data = tag_breeding_data_tagus, 
                    local_time_zone = "Europe/Lisbon", 
                    focal_year = 2021, focal_nest_ID = "BC02") 
P01902_dist_to_nest %>% 
  ggplot(.) +
  geom_boxplot(aes(x = hms::as_hms(rounded_hour),
                   y = dist_from_nest, group = rounded_hour))

ggplot(data = P01902_dist_to_nest) +
  geom_histogram(aes(log(dist_from_nest)))

# Female SNPL CM1858
CM1858_dist_to_nest <- 
  bird_to_nest_dist(bird_ring = "CM1858", 
                    tag_and_breeding_data = tag_breeding_data_ceuta, 
                    local_time_zone = "America/Mazatlan", 
                    focal_year = 2022, cut_time = "2022-06-13 07:00:00") 
CM1858_dist_to_nest %>% 
  ggplot(.) +
  geom_boxplot(aes(x = hms::as_hms(rounded_hour),
                   y = dist_from_nest, group = rounded_hour))

ggplot(data = CM1858_dist_to_nest) +
  geom_histogram(aes(log(dist_from_nest)))

# Female SNPL CA3224
CA3224_dist_to_nest <- 
  bird_to_nest_dist(bird_ring = "CA3224", 
                    tag_and_breeding_data = tag_breeding_data_ceuta, 
                    local_time_zone = "America/Mazatlan", 
                    focal_year = 2022, cut_time = "2022-04-21 07:00:00")
CA3224_dist_to_nest %>% 
  ggplot(.) +
  geom_boxplot(aes(x = hms::as_hms(rounded_hour),
                   y = dist_from_nest, group = rounded_hour))

ggplot(data = CA3224_dist_to_nest) +
  geom_histogram(aes(log(dist_from_nest)))

# Female SNPL CN0937
CN0937_dist_to_nest <- 
  bird_to_nest_dist(bird_ring = "CN0937", 
                    tag_and_breeding_data = tag_breeding_data_ceuta, 
                    local_time_zone = "America/Mazatlan", 
                    focal_year = 2022, cut_time = "2022-05-06 07:00:00") 
CN0937_dist_to_nest %>% 
  ggplot(.) +
  geom_boxplot(aes(x = hms::as_hms(rounded_hour),
                   y = dist_from_nest, group = rounded_hour))

ggplot(data = CN0937_dist_to_nest) +
  geom_histogram(aes(log(dist_from_nest)))

# Male SNPL CA3340 from 2022 (PinPoint 20-min then 12-hour @ 1000/2200) perfect circadian incubation pattern
CA3340_dist_to_nest <- 
  bird_to_nest_dist(bird_ring = "CA3340", 
                    tag_and_breeding_data = tag_breeding_data_ceuta, 
                    local_time_zone = "America/Mazatlan", 
                    focal_year = 2022, cut_time = "2022-04-22 07:00:00") 
CA3340_dist_to_nest %>% 
  ggplot(.) +
  geom_boxplot(aes(x = hms::as_hms(rounded_hour),
                   y = dist_from_nest, group = rounded_hour))

ggplot(data = CA3340_dist_to_nest) +
  geom_histogram(aes(log(dist_from_nest)))

# Male SNPL CN0066 from 2022 (PinPoint 20-min) decent circadian incubation pattern, but reduced sample
CN0066_dist_to_nest <- 
  bird_to_nest_dist(bird_ring = "CN0066", 
                    tag_and_breeding_data = tag_breeding_data_ceuta, 
                    local_time_zone = "America/Mazatlan", 
                    focal_year = 2022, cut_time = "2022-05-03 07:00:00") 
CN0066_dist_to_nest %>% 
  ggplot(.) +
  geom_boxplot(aes(x = hms::as_hms(rounded_hour),
                   y = dist_from_nest, group = rounded_hour))

# Male KEPL D59946 great circadian incubation pattern
D59946_dist_to_nest <- 
  bird_to_nest_dist(bird_ring = "D59946", 
                    tag_and_breeding_data = tag_breeding_data_tagus, 
                    local_time_zone = "Europe/Lisbon", 
                    focal_year = 2021, focal_nest_ID = "BC06") 
D59946_dist_to_nest %>% 
  ggplot(.) +
  geom_boxplot(aes(x = hms::as_hms(rounded_hour),
                   y = dist_from_nest, group = rounded_hour))

# Male KEPL P01903 decent circadian incubation pattern
P01903_dist_to_nest <- 
  bird_to_nest_dist(bird_ring = "P01903", 
                    tag_and_breeding_data = tag_breeding_data_tagus, 
                    local_time_zone = "Europe/Lisbon", 
                    focal_year = 2021, focal_nest_ID = "BC03") 
P01903_dist_to_nest %>% 
  ggplot(.) +
  geom_boxplot(aes(x = hms::as_hms(rounded_hour),
                   y = dist_from_nest, group = rounded_hour))

# bind all datasets together and select columns of interest
tagus_data <- 
  bind_rows(P01902_dist_to_nest, 
            D59946_dist_to_nest, 
            P01903_dist_to_nest) %>% 
  mutate(sex = as.factor(sex),
         ring = as.factor(ring)) %>% 
  mutate(across(c("nightEnd", "night", 
                  "sunrise", "sunset", 
                  "dawn", "dusk", 
                  "nauticalDawn", "nauticalDusk"), ~ as.character(as_hms(ymd_hms(.x, tz = "Europe/Lisbon"))))) %>% 
  dplyr::select(ring, sex, species, population, nightEnd, night, sunrise, 
                sunset, dawn, dusk, nauticalDawn, nauticalDusk, 
                dist_from_nest, time_of_day)

ceuta_data <- 
  bind_rows(CM1858_dist_to_nest, 
            CA3224_dist_to_nest, 
            CN0937_dist_to_nest, 
            CA3340_dist_to_nest, 
            CN0066_dist_to_nest) %>% 
  mutate(sex = as.factor(sex),
         ring = as.factor(ring)) %>% 
  mutate(across(c("nightEnd", "night", 
                  "sunrise", "sunset", 
                  "dawn", "dusk", 
                  "nauticalDawn", "nauticalDusk"), ~ as.character(as_hms(ymd_hms(.x, tz = "America/Mazatlan"))))) %>% 
  dplyr::select(ring, sex, species, population, nightEnd, night, sunrise, 
                sunset, dawn, dusk, nauticalDawn, nauticalDusk, 
                dist_from_nest, time_of_day)

all_data <- 
  bind_rows(tagus_data, ceuta_data) %>% 
  mutate(seconds_of_day = lubridate::hms(time_of_day) %>% period_to_seconds(.)) %>% 
  mutate(across(c("nightEnd", "night", 
                  "sunrise", "sunset", 
                  "dawn", "dusk", 
                  "nauticalDawn", "nauticalDusk"), ~ period_to_seconds(lubridate::hms(.x))))

# summarise the sunlight means for each population/species
population_sunlighttimes <- 
  all_data %>% 
  group_by(species) %>% 
  summarise(mean_nightEnd = mean(nightEnd), 
            mean_night = mean(night), 
            mean_sunrise = mean(sunrise), 
            mean_sunset = mean(sunset), 
            mean_dawn = mean(dawn), 
            mean_dusk = mean(dusk), 
            mean_nauticalDawn = mean(nauticalDawn), 
            mean_nauticalDusk = mean(nauticalDusk))

# fit a GAMM
# distance from nest was log transformed modeled as a Gaussian distribution
# 
dist_to_nest_mod <- 
  gam(log(dist_from_nest) ~ sex + species +
        s(seconds_of_day, by = interaction(sex, species), bs = "cc") + 
        s(ring, bs = 're'), 
      data = all_data)

draw(dist_to_nest_mod, parametric = FALSE)

hist(log(all_data$dist_from_nest))
ggplot(data = all_data) +
  geom_histogram(aes(log(dist_from_nest)), bins = 15)

exp(5)

newdata_seconds_sex <- 
  expand.grid(seconds_of_day = seq(1, 60*60*24),
              sex = as.factor(c("F", "M")),
              species = as.factor(c("SNPL", "KEPL")))

dist_to_nest_mod_fits <- 
  predict(dist_to_nest_mod, 
          newdata = newdata_seconds_sex, 
          type = 'response', se = TRUE,
          newdata.guaranteed = TRUE, exclude = "s(ring)")

dist_to_nest_mod_predicts <-  
  data.frame(newdata_seconds_sex, dist_to_nest_mod_fits) %>% 
  mutate(lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit,
         time_of_day2 = as_hms(seconds_of_day)) %>% 
  mutate(fit_trans = exp(fit),
         lower_trans = exp(lower),
         upper_trans = exp(upper)) %>% 
  arrange(sex)

species_pop_labels <- c(
  'SNPL' = "Snowy Plover\nBahía de Ceuta, Mexcio",
  'KEPL' = "Kentish Plover\nTagus Estuary, Portugal"
)

ggplot() +
  geom_rect(data = population_sunlighttimes,
            aes(xmin = 0, xmax = hms::as_hms(mean_nightEnd), ymin = 0, ymax = Inf),
            fill = "#023858", alpha = 0.75) +
  geom_rect(data = population_sunlighttimes,
            aes(xmin = hms::as_hms(mean_nightEnd), xmax = hms::as_hms(mean_nauticalDawn), ymin = 0, ymax = Inf),
            fill = "#045a8d", alpha = 0.75) +
  geom_rect(data = population_sunlighttimes,
            aes(xmin = hms::as_hms(mean_nauticalDawn), xmax = hms::as_hms(mean_dawn), ymin = 0, ymax = Inf),
            fill = "#0570b0", alpha = 0.75) +
  geom_rect(data = population_sunlighttimes,
            aes(xmin = hms::as_hms(mean_dawn), xmax = hms::as_hms(mean_sunrise), ymin = 0, ymax = Inf),
            fill = "#3690c0", alpha = 0.75) +
  geom_rect(data = population_sunlighttimes,
            aes(xmin = hms::as_hms(mean_sunset), xmax = hms::as_hms(mean_dusk), ymin = 0, ymax = Inf),
            fill = "#3690c0", alpha = 0.75) +
  geom_rect(data = population_sunlighttimes,
            aes(xmin = hms::as_hms(mean_dusk), xmax = hms::as_hms(mean_nauticalDusk), ymin = 0, ymax = Inf),
            fill = "#0570b0", alpha = 0.75) +
  geom_rect(data = population_sunlighttimes,
            aes(xmin = hms::as_hms(mean_nauticalDusk), xmax = hms::as_hms(mean_night), ymin = 0, ymax = Inf),
            fill = "#045a8d", alpha = 0.75) +
  geom_rect(data = population_sunlighttimes,
            aes(xmin = hms::as_hms(mean_night), xmax = Inf, ymin = 0, ymax = Inf), 
            fill = "#023858", alpha = 0.75) +
  # geom_ribbon(data = dist_to_nest_mod_predicts,
  #             aes(x = time_of_day2, ymin = lower_trans, ymax = upper_trans, fill = sex, color = sex),
  #             alpha = 0.8) +
  # geom_line(data = dist_to_nest_mod_predicts,
  #           aes(x = time_of_day2, y = fit_trans, color = sex)) +
  geom_ribbon(data = dist_to_nest_mod_predicts,
              aes(x = time_of_day2, ymin = lower_trans, ymax = upper_trans, fill = sex, color = sex),
              alpha = 0.8) +
  geom_line(data = dist_to_nest_mod_predicts,
            aes(x = time_of_day2, y = fit_trans, color = sex)) +
  geom_jitter(data = all_data,
              aes(x = as_hms(time_of_day),
                  y = dist_from_nest + 0.00001, fill = sex),
              position = position_jitter(width = 60*20, seed = 12356),
              alpha = 0.5,
              shape = 21, color = "white", size = 3) +
  scale_x_time(labels = label_time(format = "%H"), 
               name = "hour of day", 
               expand = c(-0.01, 0.0),
               breaks = as_hms(c('01:00:00', '06:00:00', '12:00:00', '18:00:00', '23:00:00'))) +
  scale_y_continuous(trans = 'log10') +
  ylab("distance from nest (log10, m)") +
  facet_grid(species ~ ., labeller = labeller(.rows = species_pop_labels)) +
  luke_theme +
  theme(legend.position = c(0.5, 0.95),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        strip.background = element_blank()) +
        # panel.grid.major = element_line(colour = "grey70", size = 0.25),
        # panel.grid.minor = element_line(colour = "grey70", size = 0.1)) +
        #legend.position = c(0.15, 0.825),
        #legend.title = element_text(size = 9),
        # axis.title.x = element_blank(),
        # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_colour_manual(values = c("white", "white"), 
                      labels = c("Female", "Male")) +
  scale_fill_manual(values = brewer.pal(8, "Dark2")[c(2, 1)], 
                      labels = c("Female", "Male"))

population_sunlighttimes %>% 
  filter(species == "SNPL") %>% 
  mutate(mean_sunrise = as_hms(mean_sunrise)) %>% ymd_hms()), scale = "radian")) %>% 
  pull(mean_sunrise)

gettime(CM1858_dist_to_nest$sunrise[1], scale = "radian") * 180/pi
gettime(CM1858_dist_to_nest$sunset[1], scale = "radian") * 180/pi
# "#be9c2e""#016392"
# test circlize figure
{
sectors = letters[1]
circos.par("gap.degree" = 0, "cell.padding" = c(0, 0, 0, 0), "start.degree" = 90)
circos.initialize(sectors, xlim = c(0, 60*60*24), )
circos.trackPlotRegion(ylim = c(0, log(20000)), 
                       track.height = 0.8, bg.border = NA, 
                       panel.fun = function(x, y) {
  draw.sector(start.degree = gettime(CM1858_dist_to_nest$sunrise[1], scale = "radian") * 180/pi + 57, 
              end.degree = gettime(CM1858_dist_to_nest$sunset[1], scale = "radian") * 180/pi + 57, 
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

"#00FF0080"

set.seed(123)
sectors = letters[1:4]
circos.initialize(sectors, xlim = c(0, 1))
circos.trackPlotRegion(ylim = c(-3, 3), track.height = 0.9, panel.fun = function(x, y) {
  x1 = runif(20)
  y1 = x1 + rnorm(20)
  or = order(x1)
  x1 = x1[or]
  y1 = y1[or]
  loess.fit = loess(y1 ~ x1)
  loess.predict = predict(loess.fit, x1, se = TRUE)
  d1 = c(x1, rev(x1))
  d2 = c(loess.predict$fit + loess.predict$se.fit,
         rev(loess.predict$fit - loess.predict$se.fit))
  circos.polygon(d1, d2, col = "#CCCCCC", border = NA)
  circos.points(x1, y1, cex = 0.5)
  circos.lines(x1, loess.predict$fit)
})
circos.clear()

male_dist_to_nest_mod <- 
  gam(log(dist_from_nest) ~ s(seconds_of_day, bs = "cc"), data = all_data %>% filter(sex == "M"))

female_dist_to_nest_mod <- 
  gam(log(dist_from_nest) ~ s(seconds_of_day, bs = "cc"), data = all_data %>% filter(sex == "F"))



male_dist_to_nest_mod_fits <- 
  predict(male_dist_to_nest_mod, 
          newdata = newdata_seconds, 
          type = 'response', se = TRUE)

female_dist_to_nest_mod_fits <- 
  predict(female_dist_to_nest_mod, 
          newdata = newdata_seconds, 
          type = 'response', se = TRUE)



male_dist_to_nest_mod_predicts <-  
  data.frame(newdata_seconds, male_dist_to_nest_mod_fits) %>% 
  mutate(lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit,
         time_of_day2 = as_hms(seconds_of_day)) %>% 
  mutate(fit_trans = exp(fit),
         lower_trans = exp(lower),
         upper_trans = exp(upper))

female_dist_to_nest_mod_predicts <-  
  data.frame(newdata_seconds, female_dist_to_nest_mod_fits) %>% 
  mutate(lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit,
         time_of_day2 = as_hms(seconds_of_day)) %>% 
  mutate(fit_trans = exp(fit),
         lower_trans = exp(lower),
         upper_trans = exp(upper))



ggplot() +
  geom_jitter(data = filter(all_data, sex == "M"), 
              aes(x = as_hms(time_of_day), 
                  y = dist_from_nest), 
              width = 2, alpha = 0.75) +
  geom_ribbon(data = male_dist_to_nest_mod_predicts, 
              aes(x = time_of_day2, ymin = lower_trans, ymax = upper_trans), 
              fill = brewer.pal(8, "Dark2")[c(2)], alpha = 0.3) +
  geom_line(data = male_dist_to_nest_mod_predicts, 
            aes(x = time_of_day2, y = fit_trans), 
            color = brewer.pal(8, "Dark2")[c(6)]) +
  scale_x_time(labels = label_time(format = "%H"), 
               name = "hour of day", 
               expand = c(0.01, 0.01),
               breaks = as.hms(c('00:00:00', '06:00:00', '12:00:00', '18:00:00'))) +
  ylab("distance from nest (m)")

ggplot() +
  geom_jitter(data = filter(all_data, sex == "F"), 
              aes(x = as_hms(time_of_day), 
                  y = dist_from_nest), 
              width = 2, alpha = 0.75) +
  geom_ribbon(data = female_dist_to_nest_mod_predicts, 
              aes(x = time_of_day2, ymin = lower_trans, ymax = upper_trans), 
              fill = brewer.pal(8, "Dark2")[c(2)], alpha = 0.3) +
  geom_line(data = female_dist_to_nest_mod_predicts, 
            aes(x = time_of_day2, y = fit_trans), 
            color = brewer.pal(8, "Dark2")[c(6)]) +
  scale_x_time(labels = label_time(format = "%H"), 
               name = "hour of day", 
               expand = c(0.01, 0.01),
               breaks = as.hms(c('00:00:00', '06:00:00', '12:00:00', '18:00:00'))) +
  ylab("distance from nest (m)")


# +
  # labs(colour = "Country measured") +
  luke_theme +
  theme(legend.position = "none",
        # panel.grid.major = element_line(colour = "grey70", size = 0.25),
        panel.grid.minor = element_line(colour = "grey70", size = 0.1),
        #legend.position = c(0.15, 0.825),
        #legend.title = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_colour_manual(values = brewer.pal(8, "Dark2")[c(1, 8)], 
                      labels = c("Australia", "New Zealand"))

seconds_to_period()
test <- 
  lm(log(dist_from_nest) ~ radian_bird_time * sex, data = all_data)
summary(test)
plot(test)



#### Fit GAM to mass data ----


newdata_jul_days <- 
  data.frame(JulianDay = seq(1, 365))

annual_mass_mod_fits <- 
  predict(annual_mass_mod, 
          newdata = newdata_jul_days, 
          type = 'response', se = TRUE)

annual_mass_mod_predicts <-  
  data.frame(newdata_jul_days, annual_mass_mod_fits) %>% 
  mutate(lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit,
         JulianDay2 = as.Date(JulianDay, 
                              format = "%j", 
                              origin = as.Date("2018-01-01")))

bpnr(radian_bird_time ~ sex + dist_from_nest, all_data)

male_mod <- 
  lincircKern(ceuta_data %>% filter(sex == "M") %>% pull(radian_bird_time), 
              ceuta_data %>% filter(sex == "M") %>% pull(dist_from_nest), 
              pCI = 0.95, reps = 10, res = 512)

plot(male_mod, CircScale = 24, xaxp = c(0, 24, 4), 
     xlab = "time of day", ylab = "distance from nest")

data(BCIspeed)
i <- BCIspeed$species=="ocelot"
sp <- log(BCIspeed$speed[i])
tm <- BCIspeed$time[i]*2*pi
mod <- fitlincirc(tm, sp, reps=50)
plot(mod, CircScale=24, xaxp=c(0,24,4), xlab="Time", ylab="log(speed)")
legend(8,-3, c("Fitted speed", "Null CI"), col=1:2, lty=1:2)

data(BCIspeed)
i <- BCIspeed$species=="ocelot"
log_speed <- log(BCIspeed$speed[i])
time <- BCIspeed$time[i]*2*pi
circseq <- seq(0,2*pi,pi/256)
trend <- lincircKern(circseq, time, log_speed)
plot(time, log_speed, xlim=c(0, 2*pi))
lines(circseq, trend)
