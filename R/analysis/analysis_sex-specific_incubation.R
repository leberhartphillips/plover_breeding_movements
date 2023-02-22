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
            mean_nauticalDusk = mean(nauticalDusk)) #%>% 
  # mutate(across(c("mean_nightEnd", "mean_night", 
  #                 "mean_sunrise", "mean_sunset", 
  #                 "mean_dawn", "mean_dusk", 
  #                 "mean_nauticalDawn", "mean_nauticalDusk"), as_hms))
  

# fit a GAMM
# distance from nest was log transformed modeled as a Gaussian distribution
dist_to_nest_mod <- 
  gam(log(dist_from_nest) ~ sex + species +
        s(seconds_of_day, by = interaction(sex, species), bs = "cc") + 
        s(ring, bs = 're'), 
      # family = quasi(link = "log", variance = "mu"),
      data = all_data)

coef(dist_to_nest_mod)[1:4]
draw(dist_to_nest_mod, parametric = FALSE)

newdata_seconds_sex <- 
  expand.grid(seconds_of_day = seq(1, 60*60*24),
              sex = as.factor(c("M", "F")),
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
  'SNPL' = "Snowy Plover (Bahía de Ceuta, Mexcio)",
  'KEPL' = "Kentish Plover (Tagus Estuary, Portugal)"
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
  geom_point(data = all_data,
              aes(x = as_hms(time_of_day),
                  y = dist_from_nest + 0.00001, color = sex),
              # width = 2,
              alpha = 0.75) +
  geom_ribbon(data = dist_to_nest_mod_predicts, 
              aes(x = time_of_day2, ymin = lower_trans, ymax = upper_trans, fill = sex), 
              alpha = 0.3) +
  geom_line(data = dist_to_nest_mod_predicts, 
            aes(x = time_of_day2, y = fit_trans, color = sex)) +
  scale_x_time(labels = label_time(format = "%H"), 
               name = "hour of day", 
               expand = c(0.0, 0.0),
               breaks = as_hms(c('00:00:00', '06:00:00', '12:00:00', '18:00:00'))) +
  scale_y_continuous(trans = 'log10') +
  ylab("distance from nest (m)") +
  facet_grid(species ~ ., labeller = labeller(.rows = species_pop_labels)) +
  luke_theme +
  theme(legend.position = "top",
        # panel.grid.major = element_line(colour = "grey70", size = 0.25),
        panel.grid.minor = element_line(colour = "grey70", size = 0.1)) +
        #legend.position = c(0.15, 0.825),
        #legend.title = element_text(size = 9),
        # axis.title.x = element_blank(),
        # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_colour_manual(values = brewer.pal(8, "Dark2")[c(1, 8)], 
                      labels = c("Male", "Female")) +
  scale_fill_manual(values = brewer.pal(8, "Dark2")[c(1, 8)], 
                      labels = c("Male", "Female"))

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
