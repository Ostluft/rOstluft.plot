
fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")

data <-
  rOstluft::read_airmo_csv(fn) %>%
  dplyr::filter(parameter %in% c("T", "NOx", "O3", "StrGlo", "PM10")) %>%
  rOstluft::resample(new_interval = "h1")










# diurnal
data_summarized <- summary_periodic(data)
data_summarized <- tidyr::spread(data_summarized, stat, value)

ggplot(data_summarized, mapping = aes(x = as.numeric(starttime_of_day) + 0.5, y = mean, group = NA)) +
  geom_line(aes(y = quantile05), lty = 2, color = "gray60") +
  geom_ribbon(aes(ymin = quantile25, ymax = quantile75), fill = "gray60", alpha = 0.5) +
  geom_line(aes(y = quantile95), lty = 2, color = "gray60") +
  geom_line(color = "gray30") +
  lemon::facet_rep_wrap(parameter~., scales = "free_y") +
  scale_x_continuous(breaks = seq(1, length(unique(data_summarized$starttime_of_day))-1, 3),
                     labels = unique(data_summarized$starttime_of_day)[seq(1, length(unique(data_summarized$starttime_of_day))-1, 3)],
                     expand = c(0.025,0.025)) +
  theme_classic() +
  theme(
    strip.text = element_text(hjust = 0),
    strip.background = element_blank(),
    panel.grid.major.y = element_line(linetype = 2, size = 0.25, colour = "gray80")
  )






# diurnal-weekday
data_summarized <- summary_periodic(data, groupings = c("starttime_of_day", "weekday"))
data_summarized <- tidyr::spread(data_summarized, stat, value)

ggplot(data_summarized, mapping = aes(x = as.numeric(starttime_of_day) + 0.5, y = mean, group = NA)) +
  geom_line(aes(y = quantile05), lty = 2, color = "gray60") +
  geom_ribbon(aes(ymin = quantile25, ymax = quantile75), fill = "gray60", alpha = 0.5) +
  geom_line(aes(y = quantile95), lty = 2, color = "gray60") +
  geom_line(color = "gray30") +
  lemon::facet_rep_grid(parameter~weekday, scales = "free_y") +
  scale_x_continuous(limits = c(1, 23.5), breaks = seq(1, length(unique(data_summarized$starttime_of_day))-1, 3),
                     labels = unique(data_summarized$starttime_of_day)[seq(1, length(unique(data_summarized$starttime_of_day))-1, 3)],
                     expand = c(0,0)) +
  theme_classic() +
  theme(
    strip.text = element_text(hjust = 0),
    strip.background = element_blank(),
    panel.spacing = unit(0, "lines"),
    panel.grid.major.y = element_line(linetype = 2, size = 0.25, colour = "gray80")
  )






# weekday
data_summarized <- summary_periodic(data, groupings = "weekday")
data_summarized <- tidyr::spread(data_summarized, stat, value)

ggplot(data_summarized, mapping = aes(x = weekday, group = parameter)) +
  geom_boxplot(mapping = aes(ymin = quantile05, lower = quantile25, middle = median, upper = quantile25, ymax = quantile05),
               stat = "identity", outlier.colour = NA) +
  theme_classic() +
  theme(
    strip.text = element_text(hjust = 0),
    strip.background = element_blank(),
    panel.grid.major.y = element_line(linetype = 2, size = 0.25, colour = "gray80")
  )



data_summarized <-
  dplyr::bind_rows(
    dplyr::mutate(data_summarized,
                  `avg - dev` = mean + sd,
                  avg = mean,
                  `avg + dev` = mean + sd,
                  group = "mean +/- sd"
    ),
    dplyr::mutate(data_summarized,
                  `avg - dev` = median + sd,
                  avg = median,
                  `avg + dev` = median + sd,
                  group = "median +/- mad"
    )
  ) %>%
  dplyr::select(weekday, site, interval, parameter, unit, n, group, `avg - dev`, avg, `avg + dev`)


ggplot(data_summarized, mapping = aes(x = weekday, y = avg, group = group, color = group)) +
  geom_line(position = position_dodge(width = 0.1)) +
  geom_pointrange(mapping = aes(ymin = `avg - dev`, y = avg, ymax = `avg + dev`), shape = 21, fill = "white", fatten = 2, position = position_dodge(width = 0.1), stat = "identity") +
  lemon::facet_rep_wrap(parameter~., scales = "free_y") +
  theme_classic() +
  theme(
    strip.text = element_text(hjust = 0),
    strip.background = element_blank(),
    panel.grid.major.y = element_line(linetype = 2, size = 0.25, colour = "gray80")
  )

