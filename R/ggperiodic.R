
# fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#
# data <-
#   rOstluft::read_airmo_csv(fn) %>%
#   dplyr::filter(parameter %in% c("NOx", "O3", "PM10")) %>%
#   rOstluft::resample(new_interval = "h1")






# diurnal
data_summarized <- summary_periodic(data)
data_summarized <- tidyr::spread(data_summarized, stat, value)
data_summarized <- dplyr::mutate(data_summarized, parameter = factor(paste0(parameter, " (", unit, ")")))

ggplot(data_summarized, mapping = aes(x = as.numeric(starttime_of_day) + 0.5, y = mean, group = NA)) +
  geom_line(aes(y = quantile05), lty = 2, color = "gray60") +
  geom_ribbon(aes(ymin = quantile25, ymax = quantile75), fill = "gray60", alpha = 0.5) +
  geom_line(aes(y = median), lty = 2, color = "gray30") +
  geom_line(aes(y = quantile95), lty = 2, color = "gray60") +
  geom_line(color = "gray30") +
  lemon::facet_rep_grid(parameter~., scales = "free_y", switch = "y") + # ncol = ceiling(length(unique(data_summarized$parameter)) / 4)) +
  scale_x_continuous(breaks = seq(1, length(unique(data_summarized$starttime_of_day)) - 1, 3),
                     labels = unique(data_summarized$starttime_of_day)[seq(1, length(unique(data_summarized$starttime_of_day)) - 1, 3)],
                     expand = c(0.025,0.025)) +
  theme_classic() +
  theme(
    strip.text.x = element_text(hjust = 0),
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.grid.major.y = element_line(linetype = 2, size = 0.25, colour = "gray80"),
    axis.title = element_blank()
  )






# diurnal-weekday/weekend
data_summarized <- summary_periodic(data, groupings = c("starttime_of_day", "weekend"))
data_summarized <- tidyr::spread(data_summarized, stat, value)
data_summarized <- dplyr::mutate(data_summarized, parameter = factor(paste0(parameter, " (", unit, ")")))

ggplot(data_summarized, mapping = aes(x = as.numeric(starttime_of_day) + 0.5, y = mean, group = NA)) +
  geom_line(aes(y = quantile05), lty = 2, color = "gray60") +
  geom_ribbon(aes(ymin = quantile25, ymax = quantile75), fill = "gray60", alpha = 0.5) +
  geom_line(aes(y = median), lty = 2, color = "gray30") +
  geom_line(aes(y = quantile95), lty = 2, color = "gray60") +
  geom_line(color = "gray30") +
  lemon::facet_rep_grid(parameter~weekend, scales = "free_y", switch = "y") +
  scale_x_continuous(limits = c(1, 23.5), breaks = seq(1, length(unique(data_summarized$starttime_of_day)) - 1, 6),
                     labels = unique(data_summarized$starttime_of_day)[seq(1, length(unique(data_summarized$starttime_of_day)) - 1, 6)],
                     expand = c(0,0)) +
  theme_classic() +
  theme(
    strip.text.x = element_text(hjust = 0),
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(0, "lines"),
    panel.grid.major.y = element_line(linetype = 2, size = 0.25, colour = "gray80"),
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )






# weekday/weekend
data_summarized <- summary_periodic(data, groupings = "weekend")
data_summarized <- tidyr::spread(data_summarized, stat, value)

ggplot(data_summarized, mapping = aes(x = parameter, group = interaction(weekend, parameter), fill = weekend)) +
  geom_boxplot(mapping = aes(ymin = quantile05, lower = quantile25, middle = median, upper = quantile75, ymax = quantile95),
               stat = "identity", outlier.colour = NA) +
  geom_point(aes(y = mean), shape = 21, position = position_dodge(width = 0.9)) +
  theme_classic() +
  theme(
    strip.text = element_text(hjust = 0),
    strip.background = element_blank(),
    panel.grid.major.y = element_line(linetype = 2, size = 0.25, colour = "gray80"),
    axis.title.x = element_blank(),
    legend.title = element_blank()
  ) +
  ylab("value")






# diurnal-weekday/weekend
data_summarized <- summary_periodic(data, groupings = c("starttime_of_day", "weekend"))
data_summarized <- tidyr::spread(data_summarized, stat, value)
data_summarized <- dplyr::mutate(data_summarized, parameter = factor(paste0("Delta ", parameter, " (", unit, ")")))
data_summarized <-
  data_summarized %>%
  dplyr::select(starttime_of_day, weekend, site, interval, parameter, mean) %>%
  tidyr::spread(weekend, mean)
# data_summarized <-
#   dplyr::bind_rows(
#     dplyr::mutate(data_summarized,
#                   `avg - dev` = mean + sd,
#                   avg = mean,
#                   `avg + dev` = mean + sd,
#                   group = "mean +/- sd"
#     ),
#     dplyr::mutate(data_summarized,
#                   `avg - dev` = median + sd,
#                   avg = median,
#                   `avg + dev` = median + sd,
#                   group = "median +/- mad"
#     )
#   ) %>%
#   dplyr::select(starttime_of_day, weekend, site, interval, parameter, unit, n, group, `avg - dev`, avg, `avg + dev`)

ggplot(data_summarized, mapping = aes(x = as.numeric(starttime_of_day), y = weekday - weekend, group = parameter)) +
  geom_hline(yintercept = 0, lty = 2, color = "gray60") +
  geom_path() +
  # geom_pointrange(mapping = aes(ymin = `avg - dev`, y = avg, ymax = `avg + dev`), shape = 21, fill = "white", fatten = 2, position = position_dodge(width = 0.1), stat = "identity") +
  lemon::facet_rep_grid(parameter~., scales = "free_y", switch = "y") +
  scale_x_continuous(breaks = seq(1, length(unique(data_summarized$starttime_of_day)) - 1, 3),
                     labels = unique(data_summarized$starttime_of_day)[seq(1, length(unique(data_summarized$starttime_of_day)) - 1, 3)],
                     expand = c(0.025,0.025)) +
  theme_classic() +
  theme(
    strip.text.x = element_text(hjust = 0),
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.grid.major.y = element_line(linetype = 2, size = 0.25, colour = "gray80"),
    axis.title.x = element_blank()
  )

