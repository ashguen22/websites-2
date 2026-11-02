library(tidyverse)

top_songs <- read_rds("clean_data.rds")

data("billboard", package = "tidyr")

billboard_long <- billboard |>
  pivot_longer(
    wk1:wk76,
    names_to = "week",
    names_prefix = "wk",
    names_transform = list(week = as.integer),
    values_to = "rank"
  ) |>
  filter(!is.na(rank))

billboard_plot <- billboard_long |>
  semi_join(top_songs, by = c("artist", "track")) |>
  mutate(song = paste(track, artist, sep = " — ")) |>
  ggplot(aes(week, rank, color = song, group = song)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  scale_color_viridis_d(option = "turbo", end = 0.9) +
  scale_x_continuous(breaks = seq(1, 76, by = 10)) +
  scale_y_reverse(breaks = c(1, 10, 20, 40, 60, 80, 100)) +
  labs(
    title = "The songs that stayed on the chart",
    subtitle = "Six longest-charting hits from 2000 that reached the top 10",
    x = "Week on the chart",
    y = "Billboard rank",
    color = NULL
  ) +
  guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

ggsave("billboard.png", plot = billboard_plot, width = 10, height = 7)
