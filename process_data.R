library(tidyverse)

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

top_songs <- billboard_long |>
  group_by(artist, track) |>
  summarise(
    weeks_on_chart = n(),
    best_rank = min(rank),
    .groups = "drop"
  ) |>
  filter(best_rank <= 10) |>
  slice_max(weeks_on_chart, n = 6, with_ties = FALSE)

write_rds(top_songs, file = "clean_data.rds")
