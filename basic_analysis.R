library(ffsimulator)
library(tidyverse)
library(arrow)
library(ggridges)
library(tantastic)

season_summaries <- arrow::read_parquet("sim_season_summaries.parquet")

x <- season_summaries |>
  group_by(league_id,season) |>
  mutate(rank = rank(-h2h_winpct,ties.method = "random")) |>
  group_by(league_id,franchise_id) |>
  summarise(n = n(),
            first = sum(rank == 1)/n(),
            top_two = sum(rank >= 2)/n(),
            top_four = sum(rank >= 4)/n(),
            top_six = sum(rank >= 6)/n(),
            median_winpct = median(h2h_winpct)) |>
  group_by(league_id) |>
  mutate(
    overall_rank = rank(-median_winpct, ties.method = "random")
  ) |>
  ungroup() |>
  arrange(-overall_rank)

x |>
  filter(overall_rank <= 6) |>
  mutate(overall_rank = as.factor(overall_rank) |> fct_rev()) |>
  ggplot(aes(x = first, y = overall_rank, fill = overall_rank)) +
  geom_density_ridges(color = "white",quantile_lines = TRUE) +
  scale_x_continuous(breaks = seq(0.1,1,by = 0.1),labels = scales::percent_format()) +
  theme_tantastic() +
  theme(legend.position = "none") +
  labs(title = "Percent Odds of Finishing In First",
       subtitle = "250 season simulations for each of 287 SafeLeagues",
       caption = "@_TanHo | ffsimulator R package")

