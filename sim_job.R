library(ffsimulator)
library(ffscrapr)
library(furrr)
library(tidyverse)
setwd(here::here())
options(ffscrapr.cache = "filesystem")
options(nflreadr.cache = "filesystem")
plan(multisession)

x <- mfl_getendpoint(mfl_connect(2022),"leagueSearch",SEARCH = "SafeLeague") |>
  purrr::pluck("content","leagues","league") |>
  data.table::rbindlist() |>
  mutate(conn = map(id, ~mfl_connect(2022, .x, rate_limit = FALSE)))

sim_league <- function(conn,p){
  ffscrapr:::.fn_set_useragent(paste0("dynastyprocess/",runif(1)))
  sim <- ff_simulate(conn,n_seasons = 250,seed = 613,verbose = FALSE)
  saveRDS(sim$summary_season,glue::glue("sims/{conn$league_id}.rds"))
  p()
  sim$summary_season
}

sim_leagues <- function(conns){
  progressr::with_progress({
    p <- progressr::progressor(steps = length(conns))
    sims <- furrr::future_map(conns,sim_league,p)
  })
  return(sims)
}

sim_leagues(x$conn)
# progressr::with_progress({
  # p <- progressr::progressor(steps = nrow(x))
# })

