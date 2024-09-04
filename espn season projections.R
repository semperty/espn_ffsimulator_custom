# import libraries
library(ffsimulator)
library(ggplot2)
library(ggridges)
library(dplyr)

# connect to the league
#conn <- espn_connect(season = 2024, league_id = 366290023) # tom home
#conn <- espn_connect(season = 2024, league_id = 1470089) # richies fantasy
conn <- espn_connect(season = 2024, league_id = 757681021) # parlay

# importing the data
scoring_history <- ffscrapr::ff_scoringhistory(conn, seasons = 2018:2023)

latest_rankings <- ffs_latest_rankings(type = "draft")

rosters <- ffs_rosters(conn)
rosters$player_name <- nflreadr::clean_player_names(rosters$player_name, lowercase = TRUE)

ff_ids <- nflreadr::load_ff_playerids()
ff_ids$name <- nflreadr::clean_player_names(ff_ids$name, lowercase = TRUE)
names(ff_ids)[names(ff_ids) == 'name'] <- 'player_name'
names(ff_ids)[names(ff_ids) == 'position'] <- 'pos'
ff_ids$pos <- replace(ff_ids$pos, ff_ids$pos == "PK", "K")

rosters <- merge(x = rosters, y = ff_ids[,c("player_name", "team", "pos", "fantasypros_id")], by = c("player_name", "team", "pos"), all.x = TRUE)

rosters$fantasypros_id <- dplyr::coalesce(rosters$fantasypros_id.x, rosters$fantasypros_id.y)

rosters <- rosters%>%
  dplyr::filter(fantasypros_id.x != 17541 | is.na(fantasypros_id.x))

lineup_constraints <- ffs_starter_positions(conn)

league_info <- ffscrapr::ff_league(conn)


# generating projections
n_seasons = 100
n_weeks = 14

adp_outcomes <- ffs_adp_outcomes(
  scoring_history = scoring_history,
  gp_model = "none", # or "simple"
  pos_filter = c("QB","RB","WR","TE", "K")
)

projected_scores <- ffs_generate_projections(
  adp_outcomes = adp_outcomes,
  latest_rankings = latest_rankings,
  n_seasons = n_seasons,
  weeks = 1:n_weeks, 
  rosters = rosters 
)

# calculating roster scores
roster_scores <- ffs_score_rosters(
  projected_scores = projected_scores,
  rosters = rosters
)

optimal_scores <- ffs_optimise_lineups(
  roster_scores = roster_scores,
  lineup_constraints = lineup_constraints,
  lineup_efficiency_mean = 0.85,
  lineup_efficiency_sd = 0.05,
  best_ball = FALSE, # or TRUE
  pos_filter = c("QB","RB","WR","TE", "K")
)

# building schedules
schedules <- ffs_build_schedules(
  n_seasons = n_seasons,
  n_weeks = n_weeks,
  seed = NULL,
  franchises = ffs_franchises(conn)
)

# aggregating results
summary_week <- ffs_summarise_week(optimal_scores, schedules)
summary_season <- ffs_summarise_season(summary_week)
summary_simulation <- ffs_summarise_simulation(summary_season)

### plot simulated wins
ss <- summary_season
data.table::setDT(ss)
h2h_wins <- NULL
franchise_name <- NULL

ss_levels <- ss[
  , list(franchise_name,h2h_wins = mean(h2h_wins, na.rm = TRUE))
  ,by = "franchise_name"
][
  order(h2h_wins)
]

ss$franchise_name <- factor(ss$franchise_name, levels = ss_levels$franchise_name)

wins_plot <- ggplot2::ggplot(ss,
                ggplot2::aes(
                  x = h2h_wins,
                  y = franchise_name,
                  fill = franchise_name
                )
) + 
  ggridges::geom_density_ridges(
    stat = "binline",
    color = "white",
    binwidth = 1,
    scale = 1.3,
    alpha = 0.8,
    show.legend = FALSE
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq.int(0, max(summary_season$h2h_wins) + 1, by = 2)
  ) +
  ggplot2::xlab("Season Wins") +
  ggplot2::ylab(NULL) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    plot.title.position = "plot"
  ) +
  ggplot2::labs(
    title = glue::glue("Season Win Totals - {n_seasons} Simulated Seasons"),
    subtitle = glue::glue("{league_info$league_name}"),
    caption = glue::glue("ffsimulator R pkg | Based on rankings as of {latest_rankings$scrape_date}")
  )

png(paste(gsub("-","",toString(Sys.Date())), league_info$league_name, "- Season Win Totals.png"),
    
    # set the height of the image
    height = 600,
    
    # set the width of the image
    width = 800,
    
    # set the measurement style to pixels
    units = "px")

# specify which pieces you want to save to the image
print(wins_plot)

# close the image
dev.off()


### plot rank 
ss <- summary_season
data.table::setDT(ss)
h2h_wins <- NULL
franchise_name <- NULL
season_rank <- NULL

ss[
  ,`:=`(season_rank = rank(-h2h_wins, ties.method = "min"))
  , by = "season"
]

ss_levels <- ss[
  , list(franchise_name, season_rank = mean(season_rank, ties.method = "min"))
  , by = "franchise_name"
][
  order(season_rank)
]

ss$franchise_name <- factor(ss$franchise_name, levels = ss_levels$franchise_name)
ss$rank_label <- factor(scales::ordinal(ss$season_rank), scales::ordinal(sort(unique(ss$season_rank))))

rank_plot <- ggplot2::ggplot(
  ss,
  ggplot2::aes(x = franchise_name, color = franchise_name, fill = franchise_name)) +
  ggplot2::geom_bar() +
  ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(position = "none"))+
  ggplot2::facet_wrap(~ .data$rank_label) +
  ggplot2::xlab(NULL) +
  ggplot2::ylab("Number of Seasons") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  ) +
  ggplot2::labs(
    title = glue::glue("Final Season Rank - {n_seasons} Simulated Seasons"),
    subtitle = glue::glue("{league_info$league_name}"),
    caption = glue::glue("ffsimulator R pkg | Based on rankings as of {latest_rankings$scrape_date}"),
    fill = "Franchise Name",
    color = "Franchise Name"
  )


png(paste(gsub("-","",toString(Sys.Date())), league_info$league_name, "- Season Rank.png"),
    
    # set the height of the image
    height = 800,
    
    # set the width of the image
    width = 600,
    
    # set the measurement style to pixels
    units = "px")

# specify which pieces you want to save to the image
print(rank_plot)

# close the image
dev.off()


### plot points 
sw <- summary_week
data.table::setDT(sw)
team_score <- NULL
sw_levels <- sw[
  , list(team_score = stats::median(team_score, na.rm = TRUE))
  ,by = c("franchise_name")
][
  order(team_score)
]
sw$franchise_name <- factor(sw$franchise_name, levels = sw_levels$franchise_name)

points_plot <- ggplot2::ggplot(
  sw,
  ggplot2::aes(
    x = team_score,
    y = franchise_name,
    fill = franchise_name
  )) +
  ggridges::geom_density_ridges(
    color = "white",
    quantile_lines = TRUE,
    scale = 1.3,
    alpha = 0.8,
    show.legend = FALSE
  ) +
  ggplot2::scale_x_continuous(n.breaks = 8) +
  ggplot2::xlab("Weekly Score") +
  ggplot2::ylab(NULL) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    plot.title.position = "plot"
  ) +
  ggplot2::labs(
    title = glue::glue("Weekly Scores - {n_seasons * n_weeks} Simulated Weeks"),
    subtitle = glue::glue("{league_info$league_name}"),
    caption = glue::glue("ffsimulator R pkg | Based on rankings as of {latest_rankings$scrape_date}")
    
    )


png(paste(gsub("-","",toString(Sys.Date())), league_info$league_name, "- Points Plot.png"),
    
    # set the height of the image
    height = 600,
    
    # set the width of the image
    width = 800,
    
    # set the measurement style to pixels
    units = "px")

# specify which pieces you want to save to the image
print(points_plot)

# close the image
dev.off()

