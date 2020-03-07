library(tidyverse)
library(extrafont)

# get data
top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')

season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

# combine datasets

troublemaker <- season_goals %>%
  group_by(player) %>%
  summarise(total_penalties = sum(penalty_min),
            total_goals = sum(goals),
            total_assists = sum(assists),
            total_games = sum(season_games)) %>%
  mutate(gpg = total_goals / total_games,
         pmpg = total_penalties / total_games) %>%
  left_join(top_250 %>% select(player, raw_rank), 
            by = "player") %>%
  mutate(raw_rank = ifelse(is.na(raw_rank), 999, raw_rank)) # to fight NAs

# draw a chart

troublemaker.plot <- ggplot(troublemaker, aes(x = pmpg, y = gpg)) +
  geom_point(alpha = .4, col = ifelse(troublemaker$raw_rank <= 10, "red", "grey")) +
  ggrepel::geom_text_repel(data = subset(troublemaker, raw_rank <= 10 & pmpg >= 0.6), 
                           aes(label = player), 
                           nudge_y = .1, 
                           nudge_x = 2 - subset(troublemaker, raw_rank <= 10 & pmpg >= 0.6)$pmpg, 
                           segment.color = "grey50", 
                           direction = "y") +
  ggrepel::geom_text_repel(data = subset(troublemaker, raw_rank <= 10 & pmpg < 0.6), 
                           aes(label = player), 
                           nudge_y = 1 - subset(troublemaker, raw_rank <= 10 & pmpg < 0.6)$pmpg, 
                           nudge_x = .1,
                           segment.color = "grey50", 
                           direction = "x") +
  geom_line(aes(x = mean(troublemaker$pmpg)), alpha = .5, linetype = 3) +
  geom_text(aes(x = mean(troublemaker$pmpg), y = 0, label = "Median", angle = 90,
                fontface = 'italic', size = 2, family = "Verdana"), 
            nudge_y = .2,
            nudge_x = -.05,
            col = "grey50") +
  geom_line(aes(y = mean(troublemaker$gpg)), alpha = .5, linetype = 3) +
  geom_text(aes(y = mean(troublemaker$gpg), x = 0, label = "Median", angle = 0,
                fontface = 'italic',
                size = 2,
                family = "Verdana"),
            nudge_y = .02,
            nudge_x = .2,
            col = "grey50"
  ) +
  theme_light() +
  labs(x = "Penalty minutes per game", y = "Goals per game") +
  theme(legend.position = "none", text = element_text(family = "Verdana")) # couldn't really make fonts same
troublemaker.plot
