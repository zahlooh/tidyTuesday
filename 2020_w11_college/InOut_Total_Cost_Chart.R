library(tidyverse)

colors <- c("#F8F2D2", "#819975", "#DE6D43", "#424864")

theme_set(theme_light(base_size = 10))
theme_update(panel.background = element_rect(fill=colors[4]),
             plot.background = element_rect(fill=colors[4]),
             legend.background = element_rect(fill=colors[4]),
             panel.grid = element_line(color = colors[1]),
             text = element_text(color = colors[1]),
             axis.text = element_text(color = colors[1]),
             rect = element_rect(fill = colors[3], color = colors[3]))

if (!exists("tuition_cost")) {
  tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')
}

colors <- c("#F8F2D2", "#819975", "#DE6D43", "#424864")

tuition_cost %>%
  filter(!is.na(state)) %>%
  group_by(state) %>%
  summarise(outof = mean(out_of_state_tuition),
            inst = mean(in_state_tuition)) %>%
  ungroup() %>%
  mutate(state = fct_reorder(state, outof / inst)) %>%
  pivot_longer(cols = c(outof, inst), 
               names_to = "inout",
               values_to = "cost") %>%
  mutate(inout = ifelse(substr(as.character(inout), 1, 3) == "out", "out", "in"),
         inout = factor(inout, levels = c("out", "in"))) %>%
  ggplot(aes(x = state, y = cost, fill = inout)) +
    geom_col(position = "fill", width = .8) +
    scale_fill_manual(values = c("in" = colors[2], "out" = colors[3])) +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    labs(title = "In- vs Out-of- state tuition cost", caption = "Source of data: tuitiontracker.org") +
    theme(axis.title = element_blank(),
          legend.title = element_blank())
