library(tidyverse)
library(rvest)
library(colorspace)
library(ggtext)
library(myggthemes)
library(lubridate)
library(colorspace)

theme_set(theme_clean_lightbg())


## Download data =========================

fte_url <- "https://projects.fivethirtyeight.com/polls-page/president_polls.csv"
polls <- read_csv(fte_url, guess_max = 10000)

glimpse(polls)


# reshape data and keep only state polls
polls_prep <- polls %>% 
  filter(!is.na(state)) %>%
  mutate(across(c("start_date", "end_date", "election_date"), 
                ~parse_date(.x, format = "%m/%d/%y"))) %>% 
  filter(election_date == as_date("2020-11-03"),
         candidate_name %in% c("Joseph R. Biden Jr.", "Donald Trump")) %>% 
  mutate(candidate_name = ifelse(candidate_name == "Donald Trump", "Trump", "Biden"),
         time_to_election = difftime(election_date, start_date)) %>% 
  select(question_id, poll_id, state, display_name,
         start_date, end_date, time_to_election, candidate_name, pct) %>%
  pivot_wider(names_from = "candidate_name", values_from = "pct") %>% 
  mutate(trump_margin = Trump - Biden,
         biden_margin = -trump_margin) 



## Election results =========================================

election_results <- tribble(
  ~state, ~biden_vote, ~trump_vote,
  "Alabama", 36.85, 62.04,
  "Alaska", 33.05, 62.91,
  "Arizona", 49.72, 48.80,
  "Arkansas", 34.70, 62.46,
  "California", 65.23, 32.93,
  "Colorado", 55.77, 41.77,
  "Connecticut", 57.71, 40.79,
  "District of Columbia", 93.56, 5.01,
  "Delaware", 58.80, 39.78,
  "Florida", 47.82, 51.20,
  "Georgia", 49.45, 49.31,
  "Hawaii", 65.00, 33.08,
  "Idaho", 33.09, 63.88,
  "Illinois", 54.66, 43.47,
  "Indiana", 38.13, 59.86,
  "Iowa", 44.99, 53.23,
  "Kansas", 41.06, 56.76,
  "Kentucky", 35.70, 62.56,
  "Louisiana", 39.85, 58.47,
  "Maine", 53.42, 43.73,
  "Maryland", 63.21, 35.13,
  "Massachusetts", 65.76, 32.35,
  "Michigan", 47.65, 45.16,
  "Minnesota", 52.48, 45.37,
  "Mississippi", 39.68, 58.92,
  "Missouri", 41.31, 56.90,
  "Montana", 41.16, 56.40,
  "Nebraska", 39.30, 58.59,
  "Nevada", 49.83, 48.04,
  "New Hampshire", 52.63, 45.75,
  "New Jersey", 61.02, 37.88,
  "New Mexico", 53.96, 43.84,
  "New York", 55.73, 42.93,
  "North Carolina", 48.69, 50.09,
  "North Dakota", 31.93, 65.42,
  "Ohio", 45.19, 53.36,
  "Oklahoma", 32.29, 65.37,
  "Oregon", 57.53, 40.14,
  "Pennsylvania", 49.64, 49.21,
  "Rhode Island", 59.19, 39.34,
  "South Carolina", 43.10, 55.40, 
  "South Dakota", 33.20, 64.11,
  "Tennessee", 37.41, 60.73,
  "Texas", 46.39, 52.21,
  "Utah", 38.31, 58.19,
  "Vermont", 66.47, 30.81,
  "Virginia", 53.88, 44.67,
  "Washington", 60.87, 36.74,
  "West Virginia", 29.85, 68.49,
  "Wisconsin", 49.57, 48.94,
  "Wyoming", 26.56, 70.51
)

candidate_colors <- c("trump" = "#FB464C", "biden" = "#4575A1")
y_labels <- c("<span style='color:#FB464C'>\u2190 Trump win</span>", "<b style='color:#4575A1'>Biden win \u2192</b>")
plot_subtitle <- "Coloured dots indicate the election margins.<br>Grey scattered dots show margins from the polls."
plot_caption <- "Source: @4nsgarW. Data: Polls (FiveThirtyEight.com), election results (decisiondeskhq.com).<br>
State polls from the last 4 weeks running up to Election Day."

polls_prep %>% 
  # keep only polls which field time started within 4 weeks before election day
  filter(time_to_election <= duration(4, "weeks")) %>% 
  left_join(election_results, by = "state") %>% 
  mutate(biden_margin_election = biden_vote - trump_vote) %>% 
  filter(!str_detect(state, "CD-")) %>% 
  ggplot(aes(reorder(state, biden_margin_election))) +
  geom_jitter(aes(y = biden_margin), alpha = 0.4, width = 0.15, size = 0.1) +
  geom_point(aes(y = biden_margin_election, 
                 fill = ifelse(biden_margin_election > 0, "biden", "trump")), 
             shape = 21, alpha = 0.7, size = 3, col = "white", show.legend = FALSE) +
  geom_hline(yintercept = 0, col = "grey50", size = 0.15) +
  annotate("segment", x = 22.5, xend = 22.5, y = -8, yend = 8,
           size = 0.1, col = "grey5",
           arrow = arrow(angle = 90, ends = "both", length = unit(1, "mm"))) +
  scale_y_continuous(breaks = c(-25, 25), labels = y_labels) +
  scale_fill_manual(values = candidate_colors) +
  coord_flip() + 
  labs(title = "How did the Polls Fare in the<br>U.S. Presidential Election 2020?",
       subtitle = plot_subtitle,
       caption = plot_caption,
       x = NULL, 
       y = NULL) +
  theme(plot.title = element_markdown(family = "Source Sans Pro SemiBold"),
        axis.text.x = element_markdown(family = "Source Sans Pro SemiBold", size = 11),
        axis.text.y = element_markdown(family = "Source Sans Pro"),
        axis.title = element_markdown(),
        panel.grid.major.y = element_line(size = 0.1, color = "grey80"))

ggsave(here::here("plots", "polls_vs_election.png"),
       type = "cairo", dpi = 320, width = 5, height = 8, scale = 1.25)



battleground_states <- c("Florida", "Arizona", "Pennsylvania", "Georgia", "Nevada", 
                         "North Carolina", "Michigan", "Wisconsin", "Texas")

polls_prep %>% 
  filter(state %in% battleground_states) %>% 
  filter(time_to_election < duration(8, "weeks")) %>% 
  arrange(time_to_election) %>% 
  mutate(time_to_election_grp = case_when(
    time_to_election <= duration(1, "week") ~ "1 week",
    time_to_election <= duration(2, "week") ~ "2 weeks",
    time_to_election <= duration(4, "week") ~ "2-4 weeks",
    time_to_election <= duration(8, "week") ~ "4-8 weeks",
  )) %>% 
  left_join(election_results, by = "state") %>% 
  mutate(biden_margin_election = biden_vote - trump_vote) %>% 
  ggplot(aes(reorder(state, biden_margin_election), biden_margin)) +
  stat_summary() +
  geom_jitter(alpha = 0.3, width = 0.15, size = 0.5) +
  geom_point(aes(y = biden_margin_election, 
                 fill = ifelse(biden_margin_election > 0, "biden", "trump")), 
             shape = 21, alpha = 0.7, size = 3, col = "white", show.legend = FALSE) +
  geom_hline(yintercept = 0, col = "grey50", size = 0.15) +
  coord_flip() +
  scale_fill_manual(values = candidate_colors) +
  facet_wrap(vars(time_to_election_grp), dir = "v") +
  labs(title = "How did the Polls Fare in Battleground States<br>in the U.S. Presidential Election 2020?",
       subtitle = plot_subtitle,
       caption = plot_caption,
       x = NULL, 
       y = NULL) +
  theme(plot.title = element_markdown(family = "Source Sans Pro SemiBold"),
        axis.text.x = element_markdown(family = "Source Sans Pro SemiBold", size = 11),
        axis.text.y = element_markdown(family = "Source Sans Pro"),
        axis.title = element_markdown(),
        panel.grid.major.y = element_line(size = 0.1, color = "grey80"))
  

ggsave(here::here("plots", "polls_vs_election_battleground_time.png"),
       type = "cairo", dpi = 320, width = 5, height = 8, scale = 1.25)



plot_subtitle <- "The <b>coloured lines</b> indicate the election margins in each state.<br>
Grey <b>scattered dots</b> show margins from the polls.<br>
The <b>dashed grey line</b> indicates the trends of poll margins.<br>
The big <b>coloured dot</b> indicates the poll average (median)."
plot_caption <- "Source: @4nsgarW. Data: Polls (FiveThirtyEight.com), election results (decisiondeskhq.com).<br>
State polls from the last 8 weeks running up to Election Day."


polls_prep %>% 
  filter(state %in% battleground_states) %>% 
  filter(time_to_election < duration(8, "weeks")) %>% 
  arrange(time_to_election) %>% 
  mutate(time_to_election_grp = case_when(
    time_to_election <= duration(1, "week") ~ "1 week",
    time_to_election <= duration(2, "week") ~ "2 weeks",
    time_to_election <= duration(4, "week") ~ "2-4 weeks",
    time_to_election <= duration(8, "week") ~ "4-8 weeks",
  ),
  time_to_election_grp = fct_inorder(time_to_election_grp)) %>% 
  left_join(election_results, by = "state") %>% 
  mutate(biden_margin_election = biden_vote - trump_vote) %>% 
  ggplot(aes(time_to_election_grp, biden_margin)) +
  geom_hline(yintercept = 0, col = "grey50", size = 0.15) +
  stat_summary(aes(shape = "Poll average",
                   fill = ifelse(after_stat(y) > 0, "biden", "trump")), 
               fun = median,
               shape = 21, 
               # fill = "grey2", 
               col = "white", 
               alpha = 0.7, size = 0.8) +
  geom_smooth(aes(group = 1),
              method = "lm", se = FALSE, 
              size = 0.3, col = "grey30", lty = "dashed") +
  # geom_point(aes(y = biden_margin_election, shape = "Election result",
  #                fill = ifelse(biden_margin_election > 0, "biden", "trump")), 
  #            shape = 21, alpha = 0.7, size = 3, col = "white") +
  geom_hline(aes(yintercept = biden_margin_election,
                 col = ifelse(biden_margin_election > 0, "biden", "trump")),
             show.legend = FALSE,
             size = 1, alpha = 0.7) +
  geom_jitter(alpha = 0.3, width = 0.15, size = 0.5, col = "grey10") +
  coord_flip() +
  scale_y_continuous(breaks = seq(-5, 15, 5), position = "right") +
  scale_fill_manual(values = candidate_colors, aesthetics = c("fill", "color")) +
  facet_wrap(vars(state)) +
  guides(fill = FALSE) +
  labs(title = "How did the Polls Fare in Battleground States<br>in the U.S. Presidential Election 2020?",
       subtitle = plot_subtitle,
       caption = plot_caption,
       x = NULL, 
       y = "<b style='color:#FB464C'>\u2190 Trump win</b> | <b style='color:#4575A1'>Biden win \u2192</b>") +
  # theme_minimal() +
  theme(plot.title = element_markdown(family = "Source Sans Pro SemiBold"),
        axis.text.x = element_markdown(family = "Source Sans Pro", size = 9, color = "grey10"),
        # axis.text.x = element_blank(),
        axis.text.y = element_markdown(family = "Source Sans Pro", size = 11),
        axis.title.x.top = element_markdown(family = "Source Sans Pro SemiBold",
                                      margin = margin(b = 15)),
        panel.grid.major.y = element_line(size = 0.1, color = "grey80"),
        panel.spacing = unit(8, "mm"),
        strip.text.x = element_textbox(fill = "grey50", color = "white", size = 10,
                                       family = "Source Sans Pro SemiBold",
                                       valign = 0,
                                     r = unit(4, "pt"), margin = margin(3, 3, 3, 3), 
                                     padding = margin(3, 5, 3, 5))
        )

ggsave(here::here("plots", "polls_vs_election_battleground_time_flip.png"),
       type = "cairo", dpi = 320, width = 7, height = 8, scale = 1.1)

