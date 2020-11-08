library(tidyverse)
library(rvest)
library(colorspace)
library(ggtext)
library(myggthemes)

theme_set(theme_clean_lightbg())

# electoral college scraped from Wikipedia
electors <- read_rds(here::here("data", "electors.rds"))

# development of electors

electors %>% 
  filter(!is.na(electors)) %>% 
  group_by(state) %>% 
  filter(year == min(year) | year == max(year)) %>% 
  mutate(when = ifelse(year == min(year), "Earliest", "2020"),
         when = factor(when, levels = c("Earliest", "2020"), labels = c("Earliest", "2020"))) %>% 
  ungroup() %>% 
  mutate(state_highlight = case_when(
    state %in% c("California", "Texas", "Maine", "New York") ~ state,
    TRUE ~ "ZZZ"
  )) %>% 
  arrange(desc(state_highlight)) %>% 
  ggplot(aes(when, electors, group = state)) +
  geom_line(aes(col = state_highlight, size = state_highlight != "ZZZ"), show.legend = FALSE) +
  geom_point(aes(col = state_highlight), size = 2, show.legend = FALSE) +
  geom_text(aes(label = ifelse(state_highlight != "ZZZ" & when == "2020", state_highlight, "")),
            hjust = 0, nudge_x = 0.05, show.legend = FALSE, family = "Open Sans Light", size = 3.5) +
  scale_color_manual(values = c(colorspace::qualitative_hcl(4), "grey90")) +
  scale_size_manual(values = c(0.1, 1)) +
  labs(title = "Go West...<br>and Concrete Jungle which Dreams Are Made of",
       subtitle = "How the number of electors changed per state
       from earliest time citizens of that state<br>
       were eligible to vote in a presidential election to today",
       x = NULL, y = "\\# electors in Electoral College")

ggsave(here::here("plots/development_electors_time.png"), type = "cairo", dpi = 320)



## Load US Census Data on state-level
state_pop_url <- "https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/totals/nst-est2019-01.xlsx"
state_pop_file <- file.path("data", "nst-est2019-01.xlsx")
if (!file.exists(state_pop_file)) {
  download.file(state_pop_url, state_pop_file, mode = "wb")
} else message(glue::glue("Skipping download. File {state_pop_file} already exists."))

state_pop <- readxl::read_xlsx(state_pop_file, sheet = "NST01", skip = 3)
# colnames(state_pop)[1:3] <- c("geographic_area", "2010_census", "2010_estimates_base")
colnames(state_pop)[1] <- c("geographic_area")
colnames(state_pop)

# transform into long format
state_pop <- state_pop %>% 
  pivot_longer(cols = -geographic_area, names_to = "year", values_to = "pop") %>% 
  mutate(geographic_area = str_remove(geographic_area, "^\\.") %>% 
           str_trim(),
         geographic_area = ifelse(geographic_area == "District of Columbia", "D.C.", geographic_area)) %>% 
  filter(!is.na(pop))

state_pop_2019 <- state_pop %>% 
  filter(year == 2019)

electors %>% 
  filter(year == 2020) %>% 
  anti_join(state_pop_2019, by = c("state" = "geographic_area"))




exclude_aggregate_pop <- c("United States", "Northeast", "Midwest", "South", "West")


electors_pop_2020 <- electors %>% 
  filter(year == 2020) %>%
  right_join(state_pop_2019, by = c("state" = "geographic_area"), suffix = c(".ec", ".census")) %>% 
  filter(!state %in% exclude_aggregate_pop) %>% 
  mutate(electors = replace_na(electors, 0),
         year.ec = replace_na(year.ec, "2020"),
         pop_share = pop / sum(pop),
         ec_share = electors / sum(electors)) 


# which states stand out?
states_highlight <- electors_pop_2020 %>% 
  mutate(ratio = ec_share / pop_share) %>% 
  filter(ratio > 1.5 | ratio < 0.86) %>% 
  pull(state)


plot_subtitle <- ""
plot_caption <- "Quelle: @4nsgarW. Daten: census.gov (Bevölkerungszahlen), Wikipedia (Electoral College)."

electors_pop_2020 %>% 
  ggplot(aes(pop_share, ec_share, group = state)) +
  geom_abline(slope = 1, intercept = 0,
              size = 0.5, col = "grey60") +
  geom_point(aes(size = pop, fill = state %in% states_highlight), 
             shape = 21, col = "white", alpha = 0.7
  ) +
  ggrepel::geom_text_repel(data = . %>% filter(state %in% states_highlight),
                           aes(label = state), 
                           size = 2.5, family = "Open Sans Light", 
                           # col = "grey20",
                           col = "grey98",
                           segment.size = 0.1, segment.color = "grey80") +
  # annotation explaining the line
  annotate("text", x = 0.0085, y = 0.08, label = "Staaten auf der Linie stellen Wahlpersonen
im Electoral College entsprechend ihrer
Bevölkerungszahl",
           hjust = 0, col = "grey98", family = "Inconsolata", size = 2.5) +
  geom_curve(data = NULL,
               aes(x = 0.035, xend = 0.042, y = 0.0725, yend = 0.044),
             inherit.aes = FALSE, curvature = -0.2,
             col = "grey98", size = 0.1,
             arrow = arrow(type = "closed", length = unit(1, "mm"))) +
  # annotation to highlight Puerto Rico
  annotate("text", x = 0.0125, y = 0.0025, label = "Ja, sie schließen rund 1 %\nvon der Wahl aus",
           hjust = 0, col = "grey98", family = "Inconsolata", size = 2.5) +
  geom_curve(data = NULL,
             aes(x = 0.012, xend = 0.0095, y = 0.0025, yend = 0.0015),
             inherit.aes = FALSE,
             col = "grey98", size = 0.1,
             arrow = arrow(type = "closed", length = unit(1, "mm"))) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1, decimal.mark = ","), 
                     trans = "log2", 
                     limits = c(0.0015, NA),
                     breaks = c(0.002, 0.005, 0.01, 0.02, 0.04, 0.08, 0.12)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1, decimal.mark = ","), 
                     trans = "log2", 
                     limits = c(0.0015, NA),
                     breaks = c(0.005, 0.01, 0.02, 0.04, 0.08, 0.12)) +
  scale_fill_manual(values = c("FALSE" = "grey85", "TRUE" = "darkred")) +
  guides(fill = FALSE, size = FALSE) +
  labs(title = "Bevölkerungsarme Staaten überrepräsentiert im Electoral College",
       subtitle = plot_subtitle,
       caption = plot_caption,
       x = "Anteil an Gesamtbevölkerung (log)",
       y = "Anteil am Electoral College (log)",
       size = "Einwohner") +
  theme(panel.grid.major = element_line(color = "grey20", size = 0.1),
        plot.background = element_rect(color = NA, fill = "#05070f"),
        plot.title = element_markdown(color = "white"),
        plot.subtitle = element_markdown(color = "grey98"),
        plot.caption = element_markdown(color = "grey95"),
        text = element_text(color = "grey98"),
        axis.text = element_text(color = "grey98"))

ggsave(here::here("plots/state_pop_electors.png"), type = "cairo", dpi = 320,
       width = 6, height = 4, scale = 1.25)



## ========================================
## State winners (2:30 pm CET)

state_winners <- tribble(
  ~state, ~winner,
  "Alabama", "Trump",
  "Alaska", "Trump",
  "Arizona", "Biden-lean",
  "Arkansas", "Trump",
  "California", "Biden",
  "Colorado", "Biden",
  "Connecticut", "Biden",
  "D.C.", "Biden",
  "Delaware", "Biden",
  "Florida", "Trump",
  "Georgia", "Trump-lean",
  "Hawaii", "Biden",
  "Idaho", "Trump",
  "Illinois", "Biden",
  "Indiana", "Trump",
  "Iowa", "Trump",
  "Kansas", "Trump",
  "Kentucky", "Trump",
  "Louisiana", "Trump",
  "Maine", "Biden",
  "Maryland", "Biden",
  "Massachusetts", "Biden",
  "Michigan", "Trump-lean",
  "Minnesota", "Biden",
  "Mississippi", "Trump",
  "Missouri", "Trump",
  "Montana", "Trump",
  "Nebraska", "Trump",
  "Nevada", "Biden-lean",
  "New Hampshire", "Biden",
  "New Jersey", "Biden",
  "New Mexico", "Biden",
  "New York", "Biden",
  "North Carolina", "Trump-lean",
  "North Dakota", "Trump",
  "Ohio", "Trump",
  "Oklahoma", "Trump",
  "Oregon", "Biden",
  "Pennsylvania", "Trump-lean",
  "Rhode Island", "Biden",
  "South Carolina", "Trump",
  "South Dakota", "Trump",
  "Tennessee", "Trump",
  "Texas", "Trump",
  "Utah", "Trump",
  "Vermont", "Biden",
  "Virginia", "Biden",
  "Washington", "Biden",
  "West Virginia", "Trump",
  "Wisconsin", "Biden-lean",
  "Wyoming", "Trump"
) %>% 
  mutate(winner2 = str_remove(winner, "-lean"),
         winner = factor(winner, levels = c("Biden", "Biden-lean", "Trump-lean", "Trump")))



### Dot-plot with ratios

electors_pop_2020 %>% 
  filter(state != "Puerto Rico") %>% 
  inner_join(state_winners, by = "state") %>% 
  mutate(ratio = ec_share / pop_share,
         ratio2 = ifelse(ratio >= 1, ratio, -1/ratio)) %>% 
  ggplot(aes(reorder(state, ratio), ratio)) +
  geom_segment(aes(xend = state, y = 1, yend = ratio, col = winner),
               size = 1.5) +
  geom_point(shape = 21, fill = "white", col = "black", size = 3) +
  geom_hline(yintercept = 1, col = "grey20") +
  coord_flip()





## ========================================
library(treemapify)

electors_2020 <- electors %>% 
  filter(year == 2020) 

electors_2020 %>% 
  ggplot(aes(area = electors, fill = electors, label = state)) +
  geom_treemap() +
  geom_treemap_text(color = "grey98", family = "Source Sans Pro Light", grow = TRUE)

electors_pop_2020 %>% 
  inner_join(state_winners, by = "state") %>% 
  # group_by(winner) %>% 
  # summarize(sum(pop_share))
  ggplot(aes(area = pop_share, fill = winner, label = state, subgroup = winner2)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_text(color = "grey98", family = "Source Sans Pro Light", grow = TRUE)




devtools::install_github("hrbrmstr/waffle")
library(waffle)

electors_pop_2020 %>% 
  inner_join(state_winners, by = "state") %>% 
  count(winner2, winner, wt = pop_share) %>%
  mutate(n = round(n * 100)) %>% 
  ggplot(aes(fill = winner, values = n, group = winner)) +
  geom_waffle(n_rows = 10, size = 0.25, col = "white") +
  # geom_hline(yintercept = 10) +
  scale_x_discrete() +
  scale_fill_manual(values = c("blue", "lightblue", "red", "darkred")) +
  coord_equal() +
  facet_wrap(vars(winner2), nrow = 1, strip.position = "bottom") +
  theme(panel.grid = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))




electors_pop_2020 %>% 
  filter(state != "Puerto Rico") %>% 
  # calculate cumulative sum of electors and population 
  arrange(pop) %>% 
  mutate(cumul_pop = cumsum(pop),
         cumul_pop_share = cumul_pop / sum(pop),
         cumul_ec = cumsum(electors),
         cumul_ec_share = cumul_ec / sum(electors)) %>% View()





## 1900 ==========================================

state_pop_1900_1910_url <- "https://www2.census.gov/programs-surveys/popest/tables/1980-1990/state/asrh/st0009ts.txt"
# state_pop_1900_1910 <- read_file(state_pop_1900_1910_url)
# state_pop_1900_1910 <- state_pop_1900_1910 %>% 
#   str_split(pattern = "\\r\\n") 
# foo <- state_pop_1900_1910[[1]][16:72] %>% 
#   str_replace_all("\\s+", "\t") %>% 
#   # str_split("\\t") %>% 
#   tibble(foo = .) %>% 
#   separate(foo, into = str_c("X", 1:7), sep = "\t")

state_pop_1900_1910 <- read_fwf(state_pop_1900_1910_url, 
                                fwf_cols(state_pop_1900_1910_url, 
                                          col_names = c("state_abbr" = 20, 
                                                        "1900" = 9, 
                                                        "1901" = 9, 
                                                        "1902" = 9, 
                                                        "1903" = 9, 
                                                        "1904" = 9, 
                                                        "1905" = 9)),
                                skip = 15, n_max = 10)

# electors_pop_1900 <- electors %>% 
#   filter(year == 1900) %>%
#   right_join(state_pop_2019, by = c("state" = "geographic_area"), suffix = c(".ec", ".census")) %>% 
#   filter(!state %in% exclude_aggregate_pop) %>% 
#   mutate(electors = replace_na(electors, 0),
#          year.ec = replace_na(year.ec, "2020"),
#          pop_share = pop / sum(pop),
#          ec_share = electors / sum(electors)) 
 
