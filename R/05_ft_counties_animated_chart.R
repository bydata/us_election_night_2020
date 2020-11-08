library(tidyverse)
library(rvest)
library(colorspace)
library(ggtext)
library(lubridate)
library(colorspace)
library(extrafont)

### THEME =====================================================

# fonts
loadfonts()

# custom theme
custom_theme <- function(base_family = "Source Sans Pro", base_size = 20) {
  theme_minimal(base_family = base_family, base_size = base_size) +
  theme(
      plot.background = element_rect(
        fill = "#fffffc",
        color = NA,
        size = 0.5
      ),
      plot.title = element_markdown(
        family = "Source Sans Pro SemiBold",
        size = base_size * 1.6,
        margin = margin(t = 16, b = 10)
      ),
      plot.subtitle = ggtext::element_markdown(
        size = base_size * 1.025,
        margin = margin(b = 16),
        lineheight = 1.3
      ),
      plot.caption = element_markdown(
        hjust = 0,
        margin = margin(t = 10, b = 6),
        color = "grey35",
        size = base_size * 0.8,
        lineheight = 1.2
    ),
      strip.text = element_markdown(),
      text = element_text(color = "grey25"),
      axis.title.x = element_markdown(hjust = 0),
      axis.title.y = element_markdown(hjust = 1),
      axis.text = element_text(family = "Inconsolata"),
      axis.ticks.x = element_blank(),
      legend.position = "top",
      legend.justification = "left",
      plot.margin = margin(l = 12, r = 12, b = 6),
      plot.title.position = "plot",
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", size = 0.1)
  )
}

theme_set(custom_theme())


### DATA ======================================================

# Source: https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ/HEIJCQ&version=6.0
countypres_url <- "https://dataverse.harvard.edu/api/access/datafile/3641280?format=original&gbrecs=true"
countypres <- read_csv(countypres_url)

glimpse(countypres)

countypres_wide <- countypres %>% 
  filter(party %in% c("republican", "democrat"), office == "President") %>% 
  mutate(candidate_share = candidatevotes / totalvotes) %>% 
  pivot_wider(id_cols = c(year, state, state_po, county), names_from = "party", values_from = c("candidate_share", "totalvotes"), values_fn = sum) %>%
  rename(totalvotes = totalvotes_republican) %>% 
  select(-totalvotes_democrat) %>% 
  mutate(margin_rep = candidate_share_republican - candidate_share_democrat,
         margin_abs = abs(margin_rep)) %>% 
  filter(!is.na(margin_rep)) %>% 
  nest(data = -year) %>% 
  mutate(data = map(data, 
                    ~mutate(.x,
                            totalvotes_share = totalvotes / sum(totalvotes)))) %>% 
  unnest(cols = data)


### PLOTS ========================================================

color_palette <- c(
  "#1056b6", "#3c84cc", "#67b4e2",
  "#ead8ca", 
  "#ffada2", "#f67a78", "#ec4652"
)

countypres_wide %>% 
  filter(year == "2012") %>% 
  ggplot(aes(margin_rep)) +
  geom_histogram(aes(fill = case_when(
    margin_rep < -0.2 ~ "dem",
    margin_rep > 0.2 ~ "rep",
    TRUE ~ "---"
  )),
                 binwidth = 0.1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(-0.8, 0.8, 0.2)) +
  coord_cartesian(xlim = c(-1, 1))


library(gganimate)


# Chart: https://twitter.com/ftdata/status/1325060590461710336/photo/1
# 1.18 cm represent 5 %-points
percentages_to_cm <- 0.05 / 1.18
chart_ft_2020 <- tribble(
  ~margin_rep_grp, ~bar_height,
  -90, 0.08,
  -80, 0.02,
  -70, 0.58,
  -60, 0.41,
  -50, 0.77,
  -40, 1.09,
  -30, 2.01,
  -20, 3.12,
  -10, 2.45,
  10, 1.92,
  20, 2.6,
  30, 2.33,
  40, 1.94,
  50, 1.89,
  60, 1.33,
  70, 0.86,
  80, 0.22,
  90, 0.02
) %>% 
  mutate(year = 2020,
         # margin_rep_grp = factor(margin_rep_grp),
         n = NA,
         totalvotes_share = bar_height * percentages_to_cm,
         #bar_color = NA
         ) %>% 
  select(year, everything(), -bar_height)



df <- countypres_wide %>%
  mutate(margin_rep_grp = ifelse(
    margin_rep < 0,
    floor(10 * margin_rep) * 10,
    ceiling(10 * margin_rep) * 10
  )) %>%
  group_by(year, margin_rep_grp) %>%
  summarize(
    n = n(),
    totalvotes_share = sum(totalvotes_share),
    # bar_color = first(bar_color),
    .groups = "drop"
  ) %>%
  bind_rows(chart_ft_2020) %>%
  mutate(
    bar_color = case_when(
      margin_rep_grp < -60 ~ color_palette[1],
      margin_rep_grp < -40 ~ color_palette[2],
      margin_rep_grp < -20 ~ color_palette[3],
      margin_rep_grp > 60 ~ color_palette[7],
      margin_rep_grp > 40 ~ color_palette[6],
      margin_rep_grp > 20 ~ color_palette[5],
      TRUE ~ color_palette[4]
    ),
    margin_rep_grp = factor(margin_rep_grp)
  ) %>%
  mutate(
    race = case_when(
      year == 2000 ~ "Bush (R) vs. Gore (D)",
      year == 2004 ~ "Bush (R) vs. Kerry (D)",
      year == 2008 ~ "Obama (D) vs. McCain (R)",
      year == 2012 ~ "Obama (D) vs. Romney (R)",
      year == 2016 ~ "Trump (R) vs. Clinton (D)",
      year == 2020 ~ "Biden (D) vs. Trump (R)"
    ),
    year_race_label = glue::glue(
      "<span style='font-size:48pt;color:grey50'>
    {year}<br>
    <span style='font-size:16pt'>{race}</span>"
    )
  )

x_breaks <- seq(-80, 80, 20) %>% as.character()
x_labels <- str_remove(x_breaks, "-") %>% str_c("+", .) 
x_labels[1] <- paste(x_labels[1], "D")
x_labels[length(x_labels)] <- paste(x_labels[length(x_labels)], "R")
color_palette2 <- c(color_palette[1], color_palette, color_palette[length(color_palette)])
x_labels_colored <- glue::glue("<span style='color:{color_palette2}'>{x_labels}</span>")

plot_title <- "The Hollowing out of US Politics"
plot_subtitle <- "How the US has grown more polarised by place,<br>
with fewer evenly contested areas and ever more ultra-partisan counties."
plot_caption <- "Source: @4nsgarW. Data: Harvard Dataverse (2000-2016), FT (2020). Inspiration: FT / John Burn-Murdoch."

p <- df %>%
  ggplot(aes(margin_rep_grp, totalvotes_share)) +
  geom_col(aes(fill = bar_color), alpha = 0.7) +
  geom_richtext(aes(label = year_race_label, x = 13, y = 0.15), 
            stat = "unique",
            family = "Source Sans Pro SemiBold",
            hjust = 0,
            label.color = NA,
            fill = NA,
            col = "grey60"
            ) +
  scale_x_discrete(breaks = x_breaks, labels = x_labels_colored) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     sec.axis =  sec_axis(~ ., labels = scales::percent_format(accuracy = 1))) +
  scale_fill_identity() +
  labs(title = plot_title,
       subtitle = plot_subtitle,
       caption = plot_caption,
       x = "Partisan margin of county",
       y = "Share of all votes cast nationwide") +
  theme(
    plot.caption = element_markdown(size = 12),
    panel.grid.major.y = element_line(color = "grey80", size = 0.1),
    axis.title.y = element_markdown(angle = 90, vjust = 1),
    axis.text.x = element_markdown()
  )

p
anim <- p +
  transition_states(year_race_label, transition_length = 1, state_length = 1, wrap = FALSE) +
  enter_fade() +
  exit_fade()

anim_save(here::here("plots", "ft_hollowing_animated.gif"), 
          anim,
          duration = 16, end_pause = 20, width = 800, height = 600, type = "cairo")


# Optimize gif at https://ezgif.com/optimize

  
