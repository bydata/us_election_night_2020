library(tidyverse)
library(rvest)
library(colorspace)
library(ggtext)
library(lubridate)
library(colorspace)
library(myggthemes)

theme_set(theme_clean_lightbg())

file <- "C:/Users/AnsgarWolsing/Desktop/Notes/trump_speech_networks.tsv"

df <- read_tsv(file, n_max = 7,
               col_types = cols(col_character(),
                                col_character(),
                                col_character(),
                                col_logical(),
                                col_logical(),
                                col_character()
               ))

df <- df %>% 
  mutate(start_ts = ms(start),
         interrupt_ts = ms(interrupt),
         time_till_interrupt = interrupt_ts - start_ts,
         time_till_interrupt_sec = as.numeric(time_till_interrupt)) %>% 
  # adjust Fox News time
  mutate(time_till_interrupt_sec = ifelse(network == "Fox News", 
                                          max(time_till_interrupt_sec), 
                                          time_till_interrupt_sec),
         time_till_interrupt_min = time_till_interrupt_sec / 60) 

network_icon_urls <- c(
  "Fox News" = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/67/Fox_News_Channel_logo.svg/150px-Fox_News_Channel_logo.svg.png",
  "CNN" = "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b1/CNN.svg/263px-CNN.svg.png",
  "MSNBC" = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/62/MSNBC_2015_logo.svg/375px-MSNBC_2015_logo.svg.png",
  "NBC" = "https://upload.wikimedia.org/wikipedia/en/thumb/8/82/NBC_logo_%282013%29.svg/330px-NBC_logo_%282013%29.svg.png",
  "ABC" = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/56/ABC_%282013%29_Dark_Grey.svg/225px-ABC_%282013%29_Dark_Grey.svg.png",
  "CBS" = "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ee/CBS_logo_%282020%29.svg/330px-CBS_logo_%282020%29.svg.png",
  "CNBC" = "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e3/CNBC_logo.svg/300px-CNBC_logo.svg.png"
)

network_icon_files <- c(
  "Fox News" = "input/icon_foxnews.png",
  "CNN" = "input/icon_cnn.png",
  "MSNBC" = "input/icon_msnbc.png",
  "NBC" = "input/icon_nbc.png",
  "ABC" = "input/icon_abc.png",
  "CBS" = "input/icon_cbs.png",
  "CNBC" = "input/icon_cnbc.png"
  )

# download.file(network_icon_urls, network_icon_files, mode = "wb", method = "wget")
walk2(network_icon_urls, network_icon_files, download.file, mode = "wb")

colors <- qualitative_hcl(2, palette = "Harmonic")
plot_title <- glue::glue("Networks <span style='color:{colors[1]}'>cut off Trump</span> as he spreads lies and<br>disinformation about the outcome of the election")
plot_subtitle <-  glue::glue("Minutes until the host first interrupted the stream.<br>
Only CNN and Fox News aired the <span style='color:{colors[2]}'>full press conference</span>.")
plot_caption <- "Source: @4nsgarW. Videoplay times based on YouTube uploads from the networks."


df %>% 
  inner_join(tibble(network = names(network_icon_files), icon_file = network_icon_files),
             by = "network") %>% 
  mutate(icon_label = glue::glue("<img src='{icon_file}' width=25 height=10>")) %>% 
  ggplot(aes(reorder(network, -time_till_interrupt_min), time_till_interrupt_min)) +
  geom_col(aes(fill = uninterrupted), alpha = 0.8, width = 0.25, show.legend = FALSE) +
  geom_richtext(aes(label = icon_label, y = -0.75), label.color = "grey80", label.padding = unit(2, "mm")) +
  scale_y_continuous(breaks = seq(0, 18, 3)) +
  scale_fill_discrete_qualitative(palette = "Harmonic") +
  coord_flip(clip = "off") +
  labs(title = plot_title,
       subtitle = plot_subtitle,
       caption = plot_caption,
       x = NULL, y = "Minutes until host interrupted") +
  theme(panel.grid.major.x = element_line(size = 0.1, color = "grey80"),
        plot.background = element_rect(color = NA, fill = "#05070f"),
        plot.title = element_markdown(color = "white", family = "Source Sans Pro SemiBold"),
        plot.subtitle = element_markdown(color = "grey98", family = "Source Sans Pro"),
        plot.caption = element_markdown(color = "grey95", family = "Source Sans Pro"),
        text = element_text(color = "grey98", family = "Source Sans Pro"),
        axis.text.x = element_markdown(color = "grey98", family = "Source Sans Pro"),
        axis.text.y = element_blank())

ggsave(here::here("plots", "trump_speech_interrupted_networks.png"),
       type = "cairo", dpi = 320, width = 5, height = 7)

