library(tidyverse)
library(rvest)

# scrape the table from the Wikipedia page
url <- "https://en.wikipedia.org/wiki/United_States_Electoral_College"
page <- read_html(url)
tables <- html_nodes(page, css = "table.wikitable")
table <- tables[[2]]

electors <- html_table(table, fill = TRUE, header = FALSE) %>% 
  slice(-1:-4) %>% 
  select(-X1) %>% 
  mutate_all(na_if, "")

centuries <- c(rep("17", 3), rep("18", 18), rep("19", 11), rep("20", 2))
two_digit_years <- (html_nodes(table, css = "th") %>% 
                      html_text())[6:39] %>% 
  str_remove("\\n") %>% 
  str_remove("^'")
years <- str_c(centuries, two_digit_years)

colnames(electors) <- c("state", years)

electors <- electors %>% 
  pivot_longer(cols = -state, 
               names_to = "election", 
               values_to = "electors") %>% 
  separate_rows(election, sep = "'") %>% 
  fill(election, .direction = "down") %>% 
  group_by(state) %>% 
  mutate(year = seq(1788, 2020, 4),
         electors = as.numeric(electors)) %>% 
  ungroup() %>% 
  select(state, year, electors) %>% 
  # remove totals row
  filter(state != "Total")

write_rds(electors, here::here("data/electors.rds"))




