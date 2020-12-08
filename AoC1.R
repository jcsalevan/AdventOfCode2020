# Advent of Code 2020, Day 1
# see https://adventofcode.com/2020/day/1
# JC Salevan, Dec 7 2020

library(tidyverse)
library(readr)

AoC1_input = read_table("AoC1.txt", col_names = F)$X1

# part 1 ----
AoC1 = tibble(
  input = AoC1_input,
  diff_2020 = 2020 - input
)
AoC1_1 = AoC1 %>%
  select(input) %>%
  bind_rows(AoC1 %>% select(input = diff_2020)) %>%
  group_by(input) %>%
  filter(n() > 1) %>%
  distinct() %>%
  mutate(index = ifelse(input > 1010, "a","b")) %>%
  spread(key = index, value = input) %>%
  summarize(answer = a*b)
AoC1_1

#lol that was not very elegant

# part 2 ----
AoC1_2 = tibble(a = AoC1_input) %>%
  mutate(b = list(AoC1_input)) %>%
  unnest(b) %>%
  mutate(c = 2020-a-b) %>%
  filter(c %in% AoC1_input) %>%
  mutate(answer = a*b*c) %>%
  select(answer) %>% distinct()
AoC1_2

#again, kinda brute forcey, but whatever! 