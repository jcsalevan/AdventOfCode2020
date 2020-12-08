# Advent of Code 2020, Day 7
# see https://adventofcode.com/2020/day/7
# JC Salevan, Dec 8 2020

library(tidyverse)
library(readr)

rm(list = ls(pattern = "AoC"))

AoC7 = read_table("AoC7.txt", col_names = F, skip_empty_rows = F) %>%
  mutate(X1 = str_split(X1, pattern = " contain ")) %>%
  unnest_wider(X1, names_sep = "_") %>%
  rename(bag = X1_1,
         contents = X1_2) %>%
  mutate(contents = str_split(str_remove(contents,"[.]"), ", ")) %>%
  unnest_longer(contents) %>%
  mutate(contents = str_split(contents, " ", 2)) %>%
  unnest_wider(contents, names_sep = "_") %>%
  rename(contents_n = contents_1,
         contents_bag = contents_2) %>%
  mutate(across(contains("bag"), ~str_squish(str_remove(.x, "(bags|bag)")))) %>%
  mutate(contents_n = ifelse(contents_n == "no", 0, contents_n),
         contents_bag = ifelse(contents_bag == "other", "none", contents_bag),
         contents_n = as.integer(contents_n)) 

# ok cute, I have a nice data frame. done in, as usual, a way with too many 
# steps, but whatever

# part 1 ----
# How many bag colors can eventually contain at least one shiny gold bag?

# okay this is dumb as shit but it's not a very large data frame, so fuck it
AoC7_1 = AoC7 %>% 
  filter(contents_n > 0) %>%
  mutate(sg_parent0 = contents_bag == "shiny gold") %>%
  mutate(sg_parent1 = contents_bag %in% filter(.,sg_parent0)$bag) %>%
  mutate(sg_parent2 = contents_bag %in% filter(.,sg_parent1)$bag) %>%
  mutate(sg_parent3 = contents_bag %in% filter(.,sg_parent2)$bag) %>%
  mutate(sg_parent4 = contents_bag %in% filter(.,sg_parent3)$bag) %>%
  mutate(sg_parent5 = contents_bag %in% filter(.,sg_parent4)$bag) %>%
  mutate(sg_parent6 = contents_bag %in% filter(.,sg_parent5)$bag) %>%
  mutate(sg_parent7 = contents_bag %in% filter(.,sg_parent6)$bag) %>%
  mutate(sg_parent8 = contents_bag %in% filter(.,sg_parent7)$bag) %>%
  mutate(sg_parent9 = contents_bag %in% filter(.,sg_parent8)$bag) %>%
  mutate(sg_parent10 = contents_bag %in% filter(.,sg_parent9)$bag) %>%
  mutate(sg_parent11 = contents_bag %in% filter(.,sg_parent10)$bag) %>%
  mutate(sg_parent12 = contents_bag %in% filter(.,sg_parent11)$bag) %>%
  mutate(sg_parent = sg_parent0+sg_parent1+sg_parent2+sg_parent3+sg_parent4+
                         sg_parent5+sg_parent6+sg_parent7+sg_parent8+sg_parent9+
                         sg_parent10+sg_parent11+sg_parent12) %>%
  filter(sg_parent > 0) %>%
  select(bag) %>%
  distinct() %>%
  count()
AoC7_1 # 205 bags

# part 2 ----

  
