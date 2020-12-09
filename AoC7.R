# Advent of Code 2020, Day 7
# see https://adventofcode.com/2020/day/7
# JC Salevan, Dec 8 2020

library(tidyverse)
library(readr)
library(network)

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
# How many individual bags are required inside your single shiny gold bag?
# hey fuck it, I'll just do the opposite now lol

AoC7_2 = AoC7 %>%
  mutate(sg0 = bag == "shiny gold") %>%
  mutate(sg1 = bag %in% filter(.,sg0)$contents_bag) %>%
  mutate(sg2 = bag %in% filter(.,sg1)$contents_bag) %>%
  mutate(sg3 = bag %in% filter(.,sg2)$contents_bag) %>%
  mutate(sg4 = bag %in% filter(.,sg3)$contents_bag) %>%
  mutate(sg5 = bag %in% filter(.,sg4)$contents_bag) %>%
  mutate(sg6 = bag %in% filter(.,sg5)$contents_bag) %>%
  mutate(sg7 = bag %in% filter(.,sg6)$contents_bag) %>%
  mutate(sg8 = bag %in% filter(.,sg7)$contents_bag) %>%
  mutate(sg9 = bag %in% filter(.,sg8)$contents_bag) %>%
  # mutate(sg10 = bag %in% filter(.,sg9)$contents_bag) #9 is the final level!
  filter(sg0+sg1+sg2+sg3+sg4+sg5+sg6+sg7+sg8+sg9 > 0) %>%
  mutate(contents_n = ifelse(contents_n == 0, NA_integer_, contents_n))

# ok so there's 9 levels below shiny gold

AoC7_2a = AoC7_2 %>%
  group_by(bag) %>%
  summarize(n_tot = sum(contents_n, na.rm = T))

sg0 = AoC7_2 %>% filter(sg0) %>% select(bag, contents_n, contents_bag)
sg1 = AoC7_2 %>% filter(sg1) %>% select(bag, contents_n, contents_bag)
sg2 = AoC7_2 %>% filter(sg2) %>% select(bag, contents_n, contents_bag)
sg3 = AoC7_2 %>% filter(sg3) %>% select(bag, contents_n, contents_bag)
sg4 = AoC7_2 %>% filter(sg4) %>% select(bag, contents_n, contents_bag)
sg5 = AoC7_2 %>% filter(sg5) %>% select(bag, contents_n, contents_bag)
sg6 = AoC7_2 %>% filter(sg6) %>% select(bag, contents_n, contents_bag)
sg7 = AoC7_2 %>% filter(sg7) %>% select(bag, contents_n, contents_bag)
sg8 = AoC7_2 %>% filter(sg8) %>% select(bag, contents_n, contents_bag)
sg9 = AoC7_2 %>% filter(sg9) %>% select(bag, contents_n, contents_bag)

AoC7_2_tree = tibble(l0 = "shiny gold") %>%
  left_join(sg0, by = c("l0" = "bag")) %>% rename(l1 = contents_bag,
                                              l1n = contents_n) %>%
  left_join(sg1, by = c("l1" = "bag")) %>% rename(l2 = contents_bag,
                                              l2n = contents_n) %>%
  left_join(sg2, by = c("l2" = "bag")) %>% rename(l3 = contents_bag,
                                              l3n = contents_n) %>%
  left_join(sg3, by = c("l3" = "bag")) %>% rename(l4 = contents_bag,
                                              l4n = contents_n) %>%
  left_join(sg4, by = c("l4" = "bag")) %>% rename(l5 = contents_bag,
                                              l5n = contents_n) %>%
  left_join(sg5, by = c("l5" = "bag")) %>% rename(l6 = contents_bag,
                                              l6n = contents_n) %>%
  left_join(sg6, by = c("l6" = "bag")) %>% rename(l7 = contents_bag,
                                              l7n = contents_n) %>%
  left_join(sg7, by = c("l7" = "bag")) %>% rename(l8 = contents_bag,
                                              l8n = contents_n) %>%
  left_join(sg8, by = c("l8" = "bag")) %>% rename(l9 = contents_bag,
                                              l9n = contents_n) %>%
  mutate(tot_branch = case_when(
    !is.na(l9n) ~ l1n * l2n * l3n * l4n * l5n * l6n * l7n * l8n * l9n,
    !is.na(l8n) ~ l1n * l2n * l3n * l4n * l5n * l6n * l7n * l8n,
    !is.na(l7n) ~ l1n * l2n * l3n * l4n * l5n * l6n * l7n,
    !is.na(l6n) ~ l1n * l2n * l3n * l4n * l5n * l6n,
    !is.na(l5n) ~ l1n * l2n * l3n * l4n * l5n,
    !is.na(l4n) ~ l1n * l2n * l3n * l4n,
    !is.na(l3n) ~ l1n * l2n * l3n,
    !is.na(l2n) ~ l1n * l2n,
    !is.na(l1n) ~ l1n
  ))
  

answer1 = AoC7_2_tree %>% summarize(n_tot = sum(tot_branch)) # too low, 65822

answer2 = AoC7_2_tree %>%
  ungroup() %>%
  summarize(nbags = sum(l1n, na.rm = T) + sum(l2n, na.rm = T) + 
              sum(l3n, na.rm = T) + sum(l4n, na.rm = T) + 
              sum(l5n, na.rm = T) + sum(l6n, na.rm = T) + 
              sum(l7n, na.rm = T) + sum(l8n, na.rm = T) + 
              sum(l9n, na.rm = T))

answer = answer1 + answer2

answer


