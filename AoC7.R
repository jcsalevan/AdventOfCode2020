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

# ok cute, I have a nice data frame. done in, as usual, a way with mantoo many 
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
  mutate(contents_n = ifelse(contents_n == 0, NA_integer_, contents_n),
         contents_bag = ifelse(contents_bag == "none", NA_character_, contents_bag))

# ok so there's 9 levels below shiny gold

sg0 = AoC7_2 %>% filter(sg0) %>% select(l0 = bag, l1n = contents_n, l1 = contents_bag)
sg1 = AoC7_2 %>% filter(sg1) %>% select(l1 = bag, l2n = contents_n, l2 = contents_bag)
sg2 = AoC7_2 %>% filter(sg2) %>% select(l2 = bag, l3n = contents_n, l3 = contents_bag)
sg3 = AoC7_2 %>% filter(sg3) %>% select(l3 = bag, l4n = contents_n, l4 = contents_bag)
sg4 = AoC7_2 %>% filter(sg4) %>% select(l4 = bag, l5n = contents_n, l5 = contents_bag)
sg5 = AoC7_2 %>% filter(sg5) %>% select(l5 = bag, l6n = contents_n, l6 = contents_bag)
sg6 = AoC7_2 %>% filter(sg6) %>% select(l6 = bag, l7n = contents_n, l7 = contents_bag)
sg7 = AoC7_2 %>% filter(sg7) %>% select(l7 = bag, l8n = contents_n, l8 = contents_bag)
sg8 = AoC7_2 %>% filter(sg8) %>% select(l8 = bag, l9n = contents_n, l9 = contents_bag)

AoC7_2_tree = tibble(l0 = "shiny gold") %>%
  left_join(sg0) %>%
  left_join(sg1) %>%
  left_join(sg2) %>% 
  left_join(sg3) %>%
  left_join(sg4) %>%
  left_join(sg5) %>%
  left_join(sg6) %>%
  left_join(sg7) %>%
  left_join(sg8) 

# sum the products of those s1n, s2n, etc. for rows that are unique column subsets...
answer = 0
for (ii in seq(3,19,2)) {

s1 = AoC7_2_tree[1:ii] %>%
  distinct() %>%
  rowwise() %>%
  mutate(rowprod = prod(c_across(ends_with("n")))) %>%
  ungroup() %>%
  summarize(sum = sum(rowprod, na.rm = T))

answer = answer + s1$sum
}
answer

# ta da! 80902
