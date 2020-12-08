# Advent of Code 2020, Day 2
# see https://adventofcode.com/2020/day/2
# JC Salevan, Dec 7 2020

library(tidyverse)
library(readr)

AoC2 = read_table("AoC2.txt", col_names = F) %>%
  rename(input = X1)

# part 1 ----
AoC2_1 = AoC2 %>%
  mutate(input = str_split(input, ": ")) %>%
  unnest_wider(input, names_sep = "_") %>%
  mutate(input_1 = str_split(input_1, " ")) %>%
  unnest_wider(input_1, names_sep = "_") %>%
  mutate(input_1_1 = str_split(input_1_1, "-")) %>%
  unnest_wider(input_1_1, names_sep = "_") %>%
  rename(lo_bound = input_1_1_1,
         hi_bound = input_1_1_2,
         rule = input_1_2,
         pass = input_2) %>%
  mutate(lo_bound = as.numeric(lo_bound),
         hi_bound = as.numeric(hi_bound),
    matches = str_count(pass, rule),
         fits = matches >= lo_bound & matches <= hi_bound) %>%
  summarize(answer = sum(fits))
  
AoC2_1

# part 2 ----

AoC2_2 = AoC2 %>%
  mutate(input = str_split(input, ": ")) %>%
  unnest_wider(input, names_sep = "_") %>%
  mutate(input_1 = str_split(input_1, " ")) %>%
  unnest_wider(input_1, names_sep = "_") %>%
  mutate(input_1_1 = str_split(input_1_1, "-")) %>%
  unnest_wider(input_1_1, names_sep = "_") %>%
  rename(index1 = input_1_1_1,
         index2 = input_1_1_2,
         rule = input_1_2,
         pass = input_2) %>%
  filter(str_detect(pass, rule)) %>%
  mutate(index1 = as.numeric(index1),
         index2 = as.numeric(index2),
         str_pass = strsplit(pass,"")) %>%
  rowwise() %>%
  mutate(match1 = unlist(str_pass)[index1] == rule,
         match2 = unlist(str_pass)[index2] == rule,
         fits = sum(match1, match2) == 1) %>%
  ungroup() %>%
  summarize(answer = sum(fits))
AoC2_2

         