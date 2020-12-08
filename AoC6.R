# Advent of Code 2020, Day 6
# see https://adventofcode.com/2020/day/6
# JC Salevan, Dec 8 2020

library(tidyverse)
library(readr)

AoV6 = read_table("AoV6.txt", col_names = F, skip_empty_rows = F) %>%
  mutate(X1 = case_when(
    is.na(X1) ~ "---",
    T ~ X1
  ),
  grp_id = 0
  )

index = 1
for (nn in 1:nrow(AoV6)) {
  if(AoV6$X1[nn] == "---"){
    index = index + 1
  } else AoV6$grp_id[nn] = index
}

# part 1 ----

AoV6_1 = AoV6 %>% 
  filter(grp_id != 0) %>%
  mutate(X1 = strsplit(X1, "")) %>%
  unnest_longer(X1) %>%
  distinct()
nrow(AoV6_1)

# part 2 ----

AoV6_2 = AoV6 %>% 
  filter(grp_id != 0) %>%
  group_by(grp_id) %>% mutate(n_people = n()) %>% ungroup() %>%
  mutate(X1 = strsplit(X1, "")) %>%
  unnest_longer(X1) %>%
  group_by(grp_id, X1, n_people) %>%
  summarize(n_answers = n()) %>%
  filter(n_answers == n_people)
nrow(AoV6_2)
  