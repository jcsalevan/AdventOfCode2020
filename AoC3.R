# Advent of Code 2020, Day 3
# see https://adventofcode.com/2020/day/3
# JC Salevan, Dec 7 2020

library(tidyverse)
library(readr)

Aoc3 = read_table("Aoc3.txt", col_names = F) %>%
  rename(input = X1)

# part 1 ----

Aoc3_1 = Aoc3 %>%
  mutate(row = 1:nrow(Aoc3),
         row_width = str_length(input),
         position = 3*(row-1) + 1 ,
         trans_pos = ifelse(position %% row_width != 0, position %% row_width, row_width),
         element = str_sub(input,trans_pos,trans_pos),
         tree = element == "#") %>%
  summarize(answer = sum(tree))
Aoc3_1

# part 2 ----
# right 1, down 1
Aoc3_2_11 = Aoc3 %>%
  mutate(row = 1:nrow(Aoc3),
         row_width = str_length(input),
         position = 1*(row-1) + 1 ,
         trans_pos = ifelse(position %% row_width != 0, position %% row_width, row_width),
         element = str_sub(input,trans_pos,trans_pos),
         tree = element == "#") %>%
  summarize(answer = sum(tree))

# right 3, down 1
Aoc3_2_31 = Aoc3_1

# Right 5, down 1
Aoc3_2_51 = Aoc3 %>%
  mutate(row = 1:nrow(Aoc3),
         row_width = str_length(input),
         position = 5*(row-1) + 1 ,
         trans_pos = ifelse(position %% row_width != 0, position %% row_width, row_width),
         element = str_sub(input,trans_pos,trans_pos),
         tree = element == "#") %>%
  summarize(answer = sum(tree))

# Right 7, down 1.
Aoc3_2_71 = Aoc3 %>%
  mutate(row = 1:nrow(Aoc3),
         row_width = str_length(input),
         position = 7*(row-1) + 1 ,
         trans_pos = ifelse(position %% row_width != 0, position %% row_width, row_width),
         element = str_sub(input,trans_pos,trans_pos),
         tree = element == "#") %>%
  summarize(answer = sum(tree))

# Right 1, down 2.
Aoc3_2_12 = Aoc3 %>%
  mutate(row = 1:nrow(Aoc3)) %>%
  filter(row %% 2 == 1) %>%
  mutate(row_width = str_length(input),
         position = 1*(row-1)/2 + 1 ,
         trans_pos = ifelse(position %% row_width != 0, position %% row_width, row_width),
         element = str_sub(input,trans_pos,trans_pos),
         tree = element == "#") %>%
  summarize(answer = sum(tree))

Aoc3_2 = Aoc3_2_11 * Aoc3_2_31 * Aoc3_2_51 * Aoc3_2_71 * Aoc3_2_12
Aoc3_2

# lol this is suuuuch a brute force method, but it was quick!