# Advent of Code 2020, Day 5
# see https://adventofcode.com/2020/day/5
# JC Salevan, Dec 8 2020

library(tidyverse)
library(readr)

AoC5 = read_table("AoC5.txt", col_names = F, skip_empty_rows = F) %>%
  rename(seat = X1)

# zero indexing, welp. 0-127 are our seat rows

# part 1 ----

AoC5_1 = AoC5 %>%
  mutate(seatID = strsplit(seat, "")) %>%
  unnest_wider(col = seatID, names_sep = "_") %>%
  mutate(max = 127, min = 0, # rows first
         max = ifelse(seatID_1 == "F", min+63, max),
         min = max-63,
         max = ifelse(seatID_2 == "F", min+31, max),
         min = max-31,
         max = ifelse(seatID_3 == "F", min+15, max),
         min = max-15,
         max = ifelse(seatID_4 == "F", min+7, max),
         min = max-7,
         max = ifelse(seatID_5 == "F", min+3, max),
         min = max-3,
         max = ifelse(seatID_6 == "F", min+1, max),
         min = max-1,
         row = ifelse(seatID_7 == "F", min, max),
         
         max = 7, min = 0, # repeat for seats
         max = ifelse(seatID_8 == "L", min+3, max),
         min = max-3,
         max = ifelse(seatID_9 == "L", min+1, max),
         min = max-1,
         col = ifelse(seatID_10 == "L", min, max),
         
         seatID = row*8 + col
         )

max(AoC5_1$seatID)

# part 2 ----

AoC5_2 = AoC5_1 %>%
  select(seat, row, col, seatID) %>%
  arrange(seatID) %>%
  mutate(seatDiff = seatID - lag(seatID))# %>%
  filter(seatDiff > 1)
AoC5_2$seatID - 1
