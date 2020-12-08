# Advent of Code 2020, Day 7
# see https://adventofcode.com/2020/day/7
# JC Salevan, Dec 8 2020

library(tidyverse)
library(readr)

rm(list = ls(pattern = "AoC"))

AoC7 = read_table("AoC7.txt", col_names = F, skip_empty_rows = F)
