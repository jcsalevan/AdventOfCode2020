# Advent of Code 2020, Day 4
# see https://adventofcode.com/2020/day/4
# JC Salevan, Dec 7 2020

library(tidyverse)
library(readr)

AoC4 = read_table("AoC4.txt", col_names = F, skip_empty_rows = F) %>%
  mutate(X1 = case_when(
    is.na(X1) ~ "---",
    T ~ X1
  ),
  row_id = 0
  )

index = 1
for (nn in 1:nrow(AoC4)) {
  if(AoC4$X1[nn] == "---"){
    index = index + 1
  } else AoC4$row_id[nn] = index
}

AoC4a = AoC4 %>% 
  filter(row_id != 0) %>%
  mutate(X1 = strsplit(X1, " ")) %>%
  unnest_longer(X1) %>%
  mutate(X1 = strsplit(X1, ":")) %>%
  unnest_wider(X1, names_sep = "_") %>%
  spread(key = X1_1, value = X1_2, fill = NA) 

# part 1 ----

AoC4_1 = AoC4a %>%
  select(-cid) %>%
  na.omit() %>%
  count()
AoC4_1

# part 2 ----
AoC4b = AoC4a %>%
  select(-cid) %>%
  na.omit()
AoC4_2 = AoC4b %>%
  filter(as.integer(byr) >= 1920 & as.integer(byr) <= 2002) %>% # 17 records
  filter(as.integer(iyr) >= 2010 & as.integer(iyr) <= 2020) %>% # 7 records
  filter(as.integer(eyr) >= 2020 & as.integer(eyr) <= 2030) %>% # 4 records
  filter(endsWith(hgt, "cm") | endsWith(hgt, "in")) %>% # 1 record
  mutate(hgt_unit = ifelse(endsWith(hgt, "cm"), "cm", "in"),
         hgt_num = as.integer(str_sub(hgt, 1, -3)),
         hgt_valid = (hgt_unit == "cm" & hgt_num >= 150 & hgt_num <= 193) |
           (hgt_unit == "in" & hgt_num >= 59 & hgt_num <= 76)
         ) %>%
  filter(hgt_valid) %>% # 2 records
  filter(startsWith(hcl, "#")) %>% # 4 records
  mutate(hcl = str_sub(hcl,2,-1)) %>%
  filter(str_length(hcl) == 6 & !is.na(strtoi(hcl,16))) %>% # 0 records
  filter(ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")) %>% # 2 records
  filter(str_length(pid) == 9 & !is.na(as.integer(pid))) %>% # 4 records
  summarize(answer = n())
AoC4_2
