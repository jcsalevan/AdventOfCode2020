# Advent of Code 2020, Day 8
# see https://adventofcode.com/2020/day/8
# JC Salevan, Dec 8 2020

library(tidyverse)
library(readr)

rm(list = ls(pattern = "AoC"))

AoC8 = read_table("AoC8.txt", col_names = F, skip_empty_rows = F) %>%
  rename(op = X1,
         inst = X2) 

# part 1 ----
AoC8_1 = AoC8 %>%
  mutate(line = 1:nrow(AoC8)) %>%
  mutate(goto = case_when(
    op %in% c("acc","nop") ~ line + 1,
    op == "jmp" ~ line + inst
  )
  ) 

placekeeper = 1
step = head(AoC8_1,1) %>% mutate(step = 1, acc = inst) 
step = tibble()
step_temp = 0
repeat_count = 0
while(repeat_count == 0){
  what_do = AoC8_1 %>% filter(line == placekeeper)
  
  placekeeper = what_do$goto
  
  step_temp = step_temp + 1
  acc_temp = ifelse(what_do$op == "acc", what_do$inst, 0)
  
  temp = bind_cols(what_do, step = step_temp, acc = acc_temp)
  
  step = bind_rows(step, 
                   temp)
  
  repeat_count = nrow(step %>% group_by(goto) %>% filter(n() > 1))
  
}
last(step$acc)

# 1684!

# part 2 ----

AoC8_flag = AoC8_1 %>% filter(op %in% c("jmp","nop"))
  
placekeeper = 1
step = head(AoC8_1,1) %>% mutate(step = 1, acc = inst) 
step = tibble()
step_temp = 0
repeat_count = 0

for (nn in AoC8_flag$line) {
  temp = AoC8_flag %>% filter(line == nn)
  AoC8_2 = AoC8_1 %>%
    mutate(op = case_when(
      line == temp$line & op == "nop" ~ "jmp",
      line == temp$line & op == "jmp" ~ "nop",
      T ~ op
      )
      ,
      goto = case_when(
      op %in% c("acc","nop") ~ line + 1,
      op == "jmp" ~ line + inst
    )
    ) 
  placekeeper = 1
  step = head(AoC8_2,1) %>% mutate(step = 1, acc = inst) 
  step = tibble()
  step_temp = 0
  repeat_count = 0
  
  while(repeat_count == 0 & nrow(step) <= nrow(AoC8_2)){
    what_do = AoC8_2 %>% filter(line == placekeeper)
  
    placekeeper = what_do$goto
  
    step_temp = step_temp + 1
    acc_temp = ifelse(what_do$op == "acc", what_do$inst, 0)
  
    temp = bind_cols(what_do, step = step_temp, acc = acc_temp)
  
    step = bind_rows(step, 
                    temp)
  
    repeat_count = nrow(step %>% group_by(goto) %>% filter(n() > 1))
  }
  if(repeat_count == 0){print("yo it's this one: ",sum(step$acc))
    stop("hey we found it")}
  print(nn)
  next
}

sum(step$acc)
# lmao it didn't fail how I expected, but it got to the right answer!
# 2188