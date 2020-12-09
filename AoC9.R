# Advent of Code 2020, Day 9
# see https://adventofcode.com/2020/day/9
# JC Salevan, Dec 8 2020

rm(list = ls(pattern = "AoC"))

library(tidyverse)
library(RcppRoll)

AoC9 = read_table("AoC9.txt", col_names = F, skip_empty_rows = F) %>%
  rename(x = X1)

# part 1 ----
for (ii in 26:nrow(AoC9)) {
  pre = AoC9[(ii-25):(ii-1),]$x
  n = AoC9[ii,]$x
  diff = n - pre
  diff = diff[diff %in% pre]
  diff_tib = tibble(
    n = n,
    d1 = diff,
    d2 = n - d1,
    check = d1+d2,
    valid = (check == n) & (d1 != d2))
  if(sum(diff_tib$valid) == 0) stop(paste0("the first one is ",n," at position ",ii))
  rm(diff_tib,pre,n,diff)
}
rm(diff_tib,pre,n,diff,ii)

# part 2 ----

inval = 1492208709 # from part 1

window = 2:300

AoC9_2 = AoC9

for (ww in window) {
  AoC9_2 = AoC9 %>%
    mutate(index = 1:nrow(AoC9),
      inval = inval,
      window = ww,
      rsum = roll_suml(x, ww),
      test = rsum == inval) %>%
    na.omit()
  if(sum(AoC9_2$test) > 0) {
    answer1 = AoC9_2 %>% filter(test)
    stop("look at answer1")
  }
}

#check
sum(AoC9[(answer1$index):(answer1$index + ww-1),]$x) == inval

answer = min(AoC9[(answer1$index):(answer1$index + ww-1),]$x) + 
  max(AoC9[(answer1$index):(answer1$index + ww-1),]$x)
answer
