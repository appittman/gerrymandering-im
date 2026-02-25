# exploring the klarner data

library(haven)
library(tidyverse)
library(here)

contests <- read_dta(here("data", 
              "102slersuoacontest20181024-1.dta")) # retrieved from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DRSACA

competitive_contests <- contests |> 
  select(state = sab,
         year,
         sen,
         dno,
         deter,
         eseats,
         dvote,
         rvote,
         ovote,
         dcand,
         rcand,
         ocand,
         dwin,
         rwin,
         dontuse) |> 
  filter(dontuse == 0,
         deter == 1,
         year == 2016) |> 
  select(-deter, -dontuse) |> 
  filter((dcand + rcand + ocand) > 1)

competitive_contests |> 
  filter(rwin != dwin) |> 
  mutate(winner = case_when(rwin > dwin ~ "rep",
                            dwin > rwin ~ "dem"),
         winner_pct = case_when(winner == "dem" ~ 
                                  dvote / (dvote + rvote + ovote),
                                winner == "rep" ~
                                  rvote / (dvote + rvote + ovote))) |> 
  filter(winner_pct > .75) |> 
  group_by(state) |> 
  summarize(n = n()) |> 
  print(n = Inf)
  
2914 + 517
#517 of 3431

# 15% of contests in 2016 were won using over 75% of the vote