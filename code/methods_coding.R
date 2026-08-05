### methods section stuff
### which states were included, which were excluded, what percentage of black and white populations do the final states represent?

library(tidyverse)
library(here)

load(here("data", "outcome.Rdata"))

dropped_states <- im |> 
  mutate(race = case_match(race,
                           "Non-Hispanic White" ~ "w",
                           "Non-Hispanic Black" ~ "b",
                           .default = NA_character_)) |>
  drop_na(state) |> 
  pivot_wider(id_cols = c("state", "state_code", "year"),
              names_from = "race",
              values_from = c("deaths", "births"),
              names_sep = "_") |> 
  filter(is.na(deaths_b)) |> 
  group_by(state) |> 
  summarize(n = n()) |> 
  print(n = Inf) |> 
  pull(state)

dropped_states <- c(dropped_states, "Nebraska", "Louisiana", 
  "Massachusetts", "Nevada", "Maryland")
  
pop_race <- read_csv(here("data", "ACSDP5Y2018.DP05.csv")) |> 
  rename(label_grouping = 1) |> 
  mutate(start = str_detect(label_grouping, "HISPANIC OR LATINO AND RACE"),
         end = str_detect(label_grouping, "Total housing units"),
         start = case_match(start,
                            TRUE ~ 1,
                            .default = NA_integer_),
         end = case_match(end,
                          TRUE ~ 1,
                          .default = NA_integer_)) |> 
  relocate(c(start,end), .after = label_grouping) |> 
  fill(start, .direction = "down") |> 
  fill(end, .direction = "down") |> 
  filter(start == 1, is.na(end)) |> 
  mutate(label_grouping = str_squish(label_grouping)) |> 
  filter(str_detect(label_grouping, "^White|^Black")) |> 
  select(label_grouping, ends_with("Estimate")) |> 
  mutate(label_grouping = str_sub(label_grouping, 1, 1)) |> 
  rename_with(~str_remove(.x, "!!Estimate")) |> 
  pivot_longer(cols = !label_grouping,
               names_to = "state",
               values_to = "pop") |> 
  rename(race = label_grouping)

pop_race |> 
  filter(!state %in% c("District of Columbia", "Puerto Rico")) |> 
  mutate(include = case_when(state %in% dropped_states ~ 0,
                             .default = 1)) |> 
  group_by(race, include) |> 
  summarize(pop_sum = sum(pop)) |> 
  mutate(pop_total = sum(pop_sum)) |> 
  filter(include == 1) |> 
  mutate(pct = pop_sum / pop_total)
         
         