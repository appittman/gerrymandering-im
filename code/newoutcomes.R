### looking at new outcomes
library(here)
library(tidyverse)
library(fixest)
library(naniar)


load(here("data", "newout.Rdata"))

######### missing data / sample size calculations
wam_lag3 <- newout_lag3 |> 
  select(-c(sumopioid:TANF2023)) |> 
  filter(!is.na(wam))

#1000 possible state-years
# get rid of NE, LA, MA, NV, MD, MT for high missingness on EG
#880 possible state-years
wam_lag3 <- wam_lag3 |> 
  filter(!(state %in% c("NE", "LA", "MA", "NV", "MD", "MT")))

gg_miss_upset(wam_lag3)

# two sources of missingness: planwriters and EG values

wam_lag3

################ MODELS
wam_m1 <- feols(wam ~ eg_z + prop_rep_planwriters_z + top10share_z + inc_z
                | state + year, 
                data = wam_lag3)

wam_m2 <- feols(wam ~ eg_z + prop_rep_planwriters_z + top10share_z + inc_z +
                  prop_rep_z | state + year,
                data = wam_lag3)

coefplot(wam_m1)
coefplot(wam_m2)

medgen_lag0 <- newout_lag0 |> 
  select(-c(sumopioid:medgen_v1, SNAP_2023:wam)) |> 
  filter(!is.na(medgen_v2))


medgen_lag0 <- medgen_lag0 |> 
  filter(!(state %in% c("NE", "LA", "MA", "NV", "MD", "MT")))

medgen_m1 <- feols(medgen_v2 ~ eg_z + prop_rep_planwriters_z + top10share_z + inc_z
                     | state + year, 
                     data = medgen_lag0)

medgen_m2 <- feols(medgen_v2 ~ eg_z + prop_rep_planwriters_z + top10share_z + inc_z + prop_rep_z + eg_z:prop_rep_z
                   | state + year, 
                   data = medgen_lag0)


coefplot(medgen_m1)
coefplot(medgen_m2)
