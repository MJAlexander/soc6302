library(tidyverse)
library(haven)

## CCHS dowloaded from:https://abacus.library.ubc.ca/dataset.xhtml?persistentId=hdl:11272.1/AB2/SEB16A


d <- read_dta("~/Downloads/dataverse_files/Data/CCHS_Annual_2017_2018.dta")

vars_to_keep <- c("GEO_PRV",
                  "DHH_SEX",
                  "DHHGMS",
                  "DHHDGHSZ",
                  "DHHDGLVG",
                  "DHHGAGE",
                  "MACG005",
                  "MAC_015",
                  "EHG2DVR3",
                  "GENDVHDI",
                  "GENDVSWL",
                  "HWTDGHTM",
                  "HWTDGWTK",
                  "HWTDGBMI",
                  #CCC_005:CCC_200,
                  "SMKDVSTY",
                  "SMKDGYCS",
                  "SMKDGSTP",
                  "SCADVQUI",
                  "ALWDVWKY",
                  #MED_005:MED_080,
                  "DRMDVLA",
                  "DRMDVLAY",
                  #drgdvlca:drgdvlac,
                  #paadvrec:paadvmva
                  "PAADVWKD",
                  "PAADVWND",
                  "CMH_005",
                  "CMHG010",
                  #lbfdvwss:lbfdvpft
                  #sdc_015:sdcdvfls,
                  "INCDGHH",
                  "WTS_M"
                  )

dr <- d |> 
  janitor::clean_names() |> 
  select(str_to_lower(vars_to_keep), 
         ccc_005:ccc_200,
         med_005:med_080,
         drgdvlca:drgdvlac,
         paadvrec:paadvmva,
         lbfdvwss:lbfdvpft,
         sdc_015:sdcdvfls)

# why are some of these capitals

dr |> 
  group_by(geo_prv) |> 
  tally()

dr <- dr |> 
  mutate(geo_prv = str_to_lower(as_factor(geo_prv)))


dr |> 
  group_by(dhhgage) |> 
  tally()

dr |> 
  ggplot(aes(hwtdghtm, hwtdgwtk)) + geom_point()

dr |> 
  ggplot(aes(paadvrec)) + geom_histogram() + scale_x_log10()

write_csv(dr, file = "data/cchs.csv")


# CENSUS downloaded from IPUMS

d <- read_dta("~/Downloads/ipumsi_00031.dta")

drc<- d |> 
  select(urban:marst, nativity:incearn) |> 
  select(-pernum, -edattaind) |> 
  mutate(across(c(urban, 
                  geo1_ca2011,
                  sex,
                  marst,
                  nativity,
                  citizen,
                  yrsimm,
                  edattain,
                  labforce,
                  occisco,
                  hrswork1), as_factor)
         ) |> 
  mutate(age = as.numeric(age))

write_csv(dr, file = "data/census.csv")
