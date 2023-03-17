library(tidyverse)

d <- read_csv("data/census.csv")

set.seed(853)
rows <- sample(1:nrow(d), 10000)

dr <- d[rows,]


dr |> 
  filter(inctot<999999, inctot>1) |> 
  ggplot(aes(age, inctot)) + 
  geom_point()+
  scale_y_log10()

drr <- dr |> 
  filter(inctot<999999, inctot>1) 

summary(lm(data = drr, log(inctot)~age+edattain))

write_csv(drr, "data/census_sample.csv")


d <- read_csv("data/cchs.csv")

colnames(d)

dr <- d |> 
  select(geo_prv:dhhgms,
         dhhgage,macg005,
         ehg2dvr3,
         gendvhdi:hwtdgbmi,
         smkdvsty,
         paadvwkd, paadvwnd,
         incdghh,
         paadvrca, lbfdvwss,
         sdcdvimm)


dr |> 
  ggplot(aes(hwtdgbmi, paadvrca)) + 
  geom_point()+
  scale_y_log10()

summary(lm(paadvrca~hwtdgbmi, data = dr))

write_csv(dr, "data/cchs_selected.csv")
