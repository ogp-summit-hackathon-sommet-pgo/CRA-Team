library(readr)
library(magrittr)
library(tidyverse)
library(reshape2)

# convert to an R project with relative directories
all <- read_csv(
  "C:/Users/Utku/Desktop/hack/cra-team/all.csv",
  col_types = cols(
    `[Age]` = col_character(),
    `[Hazard type]` = col_character(),
    `[Sex]` = col_character()
  )
)

# poverty
poverty <- all %>% dplyr::filter(SeriesCode == "SI_POV_EMP1") %>%
  filter(`[Age]` == "15+") %>%
  select(GeoAreaCode, GeoAreaName, TimePeriod, Value, `[Sex]`) %>%
  spread(key = `[Sex]`, Value) %>%
  rename(
    'Percent Below Poverty Line' = BOTHSEX,
    'Percent Below Poverty Line Males' = MALE,
    'Percent Below Poverty Line Females' = FEMALE
         )
# disaster not by type
disaster <- all %>% dplyr::filter(SeriesCode != "SI_POV_EMP1", SeriesCode != 'SI_POV_NAHC') %>%
  filter(!grepl('by hazard', SeriesDescription, fixed = TRUE)) %>%
  select(SeriesDescription, GeoAreaName, TimePeriod, Value) %>%
  distinct(SeriesDescription, GeoAreaName, TimePeriod, Value, .keep_all = TRUE) %>%
  melt(id.vars = c('GeoAreaName', 'TimePeriod', 'SeriesDescription'), measure.vars = 'Value') %>%
  select(-variable) %>%
  dcast(GeoAreaName + TimePeriod ~ SeriesDescription)
# disaster by type  
disaster_g <- all %>% dplyr::filter(SeriesCode != "SI_POV_EMP1", SeriesCode != 'SI_POV_NAHC') %>%
  filter(grepl('by hazard', SeriesDescription, fixed = TRUE))  
li <- as.list(unique(disaster_g$SeriesDescription))
li2<- lapply(li, 
       function(x){
         disaster_g %>% filter(SeriesDescription == x) %>%
           select(SeriesDescription, GeoAreaName, TimePeriod, `[Hazard type]`, Value) %>%
           distinct(SeriesDescription, GeoAreaName, TimePeriod, Value, .keep_all = TRUE) %>%
           spread(key = `[Hazard type]`, Value) %>%
           setNames(c(colnames(.)[1:3], paste0(colnames(.[4:ncol(.)]), '_', x))) %>%
           select(-SeriesDescription)
       }
)

# Reduce ate up too much ram for some reason, so adding them one by one.
disaster_2 <- 
  full_join(li2[[1]], li2[[2]], by = c('GeoAreaName', 'TimePeriod')) %>%
  full_join(li2[[3]], by = c('GeoAreaName', 'TimePeriod')) %>%
  full_join(li2[[4]], by = c('GeoAreaName', 'TimePeriod')) %>%
  full_join(li2[[5]], by = c('GeoAreaName', 'TimePeriod'))

# Putting all together
allvars <- full_join(poverty, disaster, by = c('GeoAreaName', 'TimePeriod')) %>%
  full_join(disaster_2, by = c('GeoAreaName', 'TimePeriod'))
# saner names
colnames(allvars) <- gsub(" *\\(.*?\\) *", "", names(allvars)) %>%
  gsub(', by hazard type',"", ., fixed = TRUE)

# bring in iso codes for maps
# 
isocodes <- read_csv("C:/Users/Utku/Desktop/hack/cra-team/isocodes.csv",
                     col_types = cols(GeoAreaCode = col_integer())) %>%
  select(GeoAreaCode, ISO)

#allvars2 <- left_join(allvars, isocodes, by = 'GeoAreaCode')


# add the preparedness index
allvars %<>% 
  mutate(preparedness = 100*(`Number of deaths and missing persons attributed to disasters per 100,000 population`/
           (`Number of deaths and missing persons attributed to disasters per 100,000 population` +
           `Number of directly affected persons attributed to disasters per 100,000 population`)))


preparedness <- allvars %>% select(GeoAreaCode, GeoAreaName, TimePeriod, contains("preparedness")) %>%
  filter(!is.na(preparedness), !is.infinite(preparedness))

write.csv(allvars, 'C:/Users/Utku/Desktop/hack/cra-team/flatdata.csv')  

write.csv(preparedness, 'C:/Users/Utku/Desktop/hack/cra-team/preparedness.csv')  
