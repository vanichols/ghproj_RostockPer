#--look at ww data

library(tidyverse)
library(readxl)
library(scales)

read_dir <- "data/raw/ww/"
rds_files <- list.files(path = read_dir, pattern = "\\.rds?$", full.names = TRUE)

dat_ww <- NULL

for (i in 1:length(rds_files)){
  
  file_path <- rds_files[i]
  
  data <- read_rds(file_path)
  
  dat_ww <- 
    dat_ww %>% 
    bind_rows(data)
    
}


#--which types of wheat
dat_ww %>% 
  group_by(name) %>% 
  summarise(n = n())


#--make postcode numeric
d1 <- 
  dat_ww %>% 
  mutate(post_code2 = as.numeric(post_code_identifier))

#--assign region
d2 <- 
  d1 %>% 
  mutate(region = case_when(
    post_code2 < 3000 ~ "Copenhagen",
    post_code2 >= 3000 & post_code2 < 3700 ~ "North Zealand",
    post_code2 >= 3700 & post_code2 < 3800 ~ "Bornholm",
    post_code2 >= 3800 & post_code2 < 4000 ~ "Greenland, Faroe Islands, etc",
    post_code2 >= 4000 & post_code2 < 5000 ~ "Zealand",
    post_code2 >= 5000 & post_code2 < 6000 ~ "Funen",
    post_code2 >= 6000 & post_code2 <= 9999 ~ "Jutland",
    TRUE ~ "XX"))

#--what are the acreage units?
#--only 2
d2 %>% 
  group_by(acreage_unit) %>% 
  summarise(n = n())
  
d2 %>% 
  filter(year_range == "2022-2023") %>% 
  group_by(region) %>% 
  summarise(tot_acreage = sum(acreage_size, na.rm = T)) %>% 
  ggplot(aes(reorder(region, tot_acreage), tot_acreage)) +
  geom_col() + 
  scale_y_continuous(labels = label_comma()) +
  coord_flip() +
    labs(title = "2022-2023")

d2 %>% 
  group_by(region, year_range) %>% 
  summarise(tot_acreage = sum(acreage_size, na.rm = T)) %>% 
  ggplot(aes(year_range, tot_acreage)) +
  geom_col() + 
  scale_y_continuous(labels = label_comma()) +
  coord_flip() +
  facet_wrap(~region)

