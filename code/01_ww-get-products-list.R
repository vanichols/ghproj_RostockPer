#--get list of products used in ww

library(tidyverse)
library(readxl)

read_dir <- "data/raw/ww/"
rds_files <- list.files(path = read_dir, pattern = "\\.rds?$", full.names = TRUE)

dat_pest <- NULL

for (i in 1:length(rds_files)){
  
  file_path <- rds_files[i]
  file.name <- tools::file_path_sans_ext(basename(rds_files[i]))
  year_range <- str_extract(file.name, "\\b\\d{4}-\\d{4}\\b")  
  
  data <- read_rds(file_path)
  
  data2 <- 
    data %>% 
    select(pesticide_name, pesticide_registration_number, year_range) %>% 
    distinct()

  dat_pest <- 
    dat_pest %>% 
    bind_rows(data2)
    
}

d <- 
  dat_pest %>% 
  mutate(use_ind = "x", crop = "winter wheat") %>% 
  pivot_wider(names_from = year_range, values_from = use_ind) %>% 
  janitor::clean_names() %>% 
  arrange(pesticide_registration_number)

dat_pest %>% 
  group_by(year_range) %>% 
  summarise(num_prod = n()) %>% 
  ggplot(aes(year_range, num_prod)) + 
  geom_col()



# B. PLI per product ----------------------------------------------------------

b <- 
  read_excel("data/raw/Bilag 1 Bekæmpelsesmiddelstatistik 2023 - Fordelingsliste 2023 - Udg. 1.xlsx", sheet = "Bh pr produkt") %>% 
  janitor::clean_names() 

b1 <- 
  b %>% 
  select(reg_nr, bi_pr_l_produkt_pr_aktivstof_kg_bi)

#--there are 121 reg_nrs with more than one BI value
b1_dupes <- 
  b1 %>% 
  arrange(b1) %>% 
  group_by(reg_nr) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

#--remove those for now
b2 <- 
  b1 %>% 
  filter(!reg_nr %in% b1_dupes$reg_nr)

b3 <- 
  dat_pest %>% 
  rename(reg_nr = pesticide_registration_number) %>% 
  left_join(b2) %>% 
  rename(pli_per_unitprod = bi_pr_l_produkt_pr_aktivstof_kg_bi)

#--there are a lot of missing products...
b3 %>% 
  mutate(pli_value = ifelse(is.na(pli_per_unitprod), "No", "Yes")) %>% 
  group_by(year_range, pli_value) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(year_range, n)) + 
  geom_col(aes(fill = pli_value))
