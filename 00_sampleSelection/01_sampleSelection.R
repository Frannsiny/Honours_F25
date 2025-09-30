library(tidyverse)

#### Goal: select samples to sequence from 'above Big Bar' populations.
# aiming for 50 per species (SK and CN) per year (pre-BB, 2019, 2020, post-BB, as available)
## balancing sex, run timing, and reach of spawning ground along the river

#### Outcome: for each species, 25 M and 25 F per available year


#### Read in and wrangle data #### 
cn <- read_delim("02_popGenomics/data/CN/biolData_scales.csv", delim = ',') %>%
  rename(stock = Stream.Name, date = Rec.Date, reach = Reach, sex = Rec.Sex, adipose = Rec.Adipose, year = Year) %>%
  separate(date, into = c("year", "month", "day"), sep = '-') %>%
  mutate(stock = as.factor(stock),
         reach= as.factor(reach),
         sex = as.factor(sex),
         month = as.factor(month))

#### CHINOOK ####

# split by stock
chilko <- cn %>%
  filter(stock == "Chilko" & adipose == "P")
chilcotin <- cn %>%
  filter(stock == "Chilcotin" & adipose == "P")

#### Chilko ####
## 2018 Chilko
# split by sex - need 25 for each
chilko.18.f <- chilko %>%
  filter(year == "2018" & sex == "F") # 433 indv
chilko.18.m <- chilko %>%
  filter(year == "2018" & sex == "M") # 296 indv

# 2018 Chilko F 
chilko.18.f.09 <- chilko.18.f %>%
  filter(month == "09") # 346 indv = 80% i.e., select 20 
chilko.18.f.10 <- chilko.18.f %>%
  filter(month == "10") # 87 indv = 20% i.e., select 5  
chilko.18.f.09.select <- sample_n(chilko.18.f.09, 20, replace = F)
chilko.18.f.10.select <- sample_n(chilko.18.f.10, 5, replace = F)
chilko.18.f.final <- bind_rows(chilko.18.f.09.select, chilko.18.f.10.select) 

# 2018 Chilko M
chilko.18.m.09 <- chilko.18.m %>%
  filter(month == "09") # 268 indv = 90% i.e., select 22
chilko.18.m.10 <- chilko.18.m %>%
  filter(month == "10") # 28 indv = 10% i.e., select 3  
chilko.18.m.09.select <- sample_n(chilko.18.m.09, 22, replace = F)
chilko.18.m.10.select <- sample_n(chilko.18.m.10, 3, replace = F)
chilko.18.m.final <- bind_rows(chilko.18.m.09.select, chilko.18.m.10.select) 

# write out final 2018 Chilko sample selection
bind_rows(chilko.18.f.final, chilko.18.m.final) %>%
  write_delim("02_popGenomics/outputs/CN/2018_chilko.csv", delim = ',')

## 2019 Chilko
# split by sex - need 25 for each
chilko.19.f <- chilko %>%
  filter(year == "2019" & sex == "F") # 97 indv
chilko.19.m <- chilko %>%
  filter(year == "2019" & sex == "M") # 62 indv

# 2019 Chilko F 
chilko.19.f.09 <- chilko.19.f %>%
  filter(month == "09") # 88 indv = 90% i.e., select 22
chilko.19.f.10 <- chilko.19.f %>%
  filter(month == "10") # 9 indv = 10% i.e., select 3 
chilko.19.f.09.select <- sample_n(chilko.19.f.09, 22, replace = F)
chilko.19.f.10.select <- sample_n(chilko.19.f.10, 3, replace = F)
chilko.19.f.final <- bind_rows(chilko.19.f.09.select, chilko.19.f.10.select) 

# 2019 Chilko M
chilko.19.m.09 <- chilko.19.m %>%
  filter(month == "09") # 59 indv = 95% i.e., select 24
chilko.19.m.10 <- chilko.19.m %>%
  filter(month == "10") # 3 indv = 5% i.e., select 1
chilko.19.m.09.select <- sample_n(chilko.19.m.09, 24, replace = F)
chilko.19.m.10.select <- sample_n(chilko.19.m.10, 1, replace = F)
chilko.19.m.final <- bind_rows(chilko.19.m.09.select, chilko.19.m.10.select) 

# write out final 2019 Chilko sample selection
bind_rows(chilko.19.f.final, chilko.19.m.final) %>%
  write_delim("02_popGenomics/outputs/CN/2019_chilko.csv", delim = ',')

## 2020 Chilko
# split by sex - need 25 for each
chilko.20.f <- chilko %>%
  filter(year == "2020" & sex == "F") # 404 indv
chilko.20.m <- chilko %>%
  filter(year == "2020" & sex == "M") # 226 indv

# 2020 Chilko F 
chilko.20.f.09 <- chilko.20.f %>%
  filter(month == "09") # 383 indv = 95% i.e., select 24
chilko.20.f.10 <- chilko.20.f %>%
  filter(month == "10") # 21 indv = 5% i.e., select 1 
chilko.20.f.09.select <- sample_n(chilko.20.f.09, 24, replace = F)
chilko.20.f.10.select <- sample_n(chilko.20.f.10, 1, replace = F)
chilko.20.f.final <- bind_rows(chilko.20.f.09.select, chilko.20.f.10.select) 

chilko.20.m.09 <- chilko.20.m %>%
  filter(month == "09") # 218 indv = 96% i.e., select 24
chilko.20.m.10 <- chilko.20.m %>%
  filter(month == "10") # 8 indv = 4% i.e., select 1
chilko.20.m.09.select <- sample_n(chilko.20.m.09, 24, replace = F)
chilko.20.m.10.select <- sample_n(chilko.20.m.10, 1, replace = F)
chilko.20.m.final <- bind_rows(chilko.20.m.09.select, chilko.20.m.10.select) 

# write out final 2020 Chilko sample selection
bind_rows(chilko.20.f.final, chilko.20.m.final) %>%
  write_delim("02_popGenomics/outputs/CN/2020_chilko.csv", delim = ',')

## 2021 Chilko
# split by sex - need 25 for each
chilko.21.f <- chilko %>%
  filter(year == "2021" & sex == "F") # 339 indv
chilko.21.m <- chilko %>%
  filter(year == "2021" & sex == "M") # 276 indv

chilko.21.f.09 <- chilko.21.f %>%
  filter(month == "09") # 248 indv = 73% i.e., select 18
chilko.21.f.10 <- chilko.21.f %>%
  filter(month == "10") # 91 indv = 27% i.e., select 7 
chilko.21.f.09.select <- sample_n(chilko.21.f.09, 18, replace = F)
chilko.21.f.10.select <- sample_n(chilko.21.f.10, 7, replace = F)
chilko.21.f.final <- bind_rows(chilko.21.f.09.select, chilko.21.f.10.select) 

# 2021 Chilko M
chilko.21.m.09 <- chilko.21.m %>%
  filter(month == "09") # 242 indv = 88% i.e., select 22
chilko.21.m.10 <- chilko.21.m %>%
  filter(month == "10") # 34 indv = 12% i.e., select 3 
chilko.21.m.09.select <- sample_n(chilko.21.m.09, 22, replace = F)
chilko.21.m.10.select <- sample_n(chilko.21.m.10, 3, replace = F)
chilko.21.m.final <- bind_rows(chilko.21.m.09.select, chilko.21.m.10.select) 

# write out final 2021 Chilko sample selection
bind_rows(chilko.21.f.final, chilko.21.m.final) %>%
  write_delim("02_popGenomics/outputs/CN/2021_chilko.csv", delim = ',')

#### Chilcotin #### 
## 2018 Chilcotin
# split by sex - need 25 for each
chilcotin.18.f <- chilcotin %>%
  filter(year == "2018" & sex == "F") # 32 indv
chilcotin.18.m <- chilcotin %>%
  filter(year == "2018" & sex == "M") # 34 indv

# 2018 Chilcotin F 
chilcotin.18.f.08 <- chilcotin.18.f %>%
  filter(month == "08") # 1 indv = 3% i.e., select 1
chilcotin.18.f.09 <- chilcotin.18.f %>%
  filter(month == "09") # 31 indv = 96% i.e., select 24
chilcotin.18.f.08.select <- sample_n(chilcotin.18.f.08, 1, replace = F)
chilcotin.18.f.09.select <- sample_n(chilcotin.18.f.09, 24, replace = F)
chilcotin.18.f.final <- bind_rows(chilcotin.18.f.08.select, chilcotin.18.f.09.select) 

# 2018 Chilcotin M
chilcotin.18.m.08 <- chilcotin.18.m %>%
  filter(month == "08") # 2 indv = 5% i.e., select 2
chilcotin.18.m.09 <- chilcotin.18.m %>%
  filter(month == "09") # 32 indv = 95% i.e., select 23
chilcotin.18.m.08.select <- sample_n(chilcotin.18.m.08, 2, replace = F)
chilcotin.18.m.09.select <- sample_n(chilcotin.18.m.09, 23, replace = F)
chilcotin.18.m.final <- bind_rows(chilcotin.18.m.08.select, chilcotin.18.m.09.select) 

# write out final 2018 Chilcotin sample selection
bind_rows(chilcotin.18.f.final, chilcotin.18.m.final) %>%
  write_delim("02_popGenomics/outputs/CN/2018_chilcotin.csv", delim = ',')

## 2020 Chilcotin
# split by sex - need 25 for each
chilcotin.20.f <- chilcotin %>%
  filter(year == "2020" & sex == "F") # 105 indv
chilcotin.20.m <- chilcotin %>%
  filter(year == "2020" & sex == "M") # 26 indv

# 2020 Chilcotin F 
chilcotin.20.f.08 <- chilcotin.20.f %>%
  filter(month == "08") # 4 indv = 3% i.e., select 1
chilcotin.20.f.09 <- chilcotin.20.f %>%
  filter(month == "09") # 101 indv = 97% i.e., select 24
chilcotin.20.f.08.select <- sample_n(chilcotin.20.f.08, 1, replace = F)
chilcotin.20.f.09.select <- sample_n(chilcotin.20.f.09, 24, replace = F)
chilcotin.20.f.final <- bind_rows(chilcotin.20.f.08.select, chilcotin.20.f.09.select) 

# 2020 Chilcotin M
chilcotin.20.m.08 <- chilcotin.20.m %>%
  filter(month == "08") # 1 indv = 3% i.e., select 1
chilcotin.20.m.09 <- chilcotin.20.m %>%
  filter(month == "09") # 25 indv = 97% i.e., select 24 
chilcotin.20.m.08.select <- sample_n(chilcotin.20.m.08, 1, replace = F)
chilcotin.20.m.09.select <- sample_n(chilcotin.20.m.09, 24, replace = F)
chilcotin.20.m.final <- bind_rows(chilcotin.20.m.08.select, chilcotin.20.m.09.select) 

# write out final 2020 Chilcotin sample selection
bind_rows(chilcotin.20.f.final, chilcotin.20.m.final) %>%
  write_delim("02_popGenomics/outputs/CN/2020_chilcotin.csv", delim = ',')

## 2021 Chilcotin
# split by sex - need 25 for each
chilcotin.21.f <- chilcotin %>%
  filter(year == "2021" & sex == "F") # 44 indv
chilcotin.21.m <- chilcotin %>%
  filter(year == "2021" & sex == "M") # 54 indv

# 2021 Chilcotin F 
chilcotin.21.f.08 <- chilcotin.21.f %>%
  filter(month == "08") # 2 indv = 5% i.e., select 2
chilcotin.21.f.09 <- chilcotin.21.f %>%
  filter(month == "09") # 42 indv = 95% i.e., select 23
chilcotin.21.f.08.select <- sample_n(chilcotin.21.f.08, 2, replace = F)
chilcotin.21.f.09.select <- sample_n(chilcotin.21.f.09, 23, replace = F)
chilcotin.21.f.final <- bind_rows(chilcotin.21.f.08.select, chilcotin.21.f.09.select) 

# 2021 Chilcotin M
chilcotin.21.m.08 <- chilcotin.21.m %>%
  filter(month == "08") # 2 indv = 4% i.e., select 1
chilcotin.21.m.09 <- chilcotin.21.m %>%
  filter(month == "09") # 52 indv = 96% i.e., select 24 
chilcotin.21.m.08.select <- sample_n(chilcotin.21.m.08, 1, replace = F)
chilcotin.21.m.09.select <- sample_n(chilcotin.21.m.09, 24, replace = F)
chilcotin.21.m.final <- bind_rows(chilcotin.21.m.08.select, chilcotin.21.m.09.select) 

# write out final 2021 Chilcotin sample selection
bind_rows(chilcotin.21.f.final, chilcotin.21.m.final) %>%
  write_delim("02_popGenomics/outputs/CN/2021_chilcotin.csv", delim = ',')


