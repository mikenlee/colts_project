library(readxl)
library(tidyverse)

# Process first sheet ----
dat <- readxl::read_xlsx("data/NCAAF Award Winners Consolidated Data - FBS & FCS.xlsx")

#drop the last 9 empty columns
dat <- dat[1:14]

dat2 <- dat %>% 
  #rename columns
  rename(name = `Winner's Name`,
         school = `Winner's School`,
         pos_NCAA = `POS - NCAA`,
         pos_NFL = `POS - NFL`,
         year_enterNFL = `Year to Enter NFL`,
         award_name = `Name of Award`,
         split_note = `Note if Split:`,
         pos_specific_award = `Position Specific Award`,
         award_year = Year,
         award_type = Award,
         award_scope = Scope) %>% 
  #trim whitespace
  mutate_if(is.character, str_trim) %>% 
  #clean duplicated entries
  mutate(award_scope = replace(award_scope, str_detect(award_scope, "Pac"), "Pac 12"), 
         award_type = replace(award_type, str_detect(award_type, "Academics"), "Academic"),
         award_type = replace(award_type, str_detect(award_type, "Off  POY"), "Off POY"),
         award_type = replace(award_type, str_detect(award_type, "Special teams"), "Special Teams"),
         year_enterNFL = replace(year_enterNFL, str_detect(year_enterNFL, "N/A"), NA),
         school = replace(school, str_detect(school, "N/A"), NA),
         school = replace(school, str_detect(school, "Washington St"), "Washington State"),
         school = replace(school, str_detect(school, "Southern Miss"), "Southern Mississippi"),
         school = replace(school, str_detect(school, "Pitt"), "Pittsburgh"),
         school = replace(school, str_detect(school, "Oregon St"), "Oregon State"),
         school = replace(school, str_detect(school, "Miss St"), "Mississippi State"),
         school = replace(school, str_detect(school, "Middle Tennessee"), "Middle Tennesee State"),
         school = replace(school, str_detect(school, "Mich. St."), "Michigan State"),
         school = replace(school, str_detect(school, "Lousiana Tech"), "Louisiana Tech"),
         school = replace(school, str_detect(school, "LA Tech"), "Louisiana Tech"),
         school = replace(school, str_detect(school, "Lousiana"), "Louisiana"),
         school = replace(school, str_detect(school, "Georgia\\[5]"), "Georgia"),
         school = replace(school, str_detect(school, "Brigham Young"), "BYU")) %>% 
  #filter out 2020 and 2021 and keep NAs
  filter(is.na(year_enterNFL) | str_detect(year_enterNFL, '2020|2020*|2021|2021*', negate = TRUE)) %>% 
  mutate(year_enterNFL = as.numeric(year_enterNFL)) %>% 
  #new columns
  mutate(award_school = paste(award_name, school, sep = "_"),
         division = "A") %>% 
  #separate first and last to switch order to join with key
  tidyr::extract(name, c("FirstName", "LastName"), "([^ ]+) (.*)") %>% 
  mutate(LastName = str_punct(LastName),
         LastName = str_filter(LastName, sep = " ", side = "right", greedy = FALSE),
         name = paste(LastName, FirstName, sep = ", "),
         name = toupper(name)) %>% 
  select(-FirstName, -LastName) %>% 
  #fill in missing years with those that have a year entered
  group_by(name, school) %>% 
  arrange(name) %>% 
  mutate(year_enterNFL = year_enterNFL[1]) %>% 
  ungroup() %>% 
  #create variable for first letter of school to join on 
  mutate(school_letter = str_sub(school, start = 1, end = 1)) %>% 
  #columns not in second sheet
  select(-pos_NCAA, -pos_NFL, -split_note, -`Comment:`, -pos_specific_award)

 



# Process second sheet ----
dat_AA <- readxl::read_xlsx("data/NCAAF Award Winners Consolidated Data - FBS & FCS.xlsx", sheet = 2)

#drop the last 4 empty columns
dat_AA <- dat_AA[1:9]

dat2_AA <- dat_AA %>% 
  #rename columns
  rename(name = `Winner's Name`,
         school = `Winner's School`,
         year_enterNFL = `Year to Enter NFL`,
         award_name = `Name of Award`,
         award_type = Award,
         award_year = Year,
         award_scope = Scope) %>% 
  #trim whitespace
  mutate_if(is.character, str_trim) %>% 
  #clean duplicated entries
  mutate(award_type = replace(award_type, str_detect(award_type, "Academics"), "Academic"),
         year_enterNFL = replace(year_enterNFL, str_detect(year_enterNFL, "N/A"), NA),
         `First Year` = replace(`First Year`, str_detect(`First Year`, "N/A"), NA),
         `First Year` = as.numeric(`First Year`),
         school = replace(school, str_detect(school, "N/A"), NA),
         school = replace(school, str_detect(school, "VMI"), "Virginia Military Institute"),
         school = replace(school, str_detect(school, "UAlbany"), "Albany"),
         school = replace(school, str_detect(school, "Troy"), "Troy State"),
         school = replace(school, str_detect(school, "Towson"), "Towson State"),
         school = replace(school, str_detect(school, "Southern"), "Southern University"),
         school = replace(school, str_detect(school, "Southeastern La"), "Southeastern Louisiana"),
         school = replace(school, str_detect(school, "Southeast Missouri"), "Southeast Missouri State"),
         school = replace(school, str_detect(school, "Prarie View A&M"), "Prairie View A&M"),
         school = replace(school, str_detect(school, "Leghigh"), "Lehigh"),
         school = replace(school, str_detect(school, "Jacksonville"), "Jacksonville State"),
         school = replace(school, str_detect(school, "Grambling"), "Grambling State"),
         school = replace(school, str_detect(school, "Grambling  State"), "Grambling State"),
         school = replace(school, str_detect(school, "East Tennessee"), "East Tennessee State")) %>% 
    #filter out 2020 and 2021 and keep NAs
    filter(is.na(year_enterNFL) | str_detect(year_enterNFL, '2020|2020*|2021|2021*', negate = TRUE)) %>% 
    mutate(year_enterNFL = as.numeric(year_enterNFL)) %>% 
    # clean text in years
    mutate(award_year = str_sub(award_year, 1, 4),
           award_year = as.numeric(award_year)) %>% 
    #new columns
    mutate(award_school = paste(award_name, school, sep = "_"),
           division = "AA")  %>% 
    #separate first and last to switch order to join with key
    tidyr::extract(name, c("FirstName", "LastName"), "([^ ]+) (.*)") %>% 
    mutate(LastName = str_punct(LastName),
           LastName = str_filter(LastName, sep = " ", side = "right", greedy = FALSE),
           name = paste(LastName, FirstName, sep = ", "),
           name = toupper(name))%>% 
    select(-FirstName, -LastName) %>% 
    #fill in missing years with those that have a year entered
    group_by(name, school) %>% 
    arrange(name) %>% 
    mutate(year_enterNFL = year_enterNFL[1]) %>% 
    ungroup() %>% 
    #create variable for first letter of school to join on 
    mutate(school_letter = str_sub(school, start = 1, end = 1))





#combine A and AA data and data_key ----

#read in data key
data_key <- readr::read_csv("data/nfl_player_data.csv")
data_key <- data_key %>% 
  #create variable for first letter of school to join on 
  mutate(school_letter = str_sub(SCHOOL, start = 1, end = 1))

dat_joined <- bind_rows(dat2, dat2_AA) %>%
  #fill in missing data with key from John
  full_join(data_key, by = c("name" = "NAME", "school_letter" = "school_letter")) %>% 
  mutate(awarded = ifelse(is.na(award_name), "no", "yes"),
         year_enterNFL = ifelse(is.na(year_enterNFL), YEAR, year_enterNFL),
         enterNFL = ifelse(is.na(year_enterNFL), "no", "yes")) %>% 
  #fix incorrect year entered
  mutate(year_enterNFL = ifelse(name == "BOSA, NICK", 2019, year_enterNFL),
         year_enterNFL = ifelse(name == "HARRELL, GRAHAM", 2010, year_enterNFL))  

  group_by(name, award_name) %>% 
  summarize(n())
  mutate(matching = ifelse(year_enterNFL == YEAR, 1, 0)) %>% 
  filter(matching == 0) %>% 
  select(name, year_enterNFL, YEAR, award_name, award_year)
  group_by(matching) %>% 
  summarize(n())


  group_by(school) %>% 
  summarize(n()) %>% 
  print(n=Inf)
  

  
  group_by(enterNFL) %>% 
  summarize(n()) %>% 
  print(n=Inf)

  group_by(name) %>% 
  summarize(n=n()) %>% 
  group_by(n) %>% 
  summarize(n())


  group_by(award_name, split) %>% 
  filter(award_year == min(award_year)) %>% 
  arrange(award_year) %>% 
  
  #summarize(n()) %>% 
  print(n=Inf)
  
  
  
  # schoolname_comp <- expand.grid(dat2$school, dat2$school) %>% 
  #   rename(V1 = Var1, V2 = Var2) %>% 
  #   tidystringdist::tidy_stringdist(.) %>% 
  #   select(V1, V2, jaccard)
  # 
  # schoolname_comp <- tidystringdist::tidy_comb_all(dat2, school) %>% 
  #   tidystringdist::tidy_stringdist() %>% 
  #   select(V1, V2, jaccard)
  # 
  # schoolname_rec <- schoolname_comp %>%
  #   filter(!is.na(jaccard)) %>% 
  #   group_by(V1) %>% 
  #   summarize(min_score = min(jaccard)) %>% 
  #   ungroup() 
  #   
  # schoolname_comp %>% 
  #   inner_join(schoolname_rec, by = c("V1" = "V1", "jaccard" = "min_score")) %>% 
  #   filter(jaccard < .5) %>% 
  #   print(n=Inf)
  #   group_by(school) %>% 
  #   summarize(n()) %>% 
  #   print(n=Inf)

