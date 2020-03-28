
dat_table_prep <- dat_joined %>% 
  #get hit rate of awards between 2002 and 2015. earlier years skew the data since enterNFL not as available
  filter(award_year >= 2007,
         award_year <= 2015) %>% 
  group_by(award_name) %>% 
  mutate(total_count = n()) %>% 
  group_by(award_name, enterNFL) %>% 
  mutate(miss_count = ifelse(enterNFL == "no", n(), 0),
         hit_count = ifelse(enterNFL == "yes", n(), 0),
         hitrate_total = hit_count/total_count) %>% 
  ungroup() %>% 
  #fill in hitrate_total of misses (0) with the hitrate
  group_by(award_name) %>% 
  arrange(award_name, desc(hitrate_total)) %>% 
  mutate(hitrate_total = hitrate_total[1]) %>% 
  ungroup() %>% 
  #get list of awards between 2002 and 2015 and their hit rates
  select(award_name, total_count, hitrate_total) %>% 
  distinct() %>% 
  
  #add awardees 
  left_join(dat_joined, by = c("award_name", "award_name")) %>% 
  select(award_name, total_count, hitrate_total, award_year, name, enterNFL) %>% 
  #paste whether they entered the NFL
  mutate(name = paste(name, enterNFL, sep = "_")) %>% 
  select(-enterNFL) %>% 
  
  #scope on 2012
  filter(award_year >= 2012) %>% 
  
  #spread out year columns
  group_by(award_name, award_year) %>% 
  mutate(grouped_id = row_number()) %>% #add an index for spread
  pivot_wider(names_from = award_year, values_from = name) %>% 
  select(-grouped_id) %>% 
  arrange(desc(hitrate_total), desc(total_count)) %>% 
  ungroup() 

# format --
dat_table_all <- dat_table_prep %>% 
  # add id
  mutate(id = as.integer(factor(award_name,
                                 levels = unique(dat_table_prep$award_name[rev(order(dat_table_prep$hitrate_total,
                                                                                     dat_table_prep$total_count))])))) %>% 
  
  # collapse rows. function in functions.R
  mutate(award_count_rate = paste(award_name, total_count, hitrate_total, sep = "~")) %>% #new column to collapse on
  select(award_count_rate, everything(), -award_name, -total_count, -hitrate_total) %>%
  group_by(award_count_rate) %>%
  collapse_rows_df(award_count_rate) %>%
  separate(award_count_rate, into = c("award_name", "total_count", "hitrate_total"), sep = "~") %>%
  ungroup() %>%
  
  #separate out whether they entered the nfl
  # separate("2007", into = c("2007", "2007_nfl"), sep="_(?=[^_]+$)") %>% 
  # separate("2008", into = c("2008", "2008_nfl"), sep="_(?=[^_]+$)") %>% 
  # separate("2009", into = c("2009", "2009_nfl"), sep="_(?=[^_]+$)") %>% 
  # separate("2010", into = c("2010", "2010_nfl"), sep="_(?=[^_]+$)") %>% 
  # separate("2011", into = c("2011", "2011_nfl"), sep="_(?=[^_]+$)") %>% 
  separate("2012", into = c("2012", "2012_nfl"), sep="_(?=[^_]+$)") %>% 
  separate("2013", into = c("2013", "2013_nfl"), sep="_(?=[^_]+$)") %>% 
  separate("2014", into = c("2014", "2014_nfl"), sep="_(?=[^_]+$)") %>% 
  separate("2015", into = c("2015", "2015_nfl"), sep="_(?=[^_]+$)") %>% 
  separate("2016", into = c("2016", "2016_nfl"), sep="_(?=[^_]+$)") %>% 
  separate("2017", into = c("2017", "2017_nfl"), sep="_(?=[^_]+$)") %>% 
  separate("2018", into = c("2018", "2018_nfl"), sep="_(?=[^_]+$)") %>% 
  # separate out scope
  separate(award_name, into = c("Name of Award", "Scope"), sep="_(?=[^_]+$)") %>% 
 
  # replace all NAs with blank
  replace(., is.na(.), "") %>% 
  
  # reformat 
  mutate(hitrate_total = formattable::percent(hitrate_total, format = "d", digits = 0),
         hitrate_total = as.character(hitrate_total),
         hitrate_total = ifelse(hitrate_total == "NA", "", hitrate_total)) %>% 
  select(id, `Name of Award`, Scope, total_count, hitrate_total, `2012`, `2013`, `2014`, `2015`, 
         `2016`, `2017`, `2018`, everything()) %>% 
  rename('Awards Given (2007-2015)' = total_count,
         '% Entered NFL' = hitrate_total) %>% 
  arrange(id)
  



library(formattable)
library(DT)
award_player_table <- as.datatable(formattable::formattable(dat_table_all, 
                         align = c("c", "l", "c", "c", "c", "l"),
                         list(`Name of Award` = formatter("span",
                                                          style = ~style(color = "grey20", 
                                                                         font.weight = "bold")),
                              `Scope` = formatter("span",
                                                  style = ~style(color = "grey")),
                              `2007` = formatter("span",
                                                 style = x ~ style(color = ifelse(dat_table_all$`2007_nfl` == "no", "red", "black"),
                                                                   font.weight = ifelse(dat_table_all$`2007_nfl` == "no", "bold", NA),
                                                                   "background-color" = csscolor("#e8e8e8", format = "hex"),
                                                                   display = "block")),
                              `2008` = formatter("span",
                                                 style = x ~ style(color = ifelse(dat_table_all$`2008_nfl` == "no", "red", "black"),
                                                                   font.weight = ifelse(dat_table_all$`2008_nfl` == "no", "bold", NA),
                                                                   "background-color" = csscolor("#e8e8e8", format = "hex"),
                                                                   display = "block")),
                              `2009` = formatter("span",
                                                 style = x ~ style(color = ifelse(dat_table_all$`2009_nfl` == "no", "red", "black"),
                                                                   font.weight = ifelse(dat_table_all$`2009_nfl` == "no", "bold", NA),
                                                                   "background-color" = csscolor("#e8e8e8", format = "hex"),
                                                                   display = "block")),
                              `2010` = formatter("span",
                                                 style = x ~ style(color = ifelse(dat_table_all$`2010_nfl` == "no", "red", "black"),
                                                                   font.weight = ifelse(dat_table_all$`2010_nfl` == "no", "bold", NA),
                                                                   "background-color" = csscolor("#e8e8e8", format = "hex"),
                                                                   display = "block")),
                              `2011` = formatter("span",
                                                 style = x ~ style(color = ifelse(dat_table_all$`2011_nfl` == "no", "red", "black"),
                                                                   font.weight = ifelse(dat_table_all$`2011_nfl` == "no", "bold", NA),
                                                                   "background-color" = csscolor("#e8e8e8", format = "hex"),
                                                                   display = "block")),
                              `2012` = formatter("span",
                                                 style = x ~ style(color = ifelse(dat_table_all$`2012_nfl` == "no", "red", "black"),
                                                                   font.weight = ifelse(dat_table_all$`2012_nfl` == "no", "bold", NA),
                                                                   "background-color" = csscolor("#e8e8e8", format = "hex"),
                                                                   display = "block")),
                              `2013` = formatter("span",
                                                 style = x ~ style(color = ifelse(dat_table_all$`2013_nfl` == "no", "red", "black"),
                                                                   font.weight = ifelse(dat_table_all$`2013_nfl` == "no", "bold", NA),
                                                                   "background-color" = csscolor("#e8e8e8", format = "hex"),
                                                                   display = "block")),
                              `2014` = formatter("span",
                                                 style = x ~ style(color = ifelse(dat_table_all$`2014_nfl` == "no", "red", "black"),
                                                                   font.weight = ifelse(dat_table_all$`2014_nfl` == "no", "bold", NA),
                                                                   "background-color" = csscolor("#e8e8e8", format = "hex"),
                                                                   display = "block")),
                              `2015` = formatter("span",
                                                 style = x ~ style(color = ifelse(dat_table_all$`2015_nfl` == "no", "red", "black"),
                                                                   font.weight = ifelse(dat_table_all$`2015_nfl` == "no", "bold", NA),
                                                                   "background-color" = csscolor("#e8e8e8", format = "hex"),
                                                                   display = "block")),
                              `2016` = formatter("span",
                                                 style = x ~ style(color = ifelse(dat_table_all$`2016_nfl` == "no", "red", "black"),
                                                                   font.weight = ifelse(dat_table_all$`2016_nfl` == "no", "bold", NA))),
                              `2017` = formatter("span",
                                                 style = x ~ style(color = ifelse(dat_table_all$`2017_nfl` == "no", "red", "black"),
                                                                   font.weight = ifelse(dat_table_all$`2017_nfl` == "no", "bold", NA))),
                              `2018` = formatter("span",
                                                 style = x ~ style(color = ifelse(dat_table_all$`2018_nfl` == "no", "red", "black"),
                                                                   font.weight = ifelse(dat_table_all$`2018_nfl` == "no", "bold", NA))),
                              
                              `2007_nfl` = FALSE,
                              `2008_nfl` = FALSE,
                              `2009_nfl` = FALSE,
                              `2010_nfl` = FALSE,
                              `2011_nfl` = FALSE,
                              `2012_nfl` = FALSE,
                              `2013_nfl` = FALSE,
                              `2014_nfl` = FALSE,
                              `2015_nfl` = FALSE,
                              `2016_nfl` = FALSE,
                              `2017_nfl` = FALSE,
                              `2018_nfl` = FALSE
                              
                              )), rownames = FALSE)

