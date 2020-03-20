library(ggplot2)
names(dat2)

# Number of awarded players by year (bar graph)   ----
year_dat <- dat_joined %>% 
  filter(division == "AA") %>% 
  group_by(award_year, enterNFL) %>% 
  summarize(n = n())

ggplot(year_dat, aes(x = award_year, y = n, fill = enterNFL)) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(name = "Entered NFL") +
  scale_y_continuous(limits = c(0, 120)) +
  # scale_x_continuous
  ylab("Number of Awarded Players") +
  xlab("Award Year")


# when did most awards  indicate entry into the NFL?
# which awards during since that time period didn't result in entering the NFL? ----

dat_joined %>% 
  group_by(award_year) %>% 
  mutate(total_count = n()) %>% 
  group_by(award_year, enterNFL) %>% 
  mutate(miss_count = ifelse(enterNFL == "no", n(), 0),
         hit_count = ifelse(enterNFL == "yes", n(), 0),
         hitrate_yearly = hit_count/total_count) %>% 
  select(award_year, enterNFL, total_count, miss_count, hit_count, hitrate_yearly) %>% 
  arrange(award_year) %>% 
  distinct() %>% 
  #filter(hitrate_yearly > 0.25) %>% 
  filter(award_year > 2000) %>% 
  print(n=100)

###2007 hit rate shot up ~ 40% from the previous year to 97.3% 

dat_joined %>% 
  filter(award_year > 2006,
         enterNFL == "no") %>% 
  select(award_name) %>% 
  distinct()

# Awards time frame (scatter plot)   ----

#set ordering of the factor levels based on earliest awards given 
award_time_levels <- factor(dat_joined$award_name, 
                             levels = unique(dat_joined$award_name[order(dat_joined$award_year,
                                                                         dat_joined$award_name)]), 
                             ordered = TRUE)

# use new order of factor levels for data to plot
award_year_dat <- dat_joined %>% 
  select(award_name, award_year) %>% 
  mutate(award_name = factor(award_name, levels = levels(award_time_levels)))
  

# plot data
ggplot(award_year_dat, aes(x = award_year, y = award_name)) +
  geom_point()
  


# Awards given out by year (dot plot)    ----

#set ordering of the factor levels using total awards given
award_totals_levels <- dat_joined %>% 
  group_by(award_name) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) 

award_levels <- factor(award_totals_levels$award_name, 
                       levels = unique(award_totals_levels$award_name[order(award_totals_levels$n,
                                                                            award_totals_levels$award_name)]), 
                       ordered = TRUE)

# use new order of factor levels 
award_year_dat <- dat_joined %>% 
  filter(division == "AA") %>% 
  select(award_name, award_year, enterNFL) %>% 
  mutate(award_name = factor(award_name, levels = levels(award_levels)))

# plot data
ggplot(award_year_dat, aes(x = award_year, fill = enterNFL)) +
  geom_dotplot(color = "grey60", binaxis = "x", binpositions = "bygroup") +
  scale_y_continuous(NULL, breaks = NULL) +
  facet_grid(award_name~., scales = "free_y", space = "free_y", switch = "y") 



# Hit rate of each award by year ----
hitrate_yearly <- dat_joined %>% 
  group_by(award_name, award_year) %>% 
  mutate(total_count = n()) %>% 
  ungroup() %>% 
  group_by(award_name, award_year, enterNFL) %>% 
  mutate(total_hit = ifelse(enterNFL == "yes", n(), 0),
         total_miss = ifelse(enterNFL == "no", n(), 0),
         hit_rate = total_hit/total_count) %>% 
  select(award_name, award_year, enterNFL, total_count, total_hit, total_miss, hit_rate) %>% 
  distinct() %>% 
  ungroup() %>% 
  #set new factor level order -- from above
  mutate(award_name = factor(award_name, levels = levels(award_levels)))
  

# plot data
ggplot(hitrate_yearly, aes(x = award_year, y = hit_rate)) +
  geom_bar(stat = "identity") +
  facet_wrap(~award_name)

    filter(award_name == "Defensive Player of the Year") %>% 
  arrange(award_year) %>% 
  print(n=Inf)
  
summarize(n=n()) %>% 
  group_by(award_name, award_year) %>% 
  summarize(award_totalYear = n()) %>% 
  filter(award_totalYear>1)

# Number of NFL entrants by the number of awards received    ----

dat2 %>% 
  group_by(name, )
