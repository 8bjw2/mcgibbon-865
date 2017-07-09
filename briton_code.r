---
  title: "MMA 865 Final Project"
output: html_document
date: "Summer 2017"
author: "Team McGibbon"
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r libraries}
library(tidyverse)
library(data.table)
library (forcats)
library(geosphere)
library(cluster)
```

Load the data

```{r}
in_path = '/global/project/queens-mma/scene-csv/sample03/clean/'

# Note: read_csv() is a the preferred way to read in a CSV file in the tidyverse.
# Howewever, fread is 10 times faster. And why should *I* wait?

scene_mbr_dim <-fread(paste(in_path, 'scene_mbr_dim.csv', sep=""), sep=",")
scene_mbr_acct_dim <-fread(paste(in_path, 'scene_mbr_acct_dim.csv', sep=""), sep=",")
scene_pt_fact <-fread(paste(in_path, 'scene_pt_fact.csv', sep=""), sep=",")

# Note: you probably won't need these tables, but in case you do, here they are:
scene_pt_tp_dim <-read_csv(paste(in_path, 'scene_pt_tp_dim.csv', sep=""))
iwd_time <-read_csv(paste(in_path, 'iwd_time.csv', sep=""))
```

Note: the above loads the "small" data set. To load the full dataset instead, change the `in_path` to say "full" instead of `sample03`.

View the first few rows of some of the tables:
  
  ```{r}
head(scene_pt_tp_dim, n=10)
head(scene_mbr_dim, n=10)
head(theater_locations, n=10)
head(postal_codes, n=10)
```

Analysis
still need to tackle the two variables below:
  scene_mbr_dim<- mutate(scene_mbr_dim, eff_duration = ymd(eff_to_tmstamp) - ymd(eff_from_tmstamp))
diff loc & pref  

```{r}
scene_mbr_dim <-  scene_mbr_dim %>%
  mutate(age = 2017 - brth_dt) %>%
  mutate(edu = fct_collapse(ed_lvl_desc,
                            hschool = c("Completed High School", "Elementary school (less than grade 9)", "Some High School"),
                            uni_college = c("Completed college, technical school", "Completed University", "Some college, technical school", "Some University"), 
                            grad = c("Some graduate school or graduate school"),
                            other = c("NULL", "Unknown", "Prefer not to say") 
  ))  %>%
  mutate(pref_time = fct_collapse(prefrd_show_tm_desc,
                                  early = c("1:00pm", "2:00pm", "3:00pm", "4:00pm"),
                                  afternoon = c("5:00pm", "6:00pm", "7:00pm"),
                                  late = c("8:00pm", "9:00pm", "10:00pm", "11:00pm", "12 midnight")
  )) %>%
  mutate(rel_status = fct_collapse(mrtl_stat_desc,
                                   single = c("Divorced", "Separated", "Single", "Widowed"),
                                   married = c("Married or Common Law"),
                                   other = c("NULL", "Unknown")
  ))
```

```{r}
# load theater locations and canadian postal code coordinates
# make sure to upload the files into the home directory
theater_locations <-fread('theater_locations.csv', sep=",")
postal_codes <-fread('ca_postal_codes.csv', sep=",")

# join latlong to user's address if available on psnl_post_cd
scene_mbr_dim <-  left_join(scene_mbr_dim, postal_codes, by = c("psnl_post_cd" = "postal_code"))
scene_mbr_dim <-  left_join(scene_mbr_dim, theater_locations, by = c("prefrd_loctn_desc" = "theater_name"))

# calculate distance between the two. it will use the center of the member's postal code
# This is the "as the crow flies" distance between 2 points. would need to use google maps API
# to find the driving distance which I don't know how to do in R, just in Power BI
scene_mbr_dim <- scene_mbr_dim %>% mutate(distance = distHaversine(cbind(pref_theater_lng, pref_theater_lat), cbind(psnl_lng, psnl_lat)))
```

```{r}
# take only the most recent record for each member based on the sequence number
scene_mbr_dim <- scene_mbr_dim %>%
  group_by(scene_mbr_key) %>%
  slice(which.max(scene_mbr_seq_num))
# drop keys and create a new table without all the extra stuff
scene_mbr_dim_reduced <- scene_mbr_dim %>% 
  select (-c(gndr_tp_ref_key, prefrd_loctn_ref_key, email_prefnc_ref_key, ed_lvl_tp_ref_key, prefrd_show_tm_ref_key, num_of_hh_pple_ref_key, movie_gng_frq_ref_key, mrtl_stat_ref_key, lang_ref_key, scene_mbr_seq_num, eff_from_tmstamp, eff_to_tmstamp)) %>%
  # drop other columns
  select (-c(ed_lvl_desc, brth_dt, prefrd_show_tm_desc, mrtl_stat_desc, pref_theater_lng, pref_theater_lat, psnl_lng, psnl_lat, email_prefnc_desc, psnl_lng, psnl_lat, pref_theater_lat, pref_theater_lng, psnl_post_cd, psnl_prov_state_cd, prefrd_loctn_desc, scene_mbr_acct_key, scene_acty_stat, pref_theater_type, pref_theater_postal_code))
```


```{r}
# Clustering time
scene_mbr_dim_reduced <- as.data.frame(scene_mbr_dim_reduced)
scene_mbr_dim_reduced$num_of_hh_pple_desc <- as.factor(scene_mbr_dim_unique_reduced$num_of_hh_pple_desc)
scene_mbr_dim_reduced$movie_gng_frq_ref_desc <- as.factor(scene_mbr_dim_unique_reduced$movie_gng_frq_ref_desc)
scene_mbr_dim_reduced$gndr_desc <- as.factor(scene_mbr_dim_unique_reduced$gndr_desc)

scene_mbr_dim_tiny <- top_n(scene_mbr_dim_unique_reduced, 1000, scene_mbr_key)
gower_dist <- daisy(scene_mbr_dim_tiny[, -c(1, 2, 12)],
                    metric = "gower", type = list(logratio = 3))
```




```{r}
# new variables for TRANSACTIONS 
# high/low number of transactions (high = >100) 
scene_pt_fact <- scene_pt_fact %>%
  group_by(scene_mbr_acct_key) %>%
  mutate(nm_trans = ifelse (n() > 100, 1,0)) %>% 
  mutate(lg_trans = ifelse(sum(as.numeric(txn_amt), na.rm=TRUE) > 500 & txn_tp_3 == "non_cin", 1,0))

#509 was median (double check this),
#just looking at transactions outside cineplex

scene_pt_fact <- scene_pt_fact %>%
  filter(time_lvl_end_dt >= ymd(20160701)) %>%
  mutate(last_yr = ifelse(as.numeric(txn_amt) > 0, 1, 0))

# did customer have transaction in the last yr (from today)

#warning: code takes a while to run, so sit back and relax :)

```

```{r}
test test test
```

```{r}
#### summary codes #####
# summary<-
#      group_by(scene_pt_fact, scene_mbr_acct_key)%>%
#      summarise(number_of_txns=n(),
#                total_points= sum(pt),
#                total_txn_amt= sum(as.numeric(txn_amt),na.rm=TRUE))%>%
#      arrange(desc(total_points))

#summary(summary$number_of_txns)

# group_by(scene_pt_fact, txn_tp_3)%>%
#   filter(txn_tp_3 == "non_cin") %>%
#   select(scene_mbr_key, txn_amt)%>%
#   arrange(desc(txn_amt))
# 
# group_by(scene_pt_fact, txn_tp_3)%>%
#   filter(txn_tp_3 == "non_cin" & txn_amt == "0.01") %>%
#   summarise(
#     count = n(),
#     minimum = min(txn_amt)
#     )
# 
# group_by(scene_pt_fact, txn_tp_3)%>%
#   filter(txn_tp_3 == "non_cin" & txn_amt > "250000") %>%
#   summarise(
#     count = n()
#     ) 
```