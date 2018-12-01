#install.packages("mice")
# Mice is how missing values are imputed

library(readr)
library(dplyr)
library(tidyr)
library(feather)
library(stringr)
library(mice) ## multiple data imputation
source('/home/hduser/Dropbox/Analytics/nfl_draft_2018/utils.R')

combine.table <- read_feather('/home/hduser/Dropbox/Analytics/nfl_draft_2018/data/combines.feather')
draft.table <- read_feather('/home/hduser/Dropbox/Analytics/nfl_draft_2018/data/drafts.feather')
college.stats <- read_feather('/home/hduser/Dropbox/Analytics/nfl_draft_2018/data/college_stats.feather')

### CLEAN THE DATA.  Fix HTTPS
combine.table$url=gsub('http:', 'https:', combine.table$url)
draft.table$url=gsub('http:', 'https:', draft.table$url)
college.stats$url=gsub('http:', 'https:', college.stats$url)

# fuckin text data needs to go number
combine.table$weight=as.numeric(combine.table$weight)
combine.table$forty=as.numeric(combine.table$forty)
combine.table$vertical=as.numeric(combine.table$vertical)
combine.table$bench=as.numeric(combine.table$bench)
combine.table$broad=as.numeric(combine.table$broad)
combine.table$threecone=as.numeric(combine.table$threecone)
combine.table$shuttle=as.numeric(combine.table$shuttle)
draft.table$pick = as.numeric(draft.table$pick)

View(college.position)
# subset college stats to rushing for this analytics bullshit
college.position = subset(college.stats, college.stats$section == "receiving")

# only get the pertinant columns.. lots of data that isnt really pertinant to this predictoin..
draft.atomic = select(draft.table, pick, url)

draft.atomic = subset(draft.atomic,url!='NA') # only train on data that isnt missing

combine.atomic = select(combine.table, year, player, college, height, weight, forty, vertical, bench, broad, threecone, shuttle, url, pos)
combine.atomic = subset(combine.atomic,url!='NA')  # only train on data we have
combine.atomic = subset(combine.atomic,pos=='WR')  # stick to RB's

View(combine.atomic)
#merge the combine and draft together
combine.draft = merge(draft.atomic, combine.atomic, by='url', all.y = TRUE)
View(combine.draft)

# K gotta deal with the redundant and nonpertinant stats for this position

college.position$fullstat = paste(college.position$section , "." , college.position$stat, sep="") 
college.atomic = within(college.position, rm(stat,section)) # delete redundant columns

college.flat = spread(college.atomic, fullstat, value)   

# merge again.. put it all together now
combine.draft.college = merge(combine.draft, college.flat,by='url')

# ok all your data is merged.  Now impute missing values.  Set null draft pick to 300 
# and null combine times to the median with .6969696 so you can see them easily

# set all values for undrafted players to 300 (ie they got drafted laaaast)
combine.draft.college$pick[is.na(combine.draft.college$pick)]= 300
# whoops, only set 300 for the guys drafted before 2018.  2018 hasnt happened yet
combine.draft.college$pick[combine.draft.college$year==2018]= NA
# I havent figured out how to do that shit in one step in R yet
View(combine.draft.college)

# k calculated fields time.  calculate height from the field that is 5-10
all_position = separate(combine.draft.college, height, c('feet', 'inches'), sep = '-', convert = TRUE)
all_position = mutate(all_position, height_inches = feet * 12 + inches)
# and nuke the redundant fields
all_position = within(all_position, rm( feet, inches))
View(all_position)

# k make the DQ list.  This is the motherfuckeres with too many missing values
dq_list = rowSums(is.na(all_position))
all_position$num_missing_vals = dq_list

# Final cuts!  
# subset down to players with less than 4 missing values from the combine 
usable_data = all_position[all_position$num_missing_vals<4,]
# fuck em if they dont have actual data

# K now replace the missing values with the mean .. I dont understand mice yet
# and put .XX69 in the value so i notice them 
# yeah im sure there's a way in R to do them all at once but I dont want to 
# do that ninja shit yet
View(usable_data)
mean_forty = round(mean(usable_data$forty, na.rm=TRUE),2)+ .0069
mean_forty

mean_bench = round(mean(usable_data$bench, na.rm=TRUE),2) + .0069
mean_bench

mean_broad = round(mean(usable_data$broad, na.rm=TRUE),2) + 0.0069
mean_broad

mean_threecone = round(mean(usable_data$threecone, na.rm=TRUE),2) + .0069
mean_threecone

mean_shuttle = round(mean(usable_data$shuttle, na.rm=TRUE),2) + .0069
mean_shuttle

mean_vertical = round(mean(usable_data$vertical, na.rm=TRUE),2) + .0069
mean_vertical

# k now start building the imputed dataset
# by reading and writing the same dataset
# Will have to learn that %>% shit later
imputed_data = usable_data

# replace missing values with the mean
imputed_data[is.na(imputed_data$bench),]$bench = mean_bench
imputed_data[is.na(usable_data$forty),]$forty = mean_forty
imputed_data[is.na(usable_data$broad),]$broad = mean_broad
imputed_data[is.na(usable_data$threecone),]$threecone = mean_threecone
imputed_data[is.na(usable_data$shuttle),]$shuttle = mean_shuttle
imputed_data[is.na(usable_data$vertical),]$vertical = mean_vertical

View(imputed_data)

str(imputed_data)

# at some point I decided after a bunch of data hunting and pecking
# to keep the dataset limited to these manageable fields so i could figure
# out wtf is going on
training_data = select(imputed_data[imputed_data$year<=2017,], pick,
                       year 
                       , weight , forty , vertical , bench , broad , threecone , shuttle , height_inches
                       , rushing.games , rushing.rush.avg
                       , rushing.rush.td , rushing.rush.yds , rushing.seasons)

# breaking these up coz the leearning algoritmns below sort of work on their own files
prediction_data = select(imputed_data[imputed_data$year==2018,], url, pick,
                       year 
                       , weight , forty , vertical , bench , broad , threecone , shuttle , height_inches
                       , rushing.games , rushing.rush.avg
                       , rushing.rush.td , rushing.rush.yds , rushing.seasons)

# this is doing the actual machine learning using linear regression
linear_model = lm(pick ~ 
                    year 
                  + weight + forty + vertical + bench + broad + threecone + shuttle + height_inches
                  + rushing.games + rushing.rush.avg
                  + rushing.rush.td + rushing.rush.yds +rushing.seasons,
                  data = training_data
                    )
# this shows you the coefficients for each feature which generally 
# make sense if you follow the slope and think logically
summary(linear_model)

# Here's some plots of me checking that
plot(training_data$pick ~ training_data$rushing.rush.td )
lm(training_data$pick ~ training_data$rushing.rush.td )
abline(196.636,-2.44)

# next i became obsessed that i can reproduce the math using the coefficients that are in the model
coefficients = t(linear_model$coefficients)
colnames(coefficients) <- paste(colnames(coefficients), "cf", sep = "_")

View(coefficients)

# K now apply the model over the 2018 prediction set 
prediction_data$predicted_pick = predict(linear_model, newdata = prediction_data)
pred_w_coefficients = cbind(prediction_data, coefficients)
str(pred_w_coefficients)

# HEre's me obsessing randomly about being able to reproduce the math
pred_w_coefficients$year_wt = pred_w_coefficients$year * pred_w_coefficients$year_cf
pred_w_coefficients$weight_wt = pred_w_coefficients$weight * pred_w_coefficients$weight_cf
pred_w_coefficients$height_inches_wt = pred_w_coefficients$height_inches * pred_w_coefficients$height_inches_cf
pred_w_coefficients$forty_wt = pred_w_coefficients$forty * pred_w_coefficients$forty_cf

pred_w_coefficients$vertical_wt = pred_w_coefficients$vertical * pred_w_coefficients$vertical_cf
pred_w_coefficients$bench_wt = pred_w_coefficients$bench * pred_w_coefficients$bench_cf
pred_w_coefficients$broad_wt = pred_w_coefficients$broad * pred_w_coefficients$broad_cf
pred_w_coefficients$threecone_wt = pred_w_coefficients$threecone * pred_w_coefficients$threecone_cf
pred_w_coefficients$shuttle_wt = pred_w_coefficients$shuttle * pred_w_coefficients$shuttle_cf


pred_w_coefficients$rushing.games_wt = pred_w_coefficients$rushing.games * pred_w_coefficients$rushing.games_cf
pred_w_coefficients$rushing.rush.avg_wt = pred_w_coefficients$rushing.rush.avg * pred_w_coefficients$rushing.rush.avg_cf
pred_w_coefficients$rushing.rush.td_wt = pred_w_coefficients$rushing.rush.td * pred_w_coefficients$rushing.rush.td_cf
pred_w_coefficients$rushing.rush.yds_wt = pred_w_coefficients$rushing.rush.yds * pred_w_coefficients$rushing.rush.yds_cf
pred_w_coefficients$rushing.seasons_wt = pred_w_coefficients$rushing.seasons * pred_w_coefficients$rushing.seasons_cf

# Here's me lazily applying the whatchamacallit intercept coz i cant figure out how to call 
# a column with a bracket in it
pred_w_coefficients$score = -2512 + pred_w_coefficients$year_wt + pred_w_coefficients$weight_wt + pred_w_coefficients$height_inches_wt  + pred_w_coefficients$forty_wt + pred_w_coefficients$vertical_wt + pred_w_coefficients$bench_wt   + pred_w_coefficients$broad_wt + pred_w_coefficients$threecone_wt +  pred_w_coefficients$shuttle_wt + pred_w_coefficients$rushing.games_wt  + pred_w_coefficients$rushing.rush.avg_wt + pred_w_coefficients$rushing.rush.td_wt  + pred_w_coefficients$rushing.rush.yds_wt + pred_w_coefficients$rushing.seasons_wt

View(pred_w_coefficients)  


################################################################333
################################################################333
################################################################333
################################################################333
################################################################333
################################################################333
# everything after here is the code that the smart guy on the internet
# wrote and me fucking around with it to figure out what is going on
# Am no longer running this code... too complicated for me


college.combine = merge(combine.table,college.flat,by="url")

View(college.combine.draft[college.combine.draft$pos.x=="RB",])
View(combine.draft[combine.draft$url=="https://www.sports-reference.com/cfb/players/ladainian-tomlinson-1.html",])

View(college.cleanup2)


college.flat = spread(college.cleanup2, metric, value)   

View("hello")
View(college.flat[college.flat$url == "https://www.sports-reference.com/cfb/players/ladainian-tomlinson-1.html",])

  


## join on url first, then name
left <- draft.table %>%
  select(year, round, pick, team,
         player,
         college,
         pos,
         age,
         carav,
         drav,
         url) %>%
  mutate(key = ifelse(is.na(url), paste(player, year, sep = '-'), url))


right <- combine.table %>%
  select(year_combine = year,
         player_combine = player,
         pos_combine = pos,
         college_combine = college,
         height,
         weight,
         forty,
         vertical,
         broad,
         bench,
         threecone,
         shuttle,
         url_combine = url) %>%
  mutate(key = ifelse(is.na(url_combine),
                      paste(player_combine, year_combine, sep = '-'),
                      url_combine)) %>%
  ## This next block filters out multiple rows with the same player
  group_by(key) %>%
  mutate(appearance = row_number()) %>%
  filter(appearance == 1) %>%
  select(-appearance) %>%
  ungroup

View(right)

combined <- full_join(left, right, by = 'key') %>%
  mutate(player = coalesce2(player, player_combine),
         pos = coalesce2(pos, pos_combine),
         college = coalesce2(college, college_combine),
         year = coalesce2(year, year_combine),
         url = coalesce2(url, url_combine))


View(combined[combined$player == "LaDainian Tomlinson HOF",])
subset = combined[combined$pos== "CB",]
View(subset)
plot(as.numeric(subset$pick) ~ as.numeric(subset$broad))
lm(as.numeric(subset$pick) ~ as.numeric(subset$broad))
abline(470,-3.028)

str(subset)

## Convert into long format so we can merge with college stats
training1 <- combined %>%
  select(key, carav,
         height, weight,
         forty, vertical,
         bench, age,
         threecone, shuttle,
         broad) %>%
  mutate(height = ifelse(is.na(height), 'NA-NA', height)) %>%
  separate(height, c('feet', 'inches'), sep = '-', convert = TRUE) %>%
  mutate(height = feet * 12 + inches) %>%
  select(-feet, -inches) %>%
  gather(metric, value, carav,
         height, weight,
         forty, vertical,
         bench, age,
         threecone, shuttle,
         broad) %>%
  filter(!is.na(value), value != '') %>%
  mutate(value = as.numeric(value))


View(training1[training1$key == "https://www.sports-reference.com/cfb/players/ladainian-tomlinson-1.html",])

## Impute the missing combine data
## A. Convert to wide
training1a <- training1 %>%
  spread(metric, value, fill = NA)

## B. do the imputation and add back the non-imputed columns
training1b <- complete(mice(training1a %>% select(-key, -carav)))
training1b$key <- training1a$key
training1b$carav <- training1a$carav

View(training1b)

## C. Convert back to long format
training1c <- training1b %>%
  gather(metric, value, -key)



View(college.stats[college.stats$url == "https://www.sports-reference.com/cfb/players/karl-joseph-1.html",])
View(training2[training2$key=="https://www.sports-reference.com/cfb/players/karl-joseph-1.html",])

## Convert back into wide form
training3 <- bind_rows(training1c, training2) 
#%>%
#  spread(metric, value, fill = 0) ## note we fill zeros, not NAs

View(training3[training3$key=="https://www.sports-reference.com/cfb/players/karl-joseph-1.html",])
## Join the pick/position/college/year/team back on
## Aggregate smaller schools into representative small school
training <- combined %>%
  select(key, player, pick, pos, college, year, team) %>%
  group_by(college) %>%
  mutate(n_college_picks = n()) %>%
  ungroup %>%
  mutate(short_college = ifelse(n_college_picks < 50, 'SMALL SCHOOL', college),
         pick = ifelse(is.na(pick), 257, as.numeric(pick))) %>%
  inner_join(training2)

View(training[training$key=="https://www.sports-reference.com/cfb/players/karl-joseph-1.html",])



N <- nrow(training)
train.set <- (rbinom(N, 1, prob = 0.9) == 1 & training$year < 2018)
test.set <- (!train.set & training$year < 2018)
holdout.set <- !(test.set | train.set)

# Outcome variables
pick <- training$pick
carav <- training$carav
first.round <- as.numeric(training$pick <= 32)

