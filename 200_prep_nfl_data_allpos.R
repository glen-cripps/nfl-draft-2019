library(rpart)
library(rattle)
library(randomForest)
library(readr)
library(dplyr)
library(tidyr)
library(feather)
library(stringr)
library(mice) ## multiple data imputation
library(party)

muh_code = '/home/hduser/Dropbox/Analytics/nfl_draft_2019'
muh_cash = '/home/hduser/Dropbox/Analytics/nfl_draft_2019/cache'
muh_stash = '/home/hduser/Dropbox/Analytics/nfl_draft_2019/data'  # yeah windows directories may nw
muh_slash = substr(muh_stash,1,1)  # so, if running this on windows, if you have problems look for code

source(paste(muh_code,'utils.R', sep=muh_slash))

combine.table <- read_feather(paste(muh_stash,'combines.feather',sep=muh_slash))
draft.table <- read_feather(paste(muh_stash,'drafts.feather',sep=muh_slash))
college.stats <- read_feather(paste(muh_stash,'college_stats.feather',sep=muh_slash))

### CLEAN THE DATA.  Fix HTTPS problem in there
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

# subset college stats for a nice compact set of nonredundant data 
college.position = subset(college.stats, college.stats$section %in% c("receiving","defense","passing","rushing"))

# only get the pertinant columns.. lots of data that isnt really pertinant to this predictoin..
draft.atomic = select(draft.table, pick, url, team)
draft.atomic = subset(draft.atomic,url!='NA') # only train on data that isnt missing

combine.atomic = select(combine.table, year, player, college, height, weight, forty, vertical, bench, broad, threecone, shuttle, url, pos)
combine.atomic = subset(combine.atomic,url!='NA')  # only train on data we have
# Merge draft data with combine data
combine.draft = merge(draft.atomic, combine.atomic, by='url', all.y = TRUE)

# K gotta deal with the redundant and nonpertinant stats for these positions
college.position$fullstat = paste(college.position$section , "." , college.position$stat, sep="") 
college.atomic = within(college.position, rm(stat,section)) # delete redundant columns

# Flatten the normalized college stat data into 1 row per player
college.flat = spread(college.atomic, fullstat, value)   
college.flat[is.na(college.flat)] = 0
cols_to_drop = colnames(college.flat)
bad_stat_words = c("avg","rate","per") # k the average, rate, etc stats that cant be added year to year need to go 
cols_to_drop = subset(cols_to_drop, grepl(paste(bad_stat_words, collapse="|"), cols_to_drop))
college.trim = college.flat[ , -which(names(college.flat) %in% cols_to_drop)]

# merge again.. put it all together now
combine.draft.college = merge(combine.draft, college.trim,by='url')

# ok all your data is merged.  Now fix up some data and impute missing values.  Set null draft pick to 300 
# set all values for undrafted players to 300 (ie they got drafted laaaast)
combine.draft.college$pick[is.na(combine.draft.college$pick)]= 300
# whoops, only set 300 for the guys drafted before 2018.  2018 hasnt happened yet
combine.draft.college$pick[combine.draft.college$year==2019]= NA
combine.draft.college$first_round_pick = ifelse(combine.draft.college$pick<=32,1,0)
# k calculated fields time.  calculate height from the field that is 5-10
all_position = separate(combine.draft.college, height, c('feet', 'inches'), sep = '-', convert = TRUE)
all_position = mutate(all_position, height_inches = feet * 12 + inches)
# and nuke the redundant fields
all_position = within(all_position, rm( feet, inches))
# this is a bastard way to nuke the oline college stats which are junk... set em all to 0 positionally.. lazy 
all_position[all_position$pos %in% c("OL","OT","OG","C","G"),15:59]=0

# argh!  The 2018 data has different positions than the 2017 and before data for some... hardcode fix
all_position$pos[all_position$pos %in% c("SS","FS")] = "S" # noticed 2018 data is always S for safeties so making it consistent
all_position$pos[all_position$pos == "EDGE"] = "DE" 
all_position$pos[all_position$pos == "DB"] = "CB" 
all_position$pos[all_position$pos == "LB"] = "OLB"  # fuckin way she goes boys
all_position$pos[all_position$pos == "OL"] = "OT" 
all_position$pos[all_position$pos == "G"] = "OG" 
#View(all_position)
# k make the DQ list.  This is the players with too many missing values
dq_list = rowSums(is.na(all_position[!names(all_position) %in% c("pick","team","first_round_pick")]))
all_position$num_missing_vals = dq_list

# trim fields and reorder them
all_position = select(all_position, 
                     first_round_pick,
                     url:player, 
                     pos,college, weight, height_inches, 
                     forty:shuttle, 
                     starts_with('defense'), 
                     starts_with('passing'),
                     starts_with('rushing'), 
                     starts_with('receiving'),
                     num_missing_vals)

# this is the semi-unmanipulated data spreadsheet that went to google docs
write.csv(all_position,"/home/hduser/Dropbox/Analytics/nfl_draft_2019/data/all_position_unimputed.csv")

# Final cuts!  
# subset down to players with 4 or less missing values from the combine 
usable_data = all_position[all_position$num_missing_vals<=5,]
# toss em if they dont have actual data

install.packages("hexbin")
library(hexbin)
plot( usable_data[usable_data$pick < 300,]$weight, usable_data[usable_data$pick < 300,]$pick) 
plot( hexbin(usable_data[usable_data$pick < 300,]$weight, usable_data[usable_data$pick < 300,]$pick) )

#a = lm(usable_data[usable_data$pick < 300,]$pick ~ usable_data[usable_data$pick < 300,]$weight )
hist(usable_data$weight)

#str(usable_data)
# K now replace the missing values with the mean .. I dont understand mice yet
# I do the averages by year and position first... 
averages_by_yearpos = usable_data %>%
  group_by(year, pos) %>%
  summarise_at(vars(weight, height_inches, forty, vertical, bench, broad, threecone, shuttle), funs(mean(., na.rm=TRUE)))

# and for the small subset of records where position/year isn't good enough I use just position for 2000-2018 
averages_by_pos = usable_data %>%
  group_by(pos) %>%
  summarise_at(vars(weight, height_inches, forty, vertical, bench, broad, threecone, shuttle), funs(mean(., na.rm=TRUE)))

# K start replacing the missing values.. pass 1 by year/pos
imputed_data_pass1 = merge(usable_data, averages_by_yearpos, by=c("year","pos"),all.x=TRUE )
imputed_data_pass1 = transform(imputed_data_pass1
                               ,forty = ifelse(is.na(imputed_data_pass1$forty.x),forty.y,forty.x)
                               ,height_inches = ifelse(is.na(imputed_data_pass1$height_inches.x),height_inches.y,height_inches.x)
                               ,weight = ifelse(is.na(imputed_data_pass1$weight.x),weight.y,weight.x)
                               ,broad = ifelse(is.na(imputed_data_pass1$broad.x),broad.y,broad.x)
                               ,bench = ifelse(is.na(imputed_data_pass1$bench.x),bench.y,bench.x)
                               ,shuttle = ifelse(is.na(imputed_data_pass1$shuttle.x),shuttle.y,shuttle.x)
                               ,threecone = ifelse(is.na(imputed_data_pass1$threecone.x),threecone.y,threecone.x)
                               ,vertical = ifelse(is.na(imputed_data_pass1$vertical.x),vertical.y,vertical.x)
)
#View(imputed_data_pass2)
# clean up the fields and start with the second pass for the miniscule missing values still remaining
imputed_data_pass2 = select(imputed_data_pass1, 
                      first_round_pick,
                      year:college, weight, height_inches, 
                      forty:vertical, 
                      starts_with('defense'), 
                      starts_with('passing'),
                      starts_with('rushing'), 
                      starts_with('receiving'),
                      num_missing_vals)

# eg (bench for tom brady is still missing)
imputed_data_pass2[imputed_data_pass2$player == "Tom Brady","bench"]

# this last ditch attempt is only by position, not year.
imputed_data_pass2 = merge(imputed_data_pass2, averages_by_pos, by = "pos", all.x=TRUE)

imputed_data_pass2 = transform(imputed_data_pass2
                               ,forty = ifelse(is.na(imputed_data_pass2$forty.x),forty.y,forty.x)
                               ,height_inches = ifelse(is.na(imputed_data_pass2$height_inches.x),height_inches.y,height_inches.x)
                               ,weight = ifelse(is.na(imputed_data_pass2$weight.x),weight.y,weight.x)
                               ,broad = ifelse(is.na(imputed_data_pass2$broad.x),broad.y,broad.x)
                               ,bench = ifelse(is.na(imputed_data_pass2$bench.x),bench.y,bench.x)
                               ,shuttle = ifelse(is.na(imputed_data_pass2$shuttle.x),shuttle.y,shuttle.x)
                               ,threecone = ifelse(is.na(imputed_data_pass2$threecone.x),threecone.y,threecone.x)
                               ,vertical = ifelse(is.na(imputed_data_pass2$vertical.x),vertical.y,vertical.x)
)
# Fuck you Tom
imputed_data_pass2[imputed_data_pass2$player == "Tom Brady","bench"]

imputed_data = select(imputed_data_pass2, 
                            first_round_pick,pos,
                            year:college, weight, height_inches, 
                            forty:vertical, 
                            starts_with('defense'), 
                            starts_with('passing'),
                            starts_with('rushing'), 
                            starts_with('receiving'),
                            num_missing_vals)
# K this dataset (imputed_data) is primed and ready for machine learning

# I believe this will trick the machine learning stuff afterwards to one hot encode these 2 fields
imputed_data$pos = as.factor(imputed_data$pos)
imputed_data$college = as.factor(imputed_data$college)

# gonna shoot this into amazon and see what it thinks for fun..
write_csv(imputed_data[imputed_data$year<=2017,], paste(muh_stash,"2018_all_positions_training_aws.csv",sep=muh_slash))
write_csv(imputed_data[imputed_data$year==2018,], paste(muh_stash,"2018_all_positions_prediction_aws.csv",sep=muh_slash))

#View(imputed_data)
# at some point I decided after a bunch of data hunting and pecking
# to keep the dataset limited to these manageable fields so i could figure
# out wtf is going on


# I dont yet know how/if this will encode pos, but going to try it.
training_data = select(imputed_data[imputed_data$year<=2018,]
                       , pick, first_round_pick, year, player, pos, url, college
                       , weight , forty , vertical , bench , broad , threecone , shuttle , height_inches
                       , defense.games, defense.int, defense.int.td, defense.sacks, defense.seasons, defense.tackles
                       ,passing.attempts,passing.completions , passing.games,passing.pass.ints , passing.pass.tds ,passing.pass.yards , passing.seasons 
                       , receiving.games , receiving.rec.td , receiving.rec.yards , receiving.seasons                       
                       , rushing.games, rushing.rush.att, rushing.rush.td,rushing.rush.yds, rushing.seasons 
                       )

View(training_data)
# breaking these up coz the leearning algoritmns below sort of work on their own files
prediction_data = select(imputed_data[imputed_data$year==2019,]
                         , url, pick, first_round_pick, year, player, pos, url, college
                         , weight , forty , vertical , bench , broad , threecone , shuttle , height_inches
                         , defense.games, defense.int, defense.int.td, defense.sacks, defense.seasons, defense.tackles
                         ,passing.attempts,passing.completions , passing.games,passing.pass.ints , passing.pass.tds ,passing.pass.yards , passing.seasons 
                         , receiving.games , receiving.rec.td , receiving.rec.yards , receiving.seasons                       
                         , rushing.games, rushing.rush.att, rushing.rush.td,rushing.rush.yds, rushing.seasons 
)

#View(prediction_data)
# this is doing the actual machine learning using linear regression
linear_model = lm(pick ~ 
                    year + pos  
                  + weight + forty + vertical + bench + broad + threecone + shuttle + height_inches
                  + defense.games + defense.int + defense.int.td + defense.sacks + defense.seasons + defense.tackles
                  +passing.attempts+passing.completions + passing.games+passing.pass.ints + passing.pass.tds +passing.pass.yards + passing.seasons 
                  + receiving.games + receiving.rec.td + receiving.rec.yards + receiving.seasons                       
                  + rushing.games + rushing.rush.att + rushing.rush.td + rushing.rush.yds + rushing.seasons 
                  ,
                  data = training_data
)

# this shows you the coefficients for each feature which generally 
# make sense if you follow the slope and think logically
summary(linear_model)

logistic_model = glm(first_round_pick ~
                       year + pos 
                     + weight + forty + vertical + bench + broad + threecone + shuttle + height_inches
                     + defense.games + defense.int + defense.int.td + defense.sacks + defense.seasons + defense.tackles
                     +passing.attempts+passing.completions + passing.games+passing.pass.ints + passing.pass.tds +passing.pass.yards + passing.seasons 
                     + receiving.games + receiving.rec.td + receiving.rec.yards + receiving.seasons                       
                     + rushing.games + rushing.rush.att + rushing.rush.td + rushing.rush.yds + rushing.seasons 
                     ,
                     ,family=binomial(link='logit'),data=training_data
)

summary(logistic_model)
# K now apply the model over the 2018 prediction set 

decisiontree_model = ctree(first_round_pick ~ 
                             year + pos  
                           + weight + forty + vertical + bench + broad + threecone + shuttle + height_inches
                           + defense.games + defense.int + defense.int.td + defense.sacks + defense.seasons + defense.tackles
                           +passing.attempts+passing.completions + passing.games+passing.pass.ints + passing.pass.tds +passing.pass.yards + passing.seasons 
                           + receiving.games + receiving.rec.td + receiving.rec.yards + receiving.seasons                       
                           + rushing.games + rushing.rush.att + rushing.rush.td + rushing.rush.yds + rushing.seasons 
                           , data = training_data
                           , controls = ctree_control(mincriterion = 0.85, minsplit = 0, minbucket = 0, maxsurrogate = 3)
)


summary(decisiontree_model)
plot(decisiontree_model)
#library(rattle)
#fancyRpartPlot(model = decisiontree_model)

#rpart_model = rpart(formula = pick ~ 
#                             year + pos  
#                           + weight + forty + vertical + bench + broad + threecone + shuttle + height_inches
#                           + defense.games + defense.int + defense.int.td + defense.sacks + defense.seasons + defense.tackles
#                           +passing.attempts+passing.completions + passing.games+passing.pass.ints + passing.pass.tds +passing.pass.yards + passing.seasons 
#                           + receiving.games + receiving.rec.td + receiving.rec.yards + receiving.seasons                       
#                           + rushing.games + rushing.rush.att + rushing.rush.td + rushing.rush.yds + rushing.seasons 
#                           , data = training_data) 
# #plot(rpart_yesno_model)
# #fancyRpartPlot(rpart_yesno_model)
# summary(rpart_yesno_model)

randomforest_model <- randomForest(formula = first_round_pick ~ 
                                     year + pos  
                                   + weight + forty + vertical + bench + broad + threecone + shuttle + height_inches
                                   + defense.games + defense.int + defense.int.td + defense.sacks + defense.seasons + defense.tackles
                                   +passing.attempts+passing.completions + passing.games+passing.pass.ints + passing.pass.tds +passing.pass.yards + passing.seasons 
                                   + receiving.games + receiving.rec.td + receiving.rec.yards + receiving.seasons                       
                                   + rushing.games + rushing.rush.att + rushing.rush.td + rushing.rush.yds + rushing.seasons 
                                   , data = training_data, na.action=na.roughfix)

# install.packages("KernelKnn")
# library(KernelKnn)
# knn_model <- KernelKnn(as.integer(pick) ~ 
#                     year + pos  
#                   + weight + forty + vertical + bench + broad + threecone + shuttle + height_inches
#                   + defense.games + defense.int + defense.int.td + defense.sacks + defense.seasons + defense.tackles
#                   +passing.attempts+passing.completions + passing.games+passing.pass.ints + passing.pass.tds +passing.pass.yards + passing.seasons 
#                   + receiving.games + receiving.rec.td + receiving.rec.yards + receiving.seasons                       
#                   + rushing.games + rushing.rush.att + rushing.rush.td + rushing.rush.yds + rushing.seasons 
#                   , data = training_data
#                   ,transf_categ_cols = TRUE
#                   , y=prediction_data
#                   ,regression=FALSE)
 


prediction_data$dectree_pred = predict(decisiontree_model, newdata = prediction_data, type = "response")
prediction_data$logreg_pred = predict(logistic_model, newdata = prediction_data, type="response")
prediction_data$linreg_pred = predict(linear_model, newdata = prediction_data)
prediction_data$rforest_pred = predict(randomforest_model, newdata = prediction_data)
#@prediction_data$rpart_pred = predict(rpart_yesno_model, newdata = prediction_data)

#View(prediction_data) # the 2 columns added just above this are the predictions for linear and logistic reg

# k so we have our 3 predictions and we just gonna do a simple ranked list based on each of the 3
# and that's our baloney mock draft
prediction_data$dectree_rank = rank(-prediction_data$dectree_pred)
prediction_data$logreg_rank = rank(-prediction_data$logreg_pred)
prediction_data$linreg_rank = rank(prediction_data$linreg_pred)
#prediction_data$rpart_rank = rank(prediction_data$rpart_pred)
prediction_data$rforest_rank = rank(-prediction_data$rforest_pred)

prediction_data$total_rank = prediction_data$dectree_rank +
                             prediction_data$logreg_rank +
                             prediction_data$linreg_rank +
#                             prediction_data$rpart_rank +
                             prediction_data$rforest_rank 

  
prediction_data$power_rank = rank(prediction_data$total_rank)
View(prediction_data)

write_csv(prediction_data, paste(muh_stash,"All_Positions_Predictions.csv",sep=muh_slash))
