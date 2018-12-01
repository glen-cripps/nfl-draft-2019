#install.packages('rvest')
# If you get an error do this command line
# sudo apt-get install libssl-dev
# try again biznatch
#install.packages('rvest')

#install.packages('readr')
#install.packages('dplyr')

#install.packages('RCurl')
# do this in a terminal if RCURL dont install
#   sudo apt-get install aptitude
#   sudo apt-get install libcurl4-openssl-dev
#   sudo apt-get install libxml2-dev
# try again now...
#install.packages('RCurl')

#install.packages('tidyr')
#install.packages('stringr')
#install.packages('feather')

# if all those libraries installed properly these should work
# If problems redo the install and add dependencies=TRUE to the func call till they work
library(rvest)
library(readr)
library(dplyr)
library(RCurl)
library(tidyr)
library(stringr)
library(feather)

muh_cash = '/home/hduser/Dropbox/Analytics/nfl_draft_2019/cache'
muh_stash = '/home/hduser/Dropbox/Analytics/nfl_draft_2019/data'  # yeah windows directories may nw
muh_slash = substr(muh_stash,1,1)  # so, if running this on windows, if you have problems look for code
                                   # where sep="/" substituted for muh_slash... \ on windows / on linux/mac
                                   # if it were to be run on windows... no time rn.

# this is a user defined function.  It doesn't run till it's called, which is later
# set the cache.dir variable to a local directory where you can save a bunch of html files
read_html_cache <- function(url, cache.dir = muh_cash) {
  fn <- tail(strsplit(url, '/')[[1]], 1)
  fn.path <- paste(cache.dir, fn, sep = muh_slash)
  if (!file.exists(fn.path)) {
    text <- getURL(url)
    write(text, fn.path)
  }
  read_html(fn.path)
}

# create 2 vectors with the column headers.. vectors is just like a 1D set of numbers
draft.header <- c('round', 'pick', 'team', 'player', 'pos', 'age', 'to', 'ap1', 'pb', 'st', 'carav', 'drav', 'games', 'pass.cmp', 'pass.att', 'pass.yds', 'pass.tds', 'pass.ints', 'rush.att', 'rush.yds', 'rush.tds', 'receptions', 'rec.yds', 'rec.tds', 'tackles', 'ints', 'sacks', 'college', 'stats')
combine.header <- c('player', 'pos', 'college', 'stats', 'height', 'weight', 'forty', 'vertical', 'bench', 'broad', 'threecone', 'shuttle', 'drafted')

# look at your header vectors 
draft.header
combine.header
# this is also a function definition... not called yet..
url.extract <- function(tds) {
  results <- c()
  for(td in tds) {
    children <- html_children(td)
    if (length(children) == 0) {
      results <- c(results, NA)
    } else{
      results <- c(results, (html_attr(html_children(td), 'href')))
    }
  }
  results
}

# the headers you see in the html files isnt fit for using as column names
# so we manually define them once in this list of vectors
headers <- list()
headers[['defense']] <- c('year', 'school', 'conf', 'class', 'pos', 'games', 'solo.tackes', 'ast.tackles', 'tackles', 'loss.tackles', 'sacks', 'int', 'int.yards', 'int.yards.avg', 'int.td', 'pd', 'fum.rec', 'fum.yds', 'fum.tds', 'fum.forced')
headers[['scoring']] <- c('year', 'school', 'conf', 'class', 'pos', 'games', 'td.rush', 'td.rec', 'td.int', 'td.fr', 'td.pr', 'td.kr', 'td.oth', 'td.tot', 'kick.xpm', 'kick.fgm', 'twopm', 'safety', 'total.pts')
headers[['punt_ret']] <- c('year', 'school', 'conf', 'class', 'pos', 'games', 'punt.returns', 'punt.return.yards', 'punt.return.avg', 'punt.return.td', 'kick.returns', 'kick.return.yards', 'kick.return.avg', 'kick.return.td')
headers[['receiving']] <- c('year', 'school', 'conf', 'class', 'pos', 'games', 'receptions', 'rec.yards', 'rec.avg', 'rec.td', 'rush.att', 'rush.yds', 'rush.avg', 'rush.td', 'scrim.plays', 'scrim.yds', 'scrim.avg', 'scrim.tds')
headers[['rushing']] <- # fixed the ordering of this
  c('year', 'school', 'conf', 'class', 'pos', 'games', 
    'rush.att', 'rush.yds', 'rush.avg', 'rush.td', 
    'receptions', 'rec.yards', 'rec.avg', 'rec.td', 
    'scrim.plays', 'scrim.yds', 'scrim.avg', 'scrim.tds')

headers[['passing']] <- c('year', 'school', 'conf', 'class', 'pos', 'games', 'completions', 'attempts', 'comp.pct', 'pass.yards', 'yards.per.attempt', 'adj.yards.per.attempt', 'pass.tds', 'pass.ints', 'int.rate')

# another user defined function to parse data
parse_pfr_tables <- function(tables) {
#  str(tables)
  results = list()
  for (tbl in tables) {
    id <- html_attr(tbl, 'id')
    if (id %in% names(headers)) {
      
      df <- html_table(tbl) %>%
        head(-1) %>% tail(-1)
# the multi college  like baker mayfield + russell wilson
# the total row is offset by 2 for them... painfully simple fix but it works
      df2 = tail(df,2)
      if (df2[1,1]=="Career" & df2[1,2]=="Overall") { 
        df = df[1:(nrow(df)-2),]
      }      
## end of fix
      if(ncol(df) == length(headers[[id]])) {
        colnames(df) <- headers[[id]]
      } else {
        next;
      }
      melted <- df %>%
        select(-year, -school, -conf, -class, -pos) %>%
        mutate(seasons = 1) %>%
        gather(stat, value) %>%
        mutate(stat = as.character(stat)) %>%
        filter(value != '') %>%
        mutate(value = as.numeric(value),
               section = id)
      results[[id]] <- melted
    }
  }
  bind_rows(results)
}

# so - on first run, this will be empty and it'll go from there...
# this block is getting college stats from pro-football-reference
if (!file.exists(paste(muh_stash, 'drafts.feather', sep=muh_slash))) {
  # 2018 draft hasnt happened yet and it's what we are trying to predict
  # so only read up till 2017... 
  draft.table <- data_frame(year = 2000:2018) %>%
    group_by(year) %>% do({
      url <- paste('https://www.pro-football-reference.com/years/', .$year, '/draft.htm', sep ='')
      doc <- read_html(url)
      html.table <- doc %>%
        html_nodes('table') %>%
        first
      urls <- html.table %>%
        html_nodes('tr td:nth-child(29)') %>%
        url.extract
      my.table <- html_table(html.table)
      colnames(my.table) <- draft.header
      my.table <- my.table %>%
        filter(pos != 'Pos') %>%
        mutate(url = urls)
      my.table
    }) %>%
    ungroup
  write_feather(draft.table, paste(muh_stash, 'drafts.feather', sep=muh_slash))
# this block may run for awhile... it's getting all the data from the web  
}

 
if (!file.exists(paste(muh_stash, 'combines.feather', sep=muh_slash)))   {
  
  combine.table <- data_frame(year = 2000:2018) %>%
    group_by(year) %>% do({
      url <- paste('https://www.pro-football-reference.com/draft/', .$year, '-combine.htm', sep ='')
      html.table <- read_html(url) %>%
        html_nodes('table') %>%
        first
      urls <- html.table %>%
        html_nodes('tr td:nth-child(4)') %>%
        url.extract
      my.table <- html_table(html.table)
      colnames(my.table) <- combine.header
      my.table <- my.table %>%
        filter(pos != 'Pos') %>%
        mutate(url = urls)
      my.table
    }) %>%
    ungroup

  # this is the carson wentz fix.  Him and some other small school players dont have a linkback 
  # in their pfr table... so if its missing i guess the url... gets most of em
  combine.table$hacked_url = paste("http://www.sports-reference.com/cfb/players/"
                                   ,gsub("'","",gsub("\\.","",gsub(" ","-",tolower(combine.table$player))))
                                   ,"-1.html", sep="")
  
  combine.table[is.na(combine.table$url),]$url = combine.table[is.na(combine.table$url),]$hacked_url
  combine.table = within(combine.table, rm(hacked_url))  
  write_feather(combine.table, paste(muh_stash, 'combines.feather', sep=muh_slash))
}

all.urls <- combine.table %>%
  select(url) %>%
  full_join(draft.table %>% select(url)) %>%
  filter(!is.na(url))

# some http need to go https... painfully strange problems if not fixed
all.urls$url <- gsub('http:', 'https:', all.urls$url)

college.stats <- all.urls %>%
  group_by(url) %>% do({
    doc <- read_html_cache(.$url)
    stats <- doc %>%
      html_nodes('table') %>%
      parse_pfr_tables
         if (nrow(stats) > 0) {
      stats <- stats %>%
        group_by(section, stat) %>%
        summarise(value = sum(value))
    }
    stats
  })

write_feather(college.stats, paste(muh_stash, 'college_stats.feather', sep=muh_slash))
