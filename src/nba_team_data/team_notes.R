setwd("~/Documents/fantasy-basketball/data/nba_player_stats_by_team")
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.delim)


setwd("~/Documents/fantasy-basketball/data/nba_player_stats_by_team/ATL/")
tbl <- list.files(pattern = "*.csv", 
                  full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c")) %>% mutate(year = 2020)) 


# little messy, but this works fine 
setwd("~/Documents/fantasy-basketball/data/nba_player_stats_by_team/ATL/")
files = list.files(pattern = "*.csv")
data2=lapply(files, read.table, header=FALSE, sep=",")
for (i in 1:length(data2)){data2[[i]]<-cbind(data2[[i]],files[i])}
data_rbind <- do.call("rbind", data2) 
# then move first row to col_names 
# filter out all rows with V1 = Rk 
# split text in team/year column 
# bind coach, etc data 



##
setwd("~/Documents/fantasy-basketball/data/nba-player-data/ATL/")
rawHTML <- paste(readLines("ATL_1949_1950.html"), collapse="\n")
#format_from_signature("ATL_1950_1951.xls")
#setwd("~/Documents/fantasy-basketball/data/")

check <- read_csv("ATL_1949_1950.csv")

df = read.table("ATL_1950_1951.xls")

tbl_fread <- 
  list.files(pattern = "*.csv") %>% 
  map_df(~fread(.))




nbap_200708 <- read_csv("nba_200708pts_totals.csv") %>% clean_names() %>% mutate(year = 2008)
nbap_200809 <- read_csv("nba_200809pts_totals.csv") %>% clean_names() %>% mutate(year = 2009)
nbap_200910 <- read_csv("nba_200910pts_totals.csv") %>% clean_names() %>% mutate(year = 2010)
nbap_201011 <- read_csv("nba_201011pts_totals.csv") %>% clean_names() %>% mutate(year = 2011)
nbap_201112 <- read_csv("nba_201112pts_totals.csv") %>% clean_names() %>% mutate(year = 2012)
nbap_201213 <- read_csv("nba_201213pts_totals.csv") %>% clean_names() %>% mutate(year = 2013)
nbap_201314 <- read_csv("nba_201314pts_totals.csv") %>% clean_names() %>% mutate(year = 2014)
nbap_201415 <- read_csv("nba_201415pts_totals.csv") %>% clean_names() %>% mutate(year = 2015)
nbap_201516 <- read_csv("nba_201516pts_totals.csv") %>% clean_names() %>% mutate(year = 2016)
nbap_201617 <- read_csv("nba_201617pts_totals.csv") %>% clean_names() %>% mutate(year = 2017)
nbap_201718 <- read_csv("nba_201718pts_totals.csv") %>% clean_names() %>% mutate(year = 2018)
nbap_201819 <- read_csv("nba_201819pts_totals.csv") %>% clean_names() %>% mutate(year = 2019)
nbap_201920 <- read_csv("nba_201920pts_totals.csv") %>% clean_names() %>% mutate(year = 2020)