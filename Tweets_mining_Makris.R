library(twitteR)
library(ROAuth)
library(httr)
library(base64enc)
library(tidyverse)
library(rio)
library(readxl)
### API connection ####
Sys.setlocale("LC_CTYPE", "greek")
api_key = ""
api_secret = ""
access_token = ""
access_token_secret = ""

#download.file(url = "http://curl.haxx.se/ca/cacert.pem",
              #destfile = "C:/Users/User/Desktop/cacert.pem")

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


### Tweets Extraction####
twitter_search <- function(x){
  search.string <- x
  no.tweets <- 3200
  website_account <- userTimeline(search.string, no.tweets)
  df <- do.call("rbind",lapply(website_account, as.data.frame))
}
kathimerini <- twitter_search("@Kathimerini_gr")
protothema <- twitter_search("@protothema")
avgi <- twitter_search("@AvgiOnline")
ef_syn <- twitter_search("@EFSYNTAKTON")
ta_nea <- twitter_search("@ta_nea")
to_vima <- twitter_search("@tovimagr")
real <- twitter_search("@Real_gr")


### Tweets manipulation and storing####
twitter_accounts <- bind_rows(kathimerini, protothema, avgi, ef_syn, ta_nea, to_vima, real)
twitter_accounts <- twitter_accounts %>% filter(str_detect(created, "\\d{4}-02-\\d{2}"))
twitter_accounts <- twitter_accounts %>% mutate(urls = str_extract(text, "https.+"))
library(longurl)
expanded_urls <- expand_urls(twitter_accounts$urls)
twitter_accounts <- twitter_accounts %>% full_join(expanded_urls, by = c("urls" = "orig_url")) %>% distinct()
export(twitter_accounts, "News websites tweets-February.xlsx")  
















