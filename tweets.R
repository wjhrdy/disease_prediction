library(dplyr)
library(twitteR)
library(ggmap)
library(tidytext)
library(stringr)

# follow instructions here for setup: http://thinktostart.com/twitter-authentification-with-r/

# twitteR::setup_twitter_oauth(consumer_key = "abc",
#                              consumer_secret = "abc",
#                              access_token = "abc",
#                              access_secret = "abc")

top_words_related_to_query_near_location <- function(query = "rstats", 
                                                     location = "nc central university") {
  location <- ggmap::geocode(location)
  tweets <- twitteR::searchTwitter(query, 
                                   n=1000, 
                                   geocode = paste(location$lat[1], 
                                                   location$lon[1], 
                                                   "20mi",
                                                   sep = ","))
  tweet_text <- plyr::ldply(tweets, function(x) data.frame(text = x$getText()))
  replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|https"
  unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
  tidy_tweets <- tweet_text %>% 
    filter(!str_detect(text, "^RT")) %>%
    mutate(text = str_replace_all(text, replace_reg, "")) %>%
    unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
    filter(!word %in% stop_words$word,
           str_detect(word, "[a-z]"))
  frequency <- tidy_tweets %>% 
    count(word, sort = TRUE) %>% 
    mutate(total = n()) %>%
    mutate(freq = n/total)
  return(frequency)
}

top <- top_words_related_to_query_near_location("rstats", "nc central university")
