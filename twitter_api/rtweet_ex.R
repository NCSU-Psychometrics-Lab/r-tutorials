#connect to the Twitter API 

rm(list = ls()) #clean everything

#set working dir
#ctrl + shift + h

#install necessary packages and load via pacman library
if (!require("pacman")) install.packages("pacman")
pacman::p_load(devtools, rtweet, httpuv, tidyverse, tidytext, maps, ggthemes)

#-----------------------
#Twitter API connection
#----------------------

#view the link below for a vignette of rtweet package
# https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html

#name of app
appname = "blm_tweet_test"

## api key (to be updated w/ real key)
key = "0E4OOXXrAYx8im15B34loKz9R"

## api secret (example below is not a real key)
secret = "7Q6N87Elb3XRSuoI59IKO2o8LpejKHojNgquH0mKUm2JLwopAq"

## authentication api tokens
access_token = "538600108-w6VsifMHjKCW4gAJpDJfWTfCt3wxxFH7eFUbc8yI"
access_secret = "d7keUil8qekjvZMwPEJ3nmbsODkEXT02knMHUaCWOc1xV"

# create token to obtain access to api
twitter_token = create_token(
    app = appname,
    consumer_key = key,
    consumer_secret = secret,
    access_token = access_token,
    access_secret = access_secret)
# twitter_token

#query twets on 
blm_tweets = search_tweets(q = "#BLM", #search term
                           n = 200, #~number of tweets to query
                           include_rts = FALSE, #exclude retweets
                           #wake county, nc coordinates - 20mi radius
                           geocode = "35.78,-78.63,20mi")

#random display 10 of the quered tweets
sample(blm_tweets$text, 10)

#query users who tweeted about BLM - location invariate
blm_users = search_users(q = "#BLM", 
                         n = 200)

#randomly sample users and preview tweet text
sample(blm_users$text, 10) #clearly users aren't always tweeting about BLM!

#how many locations did we end up catching 
length(unique(blm_tweets$location)) #21 diffeent places!

blm_tweets %>%
    filter(!is.na(location)) %>% #rm missing but random characters still remain
    ggplot(aes(location)) + #column of interest
    geom_bar() + #chart type
    coord_flip() + #flip axes
    labs(x = "Count",
         y = "Location",
         title = "#BLM Tweet Locations", 
         subtitle = "GPS Coordinates: 20mi Radius of Wake County")

blm_users %>% 
    filter(!is.na(location)) %>% 
    ggplot(aes(location)) +
    geom_bar() + coord_flip() +
    labs(x = "Count",
         y = "Time Zone",
         title = "Twitter users - unique time zones ")

#check out words
data("stop_words")
head(stop_words, 10)

# remove http elements manually
blm_tweets$stripped_text =  gsub("http.*","",  blm_tweets$text)
blm_tweets$stripped_text = gsub("https.*","", blm_tweets$stripped_text)

blm_tweets_cln = blm_tweets %>% 
    dplyr::select(stripped_text) %>%
    unnest_tokens(word, stripped_text)

# remove stop words from your list of words
final_blm = blm_tweets_cln %>%
    anti_join(stop_words)

final_blm %>%
    count(word, sort = TRUE) %>%
    top_n(15) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    labs(y = "Count",
         x = "Unique words",
         title = "Count of unique words found in tweets",
         subtitle = "Stop words removed from the list")
