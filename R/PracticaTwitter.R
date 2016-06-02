install.packages("twitteR")
library(twitteR)

# Twitter authentication configuration
consumer_key <- NULL
consumer_secret <- NULL
access_token <- NULL
access_token_secret <- NULL

twitter_conf()

#twitter.cnf.path = "data/twitter2.cnf"
#twitter.cnf <<- NULL
#twitter.cnf <- yaml::yaml.load_file(twitter.cnf.path)


# Twitter authentication
twitteR::setup_twitter_oauth(consumer_key,
                             consumer_secret,
                             access_token,
                             access_token_secret)


# Tweet
twitteR::tweet("bip...bip...")

# Are they following me?
x <- twitteR::friendships(screen_names = "notwhatsoever")

# Info about users with permission
tuser <- twitteR::getUser("yesmastertweet")
tusers <- twitteR::lookupUsers(c("yesmastertweet", "notwhatsoever"))
