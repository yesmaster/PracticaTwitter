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

# User information extraction
tuser <- twitteR::getUser("yesmastertweet") 

# Lists of friends/followers
fr_names <- tuser$getFriends() # List of friends names
fr_ids <- tuser$getFriendIDs() # List of friends IDs
fol_names <- tuser$getFollowers() # List of followers names
fol_ids <- tuser$getFollowerIDs()  # List of followers IDs
usr_info <- tuser$toDataFrame("notwhatsoever", optional=FALSE) # Information from 1 user

# Friends information extraction
fr_num <- tuser$getriendsCount()
fr_ids <- tuser$getFriendIDs() # chr[] with friends IDs
friends <- friendships(user_ids=fr_ids) # data.frame with friends information
tfriends <- lookupUsers(friends$name) # friends information extraction 

# Followers information extraction
fol_num <- tuser$getFollowersCount()
fol_ids <- tuser$getFollowerIDs() # chr[] with followers IDs
followers <- friendships(user_ids=fol_ids) # data.frame with followers information
tfollowers <- lookupUsers(followers$name) # followers information extraction


ttweets <- userTimeline(tuser, n=20)  # Last tweets
ttweets[[1]]$getCreated() # Tweet date
ttweets[[1]]$getId()  # Tweet id
twitteR::tweet("bip...bip...")  # Tweet

##############################################################

# Search friends & friends of friends -> create a tree
# Look for a user, its friends, their tweets, search a key word & extract results
# Location??


