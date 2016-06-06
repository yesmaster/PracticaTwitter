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

fr_num <- tuser$getFriendsCount() # num of friends
fr_ids <- tuser$getFriendIDs() # List of friends IDs
friends <- friendships(user_ids=fr_ids) # data.frame with friends information
tfriends <- lookupUsers(friends$name) # tuser structures with friends information 

fol_num <- tuser$getFollowersCount()  # num of followers
fol_names <- tuser$getFollowers() # List of followers names
followers <- friendships(user_ids=fol_ids) # data.frame with followers information
tfollowers <- lookupUsers(followers$name) # followers information extraction

# Store 20 tweets per friend + friend ID + friend ScreenName
tuser <- twitteR::getUser("yesmastertweet") 
ttweets <- userTimeline(tuser, n=20)

db_tweets <- data.frame ("id" = 1:fr_num, "usr" = 1:fr_num, "mssg1" = 1:fr_num, "mssg2" = 1:fr_num)
for (i in 1:fr_num){  # for 'i' friends
  ttweets <- userTimeline(tfriends[[i]], n=20)  # load 20 tweets from friend 'i'
  db_tweets[[1]][[i]] <- ttweets[[i]]$id  # copy id to col 1
  db_tweets[[2]][[i]] <- ttweets[[i]]$screenName # copy ScreenName to col 2
  for (j in 1:1)  {
    db_tweets[[2+j]][[i]] <- ttweets[[j]]$getText()
  }
}


db_tweets <- data.frame(id = 1:fr_num, screenName = c(1:fr_num))
for (i in 1:5) db_tweets<- ttweets[[i]]



db_tweets[[1]][[1]] <- tuser$getId()
db_tweets[[1]][[3]] <- tuser$getScreenName()
tuser$getId()

for (i in fr_num) { # For every friend
  ttweets <- userTimeline(tuser, n=20)
  db_tweets[[i]][[1]] <- tuser$getId()
  for (j in 1:20) {  # Last 20 tweets
    db_tweets[i][j] <- ttweets[[j]]  # Last tweets
  }
} 

ttweets <- userTimeline(tuser, n=20)

ttweets[[1]]$getCreated() # Tweet date
ttweets[[1]]$getId()  # Tweet id
ttweets[[1]]$getScreenName()  # Tweet ScreenName (@ScreenName)

##############################################################

# Search friends & friends of friends -> create a tree
# Look for a user, its friends, their tweets, search a key word & extract results
# Location??

# User information extraction
tuser <- twitteR::getUser("yesmastertweet") 

# Lists of friends/followers
fr_names <- tuser$getFriends() # List of friends names
fr_ids <- tuser$getFriendIDs() # List of friends IDs
fol_names <- tuser$getFollowers() # List of followers names
fol_ids <- tuser$getFollowerIDs()  # List of followers IDs
usr_info <- tuser$toDataFrame("notwhatsoever", optional=FALSE) # Information from 1 user

# Friends information extraction
fr_num <- tuser$getFriendsCount() # num of friends
fr_ids <- tuser$getFriendIDs() # chr[] with friends IDs
friends <- friendships(user_ids=fr_ids) # data.frame with friends information
tfriends <- lookupUsers(friends$name) # tuser structures with friends information 

# Followers information extraction
fol_num <- tuser$getFollowersCount()
fol_ids <- tuser$getFollowerIDs() # chr[] with followers IDs
followers <- friendships(user_ids=fol_ids) # data.frame with followers information
tfollowers <- lookupUsers(followers$name) # followers information extraction


ttweets <- userTimeline(tuser, n=20)  # Last tweets
ttweets[[1]]$getCreated() # Tweet date
ttweets[[1]]$getId()  # Tweet id
ttweets[[1]]$getScreenName()  # Tweet ScreenName (@ScreenName)
twitteR::tweet("bip...bip...")  # Tweet
searchTwitter("http", n=10, since = "2014-06-04") #links search on Tweeter

