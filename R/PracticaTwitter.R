if (!require("twitteR")) {
  install.packages("twitteR")
  library("twitteR")
}
if (!require("igraph")){
  install.packages("igraph")
  library("igraph")
}

library("yaml")
auth = yaml.load_file("data/auth.yml") # Load authentication file

consumer_key <- auth$twitter_auth$consumer_key
consumer_secret <- auth$twitter_auth$consumer_secret
access_token <- auth$twitter_auth$access_token
access_secret <- auth$twitter_auth$access_secret
options(httr_oauth_cache=T) #This will enable the use of a local file to cache OAuth access credentials between R sessions.
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

# User information extraction
tuser <- twitteR::getUser("yesmastertweet") 

fr_num <- tuser$getFriendsCount() # num of friends
fr_ids <- tuser$getFriendIDs() # List of friends IDs
friends <- friendships(user_ids=fr_ids) # data.frame with friends information

for(i in 1:length(friends)){
  print(tuser$toDataFrame(friends[i]$screen_name, optional=FALSE))
}

tfriends <- lookupUsers(friends$name) # tuser structures with friends information 

fol_num <- tuser$getFollowersCount()  # num of followers
fol_names <- tuser$getFollowers() # List of followers names
followers <- friendships(user_ids=fol_ids) # data.frame with followers information
tfollowers <- lookupUsers(followers$name) # followers information extraction

# Store 20 tweets per friend + friend ID + friend ScreenName
tuser <- twitteR::getUser("yesmastertweet") 
ttweets <- twitteR::userTimeline(tuser, n=20)

db_tweets <- data.frame ("id" = 1:fr_num, 
                         "usr" = 1:fr_num, 
                         "mssg1" = 1:fr_num, 
                         "mssg2" = 1:fr_num)


 
fillMatrixOfTweets  <- function() {
  mat <- matrix(nrow = fr_num, ncol=4)
  for (i in 1:fr_num){  # for 'i' friends
    if(!tfriends[[i]]$protected){
      ttweets <- userTimeline(tfriends[[i]], n=4)  # load 20 tweets from friend 'i'
      if(length(ttweets) > 0){
        for (j in 1:length(ttweets))  {
          print(paste("friend", i, sep= " "))
          print(paste("tweet", j, length(ttweets), sep= " "))
          mat[i,j] <- ttweets[[j]]$getText()
        }
      }
      if(i%%5==0){
        Sys.sleep(10)
      }
    }
  }
}

tweets_matrix <- fillMatrixOfTweets()
########################

for (i in 1:fr_num){  # for 'i' friends
  ttweets <- userTimeline(tfriends[[i]], n=20)  # load 20 tweets from friend 'i'
  db_tweets[[1]][[i]] <- ttweets[[i]]$id  # copy id to col 1
  db_tweets[[2]][[i]] <- ttweets[[i]]$screenName # copy ScreenName to col 2
  for (j in 1:2)  {
    if (!ttweets[[j]]$getText()) db_tweets[[2+j]][[i]] <- ttweets[[j]]$getText()
  }
}

tweets <- list()
for(i in 1:length(ttweets)){
  tweets <- c(ttweets[[i]], tweets)
}

db_tweets <- data.frame ()



########################


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
searchTwitter("#podemos", geocode = "41.38,2.115,5km", n=70, retryOnRateLimit=1) #links search on Tweeter

