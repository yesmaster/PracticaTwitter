loadLibraries()
tweeterAuthentication()

fillMatrixOfUsers(x = "yesmastertweet")
tweets_matrix <- fillMatrixOfTweets()
KeyWordsVector()

# FUNCTION: Store 20 tweets per friend + friend ID + friend ScreenName
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
      if(i%%10==0){
        Sys.sleep(15*60)
      }
    }
  }
}

# FUNCTION: Search vector of keyworkds among tweets_matrix


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


#########################################
library(plyr)

# Data frame creation
usersData <- list()
for(i in 1:fr_num){
  usersData[[i]] <- data.frame(getUser(tuser$getFollowers()[[i]])$toDataFrame())
  #usersData[[i]] <- data.frame(getUser(tuser$getFriends()[[i]])$toDataFrame())
}
usersFrame <- ldply(usersData, rbind)

getTopFollowers <- function(usersFrame){
  ordredFrame <- usersFrame[with(usersFrame, order(-followersCount)),]
  return(data.frame(user = ordredFrame$screenName, followers = ordredFrame$followersCount))
}

getTopFriends <- function(usersFrame){
  ordredFrame <- usersFrame[with(usersFrame, order(-friendsCount)),]
  return(data.frame(user = ordredFrame$screenName, friends = ordredFrame$friendsCount))
}

getTopTweets <- function(users){
  ordredFrame <- usersFrame[with(usersFrame, order(-statusesCount)),]
  return(data.frame(user = ordredFrame$screenName, statuses = ordredFrame$statusesCount))
}
