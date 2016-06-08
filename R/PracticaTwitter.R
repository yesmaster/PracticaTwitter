# Install/Execute needed libraries
loadLibraries()

# Authenticates user on Tweeter API
tweeterAuthentication()       

# Data frame creation
tuser <- twitteR::getUser("yesmastertweet")

# Analysis of Friends
friendsDataFrame <- getFriendsDataFrame(tuser)
friendsTopFollowersOfFriends <- getTopFollowers(friendsDataFrame)
friendsTopFriendsOfFriends <- getTopFriends(friendsDataFrame)
friendsTopTweetsOfFriends <- getTopTweets(friendsDataFrame)
    # Graphs
        # Compare statistics between Users
        # Friends following User?
        # Location

# Analysis of Followers
followersDataFrame <- getFollowersDataFrame(tuser)
followersTopFollowers <- getTopFollowers(followersDataFrame)
followersTopFriends <- getTopFriends(followersDataFrame)
followersTopTweets <- getTopTweets(followersDataFrame)
    # Graphs
        # Compare statistics between Users
        # Followers followed by User?
        # Location

# ¿To be included?: Users in common between 2 different usersDataFrame
# (compare 2 friendsDataFrame or 2 followersDataFrame)

# Analysis of tweets sent by User
tweetsOfUserDataFrame <- getUserTweetsDataFrame(user, 10)
    # Graph: evolution of tweets in time

# Analysis of tweets sent from Spain with defined hashtag
tweetsDataFrame <- getTweetsDataFrame("#hola", "40.418,-3.706,700km", 
                                      number=10, retryOnRateLimit=1)

    # To be included: Analysis of Keywords among tweets sent from Spain
    KeyWords()
    
    # ¿¿To be included??: Location of tweets sent from Spain


# FUNCTION: Paint location on Spain map
mapLocations <- function(usersDataFrame)  {
  map <- get_map(location = 'Spain', zoom = 6)
  coordinates <- geocode(userDataFrame$location)
  map <- ggmap(map) + geom_point(data=coordinates, aes(x=lon, y=lat),
                                 colour="coral1", size=3)
}


# FUNCTION: Search vector of keyworkds among tweets_matrix
# To be included




##############################################################

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