# Install/Execute needed libraries
loadLibraries()

# Authenticates user on Tweeter API
tweeterAuthentication()       

# Data frame creation
tuser <- twitteR::getUser("yesmastertweet")
tuserPp <- twitteR::getUser("marianorajoy")
tuserPsoe <- twitteR::getUser("sanchezcastejon")

# Analysis of Friends
friendsDataFrame <- getFriendsDataFrame(tuser)
friendsTopFollowersOfFriends <- getTopFollowers(friendsDataFrame)
friendsTopFriendsOfFriends <- getTopFriends(friendsDataFrame)
friendsTopTweetsOfFriends <- getTopTweets(friendsDataFrame)
  # Graphs
  # Distribution of users according activity
  hist(friendsDataFrame$statusesCount, col = "blue", breaks = 10)
  # Table with Tops information 
  library(xtable)
  table <- cbind(head(friendsTopFollowersOfFriends, n=3), 
                 head(friendsTopFriendsOfFriends, n=3), 
                 head(friendsTopTweetsOfFriends, n=3))
  names(table) <- c("Usuarios", "Followers", "Usuarios", 
                    "Friends", "Usuarios", "Twits")
  xt <- xtable(table)
  print(xt, type = "html")

  # Include?: Friends following User
  
  # Location
  mapLocations(friendsDataFrame)

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
tweetsOfUserDataFrame <- getUserTweetsDataFrame(tuser, 10)
tweetsOfUserDataFramePp <- getUserTweetsDataFrame(tuserPp, 10)
tweetsOfUserDataFramePsoe <- getUserTweetsDataFrame(tuserPsoe, 10)
    # Graphs:
    # Evolution of tweets in time
    library("stats")
    qplot(x = created, data = tweetsOfUserDataFrame, geom = "density", 
          colour = I("black"))
    qplot(x = created, data = tweetsOfUserDataFramePp, geom = "density", 
          colour = I("blue"))
    qplot(x = created, data = tweetsOfUserDataFramePp, geom = "density", 
          colour = I("red"))

# Analysis of tweets sent from Spain with defined hashtag
tweetsDataFrame <- getTweetsDataFrame("PSOE", "40.418,-3.706,700km", 
                                      number=100)

    # To be included: Analysis of Keywords among tweets sent from Spain
    KeyWords()
    
    # ¿¿To be included??: Location of tweets sent from Spain


# FUNCTION: Paint location on Spain map
mapLocations <- function(usersDataFrame)  {
  map <- get_map(location = 'Spain', zoom = 6)
  coordinates <- geocode(usersDataFrame$location)
  map <- ggmap(map) + geom_point(data=coordinates, aes(x=lon, y=lat),
                                 colour="blue", size=3)
  map
}


# FUNCTION: Search vector of keyworkds among tweets_matrix





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