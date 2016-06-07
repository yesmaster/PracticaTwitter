# FUNCTION: Load libraries to use
loadLibraries <- function(){
  if (!require("twitteR")) {
    install.packages("twitteR")
    library("twitteR")
  }
  if (!require("igraph")){
    install.packages("igraph")
    library("igraph")
  }
  if (!require("yaml")){
    install.packages("yaml")
    library("yaml")
  } 
  if (!require("plyr")){
    library("plyr")
  }
}

# FUNCTION: User authentication for Tweeter
tweeterAuthentication <- function(){
  auth = yaml :: yaml.load_file("data/auth.yml") # Load authentication file
  
  consumer_key <- auth$twitter_auth$consumer_key
  consumer_secret <- auth$twitter_auth$consumer_secret
  access_token <- auth$twitter_auth$access_token
  access_token_secret <- auth$twitter_auth$access_token_secret
  
  options(httr_oauth_cache=T) #This will enable the use of a local file to cache OAuth access credentials between R sessions.
  setup_twitter_oauth(consumer_key,
                      consumer_secret,
                      access_token,
                      access_token_secret)
}

# FUNCTION: Users structure creation
fillMatrixOfUsers <- function(x) {# User information extraction
  tuser <- twitteR::getUser(x) 
  friends_num <- tuser$getFriendsCount() # num of friends
  friends_ids <- tuser$getFriendIDs() # List of friends IDs
  friends <- friendships(user_ids=friends_ids) # data.frame with friends information
  
  # option 1 to generate tfriends
  
  tfriends <- lookupUsers(friends$name) # tuser structures with friends information 
  # option 2 to generate tfriends
  for(i in 1:friends_num)  {
    tfriends[i] <<- twitteR::getUser(friends[[2]][[i]])
  }
} 

getFollowersDataFrame <- function(tuser){
  usersData <- list()
  f_num <- tuser$getFollowersCount()
  for(i in 1: f_num){
    usersData[[i]] <- data.frame(getUser(tuser$getFollowers()[[i]])$toDataFrame())
  }
  usersFrame <- ldply(usersData, rbind)
  return(cbind(usersFrame, friendships(usersFrame$screenName)[4:5]))
}

getFriendsDataFrame <- function(tuser){
  usersData <- list()
  fr_num <- tuser$getFriendsCount()
  for(i in 1:fr_num){
    usersData[[i]] <- data.frame(getUser(tuser$getFriends()[[i]])$toDataFrame())
  }
  usersFrame <- ldply(usersData, rbind)
  return(cbind(usersFrame, friendships(usersFrame$screenName)[4:5]))
}

# Top functions
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

# FUNCTION: Vector of bad language
KeyWordsVector <- c("atontado", "baboso", "besugo", "bobo", "burro", "capullo",
                    "cazurro", "ceporro", "cenutrio", "cipote", "cretino",
                    "cutre", "chorra", "estúpido", "ganso", "gilipollas",
                    "idiota", "lerdo", "malparido", "marica", "maricon",
                    "memo", "mentecato", "pendejo", "percebe", "puto", "puta",
                    "tarado", "tarugo", "tolondron", "tontarrón", "tonto", "torpe",
                    "zángano")
