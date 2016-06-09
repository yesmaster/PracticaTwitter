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
  if (!require("ggmap")){
    library("ggmap")
  }  
  if (!require("mapproj")){
    library("mapproj")
  }  
}

# FUNCTION: User authentication on the Tweeter API
tweeterAuthentication <- function(){
  auth = yaml :: yaml.load_file("data/auth.yml") # Load authentication config file
  
  consumer_key <- auth$twitter_auth$consumer_key
  consumer_secret <- auth$twitter_auth$consumer_secret
  access_token <- auth$twitter_auth$access_token
  access_token_secret <- auth$twitter_auth$access_token_secret
  
  options(httr_oauth_cache=T) # Enables cache OAuth access credentials between R sessions
  setup_twitter_oauth(consumer_key,
                      consumer_secret,
                      access_token,
                      access_token_secret)
}

# FUNCTION: Data Frame creation with information of Followers from "tuser"
getFollowersDataFrame <- function(tuser){
  usersData <- list()
  CONST_FNUM <- 4
  followers <- tuser$getFollowers(n = CONST_FNUM)
  for(i in 1: length(followers)){
    usersData[[i]] <- data.frame(getUser(followers[[i]])$toDataFrame())
  }
  usersFrame <- ldply(usersData, rbind)
  return(cbind(usersFrame, friendships(usersFrame$screenName)[4:5]))
}

# FUNCTION: Data Frame creation with information of Friends from "tuser"
getFriendsDataFrame <- function(tuser){
  usersData <- list()
  CONST_FRNUM <- 6
  friends <- tuser$getFriends(CONST_FRNUM)
  for(i in 1:length(friends)){
    usersData[[i]] <- data.frame(getUser(friends[[i]])$toDataFrame())
  }
  usersFrame <- ldply(usersData, rbind)
  return(cbind(usersFrame, friendships(usersFrame$screenName)[4:5]))
}

# FUNCTIONS: Top results from a users Data Frame
# Most popular users
getTopFollowers <- function(usersFrame){
  ordredFrame <- usersFrame[with(usersFrame, order(-followersCount)),]
  return(data.frame(user = ordredFrame$screenName, followers = ordredFrame$followersCount))
}
# Most follower users
getTopFriends <- function(usersFrame){
  ordredFrame <- usersFrame[with(usersFrame, order(-friendsCount)),]
  return(data.frame(user = ordredFrame$screenName, friends = ordredFrame$friendsCount))
}
# Most active users
getTopTweets <- function(usersFrame){
  ordredFrame <- usersFrame[with(usersFrame, order(-statusesCount)),]
  return(data.frame(user = ordredFrame$screenName, statuses = ordredFrame$statusesCount))
}

# FUNCTION: Store 20 tweets per friend + friend ID + friend ScreenName
fillMatrixOfTweets  <- function(usersFrame, tweetsNumber) {
  mat <- matrix(nrow = dim(usersFrame)[1], ncol=tweetsNumber)
  for (i in 1:dim(usersFrame)[1]){  # for 'i' friends
    if(!usersFrame$protected[[i]]){
      ttweets <- userTimeline(usersFrame$screenName[[i]], n=tweetsNumber, includeRts = TRUE)  # load 20 tweets from friend 'i'
      if(length(ttweets) > 0){
        for (j in 1:length(ttweets))  {
          mat[i,j] <- ttweets[[j]]$getText()
        }
      }
      # if(i%%10==0){
      #    Sys.sleep(15*60)
      #  }
    }
  }
  return(mat)
}

# FUNCTION: Data Frame creation with "number" Tweets from a "user"
getUserTweetsDataFrame <- function(user, number)  {
  tweets<-userTimeline(user, n=number, includeRts = TRUE)
  tweetsData <- list()
  for(i in 1:length(tweets)){
    tweetsData[[i]] <- data.frame(tweets[[i]]$toDataFrame())
    print(i)
    if(i%%11==0){
        Sys.sleep(15*60)
    }
  }
  return(ldply(tweetsData, rbind))
}

# FUNCTION: Data Frame cration with "number" tweets including "textToSeach"
# sent by users within "geocode" locations
getTweetsDataFrame <- function(textToSearch, geocode, number){
  tweets<-searchTwitter(textToSearch, geocode = geocode, n=number, retryOnRateLimit=1) #links search on Tweeter
  tweetsData <- list()
  for(i in 1:length(tweets)){
    tweetsData[[i]] <- data.frame(tweets[[i]]$toDataFrame())
  }
  return(ldply(tweetsData, rbind))
}

getTweetsWithKeyword <- function(tweetsDataFrame, keyWordsList){
  resTweets <- list()
  matches <- lapply(tweetsDataFrame$text, function(i) grep(pattern = paste("*",keyWordsList[i],"*"), tweet, ignore.case = TRUE))
  print(matches)
  Sys.sleep(time = 5)
  if(length(matches)>1){
    print("Match!")      
  }
  return(resTweets)
}

getTweetsWithKeyword <- function(tweetsDataFrame, keyWordsList){
  resTweets <- data.frame()
  for(tweet in tweetsDataFrame$text){
    wordsFound <- list()
    for(keyWord in keyWordsList){
      regExp <- paste("*",keyWord,"*")
      res <- grep(pattern = regExp, tweet, ignore.case = TRUE)
      if(length(res)>0){
        #print(paste("Match with ", keyWord, " ", tweet, sep = ""))
        wordsFound <- c(wordsFound, keyWord)
      }
    }
    if(length(wordsFound)>0){
      resTweets <- rbind(resTweets, data.frame(tweet = tweet, number = length(wordsFound)))
    }
  }
  return(resTweets)
}

plotReciprocalFollowsGraph <- function(me, userDataFrame){
  v1 <- vector()
  v2 <- vector()
  for (i in 1:dim(userDataFrame)[1]) {
    row <- userDataFrame[i,]
    print(row$following)
    if(row$following == TRUE && row$followed_by == TRUE){
      v1 <- c(me$name, row$screenName, v1)
      v2 <- c(row$screenName, me$name, v2)
    }
  }
  reciprocalGraph <- graph.data.frame(data.frame(v1 = v1, v2 = v2))
  plot(reciprocalGraph)
}
