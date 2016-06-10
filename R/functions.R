
#' Title twitterAuthentication
#' 
#' User authentication on the Twitter API
#'
#' @return
#' @export
#'
#' @examples
twitterAuthentication <- function(){
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


#' Title getFollowersDataFrame
#' 
#' Data Frame creation with information of Followers from "tuser"
#'
#' @param tuser 
#'
#' @return
#' @export
#'
#' @examples
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


#' Title getFriendsDataFrame
#' 
#' Data Frame creation with information of Friends from "tuser"
#'
#' @param tuser 
#'
#' @return
#' @export
#'
#' @examples
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


#' Title getTopFollowers
#' 
#' Top results from a users Data Frame (Most popular users)
#'
#' @param usersFrame 
#'
#' @return
#' @export
#'
#' @examples
getTopFollowers <- function(usersFrame){
  ordredFrame <- usersFrame[with(usersFrame, order(-followersCount)),]
  return(data.frame(user = ordredFrame$screenName, followers = ordredFrame$followersCount))
}


#' Title getTopFriends
#' 
#' Most follower users
#'
#' @param usersFrame 
#'
#' @return
#' @export
#'
#' @examples
getTopFriends <- function(usersFrame){
  ordredFrame <- usersFrame[with(usersFrame, order(-friendsCount)),]
  return(data.frame(user = ordredFrame$screenName, friends = ordredFrame$friendsCount))
}


#' Title getTopTweets
#' 
#' Most active users
#'
#' @param usersFrame 
#'
#' @return
#' @export
#'
#' @examples
getTopTweets <- function(usersFrame){
  ordredFrame <- usersFrame[with(usersFrame, order(-statusesCount)),]
  return(data.frame(user = ordredFrame$screenName, statuses = ordredFrame$statusesCount))
}


#' Title fillMatrixOfTweets
#' 
#' Store 20 tweets per friend + friend ID + friend ScreenName
#'
#' @param usersFrame 
#' @param tweetsNumber 
#'
#' @return
#' @export
#'
#' @examples
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


#' Title getUserTweetsDataFrame
#' 
#' Data Frame creation with "number" Tweets from a "user"
#'
#' @param user 
#' @param number 
#'
#' @return
#' @export
#'
#' @examples
getUserTweetsDataFrame <- function(user, number)  {
  tweets<-userTimeline(user, n=number, includeRts = TRUE)
  tweetsData <- list()
  for(i in 1:length(tweets)){
    tweetsData[[i]] <- data.frame(tweets[[i]]$toDataFrame())
    if(i%%11==0){
        Sys.sleep(15*60)
    }
  }
  return(ldply(tweetsData, rbind))
}


#' Title getTweetsDataFrame
#' 
#' Data Frame cration with "number" tweets including "textToSeach" sent by users within "geocode" locations
#'
#' @param textToSearch 
#' @param geocode 
#' @param number 
#'
#' @return
#' @export
#'
#' @examples
getTweetsDataFrame <- function(textToSearch, geocode, number){
  tweets<-searchTwitter(textToSearch, geocode = geocode, n=number, retryOnRateLimit=1) #links search on Tweeter
  tweetsData <- list()
  for(i in 1:length(tweets)){
    tweetsData[[i]] <- data.frame(tweets[[i]]$toDataFrame())
  }
  return(ldply(tweetsData, rbind))
}

#' Title getTweetsWithKeyword
#'
#' @param tweetsDataFrame 
#' @param keyWordsList 
#'
#' @return
#' @export
#'
#' @examples
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

#' Title plotReciprocalFollowsGraph
#'
#' @param me 
#' @param userDataFrame 
#'
#' @return
#' @export
#'
#' @examples
plotReciprocalFollowsGraph <- function(me, userDataFrame){
  v1 <- vector()
  v2 <- vector()
  for (i in 1:dim(userDataFrame)[1]) {
    row <- userDataFrame[i,]
    if(row$following == TRUE && row$followed_by == TRUE){
      v1 <- c(me$name, row$screenName, v1)
      v2 <- c(row$screenName, me$name, v2)
    }
  }
  if(length(v1) > 0 && length(v2) > 0){
    reciprocalGraph <- graph.data.frame(data.frame(v1 = v1, v2 = v2))
    plot(reciprocalGraph)
  }
  else return("NA")
}



#' Title mapLocations
#'
#' @param ... 
#' @param colours 
#'
#' @return
#' @export
#'
#' @examples
mapLocations <- function(..., colours)  {
  dataFrames <- list(...)
  map <- get_map(location = 'Spain', zoom = 6)
  map <- ggmap(map)
  for(i in 1:length(dataFrames)){
    coordinates <- geocode(dataFrames[[i]]$location)
    geom <- geom_point(data=coordinates, aes(x=lon, y=lat), colour=colours[i], size=3)
    map <- map + geom
  }
  map
}

