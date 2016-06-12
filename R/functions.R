#' Title twitterAuthentication
#' 
#' User authentication on the Twitter API with the auth.yml file credentials
#'
#' @return
#' @export
#'
#' @examples
twitterAuthentication <- function(){
  library(twitteR)
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
#' @param tuser Twitter user (using getUser("name"))
#' @param number Number of followers
#'
#' @return dataframe with all information available of the followers of a user 'tuser'
#' @export
#'
#' @examples getFollowersDataFrame(getUser("dummy"), 20)
getFollowersDataFrame <- function(tuser, number){
  usersData <- list()
  followers <- tuser$getFollowers(n = number)
  for(i in 1: length(followers)){
    usersData[[i]] <- data.frame(getUser(followers[[i]])$toDataFrame())
    #if(i%%10 == 0) { # Avoid Twitter API Rate limits (wait 15 min / 10 followers)
    #  Sys.sleep(16*60)
    #}
  }
  usersFrame <- ldply(usersData, rbind)
  return(cbind(usersFrame, friendships(usersFrame$screenName)[4:5]))
}


#' Title getFriendsDataFrame
#' 
#' Data Frame creation with information of Friends from "tuser"
#'
#' @param tuser Twitter user (using getUser("name"))
#' @param number Number of friends
#'
#' @return data frame with all information about the friends of 'tuser'
#' @export
#'
#' @examples getFriendsDataFrame(getUser("dummy"), 20)
getFriendsDataFrame <- function(tuser, number){
  usersData <- list()
  friends <- tuser$getFriends(number)
  for(i in 1:length(friends)){
    usersData[[i]] <- data.frame(getUser(friends[[i]])$toDataFrame())
    #if(i%%10 == 0) { # Avoid Twitter API Rate limits (wait 15 min / 10 followers)
    #  Sys.sleep(16*60)
    #}    
  }
  usersFrame <- ldply(usersData, rbind)
  return(cbind(usersFrame, friendships(usersFrame$screenName)[4:5]))
}


#' Title getTopFollowers
#' 
#' Top followers from a users Data Frame (Most popular users / Most followers)
#'
#' @param usersFrame users data frame
#'
#' @return data frame with name/num of followers ofthe followers of a user ordered
#' ascending by the number of followers
#' @export
#'
#' @examples getTopFollowers(getFollowersDataFrame(getUser("dummy")))
getTopFollowers <- function(usersFrame){
  ordredFrame <- usersFrame[with(usersFrame, order(-followersCount)),]
  return(data.frame(user = ordredFrame$screenName, followers = ordredFrame$followersCount))
}


#' Title getTopFriends
#' 
#' Top friends of followers
#'
#' @param usersFrame users data frame
#'
#' @return data frame with name/num of friends of the followers of a user ordered
#' ascending by the number of friends
#' @export
#'
#' @examples getTopFriends(getFollowersDataFrame(getUser("dummy")))
getTopFriends <- function(usersFrame){
  ordredFrame <- usersFrame[with(usersFrame, order(-friendsCount)),]
  return(data.frame(user = ordredFrame$screenName, friends = ordredFrame$friendsCount))
}


#' Title getTopTweets
#' 
#' Most active users (followers with more tweets)
#'
#' @param usersFrame useres data frame
#'
#' @return data frame with name/num of tweets of the followers of a user ordered
#' ascending by the number of tweets
#' @export
#'
#' @examples getTopTweets(getFollowersDataFrame(getUser("dummy")))
getTopTweets <- function(usersFrame){
  ordredFrame <- usersFrame[with(usersFrame, order(-statusesCount)),]
  return(data.frame(user = ordredFrame$screenName, statuses = ordredFrame$statusesCount))
}


#' Title fillMatrixOfTweets
#' 
#' Store 20 tweets per friend + friend ID + friend ScreenName
#'
#' @param usersFrame users data frame
#' @param tweetsNumber number of tweets to extract
#'
#' @return matrix with the screen name and its twets depending on the number
#' of 'tweetsNumber'
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
      #if(i%%10==0){
      #    Sys.sleep(15*60)
      #}
    }
  }
  return(mat)
}


#' Title getUserTweetsDataFrame
#' 
#' Data Frame creation with "number" Tweets from a "user"
#'
#' @param user user data frame
#' @param number number of tweets to extract
#'
#' @return data frame of 'number' of tweets with all their relevant information
#' @export
#'
#' @examples getUserTweetsDataFrame(getUser("dummy"), number = 100)
getUserTweetsDataFrame <- function(user, number)  {
  tweets<-userTimeline(user, n=number, includeRts = TRUE)
  tweetsData <- list()
  for(i in 1:length(tweets)){
    tweetsData[[i]] <- data.frame(tweets[[i]]$toDataFrame())
    #if(i%%100 == 0) {
    #    Sys.sleep(16*60)
    #}
  }
  return(ldply(tweetsData, rbind))
}


#' Title getTweetsDataFrame
#' 
#' Data Frame creation with "number" tweets including "textToSeach" sent by users within "geocode" locations
#'
#' @param textToSearch String with a text to search on a tweet
#' @param geocode Geolocalization string
#' @param number Number of tweets
#'
#' @return data frame of 'number' of tweets containing 'textToSearch' with all their relevant information
#' @export
#'
#' @examples getTweetsDataFrame("str", ""40.2,-3.71,700km", "100")
getTweetsDataFrame <- function(textToSearch, geocode, number){
  tweets<-searchTwitter(textToSearch, geocode = geocode, n=number, retryOnRateLimit=1) #links search on Tweeter
  tweetsData <- list()
  for(i in 1:length(tweets)){
    tweetsData[[i]] <- data.frame(tweets[[i]]$toDataFrame())
    #if(i%%100 == 0){
    #  Sys.sleep(16*60)
    #}
  }
  return(ldply(tweetsData, rbind))
}

#' Title getTweetsWithKeyword
#'
#' Analizes a data frame of tweets searching for some words in a dictionary/list
#'
#' @param tweetsDataFrame data frame of tweets to analyze
#' @param keyWordsList list containing some words
#'
#' @return data frame of tweets containing words of a dictionary 'keyWordsList'
#' @export
#'
#' @examples getTweetsWithKeyword(getTweetsDataFrame(getUser("dummy")), c("a","b"))
getTweetsWithKeyword <- function(tweetsDataFrame, keyWordsList){
  resTweets <- data.frame()
  for(tweet in tweetsDataFrame$text){
    wordsFound <- list()
    for(keyWord in keyWordsList){
      regExp <- paste("*",keyWord,"*")
      res <- grep(pattern = regExp, tweet, ignore.case = TRUE)
      if(length(res)>0){
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
#' Plots a graph of the reciprocal followings of 'me' with the users
#' of the data frame
#'
#' @param me Twitter user (using getUser("name"))
#' @param userDataFrame data frame of users
#'
#' @return if the graph is empty prints 'NA'
#' @export
#'
#' @examples plotReciprocalFollowsGraph(getUser("dummy"), userDataFrame
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
#' Get the location column of each tweets dataframe and draw it on a Spain
#' map if not empty
#'
#' @param ... Data frames of tweets
#' @param colours List of colours of the data frames (of ellipsis)
#' @param title description of the data showed on the map
#'
#' @return
#' @export
#'
#' @examples mapLocations(tweetsDataFrame, tweetsDataFrame2, colours = c("red","green"), "dummy")
mapLocations <- function(..., colours, title)  {
  dataFrames <- list(...)
  map <- get_map(location = 'Spain', zoom = 6)
  map <- ggmap(map, legend = "right")
  for(i in 1:length(dataFrames)){
    coordinates <- geocode(dataFrames[[i]][["location"]])
    geom <- geom_point(data=coordinates, aes(x=lon, y=lat), colour=colours[i], size=3)
    map <- map + geom
  }
  ggtitle(title)
  map
}

