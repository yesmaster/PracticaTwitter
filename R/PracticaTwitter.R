loadLibraries()
tweeterAuthentication()

# Data frame creation
tuser <- twitteR::getUser()

userDataFrame <- getFollowersDataFrame(tuser)

tweetsMatrix <- fillMatrixOfTweets(userDataFrame,4)


keyWordsList <- readLines("data/insultos.txt")
tweetsDataFrame <- getTweetsDataFrame("#podemos", "41.38,2.115,5km", 70)

map <- get_map(location = 'Spain', zoom = 6)
coordinates <- geocode(userDataFrame$location)
map <- ggmap(map) + geom_point(data=coordinates, 
                               aes(x=lon, y=lat), colour="coral1", size=3)
