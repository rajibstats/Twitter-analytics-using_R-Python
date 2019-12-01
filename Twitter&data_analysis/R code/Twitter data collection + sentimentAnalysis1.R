#required library
library("twitteR")
library("wordcloud")
library("tm")
library("plyr")

##memory clear
rm(list = ls())

#Set working directory To Project Directory 
setwd("D:\\Rajib Documents\\MEGAsync\\UpWork\\Twitter&data_analysis\\R code")

library(plyr)        # to break big problem into smaller
library(stringr)     # make easier to work with strings
########### Getting Data from Twitter ##############

library(ROAuth)
requestURL <-  "https://api.twitter.com/oauth/request_token"
accessURL <-  "https://api.twitter.com/oauth/access_token"
authURL <-  "https://api.twitter.com/oauth/authorize"
consumerKey <-  "3eOl9JdpI5jI95Zuk9DvsND2U"
consumerSecret <-  "RIrJ2YwhHB0kc5ChsOln6SaCLr8F5efbwyc5V5go6lRbfypZDh"


my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret, requestURL=requestURL,
                             accessURL=accessURL, authURL=authURL)

## run this line and go to the URL that appears on screen
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

## now we can save oauth token for use in future sessions with twitteR or streamR
save(my_oauth, file = "my_oauth.Rdata")
#load("my_oauth.Rdata")

accessToken = '806201386740416514-wGkYikTkW385jTdXOVkOgMlyyLmJzCR'
accessSecret = 'Yacf8RVeyzfGiH32z90WEJJ0WiHPyeUVBfgrwRjQIyeCT'

## testing that it works
library(twitteR)
setup_twitter_oauth(consumer_key=consumerKey, consumer_secret=consumerSecret,
                    access_token=accessToken, access_secret=accessSecret)


tweets<-searchTwitter('#apple', n =5000, lang="en")
#searchTwitter("obama", cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

tweetFrame <- twListToDF(tweets)


userInfo <- lookupUsers(tweetFrame$screenName)  # Batch lookup of user info
userFrame <- twListToDF(userInfo)  # Convert to a nice dF

locatedUsers <- !is.na(userFrame$location)  # Keep only users with location info

locations <- geocode(userFrame$location[locatedUsers])  # Use amazing API to guess
# approximate lat/lon from textual location data.
with(locations, plot(lon, lat))

worldMap <- map_data("world")  # Easiest way to grab a world map shapefile

zp1 <- ggplot(worldMap)
zp1 <- zp1 + geom_path(aes(x = long, y = lat, group = group),  # Draw map
                       colour = gray(2/3), lwd = 1/3)
zp1 <- zp1 + geom_point(data = locations,  # Add points indicating users
                        aes(x = lon, y = lat),
                        colour = "RED", alpha = 1/2, size = 1)
zp1 <- zp1 + coord_equal()  # Better projections are left for a future post
zp1 <- zp1 + theme_minimal()  # Drop background annotations
print(zp1)


#Modify the tweets
#save text
r_stats_text <- sapply(tweets, function(x) x$getText())

#create corpus - Constructs a text document collection (corpus).
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))


#Modify the tweets

r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower)) 
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()))
wordcloud(r_stats_text_corpus)

myDtm <- TermDocumentMatrix(r_stats_text_corpus, control = list(minWordLength = 1))
findFreqTerms(myDtm, lowfreq=10)    # Find the data with given frequency


#Need user ID information
#GET USER INFO
#r_stats <- userTimeline("Trump", n=100)  #n is The maximum number of tweets to return
me <- getUser("realdonaldtrump")
me$getId() #25073877

getUser(25073877)


#Extra informations from Tweets
#We can see the ttweets trended around the world

trend <- availableTrendLocations()
head(trend)

##        name country woeid
## 1 Worldwide             1
## 2  Winnipeg  Canada  2972
## 3    Ottawa  Canada  3369
## 4    Quebec  Canada  3444
## 5  Montreal  Canada  3534
## 6   Toronto  Canada  4118

# trend <- getTrends(1)

#HU tweets analyzing

#Analyze the followers of HU
##What language do HU friends speak?###################################
user <- getUser("HarrisburgU")
friends <- user$getFriends() # who HU follows
friends_df <- twListToDF(friends)
friends_df$lang  ##What language do HU friends speak?

# what language do they speak?
unique(friends_df$lang)
#save(friends_df, file = "hu_friends.RData")

#followers <- user$getFollowers() # HU followers
#followers_df <- twListToDF(followers)
#save(followers_df, file = "hu_followers.RData")



#Draw the distribution of friends################################

library(ggplot2)

# First we take a look at number the distribution of followers of my friends. 
ggplot(friends_df, aes(x = log(followersCount))) +
  geom_density(fill ="blue") +
  theme_light() +
  labs(x = "Log of number of followers of my friends", y = "density")


#discription stats
summary(friends_df$followersCount)

# Next we take a look at the distribution of the number of tweets by my friends.
ggplot(friends_df, aes(x = log(statusesCount))) +
  geom_density(fill ="grey") +
  theme_light() +
  labs(x = "Log of number of tweets of my friends", y = "density")

##According to the plots, both the numberoffollowers and the numberoftweets of my friends follow log-normal distribution.

#How active are HU friends?#########################################
  
#Calculate tweets per day in the past year
date = as.Date(friends_df$created, format = "%Y-%m-%d")
today = as.Date("2017-02-25", format = "%Y-%m-%d")
days = as.numeric(today - date)
friends_df$statusesCount_pDay = friends_df$statusesCount/days

#plot the distribution
ggplot(friends_df, aes(x = log(statusesCount_pDay))) + 
  geom_density(fill = "grey") +
  theme_light() +
  labs(x = "Log of number of tweets per day", y = "density")

#discription stats
summary(friends_df$statusesCount_pDay)

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0579  0.7787  2.6611  6.7723  7.8310 58.1228

##On average, HU friends tweet 6.8 times a day in the past year.




##Who are my followers with the biggest network and who tweet the most#######################################
  
#person with most friends
friends_df[order(friends_df$friendsCount, decreasing = T), ]$name[1]

## [1] "Kumar Garg"

#person with most followers
friends_df[order(friends_df$followersCount, decreasing = T), ]$name[1]

## [1] "NASA"

#person who tweets the most
friends_df[order(friends_df$statusesCount, decreasing = T), ]$name[1]

## [1] "WGAL"

#Among HU friends, the person with most friends is Kumar Garg; 
#the person with the most followers is NASA; 
#the person who has tweeted the most is WGAL.


#Is there a correlation between number of followers and number of tweets?
  
ggplot(friends_df, aes(x = followersCount, y = statusesCount)) +
  geom_point()+
  labs (x = "number of followers", y = "number of tweets") +
  geom_smooth(method = "lm", formula = y~x)

# Eliminating outliers
ggplot(friends_df, aes(x = followersCount, y = statusesCount)) +
  geom_point()+
  labs (x = "number of followers", y = "number of tweets") +
  geom_smooth(method = "lm", formula = y~x)+
  scale_x_continuous(limits = quantile(friends_df$followersCount, c(0.05, 0.95))) +
  scale_y_continuous(limits = quantile(friends_df$statusesCount, c(0.05, 0.95)))

cor.test(x = friends_df$followersCount, y = friends_df$statusesCount)
cor(friends_df$statusesCount, friends_df$friendsCount)
#There appears to be a positive correlation between the number of followers and the number of tweets.

#What are the most commonly used words in HU friends followers' descriptions? (Tidytext package)###########################
#To prepare the data, I am going to unnest the words (or tokens) in the user descriptions, 
#convert them to the word stem, remove stop words and urls.

library(tidytext)
library(dplyr)

data(stop_words)

tidy_descr = unnest_tokens(friends_df, word, description)
new_descr = anti_join(tidy_descr, stop_words, by="word")
best_descr = filter(new_descr, !grepl("\\.|http", word))

wscount = as.data.frame(table(best_descr$word))
colnames(wscount) = c("word", "count")
wscount_t10 = wscount[order(wscount$count, decreasing = T),][1:10,]
ggplot(wscount_t10, aes(x = reorder(word, count), y = count)) +
  geom_col(color = "grey") +
  theme_light() +
  coord_flip() +
  labs(x = "", y = "count of words in HU friend's description")

#The Top 10 most talked about words by HU friends are 
#news, pennlive, PA, harrisburg, reporter, media, central, science, patriot and community.



##Optional: Are HU followers talk positively or negatively abut HU?#############################
clean.text <- function(some_txt)
{  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)

# Remove the text which start from "@"
some_txt = gsub("@\\w+", "", some_txt)

# Remove punctuations
some_txt = gsub("[[:punct:]]", "", some_txt)

#Remove Digits
some_txt = gsub("[[:digit:]]", "", some_txt)

#Remove links
some_txt = gsub("http\\w+", "", some_txt)

# remove extra white spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)


# Remove non-english characters
some_txt = gsub("[^\x20-\x7E]", "", some_txt)

# define "tolower error handling" function
try.tolower = function(x)
{  y = NA
try_error = tryCatch(tolower(x), error=function(e) e)
if (!inherits(try_error, "error"))
  y = tolower(x)
return(y)
}

some_txt = sapply(some_txt, try.tolower)
some_txt = some_txt[some_txt != ""]
names(some_txt) = NULL
return(some_txt)
}


# clean text
sentdata_clean = clean.text(tweets_df$text)


# remove duplicated text after cleaning
sentdata_clean=as.data.frame(sentdata_clean[!duplicated(sentdata_clean)])
names(sentdata_clean)=c("text")















########################### Lexicon based sentiment analysis ################################

# loading lexicon of positive and negative words (from Neal Caren)
lexicon <- read.csv("lexicon_polarity.csv", stringsAsFactors=F)
pos.words <- lexicon$word[lexicon$popularity=="positive"]
neg.words <- lexicon$word[lexicon$popularity=="negative"]

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  nscores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # word.list = str_split(sample_clean$text, '\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    
    # match() returns the position of the matched term or NA
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    nscore=sum(pos.matches)-sum(neg.matches)  
  
    return(nscore)
  }, pos.words, neg.words,.progress=.progress )
  
  scores.df = data.frame(score=nscores, text=sentences)
  return(scores.df)
}

score=score.sentiment(sentdata_clean$text,pos.words,neg.words)
score$level=ifelse(score < 0, "Negative", ifelse(score == 0, "Neutral", "Positive"))

########## sentiment ###
View(score[,-1])

######## preparing graph ########


names(score)=c("Value","text","level")
#score$Value=as.list(score$Value)

# positive
a2=score[ which(score$Value > 0), ]

# negative
a3=score[ which(score$Value < 0), ]

# neutral
a4=score[ which(score$Value == 0), ]

## find percentage
pos= (nrow(a2)/nrow(score))*100
neg= (nrow(a3)/nrow(score))*100
neu= (nrow(a4)/nrow(score))*100

df1=data.frame(values=c(pos,neg,neu),
               sentiment=c("Positive","Negative","Neutral"))

barplot(df1$value, names = df1$sentiment,
        xlab = "Sentiment", ylab = "Percentage",
        main = "Sentiment analysis for #HU ",
        col=c("orange","black","green"))

