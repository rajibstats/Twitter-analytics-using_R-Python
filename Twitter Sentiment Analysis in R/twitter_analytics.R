##memory clear
rm(list = ls())

#Set working directory To Project Directory 
setwd("E:\\UpWork\\Twitter Sentiment Analysis in R")

#loading library
library(data.table)
Data <- fread("realDonaldTrump\\realDonaldTrump.csv")

Data_tweet <- Data$V5
Data_tweet_df <- as.data.frame(Data_tweet)

# clean data 
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
sentdata_clean1 = clean.text(Data_tweet_df$Data_tweet)

# remove duplicated text
sentdata_clean2=sentdata_clean1[!duplicated(sentdata_clean1)]
sentdata_clean<-as.data.frame(sentdata_clean2)
names(sentdata_clean) <- c("tweets")

## loading lexicon of positive and negative words (from Neal Caren)
lexicon <- read.csv(file.choose(), stringsAsFactors=F)
pos.words <- lexicon$word[lexicon$polarity=="positive"]
neg.words <- lexicon$word[lexicon$polarity=="negative"]

# a look at a random sample of positive and negative words
sample(pos.words, 10)
sample(neg.words, 10)


## function for finding sentiment score 

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  library(plyr)
  library(stringr)
  
  
  nscores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # compare our words with list of negation words
    #negation=match(words,negation.words)
    
    
    # match() returns the position of the matched term or NA
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    #negation=!is.na(negation)
    
    # calculation of score
    score= sum(pos.matches)-sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=nscores, text=sentences)
  return(scores.df)
}


# list of negation words
#negation.words=(negation.words)

#getting sentiment score
result1= score.sentiment(sentdata_clean$tweets,pos.words,neg.words)
View(result1)


#making data frame with result
library(dplyr)
a1=data.frame(result1$score,result1$text)
names(a1)=c("score","text")
# positive score
a2=filter(a1,score>0)

# negative score
a3=filter(a1,score<0)

# neutral score
a4=filter(a1, score==0)

## find percentage
pos= (nrow(a2)/nrow(result1))*100
neg= (nrow(a3)/nrow(result1))*100
neu= (nrow(a4)/nrow(result1))*100

#result in table format
df1=data.frame(values=c(pos,neg,neu),
               sentiment=c("Positive","Negative","Neutral"))

#graphical representation
barplot(df1$value, names = df1$sentiment,
        xlab = "Sentiment", ylab = "Percentage",
        main = "Sentiment analysis for Donald Trump ",
        col=c("orange","black","gray"))


# First let us plot the distribution of emotions according to emotion categories
# We will use ggplot function from ggplot2 Package (for more look at the help on ggplot) and RColorBrewer Package
library(ggplot2)
ggplot(sentiment_dataframe, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") + ggtitle('Sentiment Analysis of Tweets on Twitter about PowerBI') +
  theme(legend.position='right') + ylab('Number of Tweets') + xlab('Emotion Categories')




ggplot(sentiment_dataframe, aes(x=polarity))+geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") + ggtitle('Sentiment Analysis of Tweets on Twitter about PowerBI') +
  theme(legend.position='right') + ylab('Number of Tweets') + xlab('Polarity Categories')
