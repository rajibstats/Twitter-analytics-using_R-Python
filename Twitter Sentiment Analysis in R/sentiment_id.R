##memory clear
rm(list = ls())

#Load packages
library(data.table)
library(plyr)        # to break big problem into smaller
library(stringr)     # make easier to work with strings
library(tm)          # tools for text mining
library(wordcloud)   # used to create word cloud
library(Rcpp)        # R and C++ integration
library(RCurl)
library(dplyr)
library(stringr)


#Set working directory To Project Directory 
setwd("E:\\UpWork\\Twitter Sentiment Analysis in R")

trump_text=fread("realDonaldTrump.csv", header=FALSE)

#sentence=trump_text$V5

###### clean_text ########
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
#some_txt = gsub("[ \t]{2,}", "", some_txt)
#some_txt = gsub("^\\s+|\\s+$", "", some_txt)


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

return(some_txt)}



######## Input ID 

tweet_id=readline(prompt="Enter an ID: ")
#id1: 965582280772276224
tweet=filter(trump_text, V1==tweet_id)


sample_clean_0=clean.text(tweet$V5)

sample_clean=sample_clean_0[sample_clean_0!="  "]

## Remove duplicate 
sample_clean=as.data.frame(sample_clean[!duplicated(sample_clean)])
names(sample_clean)=c("text")


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
    
    # compare our words with list of negation words
    #  negation.matches=match(words,negation)
    
    
    
    
    # match() returns the position of the matched term or NA
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    
    
    nscore=sum(pos.matches)-sum(neg.matches)  
    
    
    
    return(nscore)
  }, pos.words, neg.words,.progress=.progress )
  
  scores.df = data.frame(score=nscores, text=sentences)
  return(scores.df)
}

score=score.sentiment(sample_clean$text,pos.words,neg.words)
score$level=ifelse(score < 0, "Negative", ifelse(score == 0, "Neutral", "Positive"))

########## sentiment ###
#View(score[,-1])
names(score)=c("Sentiment_Score","Tweets","Sentiment_level")

write.csv(score,"E:\\UpWork\\Twitter Sentiment Analysis in R\\Sentiment by twitter id\\28.965199840471810049.csv", row.names=FALSE,na="")

######## preparing graph ########



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
        main = "Sentiment analysis for ID: 965582280772276224 ",
        col=c("orange","black","white"))

