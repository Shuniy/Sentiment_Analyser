# Loading Libraries
library(shiny)
library(reshape)
library(tm)
library(wordcloud)
library(shiny)
library(twitteR)
library(openssl)
library(httpuv)
library(tm)
library(stringr)
library(dplyr)
library(httr)
library(ROAuth)
library(shiny)
library(shinythemes)

# analyzing sentiments after finding scores
# result is list not a dataframe
analyze_sentiments <- function(result)
{
    test_positive = result[[2]]
    test_negative = result[[3]]
    
    test_positive$text = NULL
    test_negative$text = NULL
    
    test_score = result[[1]]
    test_score$text = NULL
    
    # Storing the first row (it contains sentiment scores)
    overall_row = test_score[1,]
    positive_row = test_positive[1,]
    negative_row = test_negative[1,]
    
    # Melting data or spreading data by variables
    overall_melt = melt(overall_row, var = 'Score')
    positive_melt = melt(positive_row, var = 'Positive')
    negative_melt = melt(negative_row, var = 'Negative') 
    overall_melt['Score'] = NULL
    positive_melt['Positive'] = NULL
    negative_melt['Negative'] = NULL
    
    # Creating new dataframe for positive, negative data frames.
    overall_df = data.frame(Text = result[[1]]$text, Score = overall_melt)
    positive_df = data.frame(Text = result[[2]]$text, Score = positive_melt)
    negative_df = data.frame(Text = result[[3]]$text, Score = negative_melt)
    
    # Merging three data frames into one
    final_df = data.frame(Text = overall_df$Text, Positive = positive_df$value, Negative = negative_df$value, Score = overall_df$value)
    return(final_df)
}

# Computing precentage from the final dataframe after analyzing sentiments from score
compute_percentage <- function(final_df)
{
    pos_score = final_df$Positive
    neg_score = final_df$Negative
    
    final_df$PosPercent = pos_score / (pos_score+neg_score)
    
    # Replacing NAs with zero.
    positive_score = final_df$PosPercent
    positive_score[is.nan(positive_score)] <- 0
    final_df$PosPercent = positive_score * 100
    
    # Calculating negative percentage.
    final_df$NegPercent = neg_score/ (pos_score + neg_score)

    negative_score = final_df$NegPercent
    negative_score[is.nan(negative_score)] <- 0
    final_df$NegPercent = negative_score * 100
    return(final_df)
}

# To format tweet_list to dataframe
search_tweets_and_return<-function(tweet_list)
{
    # Transforming to a data-frame.
    # Constructing a function call to apply as.data.frame
    twitter_data <- do.call("rbind", lapply(tweet_list, as.data.frame))

    # Emoticons and other icons will be removed by replacing them with empty string
    twitter_data$text <- sapply(twitter_data$text, function(row) iconv(row, "latin1", "ASCII", sub = ""))

    # Removing URL's
    twitter_data$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", twitter_data$text)
    return (twitter_data$text)
}

# Compute sentiment score from tweet
compute_sentiment_score <- function(sentences, positive, negative)
{
    list = lapply(sentences, function(sentence, positive, negative)
    {   
        # removing punctuations, newlines and decimal numbers
        clean_sentence = gsub('[[:punct:]]', ' ', sentence) 
        clean_sentence = gsub('[[:cntrl:]]', '', sentence)
        clean_sentence = gsub('\\d+', '', sentence)
        clean_sentence = gsub('\n', '', sentence)
        
        # Converting to lower case
        clean_sentence = tolower(clean_sentence)
        word_list = str_split(clean_sentence, '\\s+')
        
        # Unlisting and matching words from positive and negative
        words = unlist(word_list)
        pos_matches = match(words, positive)
        neg_matches = match(words, negative) 
        pos_matches = !is.na(pos_matches) # Add only positive words and no NA
        neg_matches = !is.na(neg_matches)
        
        # Calculating scores
        positive_score = sum(pos_matches)
        negative_score = sum(neg_matches)
        score = sum(pos_matches) - sum(neg_matches)
        list1 = c(score, positive_score, negative_score) # Append the scores to the list
        return (list1)
    }, positive, negative)
    
    score_new = lapply(list, `[[`, 1)
    positive_score_list = score = lapply(list, `[[`, 2)
    negative_score_list = score = lapply(list, `[[`, 3)
    
    scores_df = data.frame(score = score_new, text = sentences)
    positive_df = data.frame(Positive = positive_score_list, text = sentences)
    negative_df = data.frame(Negative = negative_score_list, text = sentences)
    
    list_df = list(scores_df, positive_df, negative_df)
    return(list_df)
}

# Preparing data for wordcloud
clean_wordcloud_data <- function(text)
{
    corpus_data <- VCorpus(VectorSource(text))
    
    # Cleanse data for word cloud by transforming the case, removing stop words, whitespaces etc., and return the data. 
    word_cloud_data <- tm_map(corpus_data, removePunctuation)
    word_cloud_data <- tm_map(word_cloud_data, content_transformer(tolower))
    word_cloud_data <- tm_map(word_cloud_data, removeWords, stopwords("english"))
    word_cloud_data <- tm_map(word_cloud_data, removeNumbers)
    word_cloud_data <- tm_map(word_cloud_data, stripWhitespace)
    return (word_cloud_data)
}

# Reference - https://bigdataenthusiast.wordpress.com/category/r/
get_top_trends <- function(place)
{
    all_trends = availableTrendLocations() # will return the list of all countries TwitteR
    woeid = all_trends[which(all_trends$name == place), 3]
    trend = getTrends(woeid)
    final_trends = trend[1:2]
    
    trends_bind <- cbind(final_trends$name)
    trends_list <- unlist(strsplit(trends_bind, split = ", "))
    # Removing the emoticons
    clean_trends <- grep("trends_bind", iconv(trends_bind, "latin1", "ASCII", sub="trends_bind"))
    final_trends_data <- trends_bind[-clean_trends]
    return (final_trends_data)
}

toptweeters <- function(tweetlist)
{
    tweets <- twListToDF(tweetlist)
    tweets <- unique(tweets)
    # Make a table for the number of tweets per user
    tweet_data <- as.data.frame(table(tweets$screenName)) 
    tweet_data <- tweet_data[order(tweet_data$Freq, decreasing = TRUE), ] #descending order of top charts according to frequency of tweets
    names(tweet_data) <- c("User","Tweets")
    return (tweet_data)
}