library(sentimentr)
library(jsonlite)
library(tidyverse)
library(emo)
library(utf8)

emoji_sentiment_tbl = read_csv('data/emoji_sentiment.csv')
emoji_sentiment_tbl$emoji = NA
emoji_sentiment_tbl$int_value = NA
for (i in 1:nrow(emoji_sentiment_tbl))
{
  emoji_sentiment_tbl$emoji[i] = intToUtf8( emoji_sentiment_tbl$unicode[i]) %>% utf8_print()
  emoji_sentiment_tbl$int_value[i] = utf8ToInt(emoji_sentiment_tbl$emoji[i])
}

score_sentiment_sentimentr = function(content)
{
  content = tolower(content)
  sentiment_df = content %>% sentimentr::sentiment()
  sentiment_df = sentiment_df %>% mutate(weighted_sentiment = word_count * sentiment)
  overall_sentiment_df = sentiment_df %>% group_by(element_id) %>% summarise(weighted_avg_sentiment = sum(weighted_sentiment, na.rm=T)/sum(word_count))
  sentiment_to_return = overall_sentiment_df$weighted_avg_sentiment[1]
  if (sentiment_to_return %>% is.na())
  {
    return(NA)
  } else if (sentiment_to_return == 0)
  {
    return(NA)
  }
  return(sentiment_to_return)
}

flatten_vector = function(this_vector)
{
  to_return = this_vector %>% unlist() %>% paste(collapse = ",")
  return(to_return)
}

get_sentiment_of_emojis = function(emoji_string)
{
  emoji_vector = unlist(strsplit(emoji_string, ","))
  bottom_divisor = 0
  total_sentiment = 0
  for (i in 1:length(emoji_vector))
  {
    emoji_int_value = emoji_vector[i] %>% utf8ToInt()
    
    emoji_row = emoji_sentiment_tbl %>% filter(emoji_int_value == int_value)
    if (emoji_row %>% nrow() > 0)
    {
      emoji_sentiment = emoji_row$sentiment[1]
      total_sentiment = total_sentiment + emoji_sentiment
      bottom_divisor = bottom_divisor + 1
    }
  }
  if (bottom_divisor == 0)
  {
    return(NA)
  }
  return (total_sentiment/bottom_divisor)
}

#loading RDS files
loading_RDS_files_from_directory <- function(directory){
  print(list.files(path = directory, pattern = ".RDS", full.names = FALSE))
  df <- list.files(path = directory, pattern = ".RDS", full.names = TRUE) %>% 
    map_dfr(readRDS)
}

scraped_tweets = loading_RDS_files_from_directory("data/tweets_data/")

#scraped_tweets = read_json("data/all_scraped_data.json", simplifyVector = TRUE)
more_scraped_tweets = readRDS("data/random_tweets.RDS")
even_more_scraped_tweets <- readRDS("data/big_updated_stream_tweets.RDS")

scraped_tweets = scraped_tweets %>% distinct(status_id, .keep_all=TRUE) %>% select(status_id, text)
more_scraped_tweets = more_scraped_tweets %>% distinct(status_id, .keep_all=TRUE) %>% select(status_id, text)
even_more_scraped_tweets = even_more_scraped_tweets %>% distinct(status_id, .keep_all=TRUE) %>% select(status_id, text)


scraped_tweets = scraped_tweets %>% bind_rows(more_scraped_tweets)
scraped_tweets = scraped_tweets %>% bind_rows(more_scraped_tweets) %>% bind_rows(even_more_scraped_tweets)

scraped_tweets = scraped_tweets %>% mutate(emoji = ji_extract_all(text))
scraped_tweets$emoji_text = unlist(lapply(scraped_tweets$emoji, flatten_vector))
scraped_tweets = scraped_tweets %>% mutate(emoji_count = emoji_text %>% str_count(",")) %>% filter(emoji_count > 1) %>% select(status_id, text, emoji, emoji_text)
scraped_tweets = scraped_tweets %>% mutate(emoji_text = utf8_print(emoji_text))

# get sentiment of emojis and tweets
scraped_tweets$emoji_sentiment =  unlist(lapply(scraped_tweets$emoji, get_sentiment_of_emojis))

scraped_tweets$tweet_sentiment = unlist(lapply(scraped_tweets$text, score_sentiment_sentimentr))

# test for statistically significant correlation (p<.05)
cor.test(x=scraped_tweets$emoji_sentiment, y = scraped_tweets$tweet_sentiment, use = "pairwise.complete.obs", method = "pearson")

