
install.packages("data.table", repos = "https://cran.r-project.org")

library(data.table)

getwd()

setwd("/Users/khabbabibrahim/Downloads/")

# fread function is to lead the data set into the invironment and can read text documents all types of files including csv and xlx
data <- fread("/Users/khabbabibrahim/Downloads/tweets_about_sprint.csv", 
             strip.white=T, sep=",", header=T, na.strings=c(""," ", "NA","nan", "NaN", "nannan"))

# to create a seq of integer numbers by creating a new id column with a unique id to identify each row of tweet
data$tweet_id <- seq.int(nrow(data))

head(data, n=5)

install.packages("tidytext", repos = "https://cran.r-project.org")
install.packages("dplyr", repos = "https://cran.r-project.org")

library(tidytext)
library(dplyr)
library(tidyverse)

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text

# Converting the text data into a table
text_df <- data_frame(line = 1:4, text = text)
head(text_df)

# tockanizing our text_data
# Notice that this data frame isn’t yet compatible with tidy tools. 
# We can’t filter out words or count them because each row is made up of multiple combined words (sentences or phrases)
# Within our tidy text framework, we need to both break the text into individual tokens (tokenization) and transform it to a tidy data structure. 
# To do this, we use tidytext’s unnest_tokens() function
# We need to convert it such that it has one-token-per-document-per-row.

text_df %>%
  unnest_tokens(word, text) 
# This means that in data frame text_df, tokenize column "text" by each word (standard tokenization).

# unnest based on words over tweets
tidy_text <- data %>%
  unnest_tokens(word, tweet)
tidy_text[1:40,]

# Now we can remove stop-words from our data. We can do this using anti_join function:
# We remove stop words because they don't add any meaning to the sentence
# Stop words are words like he or she to make up sentence
data(stop_words)
# overwrite tidy_text
tidy_text <- tidy_text %>%
  anti_join(stop_words)

# How to stemp a dataset

library(SnowballC)
tidy_text <- tidy_text %>%
  mutate(word = wordStem(word))

# We can also use dplyr’s count() to find the most common words in all the books as a whole.
tidy_text %>%
  count(word, sort = TRUE)

# Because we’ve been using tidy tools, our word counts are stored in a tidy data frame. 
# This allows us to pipe this directly to the ggplot2 package. 
# for example to create a visualization of the most common words:

tidy_text %>%
  count(word, sort = TRUE) %>%     
  filter(n > 5000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +         # to create a bar plot
  xlab(NULL) +
  coord_flip()       

tidy_text %>%
  count(user, word, sort = TRUE) %>% # It will count the number of words per user
  ungroup()
head(tidy_text)

# How to stemp a dataset
# mutate to genarate new words after stemming words
library(SnowballC)
tidy_text <- tidy_text %>%
  mutate(word = wordStem(word))

tidy_text %>%
  count(word, sort = TRUE) %>%     
  filter(n > 5000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +         # to create a bar plot
  xlab(NULL) +
  coord_flip()

# Sentimen Analysis
# Lexicon is a data dectionary collection of negative and positive words
sentiments

# to filter and find if a certain word is positive or negative
sentiments %>% filter(word == "abolish")

# to filter out all the positive words
sentiments %>% filter(sentiment == "positive")

# to filter out all the negative words
sentiments %>% filter(sentiment == "negative")


library(textdata)

# google afinn lexicon
# to rate negativity or positivity of a word in positive and negative numbers
get_sentiments("afinn")

# bing lexicon has a lot more words than afinn
get_sentiments("bing")

# nrc lexicon has more words and relates words to emotions
get_sentiments("nrc")

# if Im interested in a word thats related to trust, anger, or joy we can use
get_sentiments("nrc") %>% filter(sentiment == "anger")

# what words in our tweets are related to joy 
nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

# inner join tidy text tockanized data with nrcjoy and count and sort the words
tidy_text %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

# what words in our tweets are related to trust 
nrctrust <- get_sentiments("nrc") %>% 
  filter(sentiment == "trust")

# inner join tidy text tockanized data with nrcjoy and count and sort the words
tidy_text %>%
  inner_join(nrctrust) %>%
  count(word, sort = TRUE)

# now we can use bing in a similar manner
sentiment <- tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)
head(sentiment)
# The word trump might make us confused if its the president or an adjective so we can look up "Trump" and change it to ttrump for example


# sentiment of a single tweet in my dataset
sentiment <- tidy_text %>%
  filter(tweet_id == 2) %>%                    # filter by tweet_id # 2
  inner_join(get_sentiments("bing")) %>%       # inner joinning with bing lexicon
  count(word, sentiment, sort = TRUE)          # we also sort
head(sentiment)
sentiment

# to filter all tweets of a single user
sentiment <- tidy_text %>%
  filter(user == "KeepMyCoat") %>%             # filter by user name
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)
sentiment


# Word Clouds:
# visualization for design text data 
library(wordcloud)

# create a word cloud based on the count of words 
tidy_text %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100)) # create for the top 100


install.packages("reshape2", repos = "https://cran.r-project.org")
library(reshape2)


tidy_text %>%
  inner_join(get_sentiments("bing")) %>%          # sentiment using bing
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%   # related to visualization
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)                       # display the 100 most frequent words


# Topic Modeling
# to understand or identify the main topic in a document
install.packages("tm", repos = "https://cran.r-project.org")
install.packages("topicmodels", repos = "https://cran.r-project.org")
install.packages("slam", repos = "https://cran.r-project.org")
library(tm)
library(wordcloud)
library(topicmodels)
library(slam)

data <- data[1:1000,]

tidy_tweet <- data %>%
  unnest_tokens(word, tweet)

wordstoremove <- data.frame("word" = c("https","just","amp"))

tidy_tweet <- tidy_tweet %>%
  anti_join(wordstoremove)

corpus <- Corpus(VectorSource(data$tweet), 
                 readerControl=list(language="en"))

tweet_dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 3, removeNumbers = TRUE, removePunctuation = TRUE))
tweet_dtm

lda <- LDA(tweet_dtm, k = 2)
lda


library(ldatuning)

result <- FindTopicsNumber(
  tweet_dtm,
  topics = seq(from = 2, to = 22, by = 4),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

lda_td <- tidy(lda)
lda_td


top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
