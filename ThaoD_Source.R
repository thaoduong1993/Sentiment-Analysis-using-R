# Custom functions for ThaoD_NLP.R
require(rvest)
require(dplyr)
require(tidyr)
library(tm)
require(stringr)
require(scales)
require(tidytext)
require(MASS)
require(ggplot2)
require(data.table)

#Create a list of urls that can feed into read_reviews function
count_urls <- function(x) {
  #Base url with year as input x
  url0 <- paste("https://www.cars.com/research/toyota-camry-",x,"/consumer-reviews/?pg=", sep = "")
  #Find the number of pages for each year
  npages <- url0 %>% read_html() %>% html_nodes(".page-numbers") %>%  html_text() %>% str_trim() %>% str_split(" \ ") %>% unlist() %>% as.numeric() %>% tail(1)
  #combind year and pages to have a list of all the urls
  url <- paste(url0, 1:npages, sep="")
  data.frame(url)
}  

#Create a function to scrape information from each url
read_reviews <- function(url) {
  #Read the HTML code from the website
  web <- read_html(url)
  #Read the year
  year <- web %>% html_nodes(".vehicle-name .cui-heading-3") %>% html_text() %>% substr(.,1,4)
  #Scrape the stars section
  star_all <- web %>% html_nodes(".review-listing-card cars-star-rating") %>% html_attr("rating") %>% as.numeric
  star = c()
  for (i in 1:length(star_all)){
    if (i %% 7 == 1){
      star = c(star, star_all[i])
    }
    i = i+1
  }
  #Scrape the text reviews
  text <- web %>% html_nodes(".review-card-text") %>% html_text() %>% gsub("\n", "",.)
  #Create a table with all values
  data.frame(year, star, text)
}


#Create a function to add tag to each review
create_tags <- function(x) {
  #Create normalized reviews
  x$Normalized <- x$Review %>% tolower() %>% removePunctuation(., preserve_intra_word_dashes = TRUE)
  #Here's the list of tags that we want
  tags <- c("service", "price", "handling", "interior")
  #Extract each of the tag from normalized text and paste it in the new Tag column
  for (i in 1:nrow(x)) {
    tag <- str_extract(x$Normalized[i], tags)
    tag <- paste0(tag[!is.na(tag)], collapse = ", ")
    x$Tags[i] <- tag
  }
  x
}

#Create a function to separate each review text into words
create_words <- function(x) {
  #Since the data do not have an ID, create one
  x$ID <- 1:nrow(x)
  #Extract clean text from the orginal reviews, remove stop words and the most common branding words
  for (i in 1:nrow(x)) {
    x$clean[i] <- x$Review[i] %>% removePunctuation(., preserve_intra_word_dashes = TRUE) %>% tolower() %>% removeNumbers() %>% removeWords(c(stopwords("en"), "camry", "car", "toyota"))
  }
  #Create a clean data with just ID and clean text
  clean <- data.frame(ID = x$ID, text = x$clean, stringsAsFactors = FALSE)
  #Use unnest_tokens to create a words dataframe
  words <- unnest_tokens(clean, word, text, token = "words")
}

#Create a function to calculate sentiment score (Afinn, Bing, and NRC) for each review
#Function has to input, original review table x and word table created from create_words
sentiment_score <- function(x, words) {
  x$ID <- 1:nrow(x)
  #Calculate Afinn score
  afinn <- words %>% 
    merge(get_sentiments("afinn")) %>% 
    group_by(ID) %>% 
    summarise(sentiment = sum(score))
  colnames(afinn) <- c("ID", "Afinn")
  
  #Calculate Bing score by subtracting negative from positive
  bing <- words %>% 
    merge(get_sentiments("bing")) %>%
    count(ID, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  colnames(bing) <- c("ID", "Negative", "Positive", "Bing")
  
  #Calculate NRC using only positive and negative sentiment
  nrc <- words %>% 
    merge(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive","negative"))) %>%
    count(ID, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  colnames(nrc) <- c("ID", "Negative", "Positive", "NRC")
  
  #Left join the original review table with afinn, bing, and nrc for final sentiment score
  scores <- merge(x, afinn, by ='ID', all.x = TRUE) %>%
    merge(., bing[ , c("ID", "Bing")], by ='ID', all.x = TRUE) %>%
    merge(., nrc[ ,c("ID", "NRC")], by = 'ID', all.x = TRUE)
  scores
}

#Create a function to list all words for each tag
create_words_tag <- function(x) {
  clean <- data.frame(tag = x$tag, text = x$Normalized, stringsAsFactors = FALSE)
  #Use unnest_tokens to create a words dataframe
  words <- unnest_tokens(clean, word, text, token = "words")
}

#Create a function to calculate tf_idf_score from the orginal review
create_train_words <- function(train) {
  train_words <- train %>% create_words_tag() %>% count(tag, word, sort=TRUE) %>% ungroup
  total_words <- train_words %>% group_by(tag) %>% summarize(total = sum(n))
  train_words <- merge(train_words, total_words, all.x = TRUE)
}

#Create a function to add some more nrc scores to the frame for prediction
nrc_score <- function(x, words) {
  nrc <- words %>% 
    merge(get_sentiments("nrc") %>% 
            filter(sentiment %in% c("fear", "anger", "trust"))) %>%
    count(ID, sentiment) %>%
    spread(sentiment, n, fill = 0)
  colnames(nrc) <- c("ID", "Fear", "Anger", "Trust")
  scores <- merge(x, nrc, by ='ID', all.x = TRUE)
}
