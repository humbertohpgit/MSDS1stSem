
## Project Description
#Final Project: NYC311 Service Requests Analysis - Text Mining
#Goal: Explore and analyze NYC311 Service requests (from historical data sets and social network feeds) to understand diverse patterns and trends (geographic, demographic, complaint categories &  frequencies etc.) as well as service satisfaction levels derived from resolution categories and processing times.
#Planned Data Sources:
#  NYC OpenData: https://nycopendata.socrata.com/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9
#    API Endpoint: https://data.cityofnewyork.us/resource/fhrw-4uyv.json
#                  https://data.cityofnewyork.us/resource/fhrw-4uyv.csv
#    App Token: ZHxqxXyUSoZlBvzxpZsTT9QjG
#  Twitter: https://twitter.com/nyc311/with_replies
## Project Motivation
## Data Science Workflow
### Data Acquisition

#### NYC OpenData API
##### Load JSON file to a data frame
library(jsonlite)  
#"https://data.cityofnewyork.us/resource/fhrw-4uyv.json?$$app_token=ZHxqxXyUSoZlBvzxpZsTT9QjG&borough=BROOKLYN")

api_tkn <- "$$app_token=ZHxqxXyUSoZlBvzxpZsTT9QjG"
api_endpoint <- "https://data.cityofnewyork.us/resource/fhrw-4uyv.json?"
api_limit <- "&$limit=500000"
#api_filter <- "&borough=BRONX"
request_json <- fromJSON(paste0(api_endpoint, api_tkn, api_limit))
request_json_test <- fromJSON(paste0(api_endpoint, api_tkn, api_limit))
class(request_json)
colnames(request_json)
nrow(request_json)
head(request_json,2)

##### Load CSV file to a data frame

#api_endpoint_csv <- "https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?"
#request_csv <- read_csv(paste0(api_endpoint_csv, api_tkn, api_limit))
#class(request_csv)
#colnames(request_csv)
#nrow(request_csv)
#head(request_csv,2)

### Data Exploration
#### Most common complaint types (top 50)
library(plyr)
library(tidyverse)

#request_json_bkp <- request_json
#dataset_bkp <- dataset
#dataset <- dataset_bkp

dataset <- request_json
ggplot(subset(dataset, complaint_type %in% count(dataset, complaint_type, sort=T)[1:50,]$complaint_type), aes(complaint_type)) + 
  geom_histogram(stat = "count") +
  labs(x="Complaint Type", y="Service Requests") +
  coord_flip() + theme_bw()

#### Most common complaint types by borough and status
dataset_qckfilt <- subset(dataset, complaint_type %in% count(dataset, complaint_type, sort=T)[1:50,]$complaint_type)
nrow(dataset_qckfilt)
dataset_qckfilt <- dataset_qckfilt %>% select(complaint_type, borough, status) 
ggplot(dataset_qckfilt, aes(x=status, y = complaint_type )) +
  geom_point() +
  geom_count() + 
  facet_wrap(~borough)

### SR Resolutions Tidying and Analysis - Using Tidytext
library(tidytext)
#### Most frequent words used in NYC311 Service Requests
data(stop_words)
tokenized_resolutions <- dataset %>%
  select(complaint_type, descriptor, street_name, city, due_date, resolution_description, borough, open_data_channel_type) %>%
  filter(!str_detect(borough, "Unspecified")) %>% 
  unnest_tokens(word, resolution_description) %>%
  anti_join(stop_words) %>%
  group_by(borough, word) %>%
  tally()

tokenized_resolutions %>% glimpse()

tokenized_resolutions %>%
  group_by(borough) %>%
  top_n(25) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = factor(borough))) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none") +
  facet_wrap(~borough, scales = "free") + 
  coord_flip() +
  labs(x = "Words",
       y = "Frequency",
       title = "Top words used in NYC311 Service Requests by Borough",
       subtitle = "")

#### Determine terms truly characteristic for SRs by Borough (TF-IDF)

tf_idf_words <- tokenized_resolutions %>%
  bind_tf_idf(word, borough, n) %>%
  arrange(desc(tf_idf))
tf_idf_words

#### Cleaning up the tf_idf data set from irrelevant or obvious words affecting the analysis

tf_idf_words_cln <- tf_idf_words %>% filter(!str_detect(word, "[[:digit:]]+")) %>% 
                filter(!str_detect(word, "[[:punct:]]+")) %>%
                filter(!str_detect(word, "bronx|brooklyn|manhattan|queens|staten island"))

tf_idf_words_cln %>% 
  top_n(25) %>%
  arrange(desc(tf_idf)) %>%
  ggplot(aes(x = reorder(word, tf_idf), y = tf_idf, fill = borough)) +
  geom_col() +
  labs(x = "Words", y = "tf-idf",
       title = "Distinctive words used in NYC311 Service Requests by Borough",
       subtitle = "") +
  coord_flip() +
  theme(legend.position = "none") +
  facet_wrap(~ borough, scales = "free")

## Map Analysis

#### Preparing and tidying up the data for map plotting

dataset_map <- subset(dataset, complaint_type %in% count(dataset, complaint_type, sort=T)[1:50,]$complaint_type)
dataset_map <- dataset_map %>% select(complaint_type, borough, latitude, longitude) %>% drop_na()

#library(plyr)
counts <- ddply(dataset_map, .(complaint_type), "count")
counts_filtered <- filter(counts, freq > 80)
counts_filtered$freq <- as.numeric(counts_filtered$freq)
counts_filtered$longitude <- as.numeric(counts_filtered$longitude)
counts_filtered$latitude <- as.numeric(counts_filtered$latitude)

#### Map Plotting

#install.packages("rworldmap")
#install.packages("rworldxtra")

library(rworldmap)
library(rworldxtra)

newmap <- getMap(resolution = "high")
#nyc_coorflimits <- data.frame( long = c(-74.5, -73.5), lat = c(40.5, 41), stringsAsFactors = FALSE)

nyc <- ggplot() + geom_polygon(data = newmap, aes(x=long, y = lat, group = group), fill = "gray", color = "blue")  + xlim(-74.5, -73.5) + ylim(40.5, 41)

nyc_SRs <- nyc + 
  geom_point(data=counts_filtered, aes(longitude, latitude, size=freq), colour="red") +  
  facet_wrap(~complaint_type, scales = "free") + 
  labs(x = "Longitude", y = "Latitude", title = "Highest Number of SRs by Complaint Type") + scale_size(name="# of SRs")
nyc_SRs

### SR Resolutions Tidying and Analysis - Using TM

library(tm)
library(wordcloud)

dataset_filt <- subset(dataset, complaint_type %in% count(dataset, complaint_type, sort=T)[1:50,]$complaint_type)
sr_resolution <- dataset_filt$resolution_description

#### Cleaning up non-standard characters (encoding conversion)
sr_resolution_cln <- sr_resolution %>% iconv("latin1", "ASCII")
control <- list(stopwords=TRUE, removePunctuation=TRUE, removeNumbers=TRUE, minDocFreq=5) # stemming=TRUE does not provide much value

sr_corpus <- VCorpus(VectorSource(sr_resolution_cln))
sr_tdm <- TermDocumentMatrix(sr_corpus, control)

#summary(services requests)
sr_tdm

## Removing sparse terms (80% of sparse percentage of empty)
sr_tdm_unsprsd <- removeSparseTerms(sr_tdm, 0.8)
sr_tdm_unsprsd

## Inspecting matrix
inspect(sr_tdm_unsprsd)

## Top terms by frequency (mentioned at least 50 times)
length(findFreqTerms(sr_tdm_unsprsd,50))
sr_topterms <- findFreqTerms(sr_tdm_unsprsd,50)
sr_topterms

## Find top associations for the top terms (lower correlation limit of 0.4)
sr_topterms <- sr_topterms[!is.na(sr_topterms)]
sr_assocs <- findAssocs(sr_tdm_unsprsd, sr_topterms[1:10], 0.4) ## more consistent association patterns found in service requests
library(knitr)
lapply(sr_assocs, function(x) kable(x))

library(wordcloud)
sr_tdm_cloud <- as.matrix(sr_tdm_unsprsd)
v <- sort(rowSums(sr_tdm_cloud),decreasing=TRUE)
d <- data.frame(word=names(v),freq=v)	
wordcloud(d$word,d$freq,max.words=50, min.freq=10, colors=brewer.pal(8, 'Dark2'))

## NYC311 Tweets Data Collection and Exploration
#devtools::install_github("mkearney/rtweet") # Latest working version of rtweet, this is preferred version to use
library(rtweet)

## App name from API set-up
##Commands commented and keys masked

##appname <- "nyc311sentiment_analysis"
## key <- "12345678901234567890"
##secret <- "12345678901234567890abcdefghijk"
# create token named "twitter_token"
##twitter_token <- rtweet::create_token(app = appname,
##                                      consumer_key = key,
##                                      consumer_secret = secret)

##home_directory <- "C:/DATA/HHP/Personal/Degrees/Ms. Data Science (CUNY)/R Working Dir"
##file_name <- file.path(home_directory,
##                       "twitter_token.rds")
## save token to home directory
##saveRDS(twitter_token, file = file_name)

## create and save environment variable
##cat(paste0("TWITTER_PAT=", file_name),
##    file = file.path(home_directory, ".Renviron"),
##    append = TRUE)

## search for 1000 tweets using the nyc311 hashtag
nyc311_tweets <- search_tweets("nyc311", n = 1000, include_rts = FALSE)
## preview tweets data
head(nyc311_tweets$text,5) %>% kable()
# Some Users Tweeting about nyc311
nyc311_users <- users_data(nyc311_tweets) %>% unique()
kable(head(nyc311_users$name))
## plot time series
#ts_plot(nyc311_tweets)
ts_plot(nyc311_tweets, "24 hours", col=c("blue")) + theme_minimal() + theme(plot.title = ggplot2::element_text(face = "bold")) + labs(x = "Date", y = "# of Tweets", title = "NYC311 Tweets in the last 7 days")

## Sentiment Analysis - NYC311 Tweets with Syuzhet Package
#devtools::install_github("mjockers/syuzhet")
library(syuzhet)

#### Tweet's Emotions
#### 'Syuzhet' breaks the text/words into 10 different emotions - anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative and positive.

nyc311_tweets_txt <- as.vector(nyc311_tweets$text)
emotion_df <- get_nrc_sentiment(nyc311_tweets_txt)
twt_emotion_df <- cbind(nyc311_tweets_txt, emotion_df) 
kable(head(twt_emotion_df,3))

#### Sentiment Scoring
sent.value <- get_sentiment(nyc311_tweets_txt)

most.positive <- nyc311_tweets_txt[sent.value == max(sent.value)]
most.positive

most.negative <- nyc311_tweets_txt[sent.value <= min(sent.value)] 
most.negative 

positive.tweets <- nyc311_tweets_txt[sent.value > 0]
kable(head(positive.tweets,3))

negative.tweets <- nyc311_tweets_txt[sent.value < 0]
kable(head(negative.tweets,3))

neutral.tweets <- nyc311_tweets_txt[sent.value == 0]
kable(head(neutral.tweets,3))

#### Total Tweets by Sentiment
#install.packages("plotly")
library(plotly)
category_sent <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
totals <- data.frame(table(category_sent))
plot_ly(totals, labels = ~category_sent, values = ~Freq, type = 'pie',  textinfo = 'label+percent') %>% layout(title = 'NYC311 Tweets by Sentiment')
