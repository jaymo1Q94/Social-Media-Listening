#Connect to Twitter API ####

bearer_token <- "AAAAAAAAAAAAAAAAAAAAANdRhQEAAAAAdmKlyRSrb3JIiAD8E7mbqSvuXYY%3DWQmBnVlU7Hq2BjOYZEKAincz0McltyySOSBxTxTc1L7FAC19P6"


headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))

#Build EVD Query #####

#Notes - Compare query to twitteR::searchTwitteR to pull tweets over 
#multiple periods of time 

params = list(
  query ='ebola OR evd OR hemorrhagic fever OR outbreak  OR sudan virus OR ebola virus OR evola virus disease',
  max_results = '100',
  tweet.fields = "created_at,lang,author_id,context_annotations"
)

params.2 <- list(
  query = '(EVD OR evd OR ebola OR ebola virus OR ebola virus disease) -is:retweet lang:en',
  max_results = '100',
  tweet.fields = "created_at,lang,author_id,context_annotations"
)

params.3 <- list(
  query = '(ebola) -is:retweet lang:en',
  max_results = '100',
  tweet.fields = "created_at,lang,author_id,context_annotations"
)

params.4 <- list(
  query = '(ebola virus) -is:retweet lang:en',
  max_results = '100',
  tweet.fields = "created_at,lang,author_id,context_annotations"
)


#Fetch Query results ####

response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/recent',
                      httr::add_headers(.headers = headers),
                      query = params)

response2 <-httr::GET(url = 'https://api.twitter.com/2/tweets/search/recent',
                      httr::add_headers(.headers = headers),
                      query = params.2)

response3 <-httr::GET(url = 'https://api.twitter.com/2/tweets/search/recent',
                      httr::add_headers(.headers = headers),
                      query = params.3)

response4 <- response3 <-httr::GET(url = 'https://api.twitter.com/2/tweets/search/recent',
                                   httr::add_headers(.headers = headers),
                                   query = params.4)


#Check status codes ###

response$status_code

response2$status_code

response3$status_code

response4$status_code


#Status code indicates successful query 
#Building and fetching with max 100 tweets is not computationally intensive
#Good for checking multiple queries and assessing query flexibility 
#Next iterations will try larger batches with iterations of nx10 

#Convert search queries into dataframes 

#keep an eye on content functions - they are returning errors but not impeding workflow 

response_body <- httr::content(response)

response_body2 <- httr::content(response2)

response_body3 <- httr::content(response3)

response_body4 <- httr::content(response4)

response_body4$meta

?content

#Return responsebody content as a list object with tweet metadata as column features

response_temp <- lapply(response_body$data, "[", c("id", "text", "lang", "author_id",
                                                   "created_at"))

response_temp2 <- lapply(response_body2$data, "[", c("id", "text", "lang", "author_id",
                                                     "created_at"))

response_temp3 <- lapply(response_body3$data, "[", c("id", "text", "lang", "author_id",
                                                     "created_at"))

response_temp4 <- lapply(response_body4$data, "[", c("id", "text", "lang", "author_id",
                                                     "created_at"))

#Convert to a dataframe for analyses 

response_temp <- lapply(response_temp, as.data.frame)

response_temp <- do.call(rbind, response_temp)

response_temp2 <- lapply(response_temp2, as.data.frame)

response_temp2 <- do.call(rbind, response_temp2)

response_temp_df <- response_temp

response_temp2_df <- response_temp2

response_temp3 <- lapply(response_temp3, as.data.frame)

response_temp3 <- do.call(rbind, response_temp3)

response_temp3_df <- response_temp3

response_temp4 <- lapply(response_temp4, as.data.frame)

response_temp4 <- do.call(rbind, response_temp4)

response_temp4_df <- response_temp4

#Give dataframe an intuitive name 


EVD.query1 <- response_temp_df

EVD.query2 <- response_temp2_df

EVD.query3 <- response_temp3_df

EVD.query4 <- response_temp4_df

#Recent search df and next_token_update should be supplied
#with the most recent EVD.query 

recent_search_df <- EVD.query4
next_token_update <- response_body4$meta$next_token

#Merge current query with existing query as a backup 

EVD.total <- rbind(EVD.query1, EVD.query2, EVD.query3, EVD.query4)

View(EVD.total)

#Large Pull with Wide-Net Search Term

for (i in 1:100) {
  params_page = list(
    `query` = '(Ebola OR EVD OR ebola virus OR ebola virus disease) -is:retweet lang:en',
    `max_results` = '100',
    `tweet.fields` = 'created_at,lang,author_id,context_annotations',
    `next_token` = next_token_update
  )
  
  response_page <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/recent', httr::add_headers(.headers=headers), query = params_page)
  print(response_page$status_code)
  
  rp_body <- httr::content(response_page)
  
  rp_temp <- lapply(rp_body$data, "[", c("id", "text", "lang", "author_id", "created_at"))
  rp_temp <- lapply(rp_temp, as.data.frame)
  rp_temp <- do.call(rbind, rp_temp)
  
  recent_search_df <- rbind(recent_search_df, rp_temp)
  
  next_token_update <- rp_body$meta$next_token
}

#Pre-Process and Clean Data #####

#Now we have something to pre-process and clean 

#Write pre-processing function 

## pre-processing text:
clean.text <- function(x)
{
  # convert to lower case
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  # some other cleaning text
  x = gsub('https://','',x)
  x = gsub('http://','',x)
  x = gsub('[^[:graph:]]', ' ',x)
  x = gsub('[[:punct:]]', '', x)
  x = gsub('[[:cntrl:]]', '', x)
  x = gsub('\\d+', '', x)
  x = stringr::str_replace_all(x,"[^[:graph:]]", " ")
  return(x)
}

#test on corpus 

#Clean corpus and label with today's date 

EVD.twt.clean.101922 <- rp_temp %>% 
  mutate(clean_text = clean.text(text)) 

#Frequency Counts + Visualizations ####

#Note wait until you have the full set since visualizations
#wont be as insightful with smaller sets 

#Clean time labels 

EVD.twt.clean <- EVD.twt.clean %>% 
  mutate(created_at = created_at %>%
           stringr::str_remove_all(pattern = '\\+0000') %>%
           lubridate::parse_date_time(orders = '%y-%m-%d %H%M%S'))


EVD.twt.clean <- EVD.twt.clean %>%
  mutate(created_at_round = created_at %>% round(units = 'hours') %>%
           as.POSIXct())

View(EVD.twt.clean)

#Double check time range format and truncate

EVD.twt.clean %>% pull(created_at_round) %>% min()

EVD.twt.clean %>% pull(created_at_round) %>% max()

EVD.twt.clean %>% dplyr::count(created_at_round)

#Visualize 

EVD.twt.freq.viz <- EVD.twt.clean %>% dplyr::count(created_at) %>%
  ggplot(mapping = aes(x = created_at, y = n)) +
  theme_light() +
  geom_line() +
  xlab(label = 'Timeframe (1 hour)') +
  ylab(label = NULL) + 
  ggtitle(label = "Number of Tweets per Hour")

View(EVD.twt.clean)

EVD.twt.freq.viz %>% plotly::ggplotly()

#Export individual and aggregated datasets for backup ####

#Individual Pulls

write.csv(EVD.twt.clean, "EVD.tweet.pull.Oct1722.csv")

write.csv(EVD.twt.clean.101822, "EVD.tweet.pull.Oct1822.csv")

write.csv(EVD.twt.clean.101922, "EVD.tweet.pull.Oct1922.csv")

#Write out one large pull per week for feasibility analyses 

