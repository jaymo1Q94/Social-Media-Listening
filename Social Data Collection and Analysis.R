#Installation ####
install.packages(c("httr", "dplyr", "jsonlite", 
                 "DiagrammeR", "ggplot2", "ggrepel", "mi" 
                  ,"networkD3", "nlme", "RColorBrewer", "tidyr", 
                 "tidytext", "tidyselect", "tokenizers", "topicmodels",
                 "wordcloud", "textdata"))

sna.pack <- c("httr", "dplyr", "jsonlite", 
              "DiagrammeR", "ggplot2", "ggrepel", "mi" 
              ,"networkD3", "nlme", "RColorBrewer", "tidyr", 
              "tidytext", "tidyselect", "tokenizers", "topicmodels",
              "wordcloud", "textdata")

lapply(sna.pack, require, character.only = T)


#Contact - David Silva - dataesilva@gmail.com 

#https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference
#https://developer.twitter.com/en/docs/twitter-api/tweets/search/integrate/build-a-query

#Steps ####
#1. Query tweets from .json format
#2. Parse the responses/.json files 
#3. Generate bearer token and save as a variable 
#4. Specify an authorization header with your bearer token variable
#5. Build your query parameters using keywords and Boolean operators
#5a. Note that query parameters are space and Boolean sensitive 
#6. GET your query using httr and store in a variable named response
#7. Check the status code of your response variable for success (200 = success)
#8. Merge the query response lists into a dataframe object
#9. Organize a tweet search by page using a for loop
#10. Move into sentiment analysis

bearer_token <- "AAAAAAAAAAAAAAAAAAAAANdRhQEAAAAAdmKlyRSrb3JIiAD8E7mbqSvuXYY%3DWQmBnVlU7Hq2BjOYZEKAincz0McltyySOSBxTxTc1L7FAC19P6"


headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))

params = list(
  query ='monkeypox OR mpx OR orthopox OR mpox OR mpoxx OR orthopoxvirus OR poxviridae',
  max_results = '10',
  tweet.fields = "created_at,lang,author_id,context_annotations"
)

params.2 <- list(
  query = '(monkeypox OR mpx OR mpox) -is:retweet lang:en',
  max_results = '10',
  tweet.fields = "created_at,lang,author_id,context_annotations"
)

response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/recent',
                      httr::add_headers(.headers = headers),
                      query = params)

response2 <-httr::GET(url = 'https://api.twitter.com/2/tweets/search/recent',
                                  httr::add_headers(.headers = headers),
                                  query = params.2)

?httr

response2$status_code

response$status_code

response_body <- content(response)

response_body2 <- content(response2)

View(response_body)

#Status code check indicates both queries were successful 

response_temp <- lapply(response_body$data, "[", c("id", "text", "lang", "author_id",
"created_at"))

response_temp2 <- lapply(response_body2$data, "[", c("id", "text", "lang", "author_id",
                                                    "created_at"))

response_temp <- lapply(response_temp, as.data.frame)

response_temp <- do.call(rbind, response_temp)

response_temp2 <- lapply(response_temp2, as.data.frame)

response_temp2 <- do.call(rbind, response_temp2)

response_temp_df <- response_temp

response_temp2_df <- response_temp2

#Update next token and record recent search 

#Construct for loop 

recent_search_df <- response_temp
next_token_update <- response_body$meta$next_token


for (i in 1:10) {
  params_page = list(
    `query` = '(monkeypox OR mpx OR mpox) -is:retweet lang:en',
    `max_results` = '100',
    `tweet.fields` = 'created_at,lang,author_id,context_annotations',
    `next_token` = next_token_update
  )
  
  response_page <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/recent', httr::add_headers(.headers=headers), query = params_page)
  print(response_page$status_code)
  
  rp_body <- content(response_page)
  
  rp_temp <- lapply(rp_body$data, "[", c("id", "text", "lang", "author_id", "created_at"))
  rp_temp <- lapply(rp_temp, as.data.frame)
  rp_temp <- do.call(rbind, rp_temp)
  
  recent_search_df <- rbind(recent_search_df, rp_temp)
  
  next_token_update <- rp_body$meta$next_token
}

tidy_twt <- recent_search_df %>%
  unnest_tokens(word, text)
data("stop_words")

custom_stop_words <- bind_rows(data_frame(word = c("https", "t.co"), lexicon = c("custom")), stop_words)

tidy_twt_clean <- tidy_twt %>% anti_join(stop_words)

tidy_twt <- tidy_twt %>% anti_join(custom_stop_words)

tidy_twt_clean %>% count(word, sort=TRUE) %>% View()

install.packages("textdata")
library(textdata)


sent_twt <- tidy_twt_clean %>% inner_join(get_sentiments("afinn")) %>% View()

sent_twt <- tidy_twt_clean %>% inner_join(get_sentiments("afinn")) %>% group_by(id = id) %>% summarise(sentiment = sum(value))
sent_twt <- merge(recent_search_df, sent_twt)
View(sent_twt)

?tidytext
?textdata

