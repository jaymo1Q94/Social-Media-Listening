#Packages ####

library(twitteR)
library(ROAuth)
library(hms)
library(lubridate) 
library(tidytext)
library(tm)
library(wordcloud)
library(igraph)
library(glue)
library(networkD3)
library(rtweet)
library(plyr)
library(stringr)
library(ggplot2)
library(ggeasy)
library(plotly)
library(dplyr)  
library(hms)
library(lubridate) 
library(magrittr)
library(tidyverse)
library(janeaustenr)
library(widyr)

#Additional packages ####

sna.pack <- c("httr", "dplyr", "jsonlite", 
              "DiagrammeR", "ggplot2", "ggrepel", "mi" 
              ,"networkD3", "nlme", "RColorBrewer", "tidyr", 
              "tidytext", "tidyselect", "tokenizers", "topicmodels",
              "wordcloud", "textdata")

lapply(sna.pack, require, character.only = T)
