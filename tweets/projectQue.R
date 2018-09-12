# Title: Tweet collection engine
# Client: Prof. Dr. Pedro Gras-Manzano/Prof. Dr. Frank Brisard (Univerity of Antwerp)
# Description: This script will track the top-400 most frequent words in Spanish on Twitter Globally. The frequency list was taken from the Real Academia Española's website (http://corpus.rae.es/frec/1000_formas.TXT)
# Author: Jeroen Claes
# Date:  Sun Dec 24 13:34:13 2017

# ================
# PACKAGES
# ================
library(dplyr)
library(readr)
library(streamR)
library(ROAuth)
library(stringi)
library(readr)
library(utils)
library(methods)

# ================
# OPTIONS
# ================

options(scipen = 999, stringsAsFactors = F)

# ================
# CONSTANTS
# ================


# ================
# CUSTOM FUNCTIONS
# ================

logmessage <- function(string, verbose=TRUE) {
 if(verbose) {
    write(paste(Sys.time(),  string, sep=": "), file=stderr())
 }
 
}

parseTwitterDate <- function(twitterDateVector) {
  twitterDateVector <- stringi::stri_replace_all_regex(twitterDateVector, "^[:alpha:]{3} ", "")
  month<-stringi::stri_extract_first_regex(twitterDateVector, "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Oct|Sep|Nov|Dec)")
  month <- dplyr::recode(month, Jan=1, Feb=2, Mar=3, Apr=4, May=5, Jun=6, Jul=7, Aug=8, Sep=9, Oct=10, Nov=11, Dec=12)
  year<-stringi::stri_extract_last_regex(twitterDateVector, "[0-9]{4}$")
  twitterDateVector <- stringi::stri_replace_all_regex(twitterDateVector,  " [0-9]{4}$", "")
  
  twitterDateVector <- stringi::stri_replace_all_regex(twitterDateVector, "^[:alpha:]{3} ", "")
  day <- stringi::stri_extract_first_regex(twitterDateVector,"^[0-9]{1,2}")
  twitterDateVector <- stringi::stri_replace_all_regex(twitterDateVector, "^[0-9]{1,2} ", "")
  
 return(as.POSIXct(paste0(year,"-", month,"-",day," ", twitterDateVector), format="%Y-%m-%d %H:%M:%S %z"))
  
}


addGeoColumn <-function(tweetDataFrame) {
  tweetDataFrame <- tweetDataFrame %>%
    mutate(geoColumn = ifelse(!is.na(country), country, ifelse(!is.na(time_zone) & !grepl("US & Canada", time_zone), time_zone, ifelse(!is.na(place_name), place_name, ifelse(!is.na(location), location, NA)))))
  
  return(tweetDataFrame)
}
  
# ================
# DATA
# ================

# Load OATH token
setwd("/mnt/disks/storage/projectQue")
load("inputData/my_oath.rdata")

# Read frequency list and extract words from data.frame
track <- readr::read_tsv("inputData/frequencyList.txt") %>%
  dplyr::pull(Frec.absoluta) 


# ================
# LOGIC
# ================

# Create filename for our json file, which will store the tweet stream

  
# Check if supporting directories exist 
  if(!dir.exists("csv/")) {
    dir.create("csv/")
  }

  if(!dir.exists("json/")) {
    dir.create("json/")
  }
  
# Track the tweets; we will track tweets in 15-minute batches
repeat {
  date <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  filename <- paste0("json/tweets_", date, ".json")
  tweets <- tryCatch(streamR::filterStream(filename,
                         track = track, language="es",
                         timeout = 300, oauth = my_oauth), 
                     warning = function(w) w, 
                     error = function(e) e)
  
  if (!class(tweets) %in% c("try-error", "error", "warning")) {
    
    tweets <- try(readLines(filename, encoding = "UTF-16"))
    
    if(length(tweets) > 0) {
      
      tweets <- try(streamR::parseTweets(tweets))
      if(!"try-error" %in% class(tweets))
      {
      tweets <- tweets %>%
        dplyr::mutate(created_at=as.character(created_at), id_str=as.character(id_str)) %>%
        dplyr::mutate(text=stringi::stri_replace_all_regex(text, "[^[:alnum:][:punct:] ]", "")) %>%
        dplyr::mutate(in_reply_to_status_id_str=ifelse(is.na(in_reply_to_status_id_str), id_str, in_reply_to_status_id_str)) %>%
        dplyr::mutate(created_at=parseTwitterDate(created_at), user_created_at=parseTwitterDate(user_created_at)) %>%
        dplyr::filter(!is.na(text)) 
        
      
# ================
# OUTPUT
# ================
      
      readr::write_csv(tweets, path=stringi::stri_replace_all_fixed(filename, "json", "csv"))
      removal <- file.remove(filename)
      logmessage(paste0(filename, " was parsed to CSV file"))
      } else {
        logmessage("Failed to parse tweet stream to data.frame")
      }
    } else {
      logmessage("Stream is of length zero.")
    }
    
  } else {
    logmessage("Trycatch error")
  }
  
} 

