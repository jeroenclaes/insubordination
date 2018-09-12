# Title: Tweet processing engine
# Client: Prof. Dr. Pedro Gras-Manzano/Prof. Dr. Frank Brisard (Univerity of Antwerp)
# Date:  Wed Jan 31 08:49:13 2017

# ================
# PACKAGES
# ================
library(dplyr)
library(readr)
library(stringi)
library(parallel)
library(twitteR)

# ================
# OPTIONS
# ================

options(scipen = 999, stringsAsFactors = F, mc.cores=8)

# ================
# DB connection
# ================



# ================
# CUSTOM FUNCTIONS
# ================

logmessage <- function(string, verbose=TRUE) {
  if(verbose) {
    write(paste(Sys.time(),  string, sep=": "), file=stderr())
  }
  
}

addGeoColumn <-function(tweetDataFrame) {
  tweetDataFrame <- tweetDataFrame %>%
    mutate(geoColumn = ifelse(!is.na(country), country, ifelse(!is.na(time_zone) & !grepl("US & Canada", time_zone), time_zone, ifelse(!is.na(place_name), place_name, ifelse(!is.na(location), location, NA)))))
  
  return(tweetDataFrame)
}



TreeTag <- function(text, parserPath)
{
  tagged <- mclapply(text, FUN = function(x) {
    
    output <- system(
      paste0("echo ", shQuote(x),"|", parserPath, sep=""),
      intern = T, ignore.stdout = F, ignore.stderr = F)
    output <- stri_split_fixed(output, "\t")
    output <- do.call(rbind, output)
    output <- as.data.frame(output)
    names(output) <- c("Word", "POS", "Lemma")
    output$slashtags <- mapply(output$Word, output$POS, output$Lemma,
                               FUN = function(x, y, z) {
                                 paste(x, y, ifelse(z == "<unknown>", x, z), sep = "_")})
    output <- paste(output$slashtags, collapse=" ") %>%
      stri_trans_tolower() 
    return(output)
  })
  
  return(unlist(tagged))
}



getMissingTweets <-function(ids) {
  
  
  api_key <- ""
  api_secret <- "" 
  token <- "" 
  token_secret <- ""
  access_token<-""
  
  
  setup_twitter_oauth(api_key, api_secret)
  batches<- seq(1,length(ids), by=50)
  
  lapply(batches, FUN=function(batch) {
    
    if(batch!=max(batches)) {
      statuz<-lookup_statuses(ids[batch:(batch+50)])
      
    } else {
      statuz<-lookup_statuses(ids[batch:length(ids)])
      
    }
    statuz<-twListToDF(statuz)
    write_csv(statuz, paste0("missingTweets_", batch, "-",(batch+50), ".csv"))
    
    return(statuz)
  })
  
  
}

# ================
# DATA
# ================

# Extracting IDs of conversations that contain 'que' in the right structural pattern
conversationIds <- mclapply(list.files("csv/", "csv", full.names=TRUE), FUN = function(file) {
  convs<-read_csv(file, col_types = cols(
    text = col_character(),
    retweet_count = col_character(),
    favorited = col_logical(),
    truncated = col_logical(),
    id_str = col_character(),
    in_reply_to_screen_name = col_character(),
    source = col_character(),
    retweeted = col_logical(),
    created_at = col_datetime(format = ""),
    in_reply_to_status_id_str = col_character(),
    in_reply_to_user_id_str = col_character(),
    lang = col_character(),
    listed_count = col_character(),
    verified = col_logical(),
    location = col_character(),
    user_id_str = col_character(),
    description = col_character(),
    geo_enabled = col_logical(),
    user_created_at = col_datetime(format = ""),
    statuses_count = col_character(),
    followers_count = col_character(),
    favourites_count = col_character(),
    protected = col_logical(),
    user_url = col_character(),
    name = col_character(),
    time_zone = col_character(),
    user_lang = col_character(),
    utc_offset = col_character(),
    friends_count = col_character(),
    screen_name = col_character(),
    country_code = col_character(),
    country = col_character(),
    place_type = col_character(),
    full_name = col_character(),
    place_name = col_character(),
    place_id = col_character(),
    place_lat = col_character(),
    place_lon = col_character(),
    lat = col_character(),
    lon = col_character(),
    expanded_url = col_character(),
    url = col_character()
  )) %>% 
    filter(grepl("(^que |^q |^k |^ke |[[:punct:]]que |[[:punct:]]q |[[:punct:]]k |[[:punct:]]ke |^ que |^ q |^ k |^ ke |[[:punct:]] que |[[:punct:]] q |[[:punct:]] k |[[:punct:]] ke |^que |^@[[:alnum:]]+ que |^q |^@[[:alnum:]]+ q | ^k |^@[[:alnum:]]+ k |^ke |^@[[:alnum:]]+ ke  )", perl=TRUE, ignore.case=TRUE, text)) %>%
    filter(!grepl("^RT", text)) %>%
    filter(!grepl("(que tal|que ganas|que( [[:alpha:]]+)? buenos|qué ganas|qué( [[:alpha:]]+)? buenos|que asco|qué asco|que noche|que dia|que tarde|que horas|que mal|que mierda|que mal|que( [[:alpha:]]+)? linda|que( [[:alpha:]]+)? feo|que manera|que( [[:alpha:]]+)? lindo|que( [[:alpha:]]+)? vieja|que( [[:alpha:]]+)? viejo|que( [[:alpha:]]+)? bueno|que( [[:alpha:]]+)? mala|que( [[:alpha:]]+)? buena|que( [[:alpha:]]+)? buenas|que( [[:alpha:]]+)? lindos|que( [[:alpha:]]+)? lindas|que poco|que pocos|que poca|que pocas|que pena|que lastima|que lástima|que frio|que calor|que frío|que ric|que raro|que hermos|que delicia|que delicios|q ganas|que sueno|que sueño|q manera|que vergüenza|q vergüenza|que agradable|que miedo|que carajo|que aburrido|que interesante|que( [[:alpha:]]+)? bonit|que dolor|que porqueria|que porquería)",text, perl=TRUE, ignore.case=TRUE)) %>%
    pull(in_reply_to_status_id_str) %>%
    unique()
  cat(convs, file=paste0("convIds/",basename(file)
                         ,"_conversationIds.txt"), append=F, sep="\n")
  
})


# Joining Ids in one file

conversationIds <- unlist(mclapply(list.files("convIds", "txt", full.names=TRUE), readLines))
conversationIds <- conversationIds[conversationIds!=""]
conversationIds <- unique(conversationIds)

# Looping over raw data files to extract all tweets that belong to the selected conversation ids
# Retweets are excluded
dataSet <- mclapply(list.files("csv", "csv", full.names=TRUE), FUN = function(file) {
  dat<-read_csv(file,col_types = cols(
    text = col_character(),
    retweet_count = col_character(),
    favorited = col_logical(),
    truncated = col_logical(),
    id_str = col_character(),
    in_reply_to_screen_name = col_character(),
    source = col_character(),
    retweeted = col_logical(),
    created_at = col_datetime(format = ""),
    in_reply_to_status_id_str = col_character(),
    in_reply_to_user_id_str = col_character(),
    lang = col_character(),
    listed_count = col_character(),
    verified = col_logical(),
    location = col_character(),
    user_id_str = col_character(),
    description = col_character(),
    geo_enabled = col_logical(),
    user_created_at = col_datetime(format = ""),
    statuses_count = col_character(),
    followers_count = col_character(),
    favourites_count = col_character(),
    protected = col_logical(),
    user_url = col_character(),
    name = col_character(),
    time_zone = col_character(),
    user_lang = col_character(),
    utc_offset = col_character(),
    friends_count = col_character(),
    screen_name = col_character(),
    country_code = col_character(),
    country = col_character(),
    place_type = col_character(),
    full_name = col_character(),
    place_name = col_character(),
    place_id = col_character(),
    place_lat = col_character(),
    place_lon = col_character(),
    lat = col_character(),
    lon = col_character(),
    expanded_url = col_character(),
    url = col_character()
  )) %>% 
    filter(in_reply_to_status_id_str %in% conversationIds|id_str %in% conversationIds) %>%
    filter(!grepl("^RT", text)) %>%
    arrange(in_reply_to_status_id_str, created_at) %>%
    addGeoColumn() %>%
    select(text,id_str,created_at, in_reply_to_screen_name, in_reply_to_status_id_str,in_reply_to_status_id_str,  lang, user_id_str, screen_name,full_name, geoColumn) %>%
    write_csv(stri_replace_all_fixed(file, "csv/", "filteredData/"))
  return(dat)
})%>% bind_rows() 


# Excluding cases of Que that occur in the wrong structural pattern; adding text to which is reponded
dataSet <- dataSet %>%
  group_by(in_reply_to_status_id_str) %>%
  mutate(in_reply_to_text=ifelse(length(text[which(in_reply_to_status_id_str==id_str)]) >0, text[which(in_reply_to_status_id_str==id_str)], NA)) %>%
  ungroup() %>%
  mutate(que_sentence= ifelse(grepl("(^que |^q |^k |^ke |[[:punct:]]que |[[:punct:]]q |[[:punct:]]k |[[:punct:]]ke |^ que |^ q |^ k |^ ke |[[:punct:]] que |[[:punct:]] q |[[:punct:]] k |[[:punct:]] ke |^que |^@[[:alnum:]]+ que |^q |^@[[:alnum:]]+ q | ^k |^@[[:alnum:]]+ k |^ke |^@[[:alnum:]]+ ke  )", perl=TRUE, ignore.case=TRUE, text), TRUE, FALSE)) %>%
  mutate(que_sentence= ifelse(grepl("(que tal|que ganas|que( [[:alpha:]]+)? buenos|qué ganas|qué( [[:alpha:]]+)? buenos|que asco|qué asco|que noche|que dia|que tarde|que horas|que mal|que mierda|que mal|que( [[:alpha:]]+)? linda|que( [[:alpha:]]+)? feo|que manera|que( [[:alpha:]]+)? lindo|que( [[:alpha:]]+)? vieja|que( [[:alpha:]]+)? viejo|que( [[:alpha:]]+)? bueno|que( [[:alpha:]]+)? mala|que( [[:alpha:]]+)? buena|que( [[:alpha:]]+)? buenas|que( [[:alpha:]]+)? lindos|que( [[:alpha:]]+)? lindas|que poco|que pocos|que poca|que pocas|que pena|que lastima|que lástima|que frio|que calor|que frío|que ric|que raro|que hermos|que delicia|que delicios|q ganas|que sueno|que sueño|q manera|que vergüenza|q vergüenza|que agradable|que miedo|que carajo|que aburrido|que interesante|que( [[:alpha:]]+)? bonit|que dolor|que porqueria|que porquería)",text, perl=TRUE, ignore.case=TRUE), FALSE, que_sentence)) %>%
  mutate(que_sentence= ifelse(grepl("(que |q |ke |k )(insuficiente|belleza|hermos|basura|difícil|dificil|magnific|magnífic|cosa|felicidad|bien|molest|lind|aburrid|triste|pena|tiempo|clima|estúpid|grande|estrés|estres|día|calor|barbaridad|amig|hambre|puta|rabia|amor|hijo|fin|flojera|pedo|buen|problema|mujer|hombre|paso|pasó|sabes|sabés|puto|momento|ridículo|tan|estoy|estas|estás|onda|marica|maricon|maricón|idiota|lent|terrible|paja|tal|va|estupido|merda|mierda|luz|cansad|gran|semana|mes|complicad|horror|pasa|hace|cambio|pesadilla|embole|capítulo|falta|mon|hino|duro)",text, perl=TRUE, ignore.case=TRUE), FALSE, que_sentence))


# Listing missing 'target' tweet ids, tweet ids to which users respond but that are not in our data
missingTweetIds <- dataSet %>%
  ungroup() %>%
  filter(is.na(in_reply_to_text)) %>%
  pull("in_reply_to_status_id_str") %>%
  unique()

# Searching original data files for the missing tweets (usually retweets)
missingTweets <- mclapply(list.files("csv", "csv", full.names=TRUE), FUN = function(file) {
  dat<-read_csv(file,col_types = cols(
    text = col_character(),
    retweet_count = col_character(),
    favorited = col_logical(),
    truncated = col_logical(),
    id_str = col_character(),
    in_reply_to_screen_name = col_character(),
    source = col_character(),
    retweeted = col_logical(),
    created_at = col_datetime(format = ""),
    in_reply_to_status_id_str = col_character(),
    in_reply_to_user_id_str = col_character(),
    lang = col_character(),
    listed_count = col_character(),
    verified = col_logical(),
    location = col_character(),
    user_id_str = col_character(),
    description = col_character(),
    geo_enabled = col_logical(),
    user_created_at = col_datetime(format = ""),
    statuses_count = col_character(),
    followers_count = col_character(),
    favourites_count = col_character(),
    protected = col_logical(),
    user_url = col_character(),
    name = col_character(),
    time_zone = col_character(),
    user_lang = col_character(),
    utc_offset = col_character(),
    friends_count = col_character(),
    screen_name = col_character(),
    country_code = col_character(),
    country = col_character(),
    place_type = col_character(),
    full_name = col_character(),
    place_name = col_character(),
    place_id = col_character(),
    place_lat = col_character(),
    place_lon = col_character(),
    lat = col_character(),
    lon = col_character(),
    expanded_url = col_character(),
    url = col_character()
  )) %>% 
    filter(id_str %in% missingTweetIds) %>%
    addGeoColumn() %>%
    select(text,id_str,created_at, in_reply_to_screen_name, in_reply_to_status_id_str,in_reply_to_status_id_str,  lang, user_id_str, screen_name,full_name, geoColumn) 
  return(dat)
})%>% bind_rows() 


# Adding missing tweets back to data and calculating the 'target' text again

dataSet <- dataSet %>%
  bind_rows(missingTweets) %>%
  group_by(in_reply_to_status_id_str) %>%
  mutate(in_reply_to_text=ifelse(length(text[which(in_reply_to_status_id_str==id_str)]) >0, text[which(in_reply_to_status_id_str==id_str)], NA))


# check for que_sentences again
dataSet <- dataSet %>%
  ungroup() %>%
  mutate(que_sentence= ifelse(grepl("(^que |^q |^k |^ke |[[:punct:]]que |[[:punct:]]q |[[:punct:]]k |[[:punct:]]ke |^ que |^ q |^ k |^ ke |[[:punct:]] que |[[:punct:]] q |[[:punct:]] k |[[:punct:]] ke |^que |^@[[:alnum:]]+ que |^q |^@[[:alnum:]]+ q | ^k |^@[[:alnum:]]+ k |^ke |^@[[:alnum:]]+ ke  )", perl=TRUE, ignore.case=TRUE, text), TRUE, FALSE)) %>%
  mutate(que_sentence= ifelse(grepl("(que tal|que ganas|que( [[:alpha:]]+)? buenos|qué ganas|qué( [[:alpha:]]+)? buenos|que asco|qué asco|que noche|que dia|que tarde|que horas|que mal|que mierda|que mal|que( [[:alpha:]]+)? linda|que( [[:alpha:]]+)? feo|que manera|que( [[:alpha:]]+)? lindo|que( [[:alpha:]]+)? vieja|que( [[:alpha:]]+)? viejo|que( [[:alpha:]]+)? bueno|que( [[:alpha:]]+)? mala|que( [[:alpha:]]+)? buena|que( [[:alpha:]]+)? buenas|que( [[:alpha:]]+)? lindos|que( [[:alpha:]]+)? lindas|que poco|que pocos|que poca|que pocas|que pena|que lastima|que lástima|que frio|que calor|que frío|que ric|que raro|que hermos|que delicia|que delicios|q ganas|que sueno|que sueño|q manera|que vergüenza|q vergüenza|que agradable|que miedo|que carajo|que aburrido|que interesante|que( [[:alpha:]]+)? bonit|que dolor|que porqueria|que porquería)",text, perl=TRUE, ignore.case=TRUE), FALSE, que_sentence)) %>%
  mutate(que_sentence= ifelse(grepl("(que |q |ke |k )(insuficiente|belleza|hermos|basura|difícil|dificil|magnific|magnífic|cosa|felicidad|bien|molest|lind|aburrid|triste|pena|tiempo|clima|estúpid|grande|estrés|estres|día|calor|barbaridad|amig|hambre|puta|rabia|amor|hijo|fin|flojera|pedo|buen|problema|mujer|hombre|paso|pasó|sabes|sabés|puto|momento|ridículo|tan|estoy|estas|estás|onda|marica|maricon|maricón|idiota|lent|terrible|paja|tal|va|estupido|merda|mierda|luz|cansad|gran|semana|mes|complicad|horror|pasa|hace|cambio|pesadilla|embole|capítulo|falta|mon|hino|duro)",text, perl=TRUE, ignore.case=TRUE), FALSE, que_sentence))



# Calculating missing tweet Ids again
missingTweetIds <- dataSet %>%
  ungroup() %>%
  filter(is.na(in_reply_to_text)) %>%
  pull("in_reply_to_status_id_str") %>%
  unique()

# Downloading statuses by ID that do not occur in our raw data, but to which users respond in the corpus

extraData<-getMissingTweets(missingTweetIds) %>%
  bind_rows()


extraData <- extraData %>% select(text, id, screenName, created) %>%
  rename(id_str=id, created_at=created, screen_name=screenName) %>%
  mutate(in_reply_to_status_id_str=id_str, in_reply_to_screen_name=NA)

dataSet <- dataSet %>% bind_rows(extraData)



# add extra information
dataSet <- dataSet %>%
  ungroup() %>%
  mutate(que_sentence= ifelse(grepl("(^que |^q |^k |^ke |[[:punct:]]que |[[:punct:]]q |[[:punct:]]k |[[:punct:]]ke |^ que |^ q |^ k |^ ke |[[:punct:]] que |[[:punct:]] q |[[:punct:]] k |[[:punct:]] ke |^que |^@[[:alnum:]]+ que |^q |^@[[:alnum:]]+ q | ^k |^@[[:alnum:]]+ k |^ke |^@[[:alnum:]]+ ke  )", perl=TRUE, ignore.case=TRUE, text), TRUE, FALSE)) %>%
  mutate(que_sentence= ifelse(grepl("(que tal|que ganas|que( [[:alpha:]]+)? buenos|qué ganas|qué( [[:alpha:]]+)? buenos|que asco|qué asco|que noche|que dia|que tarde|que horas|que mal|que mierda|que mal|que( [[:alpha:]]+)? linda|que( [[:alpha:]]+)? feo|que manera|que( [[:alpha:]]+)? lindo|que( [[:alpha:]]+)? vieja|que( [[:alpha:]]+)? viejo|que( [[:alpha:]]+)? bueno|que( [[:alpha:]]+)? mala|que( [[:alpha:]]+)? buena|que( [[:alpha:]]+)? buenas|que( [[:alpha:]]+)? lindos|que( [[:alpha:]]+)? lindas|que poco|que pocos|que poca|que pocas|que pena|que lastima|que lástima|que frio|que calor|que frío|que ric|que raro|que hermos|que delicia|que delicios|q ganas|que sueno|que sueño|q manera|que vergüenza|q vergüenza|que agradable|que miedo|que carajo|que aburrido|que interesante|que( [[:alpha:]]+)? bonit|que dolor|que porqueria|que porquería)",text, perl=TRUE, ignore.case=TRUE), FALSE, que_sentence)) %>%
  mutate(que_sentence= ifelse(grepl("(que |q |ke |k )(insuficiente|belleza|hermos|basura|difícil|dificil|magnific|magnífic|cosa|felicidad|bien|molest|lind|aburrid|triste|pena|tiempo|clima|estúpid|grande|estrés|estres|día|calor|barbaridad|amig|hambre|puta|rabia|amor|hijo|fin|flojera|pedo|buen|problema|mujer|hombre|paso|pasó|sabes|sabés|puto|momento|ridículo|tan|estoy|estas|estás|onda|marica|maricon|maricón|idiota|lent|terrible|paja|tal|va|estupido|merda|mierda|luz|cansad|gran|semana|mes|complicad|horror|pasa|hace|cambio|pesadilla|embole|capítulo|falta|mon|hino|duro)",text, perl=TRUE, ignore.case=TRUE), FALSE, que_sentence))

# Adding these extra data to the corpus and calculating the fields again
dataSet <- dataSet %>%
  group_by(in_reply_to_status_id_str) %>%
  mutate(in_reply_to_text=ifelse(length(text[which(in_reply_to_status_id_str==id_str)]) >0, text[which(in_reply_to_status_id_str==id_str)], NA))



# Filtering dataset: only data that belong to conversations that have at least one 'que' sentence stay in the corpus
dataSet <- dataSet %>%
  group_by(in_reply_to_status_id_str) %>%
  mutate(n_que_sentence=n_distinct(text[which(que_sentence==TRUE)])) %>%
  filter(n_que_sentence > 0) %>%
  mutate(n_tweets_in_conversation= n())


dataSet <- dataSet %>%
  ungroup() %>%
  filter(!duplicated(id_str)) %>%
  filter(!duplicated(text))


# Remove non-alphanumeric, spaces and punctuation characters from in_reply_to_text
dataSet <- dataSet %>%
  mutate(in_reply_to_text=stri_replace_all_regex(in_reply_to_text, "[^[:alnum:][:punct:] ]", ""))

# checkpointing
save(dataSet, file="final_data_geocoded.rdata")

# Filter out que-sentence candidates
setToTag <- dataSet %>% 
  ungroup() %>%
  filter(que_sentence==TRUE)

# POS-tag que-sentences
setToTag$tagged_text <-TreeTag(setToTag$text, "~/treeTagger/cmd/tree-tagger-spanish-ancora")


# Add back to the dataset

dataSet <- dataSet %>% 
  ungroup() %>%
  filter(que_sentence==FALSE) %>%
  mutate(tagged_text=NA) %>%
  bind_rows(setToTag)

# Find different tags/spellings of QUE-alternatives




qq<-stri_extract_all_regex(dataSet$tagged_text, "\\b((que|ke|k|q)_[[:alnum:]\\.]+_(que|ke|k|q))\\b")
qq<-qq[!is.na(qq)] %>%
  unlist() %>%
  unique()

# Change spelling and class of /que/ alternatives to  /que_sconj_que/

for (i in c(1, 10, 20, 30, 40, 50, 60)) {
  start <- i
  stop <- i+10
  dataSet <- dataSet %>%
    mutate(tagged_text=stri_replace_all_regex(tagged_text, paste0("\\b(", paste0(qq[start:stop], collapse="|"), ")\\b"), "que_sconj_que"))  
  
  
}




# Remove double spaces from text and tagged_text
dataSet <- dataSet %>%
  mutate(text=stri_replace_all_regex(text, "[ ]+", " ")) %>%
  mutate(tagged_text=stri_replace_all_regex(tagged_text, "[ ]+", " "))

# Label as non-que-sentence all instances of /que/ followed by a noun or adjective
dataSet <- dataSet %>%
  mutate(que_sentence=ifelse(grepl("que_sconj_que [[:alpha:]]+(\\.|)_(adj|noun|propn|adv)", tagged_text, perl=TRUE), FALSE, que_sentence)) %>%
  mutate(que_sentence=ifelse(grepl("¿que", text, ignore.case=TRUE), FALSE, que_sentence)) %>%
  mutate(que_sentence=ifelse(grepl("¡que", text, ignore.case=TRUE), FALSE, que_sentence))


# Filtering dataset: only data that belong to conversations that have at least one 'que' sentence stay in the corpus
dataSet <- dataSet %>%
  group_by(in_reply_to_status_id_str) %>%
  mutate(n_que_sentence=n_distinct(text[which(que_sentence==TRUE)])) %>%
  ungroup() %>%
  filter(n_que_sentence > 0) %>%
  mutate(n_tweets_in_conversation= n())


# Add turn-initial/turn internal coding for /que/
dataSet <- dataSet %>%
  ungroup() %>%
  mutate(turn_position=ifelse(grepl("((^(@[[:alnum:]]+)+ (que |ke |q |k ))|(^(que |ke |k |q )))", text, ignore.case=TRUE, perl=TRUE), "turn-initial", "turn-internal")) %>%
  mutate(turn_position=ifelse(que_sentence==FALSE, NA, turn_position))


# Extract clause
dataSet <- dataSet %>%
  mutate(clause=stri_extract_first_regex(tagged_text,"que_sconj_que (\\b([[:alnum:]_\\.]+)\\b( |))+")) %>%
  mutate(clause=ifelse(que_sentence==FALSE, NA, clause))


# Extract verb from clause
dataSet <- dataSet %>%
  mutate(verb=stri_extract_first_regex(clause, "\\b([[:alpha:]]+_(verb|aux)[[:alnum:]\\.]+_[[:alpha:]]+)\\b"))

# Extract verb_tag from verb
dataSet <- dataSet %>%
  mutate(verb_tag=stri_extract_first_regex(verb, "_(verb|aux)[[:alnum:]\\.]+_")) %>%
  mutate(verb_tag=stri_replace_all_fixed(verb_tag, "_aux.", "")) %>%
  mutate(verb_tag=stri_replace_all_fixed(verb_tag, "_verb.", "")) %>%
  mutate(verb_tag=stri_replace_all_fixed(verb_tag, "_", ""))


# PARSE verb tag to multiple columns
# verb.ind.sing.2.pres
dataSet <- dataSet %>%
  mutate(mode=stri_extract_first_regex(verb_tag,"(ind|sub)")) %>%
  mutate(grammatical_person=stri_extract_first_regex(verb_tag, "(sing|plur)\\.(1|2|3)"))

# Add subjunctive vs indicative coding


# Add verb lemma
dataSet <- dataSet %>%
  mutate(verb_lemma=stri_extract_last_regex(verb, "_[:alpha:]+")) %>%
  mutate(verb_lemma=stri_replace_all_fixed(verb_lemma, "_", ""))


dataSet <- dataSet %>%
  mutate(grammatical_person=stri_extract_first_regex(verb, "(sing|plur)\\.[[:digit:]]{1}"))



# Arrange in correct order
dataSet <- dataSet %>%
  arrange(in_reply_to_status_id_str, created_at) 
dataSet$country<-ifelse(grepl("(canarias|tenerife|el hierro|el hierro|la palma|la gomera|gran canaria|fuerteventura|lanzarote)", dataSet$geoColumn, perl=TRUE), "Canarias", dataSet$country)

# Done
save(dataSet, file="final_data_geocoded_POS_tagged.rdata")
que_sentences_only <- dataSet %>% filter(que_sentence==TRUE)
save(que_sentences_only, file="twitterCorpus/deliverables/rdata/que_sentences_only.rdata")

# Select only in_reply_to_text, text, screen_name, country, in_reply_to_status_id_str, grammatical_person, mode, verb_lemma, one file per "country"

lapply(unique(que_sentences_only$country), FUN=function(x) {
que_sentences_only %>% 
    filter(country==x) %>% 
    mutate(in_reply_to_text=ifelse(in_reply_to_text==text, NA, in_reply_to_text)) %>% 
             select (in_reply_to_text, text, screen_name, country, in_reply_to_status_id_str, grammatical_person, mode, verb_lemma, turn_position) %>% 
             write_csv(paste0(stri_replace_all_fixed(x, "/", "_"),"_que_sentences_only.csv"), na="")
  print(x)
  
  return(NULL)
})

# summarise wordcounts and corpus information

dataSet %>% group_by(country) %>% summarise(words_total=sum(stri_count_boundaries(text)), words_que_sentences=sum(stri_count_boundaries(text[which(que_sentence==TRUE)])),n_tweets=n_distinct(id_str),n_tweets_que=n_distinct(id_str[which(que_sentence==TRUE)]),  n_conversations=n_distinct(in_reply_to_status_id_str) ) %>% write_csv("counts.csv")


## samples: 400 subjunctives per variety



lapply(unique(que_sentences_only$country), FUN=function(x) {
 ss<- que_sentences_only %>% 
    filter(country==x) %>% 
    filter(mode=="sub") %>%
    mutate(in_reply_to_text=ifelse(in_reply_to_text==text, NA, in_reply_to_text)) %>% 
    select (in_reply_to_text, text, screen_name, country, in_reply_to_status_id_str, grammatical_person, mode, verb_lemma, turn_position)
  
 if(nrow(ss)>400) {
   indices<-sample(1:nrow(ss), 400, replace = F)
   sample<-ss[indices,]
 } else {
   sample<-ss
   
 }
  
    write_csv(sample, paste0(stri_replace_all_fixed(x, "/", "_"),"SAMPLE_que_sentences_only.csv"), na="")
  print(x)
  
  return(NULL)
})



