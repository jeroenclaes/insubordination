#Packages

if(!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
} else {
  library(dplyr)
}

if(!require("stringi")) {
  install.packages("stringi")
  library(stringi)
} else {
  library(stringi)
}


if(!require("readr")) {
  install.packages("readr")
  library(readr)
} else {
  library(readr)
}

# Logging function
logmessage <- function(string, verbose=TRUE) {
  write(paste0(Sys.time(),": ", string), file=stderr())
}

# concordancer

makeConcordance <- function(filtered_corpus, pattern=NULL, col=c("tagged_text", "texto"))
{
  if(is.null(pattern)) {
    stop("You must define a search pattern for your concordance.")
  }
  
  dataFrame <- as.data.frame(filtered_corpus)
  dataFrame$clause_id<-1:nrow(dataFrame)
  lines <- mapply(dataFrame[, col],dataFrame$clause_id, SIMPLIFY = FALSE, FUN = function(x, y) {
    token <- stri_extract_all_regex(x, pattern)[[1]]
    splits <- stri_split_regex(x, pattern)
    before <- vector("character", length(token))
    after <- vector("character", length(token))
    id_str <- 1:length(token)
    id_str<-paste0("Clause:", y,"|token:",  id_str)
    
    for	(i in 1:length(token))
    {
      before[i] <- splits[[1]][i]
      after[i] <-  splits[[1]][i + 1]
    }
    return(data.frame(before = before, token = token, after = after, token_id = id_str, clause_id=y,stringsAsFactors = F))
  })
  concordance <- bind_rows(lines)
  return(left_join(concordance, dataFrame, join_by="id"))
  
}
files <- c('/Users/jeroenclaes/Desktop/UFAL_BA_TAGGED.csv', '/Users/jeroenclaes/Desktop/cola_ba_TAGGED.csv', '/Users/jeroenclaes/Desktop/cola_sc_TAGGED.csv')

lapply(files, FUN=function(x) {
  dataSet<-read_csv(x) %>%
    rename(tagged_text=tagged)
 
    
    
    
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
  taggedConc<-makeConcordance(dataSet, "(^que_sconj_que |[[:punct:]]que_sconj_que|[[:punct:]] que_sconj_que )", "tagged_text")
  
  
  textConc<-makeConcordance(dataSet, "(^que |[[:punct:]]que |[[:punct:]] que )", "texto") %>%
    select(before, token, after, token_id, clause_id) %>%
    rename(text_before=before, text_token=token, text_after=after)
  
  taggedConc <- taggedConc %>% 
      left_join(textConc, by=c("token_id", "clause_id"))
  dataSet<-taggedConc
  
  
  # Add turn-initial/turn internal coding for /que/
  dataSet <- dataSet %>%
    ungroup() %>%
    mutate(turn_position=ifelse(stri_length(before)==0, "turn-initial", "turn-internal")) 
  
  
  # Extract clause
  dataSet <- dataSet %>%
    mutate(clause=stri_extract_first_regex(tagged_text,"que_sconj_que (\\b([[:alnum:]_\\.]+)\\b( |))+"))  
  
  
  
  
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
  
  write_csv(dataSet, stri_replace_all_fixed(x, ".csv", "_PROCESSED.csv"))
  
  
})

