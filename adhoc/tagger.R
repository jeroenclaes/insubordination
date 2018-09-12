library(parallel)
library(stringi)
library(readr)
library(dplyr)
options(mc.cores=4, stringsAsFactors = FALSE)

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

files <- c('/Users/jeroenclaes/Desktop/UFAL_BA.csv', '/Users/jeroenclaes/Desktop/cola_ba.csv', '/Users/jeroenclaes/Desktop/cola_sc.csv')


lapply(files, FUN=function(file) {
  dat<-read_csv(file)
  text <- dat$texto %>%
    stri_replace_all_regex("(\\[|\\])", "") %>%
    stri_replace_all_regex("^t ", "") %>%
    stri_replace_all_regex("^[[:alnum:]]+\\:[[:digit:]]+","") %>%
    stri_replace_all_regex("^[[:alnum:]]+\\:","") %>%
    stri_replace_all_regex("^l10", "") %>%
    stri_replace_all_regex("[\\\\]+", " ") %>%
    stri_replace_all_regex( "(\\<ingl\\-\\>|\\<\\-ingl\\>|\\<i|imi\\-\\>|\\<\\-i|imi\\>)", " ")%>%
    stri_replace_all_regex("(\\<[[:alpha:]]\\>)", "") %>%
  stri_replace_all_regex("(\\<|\\>)", "") %>%
    stri_replace_all_regex("[ ]+", " ")  %>%
    stri_trim()
    
  dat$tagged<-TreeTag(text, "~/treeTagger/cmd/tree-tagger-spanish-ancora")
  
  write_csv(dat, stri_replace_all_fixed(file, ".csv", "_TAGGED.csv") )
  
  
})