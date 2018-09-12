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

# Cleanup function

cleanupText <- function(characterVector)
{
  
  characterVector <- as.character(characterVector)
  characterVector <-
    stri_replace_all_fixed(characterVector, "â", "") %>%
    stri_replace_all_fixed("Ã¡", "á") %>%
    stri_replace_all_fixed("Ã©", "é") %>%
    stri_replace_all_fixed("Ã³", "ó") %>%
    stri_replace_all_fixed("Ãº", "ú") %>%
    stri_replace_all_fixed("Ã±", "ñ") %>%
    stri_replace_all_fixed("Ã£", "ã") %>%
    stri_replace_all_fixed("Ã¼", "ü") %>%
    stri_replace_all_fixed("Ã¶", "ö") %>%
    stri_replace_all_fixed("Ã¤", "ä") %>%
    stri_replace_all_fixed("Ã¥", "å") %>%
    stri_replace_all_fixed("Ã¸", "ø") %>%
    stri_replace_all_fixed("Ã®", "î") %>%
    stri_replace_all_fixed("Ã¢", "â") %>%
    stri_replace_all_fixed("Ãª", "ê") %>%
    stri_replace_all_fixed("Ã¨", "è") %>%
    stri_replace_all_fixed("Ã§", "ç") %>%
    stri_replace_all_fixed("Ã",  "í") %>%
    stri_replace_all_regex("[\\u00B7|\\uf0b7|\\uf0a0|\\u00AD]", "") %>%
    stri_replace_all_fixed("habia", "había") %>%
    stri_replace_all_fixed("habian", "habían") %>%
    stri_replace_all_fixed("habra", "habrá") %>%
    stri_replace_all_fixed("habran", "habrán") %>%
    stri_replace_all_regex("[^[:print:]]", "") %>%
    stri_replace_all_regex("  +", " ") %>%
    stri_trans_tolower()
  return(characterVector)
}



logmessage("This script will load a plain-text file via a file selector dialog. It will then extract all 'que'-clauses from that text file and write the result to a CSV file. ")

logmessage("Press any key to launch the file selector.")
line<-readline()

inputFile<-file.choose()
dataSet <- data.frame(texto=stri_conv(readLines(inputFile, encoding = "UTF-8"), from="windows-1252", to="utf-8")) %>%
  filter(texto!="")

que<- dataSet %>%
  mutate(texto=cleanupText(texto)) %>%
  mutate(texto=stri_replace_all_regex(texto, '<[[:alnum:] ="]+/>', '')) %>%
  mutate(texto=stri_replace_all_regex(texto, '<[[:alnum:] ="]+>', '')) %>%
  mutate(texto=stri_replace_all_fixed(texto, '/', '')) %>%
  mutate(texto=stri_replace_all_regex(texto, '[ ]+', ' ')) %>%
  filter(grepl("(^que |^q |^k |^ke |[[:punct:]]que |[[:punct:]]q |[[:punct:]]k |[[:punct:]]ke |^ que |^ q |^ k |^ ke |[[:punct:]] que |[[:punct:]] q |[[:punct:]] k |[[:punct:]] ke |^que |^@[[:alnum:]]+ que |^q |^@[[:alnum:]]+ q | ^k |^@[[:alnum:]]+ k |^ke |^@[[:alnum:]]+ ke )", texto, perl=TRUE, ignore.case =TRUE))

que <- que %>%
  filter(!grepl("(que |q |ke |k )(insuficiente|belleza|hermos|basura|difícil|
                dificil|magnific|magnífic|cosa|felicidad|bien|molest|lind|aburrid|triste|pena |tiempo|clima|estúpid|grande|estrés|estres|día|calor|barbaridad|amig|hambre|p uta|rabia|amor|hijo|fin|flojera|pedo|buen|problema|mujer|hombre|paso|pasó|sab es|sabés|puto|momento|ridículo|tan|estoy|estas|estás|onda|marica|maricon|mari cón|idiota|lent|terrible|paja|tal|va|estupido|merda|mierda|luz|cansad|gran|se mana|mes|complicad|horror|pasa|hace|cambio|pesadilla|embole|capítulo|falta|mo n|hino|duro)",texto, perl=TRUE, ignore.case=TRUE))

write_csv(que, stri_replace_all_fixed(inputFile, ".txt", ".csv"))

logmessage(paste0("Processed file ", shQuote(inputFile), " and written output to", shQuote(stri_replace_all_fixed(inputFile, ".txt", ".csv"))))