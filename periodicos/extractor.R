# Title: Extractor functions 
# Description: Functions to extract structured text data from downloaded newspaper articles
# Client/project: Prof. Dr. Frank Brisard, Prof. Dr. Pedro Gras, Universiteit Antwerpen
# Author: Jeroen Claes
# Date:  Tue Aug 15 09:12:32 2018

# --------------
# Packages
# --------------

library(stringi)
library(dplyr)
library(readr)
library(rvest)
library(parallel)

# --------------
# Options
# --------------

options(mc.cores=6, scipen = 999, stringsAsFactors = F)

# --------------
# Constants
# --------------


# --------------
# Custom functions
# --------------

logmessage <- function(string, verbose=T) {
if(isTRUE(verbose)) {
  write(paste(Sys.time(),  string, sep=": "), file=stderr())
  }
}

# ==================================================
#  checkLength function, replaces zero-length fields 
# with ""
# ==================================================

checkLength <- function(field) {
  if(length(field)==0) {
    return("")
  } else {
    return(field)
  }
  
}

# ==================================================
#  CLEANUP function, fixes encoding issues and other common problems
# ==================================================
cleanup_text <- function(text) {
  text <- stri_replace_all_regex(text, "[^[:print:]]", " ") %>% #strip all non-printable characters
    stri_replace_all_regex("(\n|\r)", " ") %>% # strip all rule/line endings
    stri_replace_all_fixed( "\t", " ") %>%# strip all tab markings
    stri_replace_all_regex("[ ]+", " ") %>% # replace all double spaces with single spaces
    stri_trim() %>%    # strip leading/trailing spaces
    stri_replace_all_fixed( "â", "") %>% # The following lines fix encoding issues
    stri_replace_all_fixed( "Ã¡", "á") %>%
    stri_replace_all_fixed( "Ã©", "é") %>%
    stri_replace_all_fixed( "Ã³", "ó") %>%
    stri_replace_all_fixed( "Ãº", "ú") %>%
    stri_replace_all_fixed( "Ã±", "ñ") %>%
    stri_replace_all_fixed( "Ã£", "ã") %>%
    stri_replace_all_fixed( "Ã¼", "ü") %>%
    stri_replace_all_fixed( "Ã¶", "ö") %>%
    stri_replace_all_fixed( "Ã¤", "ä") %>%
    stri_replace_all_fixed( "Ã¥", "å") %>%
    stri_replace_all_fixed( "Ã¸", "ø") %>%
    stri_replace_all_fixed( "Ã®", "î") %>%
    stri_replace_all_fixed( "Ã¢", "â") %>%
    stri_replace_all_fixed( "Ãª", "ê") %>%
    stri_replace_all_fixed( "Ã¨", "è") %>%
    stri_replace_all_fixed( "Ã§", "ç") %>%
    stri_replace_all_fixed( "Ã",  "í")  %>%
    stri_replace_all_fixed( "Â",  "") %>%
    stri_replace_all_fixed( "¤ï¸",  " ") %>%
    stri_replace_all_regex("[ ]+", " ") %>%
    stri_trim() 
  return(text) 
}
#########################################################
# ==================================================
# newspaper -specific extraction routines
# ==================================================
#########################################################

# ==================================================
# MX_universal
# ==================================================
MX_universal <- function(file) {
  file <- read_html(file)
  
  author <- file %>%
    html_nodes(xpath="//div[contains(@id,'node-autor-nota')]") %>%
    html_text() %>%
    stri_trim()%>%
    cleanup_text()
   
  title <- file %>%
    html_nodes(xpath="//div[@class='panel-pane pane-node-title']") %>%
    html_text() %>%
    stri_trim() %>%
    cleanup_text()
  
  tags <- file %>%
    html_nodes(xpath="//div[@class='panel-pane pane-entity-field pane-node-field-tags']") %>%
    html_text() %>%
    stri_trim() %>%
    stri_split_fixed("\n") %>%
    unlist()  
    
  tags <- tags[tags!=""]
  tags <- tags[tags!="TEMAS"]
  tags <- tags %>%
    cleanup_text() %>%
    paste(collapse = ";")
  
  description <-  file %>%
    html_nodes(xpath= "//div[@class='panel-pane pane-entity-field pane-node-field-resumen']") %>%
    html_text() %>%
    stri_trim() %>%
    cleanup_text()  %>%
    paste(collapse=" ")
  
  section <- file %>%
    html_nodes(xpath= "//div[@class='breadcrumb']//span") %>%
    html_text() %>%
    unique()  
    
  if(length(section)>1) {
    section<-section[length(section)-1]
  }  
  
  section<-section %>%
    cleanup_text()  %>%
    unique() %>%
    paste(collapse = ";")
  
  text <-  file %>%
    html_nodes(xpath= "//div[@class='panel-pane pane-node-body']//p") %>%
    html_text() %>%
    paste(collapse = " ") %>%
    cleanup_text()
  
  
  date <- file %>%
    html_nodes(xpath="//div[@class='fechap']") %>%
    html_text() %>%
    cleanup_text() %>%
    as.Date(format="%d/%m/%Y")
  
  
  result <- list(source="MX_universal",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=text)
  return(result)
  
}

# ==================================================
# AR_clarin
# ==================================================
AR_clarin <- function(file) { 
  file <- read_html(file)
  
  author <- file %>%
    html_nodes(xpath="//div[@class='items']//ul[@class='list-inline']//li") %>%
    html_text()
  
  if(length(author) > 1) {
    tags <- author[1] %>%
      cleanup_text()
    author <- author[2] %>%
      cleanup_text()
  } else {
    tags <- NA
    author <- author[1] %>%
      cleanup_text()
  }
  
  title <- file %>%
    html_nodes(xpath="//h1[@id='title']") %>%
    html_text() %>%
    cleanup_text()
  
  description <-  file %>%
    html_nodes(xpath= "//div[@class='title']//h2") %>%
    html_text() %>%
    cleanup_text()  %>%
    paste(collapse=" ")
  
  section <- file %>%
    html_nodes(xpath= "//div[@class='section-name']//h1") %>%
    html_text() %>%
    cleanup_text()  %>%
    unique() %>%
    paste(collapse = ";")
  
  text <-  file %>%
    html_nodes(xpath= "//div[@class='body-nota']//p") %>%
    html_text() %>%
    paste(collapse = " ") %>%
    cleanup_text()
  
  
  date <- file %>%
    html_nodes(xpath="//div[@class='breadcrumb col-lg-6 col-lg-offset-1 col-md-12  col-sm-12 col-xs-12']//span") %>%
    html_text() %>%
    cleanup_text() %>%
    as.Date(format="%d/%m/%Y - %H:%M")
  
  
  result <- list(source="AR_clarin",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=text)
  return(result)
}

# ==================================================
# AR_lanacion 
# ==================================================

AR_lanacion <- function(file) { 
  file <- read_html(file)
  
  author <- file %>%
    html_nodes(xpath="//section[@class='author']//a | //div[@class='firma']//a") %>%
    html_text() %>%
    cleanup_text()
  
  tags <- file %>%
    html_nodes(xpath = "//strong[@class='tag']//a") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";")
  
  title <- file %>%
    html_nodes(xpath="//h1[@class='titulo']") %>%
    html_text() %>%
    cleanup_text()
  
  description <-  file %>%
    html_nodes(xpath= "//epigrafe") %>%
    html_text() %>%
    cleanup_text() %>%
    paste(collapse=" ")
  
  section <- file %>%
    html_nodes(xpath= "//strong[@class='categoria']//a") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";")
  
  text <-  file %>%
    html_nodes(xpath= "//section[@id='cuerpo']//p") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
  date <- file %>%
    html_nodes(xpath= "//section[@class='fecha']") %>%
    html_text()   %>%
    cleanup_text() 
  
  date <-   stri_replace_all_regex(date, "[^[:alnum:]\\: ]", "")  %>%
    stri_trim() %>%
    stri_replace_all_fixed("de diciembre de", "12") %>%
    stri_replace_all_fixed("de noviembre de", "11") %>%
    stri_replace_all_fixed("de octubre de", "10") %>%
    stri_replace_all_fixed("de septiembre de", "09") %>%
    stri_replace_all_fixed("de agosto de", "08") %>%
    stri_replace_all_fixed("de julio de", "07") %>%
    stri_replace_all_fixed("de junio de", "06") %>%
    stri_replace_all_fixed("de mayo de", "05") %>%
    stri_replace_all_fixed("de abril de", "04") %>%
    stri_replace_all_fixed("de marzo de", "03") %>%
    stri_replace_all_fixed("de febrero de", "02") %>%
    stri_replace_all_fixed("de enero de", "01") %>%
    stri_replace_all_regex(" +", " ") %>%
    as.Date(format="%d %m %Y %H:%M")
  
  
  result <- list(source="AR_lanacion",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=text)
  return(result)
}

# ==================================================
# AR_diariopopular
# ==================================================
AR_diariopopular <- function(file) { 
  
  file <- read_html(file, encoding = "utf-8")

  author <- "Diario Popular"
  
  tags <- file %>%
    html_nodes(xpath = "//span[@class='people-name']//a") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";")
  
  title <- file %>%
    html_nodes(xpath="//h1[@class='title']") %>%
    html_text() %>%
    cleanup_text()
  
  description <-  file %>%
    html_nodes(xpath= "//article[@id='article-post']//p[@class='interiorCopete']") %>%
    html_text() %>%
    cleanup_text() %>%
    paste(collapse=" ")
  
  section <- file %>%
    html_nodes(xpath= "//div[@class='category-wrapper']//a | //div[@class='category-wrapper']//span") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";")
  
  text <-  file %>%
    html_nodes(xpath= "//div[@class='cuerpo']//p") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
  date <- file %>%
    html_nodes(xpath= "//div[@class='fecha-detallenota']") %>%
    html_text()   %>%
    cleanup_text() 
  
  date <-   stri_replace_all_regex(date, "[^[:alnum:]\\: ]", "")  %>%
    stri_trim() %>%
    stri_replace_all_fixed("de diciembre de", "12") %>%
    stri_replace_all_fixed("de noviembre de", "11") %>%
    stri_replace_all_fixed("de octubre de", "10") %>%
    stri_replace_all_fixed("de septiembre de", "09") %>%
    stri_replace_all_fixed("de agosto de", "08") %>%
    stri_replace_all_fixed("de julio de", "07") %>%
    stri_replace_all_fixed("de junio de", "06") %>%
    stri_replace_all_fixed("de mayo de", "05") %>%
    stri_replace_all_fixed("de abril de", "04") %>%
    stri_replace_all_fixed("de marzo de", "03") %>%
    stri_replace_all_fixed("de febrero de", "02") %>%
    stri_replace_all_fixed("de enero de", "01") %>%
    stri_replace_all_regex(" +", " ") %>%
    as.Date(format="%d %m %Y")
  
  
  result <- list(source="AR_diariopopular",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=checkLength(text) 
                 )
  return(result)

}

# ==================================================
# AR_cronica
# ==================================================
AR_cronica <- function(file) { 
  
  file <- read_html(file, encoding = "utf-8")
  
  author <- file %>%
    html_nodes(xpath="//div[@class='author-data-container']") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    stri_replace_all_fixed("Por ", "")
  
  tags <- file %>%
    html_nodes(xpath = "//div[@class='entry-tags  clearfix']//a") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";")
  
  title <- file %>%
    html_nodes(xpath="//h1[@class='title-font font-700']") %>%
    html_text() %>%
    cleanup_text()
  
  description <-  file %>%
    html_nodes(xpath= "//h2[@class='text-font font-600']") %>%
    html_text() %>%
    cleanup_text() %>%
    paste(collapse=" ")
  
  section <- file %>%
    html_nodes(xpath= "//span[@class='entry-label text-font font-800']") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_trans_tolower()
  
  text <-  file %>%
    html_nodes(xpath= "//div[@class='entry-body text-font']") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
  date <- file %>%
    html_nodes(xpath= "//time[@class='entry-time text-font font-600']") %>%
    html_text()   %>%
    cleanup_text() %>%
    as.Date(format="%d-%m-%Y %H:%M")
  
  
  result <- list(source="AR_cronica",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=checkLength(text) 
  )
  return(result)
  
  
}

# ==================================================
# CH_lanacion
# ==================================================
CH_lanacion <- function(file) { 
  
  file <- read_html(file, encoding = "utf-8")
  
  author <- file %>%
    html_nodes(xpath="//div[@class='td-post-author-name']//a") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    stri_replace_all_fixed("Por ", "")
  
  tags <- file %>%
    html_nodes(xpath = "//ul[@class='td-tags td-post-small-box clearfix']//li") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_replace_all_fixed("ETIQUETAS;", "")
  
  title <- file %>%
    html_nodes(xpath="//h1[@class='entry-title']") %>%
    html_text() %>%
    cleanup_text()
  
  description <-  file %>%
    html_nodes(xpath= "//p[@class='td-post-sub-title']") %>%
    html_text() %>%
    cleanup_text() %>%
    paste(collapse=" ")
  
  section <- file %>%
    html_nodes(xpath= "//span[@itemprop='itemListElement']") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_trans_tolower()
  
  text <-  file %>%
    html_nodes(xpath= "//div[@class='td-post-content']//p") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
  date <- file %>%
    html_nodes(xpath= "//span[@class='td-post-date']") %>%
    html_text()   %>%
    cleanup_text() 
  
  
  date <-   stri_replace_all_regex(date, "[^[:alnum:]\\: ]", "")  %>%
    stri_trim() %>%
    stri_trans_tolower() %>%
    stri_replace_all_fixed("lunes ", "") %>%
    stri_replace_all_fixed("martes ", "") %>%
    stri_replace_all_fixed("miércoles ", "") %>%
    stri_replace_all_fixed("jueves ", "") %>%
    stri_replace_all_fixed("viernes ", "") %>%
    stri_replace_all_fixed("sábado ", "") %>%
    stri_replace_all_fixed("domingo ", "") %>%
    stri_replace_all_fixed("de diciembre de", "12") %>%
    stri_replace_all_fixed("de noviembre de", "11") %>%
    stri_replace_all_fixed("de octubre de", "10") %>%
    stri_replace_all_fixed("de septiembre de", "09") %>%
    stri_replace_all_fixed("de agosto de", "08") %>%
    stri_replace_all_fixed("de julio de", "07") %>%
    stri_replace_all_fixed("de junio de", "06") %>%
    stri_replace_all_fixed("de mayo de", "05") %>%
    stri_replace_all_fixed("de abril de", "04") %>%
    stri_replace_all_fixed("de marzo de", "03") %>%
    stri_replace_all_fixed("de febrero de", "02") %>%
    stri_replace_all_fixed("de enero de", "01") %>%
    stri_replace_all_regex(" +", " ") %>%
    as.Date(format="%d %m %Y")
  
  result <- list(source="CH_lanacion",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=checkLength(text) 
  )
  return(result)
  
  
}

# ==================================================
# CH_latercera
# ==================================================
CH_latercera <- function(file) { 
  
  
  file <- read_html(file, encoding = "utf-8")
  
  author <- file %>%
    html_nodes(xpath="//h4[@class='margin-0 m-top-10']//a") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    stri_replace_all_fixed("Por ", "")
  
  tags <- file %>%
    html_nodes(xpath = "//div[@class='bg-blue p-bottom-15 m-top-40 tags-interior']//ul[@class='menu_vertical tags_menu m-bottom-15']//li//a") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_replace_all_fixed("ETIQUETAS;", "")
  
  
  title <- file %>%
    html_nodes(xpath="//h1[@class='m-top-30']") %>%
    html_text() %>%
    cleanup_text()
  
  description <-  file %>%
    html_nodes(xpath= "//div[@class='bg-gray bajada-art']//h2") %>%
    html_text() %>%
    cleanup_text() %>%
    paste(collapse=" ")
  
  section <- file %>%
    html_nodes(xpath= "//div[@class='left-cat border-right-1']//h4//a") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_trans_tolower()
  
  text <-  file %>%
    html_nodes(xpath= "//div[@class='col-md-11 col-lg-10 col-md-offset-1 col-lg-offset-2 nota-interior-tx p-top-30']//p") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
  date <- file %>%
    html_nodes(xpath= "//small[@class='bottom-info']//span") %>%
    html_text()   %>%
    cleanup_text() 
  
  
  date <-   stri_replace_all_regex(date, "[^[:alnum:]\\: ]", "")  %>%
    stri_trim() %>%
    stri_trans_tolower() %>%
    stri_replace_all_fixed("lun ", "") %>%
    stri_replace_all_fixed("mar", "") %>%
    stri_replace_all_fixed("mie ", "") %>%
    stri_replace_all_fixed("jue ", "") %>%
    stri_replace_all_fixed("vie ", "") %>%
    stri_replace_all_fixed("sab ", "") %>%
    stri_replace_all_fixed("dom ", "") %>%
    stri_replace_all_fixed("dic", "12") %>%
    stri_replace_all_fixed("nov", "11") %>%
    stri_replace_all_fixed("oct", "10") %>%
    stri_replace_all_fixed("sep", "09") %>%
    stri_replace_all_fixed("ago", "08") %>%
    stri_replace_all_fixed("jul", "07") %>%
    stri_replace_all_fixed("jun", "06") %>%
    stri_replace_all_fixed("may", "05") %>%
    stri_replace_all_fixed("abr", "04") %>%
    stri_replace_all_fixed("mar", "03") %>%
    stri_replace_all_fixed("feb", "02") %>%
    stri_replace_all_fixed("ene", "01") %>%
    stri_replace_all_regex(" +", " ") %>%
    as.Date(format="%d %m %Y %H:%M %p") 
  
  date<-date[!is.na(date)]
  
  
  if(tags=="Editorial") {
    author="editor"
    description="editorial"
    section="editoriales"
    
  }
  
  result <- list(source="CH_latercera",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=checkLength(text) 
  )
  return(result)
  
  
  
  
  
  }
# ==================================================
# ES_elpais
# ==================================================
ES_elpais <- function(file) { 
  
  file <- read_html(file, encoding = "utf-8")
  
  author <- file %>%
    html_nodes(xpath="//span[@class='autor-nombre']//a") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    stri_replace_all_fixed("Por ", "")
  
  tags <- file %>%
    html_nodes(xpath = "//ul[@class='listado']//li") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_replace_all_fixed("ETIQUETAS;", "")
  
  title <- file %>%
    html_nodes(xpath="//div[@class='articulo-titulares']//h1[@class='articulo-titulo ']") %>%
    html_text() %>%
    cleanup_text()
  
  description <-  file %>%
    html_nodes(xpath= "//div[@class='articulo-subtitulos']//h2[@class='articulo-subtitulo']") %>%
    html_text() %>%
    cleanup_text() %>%
    paste(collapse=" ")
  
  section <- file %>%
    html_nodes(xpath= "//span[@itemprop='itemListElement']") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_trans_tolower()
  
  text <-  file %>%
    html_nodes(xpath= "//div[@class='articulo-cuerpo']//p") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
  date <- file %>%
    html_nodes(xpath= "//div[@class='articulo-datos']//time") %>%
    html_attr("datetime")   %>%
    as.Date()
    
  
  
  
  
  result <- list(source="ES_elpais",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=checkLength(text) 
  )
  return(result)
  
  
  
  
  }
# ==================================================
# ES_elmundo
# ==================================================
ES_elmundo <- function(file) {
  file <- read_html(file, encoding = "utf-8")
  
  author <- file %>%
    html_nodes(xpath="//div[@class='autor']//div//p//a|//ul[@class='author']//li[@itemprop='name']") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    stri_replace_all_fixed("Por ", "")
  
  tags <- file %>%
    html_nodes(xpath = "//div[@id='etiquetas']//p | //div[@id='etiquetas']//li") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_replace_all_fixed("ETIQUETAS;", "")
  
  title <- file %>%
    html_nodes(xpath="//h1[@class='titulo'] | //h1[@itemprop='headline']") %>%
    html_text() %>%
    cleanup_text()
  
  description <-  file %>%
    html_nodes(xpath= "//div[@id='entrada']//p | //div[@class='subtitle-items']//p") %>%
    html_text() %>%
    cleanup_text() %>%
    paste(collapse=" ")
  
  section <- file %>%
    html_nodes(xpath= "//div[@class='container-header']//div//ul//li//span[@itemprop='name']") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_trans_tolower()
  
  text <-  file %>%
    html_nodes(xpath= "//div[@id='cuerpo-entrada']//p | //div[@itemprop='articleBody']//p") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
  
  date <- file %>%
    html_nodes(xpath= "//li[@class='date-container']//time") %>%
    html_attr("datetime") %>%
    as.Date()
  
  
  if(is.na(date)) {
    
  
  date <- file %>%
    html_nodes(xpath= "//div[@class='autor']//div//p") %>%
    html_text()
  
 
   date <- try(stri_split_fixed(date, "-")[[1]][2]) 
   if(!"try-error" %in% class(date)) {
     
  
   date <- date %>%
     stri_replace_all_regex("[^[:alnum:]\\: ]", "") %>%
    stri_trim() %>%
    stri_trans_tolower() %>%
    stri_replace_all_fixed("lunes ", "") %>%
    stri_replace_all_fixed("martes ", "") %>%
    stri_replace_all_fixed("miércoles ", "") %>%
    stri_replace_all_fixed("jueves ", "") %>%
    stri_replace_all_fixed("viernes ", "") %>%
    stri_replace_all_fixed("sábado ", "") %>%
    stri_replace_all_fixed("domingo ", "") %>%
    stri_replace_all_fixed("de diciembre de", "12") %>%
    stri_replace_all_fixed("de noviembre de", "11") %>%
    stri_replace_all_fixed("de octubre de", "10") %>%
    stri_replace_all_fixed("de septiembre de", "09") %>%
    stri_replace_all_fixed("de agosto de", "08") %>%
    stri_replace_all_fixed("de julio de", "07") %>%
    stri_replace_all_fixed("de junio de", "06") %>%
    stri_replace_all_fixed("de mayo de", "05") %>%
    stri_replace_all_fixed("de abril de", "04") %>%
    stri_replace_all_fixed("de marzo de", "03") %>%
    stri_replace_all_fixed("de febrero de", "02") %>%
    stri_replace_all_fixed("de enero de", "01") %>%
    stri_replace_all_regex(" +", " ") %>%
    as.Date(format="%d %m %Y")
  
   } else {
     date  <- NA
   }
  
  }
  result <- list(source="ES_elmundo",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=checkLength(text) 
  )
  return(result)

}
# ==================================================
# ES_abc
# ==================================================
ES_abc <- function(file) {
  
  file <- read_html(file, encoding = "utf-8")
  
  author <- file %>%
    html_nodes(xpath="//span[@class='data']//footer[@class='autores']//span//a") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    stri_replace_all_fixed("Por ", "") %>%
    paste(collapse=" ")
  
  tags <- file %>%
    html_nodes(xpath = "//aside[@class='modulo temas']//ul//li") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_replace_all_fixed("ETIQUETAS;", "")
  
  title <- file %>%
    html_nodes(xpath="//span[@class='encabezado-articulo']//h1") %>%
    html_text() %>%
    cleanup_text()
  
  description <-  file %>%
    html_nodes(xpath= "//span[@class='encabezado-articulo']//h2") %>%
    html_text() %>%
    cleanup_text() %>%
    paste(collapse=" ")
  
  section <- file %>%
    html_nodes(xpath= "//span[@class='main']//span[@class='titulo-seccion']//a") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_trans_tolower()
  
  text <-  file %>%
    html_nodes(xpath= "//span[@class='cuerpo-texto ']//p") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
  date <- file %>%
    html_nodes(xpath= "//span[@class='data']//span[@class='fecha']//time") %>%
    html_attr("datetime") %>%
    as.Date()
  
  result <- list(source="ES_abc",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=checkLength(text) 
  )
  return(result)
  
  
  
}
# ==================================================
# ES_elperiodico
# ==================================================
ES_elperiodico <- function(file) { 
  file <- read_html(file, encoding = "utf-8")
  
  author <- file %>%
    html_nodes(xpath="//div[@class='signature']//div[@class='txt']//p//a[@class='author-link']") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    stri_replace_all_fixed("Por ", "") %>%
    paste(collapse=" ")
  
  tags <- file %>%
    html_nodes(xpath = "//div[@class='ep-themes']//p//a") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_replace_all_fixed("ETIQUETAS;", "")
  
  title <- file %>%
    html_nodes(xpath="//header[@class='ep-detail-header']//h1") %>%
    html_text() %>%
    cleanup_text()
  
  description <-  file %>%
    html_nodes(xpath= "//header[@class='ep-detail-header']//div[@class='subtitle']//h2") %>%
    html_text() %>%
    cleanup_text() %>%
    paste(collapse=" ")
  
  section <- file %>%
    html_nodes(xpath= "//div[@class='breadcrumb']//a") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_trans_tolower()
  
  text <-  file %>%
    html_nodes(xpath= "//div[@class='ep-detail-body']//p") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
  date <- file %>%
    html_nodes(xpath= "//div[@class='signature']//div[@class='txt']//time") %>%
    html_attr("datetime") %>%
    as.Date()
  
  result <- list(source="ES_elperiodico",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=checkLength(text) 
  )
  return(result)
  }
# ==================================================
# CO_eltiempo
# ==================================================
CO_eltiempo <- function(file) { 
  file <- read_html(file, encoding = "utf-8")
  
  author <- file %>%
    html_nodes(xpath="//div[@class='autor-bk']//span[@itemprop='name']") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    stri_replace_all_fixed("Por ", "") %>%
    paste(collapse=" ")
  
  tags <- file %>%
    html_nodes(xpath = "//h2[@class='tags-en-articulo-tag']//a") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_replace_all_fixed("ETIQUETAS;", "")
  
  title <- file %>%
    html_nodes(xpath="//h1[@class='titulo']") %>%
    html_text() %>%
    cleanup_text()
  
  description <-  file %>%
    html_nodes(xpath= "//div[@class='lead']//p") %>%
    html_text() %>%
    cleanup_text() %>%
    paste(collapse=" ")
  
  section <- file %>%
    html_nodes(xpath= "//li[contains(@class, 'current')]//a") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_trans_tolower()
  
  text <-  file %>%
    html_nodes(xpath= "//div[@itemprop='articleBody']//p[@class='contenido']") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
  date <- file %>%
    html_nodes(xpath= "//div[@class='fecha-publicacion-bk']//span[@class='fecha']") %>%
    html_text()
  
  date <- date %>%
    stri_replace_all_regex("[^[:alnum:]\\: ]", "") %>%
    stri_trim() %>%
    stri_trans_tolower() %>%
    stri_replace_all_fixed("lunes ", "") %>%
    stri_replace_all_fixed("martes ", "") %>%
    stri_replace_all_fixed("miércoles ", "") %>%
    stri_replace_all_fixed("jueves ", "") %>%
    stri_replace_all_fixed("viernes ", "") %>%
    stri_replace_all_fixed("sábado ", "") %>%
    stri_replace_all_fixed("domingo ", "") %>%
    stri_replace_all_fixed("de diciembre", "12") %>%
    stri_replace_all_fixed("de noviembre", "11") %>%
    stri_replace_all_fixed("de octubre", "10") %>%
    stri_replace_all_fixed("de septiembre", "09") %>%
    stri_replace_all_fixed("de agosto", "08") %>%
    stri_replace_all_fixed("de julio", "07") %>%
    stri_replace_all_fixed("de junio", "06") %>%
    stri_replace_all_fixed("de mayo", "05") %>%
    stri_replace_all_fixed("de abril", "04") %>%
    stri_replace_all_fixed("de marzo", "03") %>%
    stri_replace_all_fixed("de febrero", "02") %>%
    stri_replace_all_fixed("de enero", "01") %>%
    stri_replace_all_regex(" +", " ") %>%
    as.Date(format="%d %m %Y %H:%M %p")
  
  result <- list(source="CO_eltiempo",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=checkLength(text) 
  )
  return(result)
  
  
  
  }
# ==================================================
# CO_elespectador
# ==================================================
CO_elespectador <- function(file) { 
  file <- read_html(file, encoding = "utf-8")
  
  author <- file %>%
    html_nodes(xpath="//div[@itemprop='author']//text()") 
  author <- author[length(author)]  %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    stri_replace_all_fixed("Por ", "") %>%
    paste(collapse=" ")
  
  tags <- file %>%
    html_nodes(xpath = "//div[@class='node-tags-items']//div//a") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_replace_all_fixed("ETIQUETAS;", "")
  
  title <- file %>%
    html_nodes(xpath="//div[contains(@class, 'node-title')]//h1") %>%
    html_text() %>%
    cleanup_text()
  
  description <-  file %>%
    html_nodes(xpath= "//div[contains(@class, 'node-teaser')]//p") %>%
    html_text() %>%
    cleanup_text() %>%
    paste(collapse=" ")
  
  section <- file %>%
    html_nodes(xpath= "//div[@class='secondary-title']//a | //div[@itemprop='articleSection']//a") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_trans_tolower()
  
  text <-  file %>%
    html_nodes(xpath= "//div[contains(@class,'node-body')]//p") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
  date <- file %>%
    html_nodes(xpath= "//div[@itemprop='datePublished']") %>%
    html_text()
  
  date <- date %>%
    stri_replace_all_regex("[^[:alnum:]\\: ]", "") %>%
    stri_trim() %>%
    stri_trans_tolower() %>%
    stri_replace_all_fixed("lunes ", "") %>%
    stri_replace_all_fixed("martes ", "") %>%
    stri_replace_all_fixed("miércoles ", "") %>%
    stri_replace_all_fixed("jueves ", "") %>%
    stri_replace_all_fixed("viernes ", "") %>%
    stri_replace_all_fixed("sábado ", "") %>%
    stri_replace_all_fixed("domingo ", "") %>%
    stri_replace_all_fixed("de diciembre", "12") %>%
    stri_replace_all_fixed("de noviembre", "11") %>%
    stri_replace_all_fixed("de octubre", "10") %>%
    stri_replace_all_fixed("de septiembre", "09") %>%
    stri_replace_all_fixed("de agosto", "08") %>%
    stri_replace_all_fixed("de julio", "07") %>%
    stri_replace_all_fixed("de junio", "06") %>%
    stri_replace_all_fixed("de mayo", "05") %>%
    stri_replace_all_fixed("de abril", "04") %>%
    stri_replace_all_fixed("de marzo", "03") %>%
    stri_replace_all_fixed("de febrero", "02") %>%
    stri_replace_all_fixed("de enero", "01") %>%
    stri_replace_all_fixed("ago", "08") %>%
    stri_replace_all_fixed("jul", "07") %>%
    stri_replace_all_fixed("jun", "06") %>%
    stri_replace_all_fixed("sep", "09") %>%
    stri_replace_all_regex(" +", " ") %>%
    as.Date(format="%d %m %Y %H:%M %p")
  
  result <- list(source="CO_elespectador",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=checkLength(text) 
  )
  return(result)
 
  }
# ==================================================
# CO_Bogotaextra
# ==================================================
CO_Bogotaextra <- function(file) { 
  file <- read_html(file, encoding = "utf-8")
  
  author <- file %>%
    html_nodes(xpath="//div[@itemprop='author']//text()") 
  author <- author[length(author)]  %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    stri_replace_all_fixed("Por ", "") %>%
    paste(collapse=" ")
  
  tags <- file %>%
    html_nodes(xpath = "//div[@rel='dc:subject']") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_replace_all_fixed("ETIQUETAS;", "")
  
  title <- file %>%
    html_nodes(xpath="//div[@class='content']//div//div[@class='field field-title']//div[@class='field-items']//div[@class='field-item']//h2") %>%
    html_text() %>%
    cleanup_text()
  
  title <- title[length(title)]
  
  description <-  file %>%
    html_nodes(xpath= "//div[contains(@class, 'node-teaser')]//p") %>%
    html_text() %>%
    cleanup_text() %>%
    paste(collapse=" ")
  
  section <- file %>%
    html_nodes(xpath= "//div[@class='breadcrumb']//a") %>%
    html_text() 
  
  section<- section[length(section)] %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_trans_tolower()
  
  text <-  file %>%
    html_nodes(xpath= "//div[@property='content:encoded']//p") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
  date <- file %>%
    html_nodes(xpath= "//div[@class='content']//div[@class='field field-datetime']//div[@class='field-items']//div[@class='field-item']") %>%
    html_text()
  
  date <- date[grepl("(lunes|martes|miércoles|jueves|viernes|sábado|domingo)", date, perl=TRUE, ignore.case=TRUE)]
  
  
  date <- date %>%
    stri_replace_all_regex("[^[:alnum:]\\: ]", "") %>%
    stri_trim() %>%
    stri_trans_tolower() %>%
    stri_replace_all_fixed("lunes ", "") %>%
    stri_replace_all_fixed("martes ", "") %>%
    stri_replace_all_fixed("miércoles ", "") %>%
    stri_replace_all_fixed("jueves ", "") %>%
    stri_replace_all_fixed("viernes ", "") %>%
    stri_replace_all_fixed("sábado ", "") %>%
    stri_replace_all_fixed("domingo ", "") %>%
    stri_replace_all_fixed("diciembre", "12") %>%
    stri_replace_all_fixed("noviembre", "11") %>%
    stri_replace_all_fixed("octubre", "10") %>%
    stri_replace_all_fixed("septiembre", "09") %>%
    stri_replace_all_fixed("agosto", "08") %>%
    stri_replace_all_fixed("julio", "07") %>%
    stri_replace_all_fixed("junio", "06") %>%
    stri_replace_all_fixed("mayo", "05") %>%
    stri_replace_all_fixed("abril", "04") %>%
    stri_replace_all_fixed("marzo", "03") %>%
    stri_replace_all_fixed("febrero", "02") %>%
    stri_replace_all_fixed("enero", "01") %>%
    stri_replace_all_regex(" +", " ") %>%
    as.Date(format="%m %d %Y %H:%M")
  
  result <- list(source="CO_Bogotaextra",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=checkLength(text) 
  )
  return(result)
  
  }
# ==================================================
# MX_laprensa
# ==================================================
MX_laprensa <- function(file) { 
  file <- read_html(file, encoding = "utf-8")
  
  author <- file %>%
    html_nodes(xpath="//li[@class='credito author'] | //li[@class='credito']")  %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    stri_replace_all_fixed("Por ", "") %>%
    paste(collapse=" ")
  
  tags <- NA
  
  title <- file %>%
    html_nodes(xpath="//div[@class='nota-interior']//h2 | //div[@class='nota-interior']//h1" ) %>%
    html_text() %>%
    cleanup_text()
  
 
  
  description <-  NA
  
  section <- file %>%
    html_nodes(xpath= "//li[@class='seccion']") %>%
    html_text()  %>%
    stri_replace_all_fixed("en", "") %>%
    cleanup_text()
  
  section<- section[length(section)] %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";") %>%
    stri_trans_tolower()
  
  text <-  file %>%
    html_nodes(xpath= "//div[@class='col-xs-12']//p") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
  date <- file %>%
    html_nodes(xpath= "//li[@class='date-creditos-nota']//time") %>%
   html_attr("datetime") %>%
    as.Date()
  
  
  result <- list(source="MX_laprensa",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=checkLength(text) 
  )
  return(result)

  }
# ==================================================
# PR_elnuevodia
# ==================================================
PR_elnuevodia <- function(file) {
  
  file <- read_html(file, encoding = "utf-8")
  
  author <- file %>%
    html_nodes(xpath="//div[@class='toolbar-item item-author']//a")  %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    stri_replace_all_fixed("Por ", "") %>%
    paste(collapse=" ")
  
  tags <- file %>%
    html_nodes(xpath="//aside[@class='tags']//ul//li") %>%
html_text() %>%
    cleanup_text() %>%
    paste(collapse=";")
  
  title <- file %>%
    html_nodes(xpath="//div[@class='article-header']//h1" ) %>%
    html_text() %>%
    cleanup_text()
  
  
  
  description <-  file %>%
    html_nodes(xpath="//div[@class='article-header']//h2" ) %>%
    html_text() %>%
    cleanup_text()
  
  section <- file %>%
    html_nodes(xpath= "//strong[@class='section']//a") %>%
    html_text()   %>%
    cleanup_text() %>%
    stri_trans_tolower()
  
  
  
  text <-  file %>%
    html_nodes(xpath= "//div[@class='article-body']//p") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
  date <- file %>%
    html_nodes(xpath= "//div[@class='toolbar-item item-date']//p") %>%
    html_text() 
  
  
  date <- date %>%
    stri_replace_all_regex("[^[:alnum:]\\: ]", "") %>%
    stri_trim() %>%
    stri_trans_tolower() %>%
    stri_replace_all_fixed("de ", " ") %>%
    stri_replace_all_fixed("lunes ", "") %>%
    stri_replace_all_fixed("martes ", "") %>%
    stri_replace_all_fixed("miércoles ", "") %>%
    stri_replace_all_fixed("jueves ", "") %>%
    stri_replace_all_fixed("viernes ", "") %>%
    stri_replace_all_fixed("sábado ", "") %>%
    stri_replace_all_fixed("domingo ", "") %>%
    stri_replace_all_fixed("diciembre", "12") %>%
    stri_replace_all_fixed("noviembre", "11") %>%
    stri_replace_all_fixed("octubre", "10") %>%
    stri_replace_all_fixed("septiembre", "09") %>%
    stri_replace_all_fixed("agosto", "08") %>%
    stri_replace_all_fixed("julio", "07") %>%
    stri_replace_all_fixed("junio", "06") %>%
    stri_replace_all_fixed("mayo", "05") %>%
    stri_replace_all_fixed("abril", "04") %>%
    stri_replace_all_fixed("marzo", "03") %>%
    stri_replace_all_fixed("febrero", "02") %>%
    stri_replace_all_fixed("enero", "01") %>%
    stri_replace_all_regex(" +", " ") %>%
    as.Date(format="%d %m %Y %H:%M %p")
  
  
  
  result <- list(source="PR_elnuevodia",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=checkLength(text) 
  )
  return(result)
  
  
  
}
# ==================================================
# PR_primerahora
# ==================================================
PR_primerahora <- function(file) { 
  file <- read_html(file, encoding = "utf-8")
  
  author <- file %>%
    html_nodes(xpath="//div//h6//strong")  %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    stri_replace_all_fixed("Por ", "") %>%
    paste(collapse=" ")
  
  tags <- file %>%
    html_nodes(xpath="//div[@class='d1b_tags']//a") %>%
    html_text() %>%
    cleanup_text() %>%
    paste(collapse=";")
  
  title <- file %>%
    html_nodes(xpath="//article//div//h1" ) %>%
    html_text() %>%
    cleanup_text()
  
  description <-  file %>%
    html_nodes(xpath="//article//blockquote" ) %>%
    html_text() %>%
    cleanup_text()
  
  section <- file %>%
    html_nodes(xpath= "//nav//ul//li[@class='active']//a") %>%
    html_text()   %>%
    cleanup_text() %>%
    stri_trans_tolower()
  
  text <-  file %>%
    html_nodes(xpath= "//div[@class='body-content']//p") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
  date <- file %>%
    html_nodes(xpath="//div[@class='ntscott']//h6") %>%
    html_text() 
  
  date <- stri_extract_first_regex(date, "[0-9]{2}/[0-9]{2}/[0-9]{4}") %>%
    as.Date(format="%m/%d/%Y")
  
  
  result <- list(source="PR_primerahora",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=checkLength(text) 
  )
  return(result)
  }
# ==================================================
# CU_granma
# ==================================================
CU_granma <- function(file) { 
  
  file <- read_html(file, encoding = "utf-8")
  
  author <- file %>%
    html_nodes(xpath="//span[@class='byline-author']")  %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    stri_replace_all_fixed("Por ", "") %>%
    paste(collapse=" ")
  
  tags <- NA
  
  title <- file %>%
    html_nodes(xpath="//h1[@itemprop='headline']" ) %>%
    html_text() %>%
    cleanup_text()
  
  description <-  file %>%
    html_nodes(xpath="//p[@class='g-story-description' and @itemprop='description']" ) %>%
    html_text() %>%
    cleanup_text()
  
  section <- file %>%
    html_nodes(xpath= "//nav//ul//li[@class='active']//a") %>%
    html_text()   %>%
    cleanup_text() %>%
    stri_trans_tolower() %>%
    unique() %>%
    paste(collapse=";")
  
  text <-  file %>%
    html_nodes(xpath= "//div[@itemprop='articleBody']//p") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
  date <- file %>%
    html_nodes(xpath="//time[@class='dateline']") %>%
    html_attr("datetime") %>%
    as.Date()
  
  result <- list(source="CU_granma",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=checkLength(text) 
  )
  return(result)
  
  }
# ==================================================
# CU_juventudRebelde
# ==================================================
CU_juventudRebelde <- function(file) {
  file <- read_html(file, encoding = "utf-8")
  
  author <- file %>%
    html_nodes(xpath="//a[contains(@class, 'autor-info') and @role='button'] | //a[@class='author']")  %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    stri_replace_all_fixed("Por ", "") %>%
    paste(collapse=" ")
  
  tags <- file %>%
    html_nodes(xpath = "//div[@class='tagcloud']//a") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() 
  
  if(length(tags) > 0) {
    section <- tags[1]
  } else {
    section <- NA
  }

  tags <- tags %>%
    paste(collapse=";")
  
  title <- file %>%
    html_nodes(xpath="//section[@id='title-news']//h1 | //span[@class='news-title']" ) %>%
    html_text() %>%
    cleanup_text()
  
  description <-  file %>%
    html_nodes(xpath="//section[@id='title-news']//p" ) %>%
    html_text() %>%
    cleanup_text()
  
 
  
  text <-  file %>%
    html_nodes(xpath= "//article//p | //div[contains(@class, 'news-body')]") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
  date <- file %>%
    html_nodes(xpath="//label[@class='news-date-act-pub']") %>%
    html_text()  
  
  date <- date[length(date)]
  
  date <- date %>%
    stri_replace_all_regex("[^[:alnum:]\\: ]", "") %>%
    stri_trim() %>%
    stri_trans_tolower() %>%
    stri_replace_all_fixed("updated:", "") %>%
    stri_replace_all_fixed("publicado:", "") %>%
    stri_replace_all_fixed("de ", " ") %>%
    stri_replace_all_fixed("lunes ", "") %>%
    stri_replace_all_fixed("martes ", "") %>%
    stri_replace_all_fixed("miércoles ", "") %>%
    stri_replace_all_fixed("jueves ", "") %>%
    stri_replace_all_fixed("viernes ", "") %>%
    stri_replace_all_fixed("sábado ", "") %>%
    stri_replace_all_fixed("domingo ", "") %>%
    stri_replace_all_fixed("diciembre", "12") %>%
    stri_replace_all_fixed("noviembre", "11") %>%
    stri_replace_all_fixed("octubre", "10") %>%
    stri_replace_all_fixed("septiembre", "09") %>%
    stri_replace_all_fixed("agosto", "08") %>%
    stri_replace_all_fixed("julio", "07") %>%
    stri_replace_all_fixed("junio", "06") %>%
    stri_replace_all_fixed("mayo", "05") %>%
    stri_replace_all_fixed("abril", "04") %>%
    stri_replace_all_fixed("marzo", "03") %>%
    stri_replace_all_fixed("febrero", "02") %>%
    stri_replace_all_fixed("enero", "01") %>%
    stri_replace_all_regex(" +", " ") %>%
    stri_trim() %>%
    as.Date(format="%d %m %Y %H:%M:%S %p")
  
  result <- list(source="CU_juventudRebelde",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=checkLength(text) 
  )
  return(result)
  
}
# ==================================================
# DO_listindiario
# ==================================================
DO_listindiario <- function(file) { 
  file <- read_html(file, encoding = "utf-8")
  
  author <- file %>%
    html_nodes(xpath="//div[@id='ArticleAuthorDiv']")  %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    stri_replace_all_fixed("Por ", "") %>%
    paste(collapse=" ")
  

  
  tags <- file %>%
    html_nodes(xpath="//a//span[@class='badge badge-info']") %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    paste(collapse = ";")
  
  title <- file %>%
    html_nodes(xpath="//h1[@class='art_titulo']" ) %>%
    html_text() %>%
    cleanup_text()
  
  description <-  file %>%
    html_nodes(xpath="//section[@id='title-news']//p" ) %>%
    html_text() %>%
    cleanup_text()
  
  
  
  section <- file %>%
    html_nodes(xpath="//div[@class='art_sly_1']") %>%
    html_text() %>%
    stri_split_fixed("\r\n") %>%
    unlist() %>%
    cleanup_text()
  
  section <- section[section!=""]
  
  date  <- section[2]
  section <- section[1]
  

  
  text <-  file %>%
    html_nodes(xpath= "//div[@id='ArticleBody']//p") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
  date <- date %>%
    stri_replace_all_regex("[^[:alnum:]\\: ]", "") %>%
    stri_trim() %>%
    stri_trans_tolower() %>%
    stri_replace_all_fixed("updated:", "") %>%
    stri_replace_all_fixed("publicado:", "") %>%
    stri_replace_all_fixed("de ", " ") %>%
    stri_replace_all_fixed("lunes ", "") %>%
    stri_replace_all_fixed("martes ", "") %>%
    stri_replace_all_fixed("miércoles ", "") %>%
    stri_replace_all_fixed("jueves ", "") %>%
    stri_replace_all_fixed("viernes ", "") %>%
    stri_replace_all_fixed("sábado ", "") %>%
    stri_replace_all_fixed("domingo ", "") %>%
    stri_replace_all_fixed("diciembre", "12") %>%
    stri_replace_all_fixed("noviembre", "11") %>%
    stri_replace_all_fixed("octubre", "10") %>%
    stri_replace_all_fixed("septiembre", "09") %>%
    stri_replace_all_fixed("agosto", "08") %>%
    stri_replace_all_fixed("julio", "07") %>%
    stri_replace_all_fixed("junio", "06") %>%
    stri_replace_all_fixed("mayo", "05") %>%
    stri_replace_all_fixed("abril", "04") %>%
    stri_replace_all_fixed("marzo", "03") %>%
    stri_replace_all_fixed("febrero", "02") %>%
    stri_replace_all_fixed("enero", "01") %>%
    stri_replace_all_regex(" +", " ") %>%
    stri_trim() %>%
    as.Date(format="%d %m %Y")
  
  result <- list(source="DO_listindiario",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=checkLength(text) 
  )
  return(result)
  
  
  
  }
# ==================================================
# DO_eldia
# ==================================================
DO_eldia <- function(file) { 
  file <- read_html(file, encoding = "utf-8")
  
  author <- file %>%
    html_nodes(xpath="//div[@class='print-author']//b")  %>%
    html_text() %>%
    cleanup_text() %>%
    unique() %>%
    stri_replace_all_fixed("Por ", "") %>%
    paste(collapse=" ")
  
  
  
  tags <- NA
  
  title <- file %>%
    html_nodes(xpath="//header[@id='single-article-container']//h1" ) %>%
    html_text() %>%
    cleanup_text()
  
  description <-  file %>%
    html_nodes(xpath="//p[@id='subtitulos']" ) %>%
    html_text() %>%
    cleanup_text()
  
  section <- file %>%
    html_nodes(xpath="//header//ul[@class='categorias']//li//a") %>%
    html_text() %>%
    stri_split_fixed("\r\n") %>%
    unlist() %>%
    cleanup_text()
  
  text <-  file %>%
    html_nodes(xpath= "//section[@id='single-article-main-content']//p") %>%
    html_text() %>%
    paste(collapse = " ")  %>%
    cleanup_text()
  
date <- file %>%
  html_nodes(xpath = "//div[@class='print-author']") %>%
  html_text() %>%
  stri_split_fixed("Fecha:") %>%
  unlist()
  
date <- date[2] %>%
  cleanup_text()
  
  
  
  
  date <- date %>%
    stri_replace_all_regex("[^[:alnum:]\\: ]", "") %>%
    stri_trim() %>%
    stri_trans_tolower() %>%
    stri_replace_all_fixed("updated:", "") %>%
    stri_replace_all_fixed("publicado:", "") %>%
    stri_replace_all_fixed("de ", " ") %>%
    stri_replace_all_fixed("lunes ", "") %>%
    stri_replace_all_fixed("martes ", "") %>%
    stri_replace_all_fixed("miércoles ", "") %>%
    stri_replace_all_fixed("jueves ", "") %>%
    stri_replace_all_fixed("viernes ", "") %>%
    stri_replace_all_fixed("sábado ", "") %>%
    stri_replace_all_fixed("domingo ", "") %>%
    stri_replace_all_fixed("diciembre", "12") %>%
    stri_replace_all_fixed("noviembre", "11") %>%
    stri_replace_all_fixed("octubre", "10") %>%
    stri_replace_all_fixed("septiembre", "09") %>%
    stri_replace_all_fixed("agosto", "08") %>%
    stri_replace_all_fixed("julio", "07") %>%
    stri_replace_all_fixed("junio", "06") %>%
    stri_replace_all_fixed("mayo", "05") %>%
    stri_replace_all_fixed("abril", "04") %>%
    stri_replace_all_fixed("marzo", "03") %>%
    stri_replace_all_fixed("febrero", "02") %>%
    stri_replace_all_fixed("enero", "01") %>%
    stri_replace_all_regex(" +", " ") %>%
    stri_trim() %>%
    as.Date(format="%d %m %Y %H:%M %p")
  
  result <- list(source="DO_eldia",
                 author= checkLength(author), 
                 tags=checkLength(tags), 
                 title=checkLength(title), 
                 description=checkLength(description), 
                 section=checkLength(section), 
                 date=checkLength(date),
                 text=checkLength(text) 
  )
  return(result)
  
  
  
}
#########################################################
# =======================================================
# Function to apply the right routine to each file
# We use the parallel version of lapply for greater efficiency
# ========================================================
###########################################################
applyExtractors <- function(files) {
  require(parallel)
  options(mc.cores=6, stringsAsFactors = FALSE)
  
  if(file.exists("processed_files.txt")) {
    processed_files <- readLines("processed_files.txt")
    files <- files[!files %in% processed_files]
  }

  articles <- lapply(files, FUN=function(x) {
    write(x, stderr())
    
    if(grepl("AR_clarin", x, ignore.case=TRUE)) {
     try(AR_clarin(x) %>% 
        as.data.frame()  %>%
        write_csv(append = TRUE, path="textData.csv"))
      
    } else if(grepl("AR_lanacion", x, ignore.case=TRUE)) {
      try(AR_lanacion(x) %>% 
        as.data.frame()  %>%
        write_csv(append = TRUE, path="textData.csv"))
    }
    
    
    else if(grepl("AR_diariopopular", x, ignore.case=TRUE)) {
      try( AR_diariopopular(x) %>% 
        as.data.frame()   %>%
        write_csv(append = TRUE, path="textData.csv"))
    }
    
    else if(grepl("AR_cronica", x, ignore.case=TRUE)) {
      try( AR_cronica(x) %>% 
        as.data.frame()   %>%
        write_csv(append = TRUE, path="textData.csv"))
    }
    
    else if(grepl("CH_lanacion", x, ignore.case=TRUE)) {
      try(CH_lanacion(x) %>% 
        as.data.frame()   %>%
        write_csv(append = TRUE, path="textData.csv"))
    }
    else if(grepl("CH_latercera", x, ignore.case=TRUE)) {
      try(CH_latercera(x) %>% 
        as.data.frame()   %>%
        write_csv(append = TRUE, path="textData.csv"))
    }
    
    else if(grepl("ES_elpais", x, ignore.case=TRUE)) {
      try(ES_elpais(x) %>% 
        as.data.frame()  %>%
        write_csv(append = TRUE, path="textData.csv"))
    }
    
    else if(grepl("ES_elmundo", x, ignore.case=TRUE)) {
      try(ES_elmundo(x) %>% 
        as.data.frame()   %>%
        write_csv(append = TRUE, path="textData.csv"))
    }
    else if(grepl("ES_abc", x, ignore.case=TRUE)) {
      try(ES_abc(x) %>% 
        as.data.frame()  %>%
        write_csv(append = TRUE, path="textData.csv"))
    }
    
    else if(grepl("ES_elperiodico", x, ignore.case=TRUE)) {
      try(ES_elperiodico(x) %>% 
        as.data.frame()   %>%
        write_csv(append = TRUE, path="textData.csv"))
    }
    
    else if(grepl("CO_eltiempo", x, ignore.case=TRUE)) {
      try(CO_eltiempo(x) %>% 
        as.data.frame()   %>%
        write_csv(append = TRUE, path="textData.csv"))
    }
    
    else if(grepl("CO_elespectador", x, ignore.case=TRUE)) {
      try( CO_elespectador(x) %>% 
        as.data.frame() %>%
        write_csv(append = TRUE, path="textData.csv"))
    }
    
    else if(grepl("CO_Bogotaextra", x, ignore.case=TRUE)) {
      try(CO_Bogotaextra(x) %>% 
        as.data.frame()  %>%
        write_csv(append = TRUE, path="textData.csv"))
    }
    
    else if(grepl("MX_laprensa", x, ignore.case=TRUE)) {
      try(MX_laprensa(x) %>% 
        as.data.frame()  %>%
        write_csv(append = TRUE, path="textData.csv"))
    }
    else if(grepl("PR_elnuevodia", x, ignore.case=TRUE)) {
      try( PR_elnuevodia(x) %>% 
        as.data.frame()    %>%
        write_csv(append = TRUE, path="textData.csv"))
    }
    
    else if(grepl("PR_primerahora", x, ignore.case=TRUE)) {
      try( PR_primerahora(x) %>% 
        as.data.frame() %>%
        write_csv(append = TRUE, path="textData.csv"))
    }
    
    else if(grepl("CU_granma", x, ignore.case=TRUE)) {
      try(CU_granma(x) %>% 
        as.data.frame()  %>%
        write_csv(append = TRUE, path="textData.csv"))
    } 
    
    else if(grepl("CU_juventudRebelde", x, ignore.case=TRUE)) {
      try( CU_juventudRebelde(x) %>% 
        as.data.frame()  %>%
        write_csv(append = TRUE, path="textData.csv"))
    }
    
    else if(grepl("DO_listindiario", x, ignore.case=TRUE)) {
      try( DO_listindiario(x) %>% 
        as.data.frame()  %>%
        write_csv(append = TRUE, path="textData.csv"))
    }
    
    else if(grepl("DO_eldia", x, ignore.case=TRUE)) {
      try(DO_eldia(x) %>% 
        as.data.frame()  %>%
        write_csv(append = TRUE, path="textData.csv"))
    } 
    else if(grepl("MX_universal", x, ignore.case=TRUE)) {
      try(MX_universal(x) %>% 
            as.data.frame()  %>%
            write_csv(append = TRUE, path="textData.csv"))
    }
    else {
      return(NA)
    }
    cat(x, file="processed_files.txt", append=TRUE, sep="\n")
  })
  return(paste0("Processed: All files"))
  
}



# --------------
# Data
# --------------
files <- list.files("~/Desktop/scraping/html/", ".html", full.names = TRUE)

files <- files[grepl("bogotaextra", ignore.case=T, files)]
# --------------
# Logic
# --------------
texts <- applyExtractors(files)
# --------------
# Output
# --------------
texts <- read_csv("~/Desktop/scraping/textData.csv", col_names = c("source", "author", "tags", "title", "description", "section", "date", "text")) %>%
  filter(!duplicated(text)) %>%
  filter(!is.na(text)) %>%
  distinct() %>%
  group_by(source) %>%
  mutate(sampleCol=sample(1:n(), n(), replace=T)) %>%
  arrange(source, sampleCol) %>%
  group_by(source) %>%
  mutate(cumulative_word_count=cumsum(stri_count_boundaries(text, type="word"))) %>%
  write_csv("~/Desktop/scraping/textData_deduplicated.csv")

overview<- texts %>%
  group_by(source) %>%
  summarise(nArticles=n(), nWords=length(unlist(stri_split_boundaries(text, type="word"))) )

save(texts, compress=FALSE, file="texts.rdata")

