# Title: Twitter postprocessing after selection 
# Description: This script throws away conversation iDs that do not contain the variable of interest. Then it will extract all unique values of twitter column It will then preprocess these values to reduce the level of variation. Subsequently, it matches the values against a country databaseand  a geocoding database. Values that cannot be matched are geocoded using the Google Maps API.
# Client: Pedro Gras/Frank Brisard, University of Antwerp
# Project:
# Author: Jeroen Claes
# Date:  Thu Feb  8 20:22:31 2018

# --------------
# Packages
# --------------

library(stringi)
library(dplyr)
library(readr)
library(ggplot2)
library(parallel)
library(ggmap)

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

logmessage <- function(string) {
  write(paste(Sys.time(),  string, sep=": "), file=stderr())
}


geocode <-function(characterVector) {
  library(httr)
  
  if(file.exists("~/Desktop/twitterCorpus/code/geocodeCache.csv")) {
    geocache<- read_csv("~/Desktop/twitterCorpus/code/geocodeCache.csv", col_names = c("line", "country"))
    geocache <- geocache[!duplicated(geocache$line), ]
  } else {
    geocache<-data.frame(line=NA, country=NA)
  }
  countries<-lapply(characterVector, FUN=function(line) {
    
    if(file.exists("~/Desktop/twitterCorpus/code/geocodeCache.csv")) {
      geocache<- read_csv("~/Desktop/twitterCorpus/code/geocodeCache.csv", col_names = c("line", "country"))}
    
    if(line %in% geocache$line) {
      
      return(geocache[geocache$line==line, ]$country)
      
    } else {
      
      
      
      req<- try(GET(paste0("http://www.datasciencetoolkit.org/maps/api/geocode/json?sensor=false&address=",line) , timeout(15), user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:10.0) Gecko/20100101 Firefox/10.0")))
      
      if(!"try-error" %in% class(req)) {
        if(status_code(req)==200) {
          req<-content(req)
          if(length(req)>0) {
            if(length(req$results)>0) {
              if(length(req$results[[1]]$address_components)==2) {
                
                
                country <- req$results[[1]]$address_components[[2]]$long_name
                
                cat(paste(line,",", country), file="~/Desktop/twitterCorpus/code/geocodeCache.csv", sep="\n", append = TRUE)
                return(country)
              }  else if(length(req$results[[1]]$address_components)==1) {
                
                
                country <- req$results[[1]]$address_components[[1]]$long_name
                
                cat(paste(line,",", country), file="~/Desktop/twitterCorpus/code/geocodeCache.csv", sep="\n", append = TRUE)
                return(country)
              }  else {
                
                return(NA)
              }
              
              
            } else {
              return(NA)
            }
            
            
          } else {
            
            return(NA)
          }
        } else {
          return(NA)
        }
        
      } else {
        return(NA)
      }
    } 
  })
  
  return(unlist(countries)) 
  
}
# --------------
# Data
# --------------
#setwd("/Volumes/GoogleDrive/Mijn Drive/")
dataSet <- read_csv("dataSet.csv")

dataSet$geoColumn <- dataSet$geoColumn %>% 
  stri_replace_all_regex("[^[:alpha:] ]", "") %>%
  stri_replace_all_regex("[ ]+", " ") %>%
  stri_trans_general(id="latin-ascii") %>%
  stri_trans_tolower() 

dataSet <- dataSet %>%
  ungroup() %>%
  mutate(country=ifelse(grepl("(spain|espana|españa|madrid|barcelona|vigo|sevilla|barajas|a coruña|a coruna|toledo|galicia|galiza|gallego|galego|adra|andalucía|valenciana|valencia|andalucia|albacete|zamora|zafra|málaga|malaga|mallorca|menorca|bilbao|vizcaya|guijon|guijón|xixon|xixón|valència|catalunya|catalan|compostela)", geoColumn, perl=TRUE), "Spain", ifelse(grepl("(puerto rico|porto rico|p r|\\bpr\\b|borinquén|borinquen|borinken|borinkén|república dominicana|rep. dominicana|quisqueya|dominican republic|dominican rep|dom rep|r d |dom |cuba|havana|santo domingo|san juan|baranquilla|colombia|panamá|panama|dominicana|venezuela|venezolana|boricua|caribbean|caribe|venezuela|venezolano|aguadilla|barranquilla)", geoColumn, perl=TRUE), "Caribbean", ifelse(grepl("(mexico|méxico|méjico|mejico|monterrey|méx|mex|mx|guanajuato|acapulco|puebla|acatlán|acatlan|acuña|acuna|aguascalientes)", geoColumn, perl=TRUE), "Mexico", ifelse(grepl("(chile|valparaíso|chl|valparaiso)", geoColumn, perl=TRUE), "Chile", ifelse(grepl("(lima|cuzco|cusco|puno|bolivia|potosi|potosí|peru|perú|cali|calí)", geoColumn, perl=TRUE), "Andes",ifelse(grepl("(argentina|mendoza|buenos aires|pampa|ar |\\b(a r g e n t i n a)\\|adrogue|adrogué|agentina|alberdi|cordoba|córdoba|arg )", geoColumn, perl=TRUE), "Argentina", ifelse(grepl("(canarias|tenerife|el hierro|el hierro|la palma|la gomera|gran canaria|fuerteventura|lanzarote)", geoColumn, perl=TRUE), "Canarias",ifelse(grepl("(uruguay|montevideo|montevídeo)",perl=TRUE, geoColumn), "Uruguay", ifelse(grepl("(new york|nueva york|\\bny\\b|new orleans|albaquerque|boston|michigan|washington|usa|chicago|west coast|los angeles|palm beach|florida|miami|virginia)", geoColumn, perl=TRUE), "USA", NA))))))))))


namesToCode <- dataSet %>%
  ungroup() %>%
  filter(is.na(country)) %>%
  select(geoColumn, country) %>%
  filter(!is.na(geoColumn)) %>%
  filter(!duplicated(geoColumn))

geoCache <- read_csv("~/Desktop/twitterCorpus/code/geocodeCache.csv", col_names = c("geoColumn", "country"))
namesToCode <- namesToCode %>%
  filter(!namesToCode$geoColumn %in% geoCache$geoColumn)

namesToCode$country<-  geocode(namesToCode$geoColumn)
  
geocoded<- dataSet %>% 
  ungroup() %>%
  filter(!is.na(country))


notGeocoded <- dataSet %>% 
  ungroup() %>%
  filter(is.na(country)) %>%
  select(-country) %>%
  left_join(geoCache, by="geoColumn")


dataSet <- bind_rows(geocoded, notGeocoded) %>%
  arrange(in_reply_to_status_id_str, created_at )



# --------------
# Logic
# --------------
dataSet <- dataSet %>% ungroup()
dataSet$country <- recode(dataSet$country, "Aruba"="Caribbean", "Colombia"="Caribbean", "Venezuela"="Caribbean", "Ecuador"="Central America", "Cuba"="Caribbean", "Belize"="Central America", "Nicaragua"="Central America", "Costa Rica"="Central America", "Honduras"="Central America", "Peru"="Andes", "Guatemala"="Central America", "Uruguay"="Uruguay", "Puerto Rico"="Caribbean", "Boliva"="Andes", "El Salvador"="Central America", "Paraguay"="Paraguay", "Bahamas"="Caribbean", "Haiti"="Caribbean", "Dominican Republic"="Caribbean", "Marshall Islands"="Argentina", "Andorra"="Spain","Antigua and Barbuda"="Caribbean", "Panama"="Caribbean", "Jamaica"="Caribbean", "Sao Tome and Principe"="Caribbean","Trinidad and Tobago"="Caribbean", "Dominica"="Caribbean",  "Seychelles"="Caribbean", "Solomon Islands" ="Caribbean", "Bermuda" ="Caribbean", "Martinique"  ="Caribbean", "Reunion"   ="Caribbean","Guadeloupe"  ="Caribbean", "U.S. Virgin Islands"  ="Caribbean", "Barbados"="Caribbean",  "Saint Vincent and the Grenadines"="Caribbean",  "Cayman Islands"  ="Caribbean", "Turks and Caicos Islands"="Caribbean", "United States"="USA", "South Georgia and South Sandwich Islands"="Caribbean", "Portugal"="Other", "Greece"="Other", "Italy"="Other", "France"="Other", "Netherlands"="Other", "United Kingdom"="Other")

dataSet[is.na(dataSet$country),]$country<-ifelse(grepl("(spain|espana|españa|madrid|barcelona|vigo|sevilla|barajas|a coruña|a coruna|toledo|galicia|galiza|gallego|galego|adra|andalucía|valenciana|valencia|andalucia|albacete|zamora|zafra|málaga|malaga|mallorca|menorca|bilbao|vizcaya|guijon|guijón|xixon|xixón|valència|catalunya|catalan|compostela)", dataSet[is.na(dataSet$country),]$geoColumn, perl=TRUE), "Spain", ifelse(grepl("(puerto rico|porto rico|p r|\\bpr\\b|borinquén|borinquen|borinken|borinkén|república dominicana|rep. dominicana|quisqueya|dominican republic|dominican rep|dom rep|r d |dom |cuba|havana|santo domingo|san juan|baranquilla|colombia|panamá|panama|dominicana|venezuela|venezolana|boricua|caribbean|caribe|caracas|venezuela|venezolano|aguadilla|barranquilla)", dataSet[is.na(dataSet$country),]$geoColumn, perl=TRUE), "Caribbean", ifelse(grepl("(mexico|méxico|méjico|tamaulipas|cancun|cancún|mejico|monterrey|méx|mex|mx|guanajuato|acapulco|puebla|acatlán|acatlan|acuña|acuna|aguascalientes|distrito federal|queretaro|nuevo leon|guadalajara|oaxaca|tijuana)", dataSet[is.na(dataSet$country),]$geoColumn, perl=TRUE), "Mexico", ifelse(grepl("(chile|valparaíso|chl|valparaiso)", dataSet[is.na(dataSet$country),]$geoColumn, perl=TRUE), "Chile", ifelse(grepl("(lima|cuzco|cusco|medellin|puno|bolivia|potosi|antioquia|potosí|peru|perú|cali|calí|bogot)", dataSet[is.na(dataSet$country),]$geoColumn, perl=TRUE), "Andes",ifelse(grepl("(argentina|mendoza|buenos aires|pampa|ar |\\b(a r g e n t i n a)\\|adrogue|adrogué|agentina|alberdi|cordoba|córdoba|arg |buenosaire|buenos aire)", dataSet[is.na(dataSet$country),]$geoColumn, perl=TRUE), "Argentina", ifelse(grepl("(canarias|tenerife|el hierro|el hierro|la palma|la gomera|gran canaria|fuerteventura|lanzarote)", dataSet[is.na(dataSet$country),]$geoColumn, perl=TRUE), "Canarias",ifelse(grepl("(uruguay|montevideo|montevídeo)",perl=TRUE, dataSet[is.na(dataSet$country),]$geoColumn), "Uruguay", ifelse(grepl("(new york|nueva york|\\bny\\b|new orleans|albaquerque|boston|michigan|washington|usa|chicago|west coast|los angeles|palm beach|florida|miami|virginia|estados unidos|eeuu|ee uu)", dataSet[is.na(dataSet$country),]$geoColumn, perl=TRUE), "USA", ifelse(grepl("(paraguay|asuncion|asunción)", dataSet[is.na(dataSet$country),]$geoColumn, perl=TRUE), "Paraguay",ifelse(grepl("(guatemala|antigua|san josé|san jose|tagucigalpa|san salvador|el salvador|managua|san pedro sula|quito|choloma|costa rica|el salvador|honduras|nicaragua|ecuador)", dataSet[is.na(dataSet$country),]$geoColumn, perl=TRUE), "Central America",NA)))))))))))



dataSet <- dataSet %>%
  mutate(country=ifelse(country %in% names(sort(table(dataSet$country), decreasing=T)[1:10]), country, "other"))

save(dataSet, file="final_data_geocoded.rdata")


# --------------
# Output
# --------------