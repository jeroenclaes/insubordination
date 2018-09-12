library(rvest)
library(dplyr)
library(RCurl)
library(readr)
library(stringi)
library(httr)
library(R.utils)

setwd("~/Desktop/scraping/")

##############################
# FUNCTION TO CLEAN UP URLS
##############################

cleanup_url <- function (url) 
{
  require(stringi)
  url <- trimws(url)
  url <- stri_replace_all_fixed(url, "/NA", "/")
  url <- stri_replace_all_fixed(url, "//", "/")
  url <- stri_replace_all_fixed(url, "http:/www", "http://www")
  url <- stri_replace_all_fixed(url, "https:/www", "https://www")
  url <- stri_replace_all_fixed(url, "https:/", "https://")
  url <- stri_replace_all_fixed(url, "http:/", "http://")
  url <- stri_replace_all_fixed(url, "www", "http://www")
  url <- stri_replace_all_fixed(url, "http://http://", "http://")
  url <- stri_replace_all_fixed(url, "https:/http://", "https://")
  url <- stri_replace_all_fixed(url, "https://http://", "https://")
  url <- stri_replace_all_fixed(url, "http:/http://", "http://")
  url <- stri_replace_all_fixed(url, "http://", "http://www")
  url <- stri_replace_all_fixed(url, "http://wwwwww", "http://www")
  url <- stri_replace_all_fixed(url, "http://wwww", "http://www.")
  url <- stri_replace_all_fixed(url, "http://wwww..", "http://www.")
  url <- stri_replace_all_fixed(url, "http://", "http://www.")
  url <- stri_replace_all_fixed(url, "http://www.www", "http://www.")
  url <- stri_replace_all_fixed(url, "http://www./http://www.", 
                                "http://www.")
  url <- stri_replace_all_fixed(url, ". ", ".")
  url <- stri_replace_all_fixed(url, "..", ".")
  url <- stri_replace_all_fixed(url, ">", "")
  url <- stri_replace_all_fixed(url, "///", "//")
  url <- stri_replace_all_regex(url, "(https:/)(?=[:alnum:])", 
                                "https://")
  url <- stri_replace_all_regex(url, "(http:/)(?=[:alnum:])", 
                                "http://")
  url <- stri_replace_all_fixed(url, "https://http://", "http://")
  url <- stri_replace_all_fixed(url, "http://www.users.pandora.be", 
                                "http://users.pandora.be")
  url <- stri_replace_all_fixed(url, "http://www.users.telenet.be", 
                                "http://users.telenet.be")
  url <- stri_replace_all_fixed(url, "http://www.users.skynet.be", 
                                "http://users.skynet.be")
  return(url)
}

##############################
# FUNCTION TO DOWNLOAD WEB PAGES AS HTML FILES
##############################
url_download <- function (vat, url, path, timeout = 2, verbose = F) 
{
  
  ch <- getCurlHandle(cookiefile = "cookie.txt", cookiejar = "cookie.txt", 
                      verbose = F, followlocation = T, header = T, useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.7; rv:25.0) Gecko/20100101 Firefox/25.0")
  if (isTRUE(verbose)) {
    print(url)
  }
  url <- cleanup_url(url)
  if (length(url) > 0) {
    if ((url != "") && (!is.na(url))) {
      if (withTimeout(url.exists(url, curl = ch), timeout = timeout, 
                      onTimeout = "error")) {
        withTimeout(tekst <- read_html(url, curl = ch), 
                    timeout = timeout, onTimeout = "error")
        titel <- html_nodes(tekst, xpath = "//title") %>% 
          html_text() %>% unlist()
        cleanTitle <- stri_replace_all_regex(titel[1], 
                                             "[^[:alnum:]]", "_")
        maxLength <- ifelse(stri_length(cleanTitle) <= 
                              15, stri_length(cleanTitle), 15)
        filename <- stri_trans_tolower(stri_replace_all_fixed(paste(vat, 
                                                                    "-", substr(cleanTitle, 1, maxLength), "_", 
                                                                    as.integer(as.POSIXct(Sys.time())), "_", paste0(sample(c(letters, 
                                                                                                                             LETTERS, 0:9), 10), collapse = ""), ".html", 
                                                                    sep = "", collapse = ""), "__", "_"))
        output_path <- stri_replace_all_fixed(paste(path, 
                                                    filename, sep = "/", collapse = ""), "//", 
                                              "/")
        cat(as.character(tekst), file = output_path)
      }
      else {
        stop(paste("Warning: Cannot download URL: ", 
                   url))
      }
    }
    else {
      stop(paste("Warning: argument 'url' should not be NULL"))
    }
  }
  else {
    stop(paste("Warning: argument 'url' should not be NULL"))
  }
}

##############################
# FUNCTION TO MASK IP ADDRESS 
# BY USING A ROTATING LIST OF
# PROXIES; LIMITS SCRAPING DETECTION
##############################

rotateIP <- function() {
  proxyList<-c('37.192.12.55:21231',
               '130193148100:53281',
               '195.239.10.46:8080',
               '199.127.197.12:53281',
               '175.41.44.43:53281',
               '131.117.214.38:65103',
               '202131233202:21776',
               '185.93.3.123:8080',
               '46.37.138.89:3128',
               '178.79.76.7:21231',
               '109.236.113.30:8080',
               '202147207236:53281',
               '199.255.28.178:21231',
               '185.19.1.197:53281',
               '128.201.97.121:53281',
               '117.102.95.246:8080',
               '194.208.49.77:21231',
               '185.42.60.206:3128',
               '92.255.195.57:53281',
               '213.6.61.33:21231',
               '41.78.24.178:47502',
               '154117143126:8080',
               '138.121.75.161:53281',
               '31.7.237.78:21231',
               '103.35.171.185:8080',
               '41.190.128.82:21231',
               '91.82.239.254:53281',
               '213.6.33.234:21231',
               '46.42.36.93:53281',
               '82147116201:41234',
               '91.197.78.228:21231',
               '186.139.244.11:21776',
               '103.110.164.17:53281',
               '181.191.216.13:53281',
               '41.60.229.225:8080',
               '202.148.12.60:53281',
               '176.105.24.204:21231',
               '31132108162:50078',
               '181113225246:53281',
               '188.27.241.25:21231',
               '103.1.95.26:21776',
               '82.114.94.68:53281',
               '191.255.207.69:21776',
               '217.13.220.62:21776',
               '145128232138:53281',
               '85234126107:55555',
               '89249251196:21231',
               '185.158.1.114:21231',
               '193.109.239.22:21776',
               '186.42.184.114:53281',
               '18213205230:80',
               '18205120217:80',
               '84.232.111.74:21231',
               '176.104.184.4:53281',
               '82193103245:21231',
               '142.93.75.124:80',
               '202.21.32.148:8080',
               '103.78.98.180:21776',
               '185142208139:41258',
               '175100185145:53281',
               '185.181.17.104:21231',
               '206189222146:8080',
               '138.118.85.193:53281',
               '179.214.23.141:21776',
               '186247205182:21776',
               '92.51.114.106:53281',
               '80211187110:3128',
               '148.102.57.122:8080',
               '178.252.119.88:53281',
               '103.98.32.94:21776',
               '181.113.21.34:53281',
               '79190145141:3128',
               '87.226.37.80:21231',
               '91.203.249.9:8080',
               '176.10.41.94:21231',
               '5158119119:8080',
               '111.68.108.34:8080',
               '186216195124:21776',
               '35.190.235.61:3128',
               '202.162.197.43:41766',
               '142.93.20.181:3128',
               '70.28.29.63:8080',
               '41.222.226.45:80',
               '94.130.191.23:3128',
               '186.233.196.8:21776',
               '189.4.229.22:80',
               '93.189.158.7:3128',
               '103239255210:8080',
               '41.60.235.76:53281',
               '193106144140:21231',
               '190181113213:53281',
               '140.227.57.88:3128',
               '31.133.49.18:41258',
               '82.137.250.68:41766',
               '109.60.140.89:21231',
               '47.52.209.8:80',
               '190.214.31.154:53281',
               '178.213.175.28:53281',
               '24.124.113.53:53281',
               '190.128.226.54:8080')
  

  repeat({
   prx <- sample(proxyList,1) 
    
    set_config(use_proxy(url=stri_split_fixed(prx, ":")[[1]][1], 
                         port=as.numeric(stri_split_fixed(prx, ":")[[1]][2])), override = TRUE)
    tst <- try(GET("http://www.jeroenclaes.be"))
    if(!"try-error" %in% class(tst)) {
      break()
    } 
    
  })
  
}

##############################
# FUNCTION TO EXTRACT DOMAIN -INTERNAL 
# URLS FROM WEB PAGES
##############################

url_links <- function(url,  timeout = 15) {
  
  ch <- getCurlHandle(cookiefile = "cookie.txt", cookiejar = "cookie.txt", 
                      verbose = F, followlocation = T, header = T, useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.7; rv:25.0) Gecko/20100101 Firefox/25.0")
  url <- cleanup_url(url)
  links <- c()
  if (withTimeout(url.exists(url, curl = ch), timeout = timeout, 
                  onTimeout = "error")) {
    page <-    try( withTimeout(read_html(url, curl = ch), timeout = timeout, 
                                onTimeout = "error"))
    if(!"try-error" %in% class(links)) {
      
      links1 <- html_nodes(page, xpath = "//a") %>% html_attr("href")
      links2 <- html_nodes(page, xpath = "//frame") %>% 
        html_attr("src")
      links <- c(links2, links1)
    } else {
      print("error")
      links <- c()
    }
  }
  links <- ifelse(!grepl("^(http|https|www)", links), paste0(url, links), links)
  links <- cleanup_url(links)
  url <- stri_replace_all_regex(stri_replace_all_fixed(url, "https", "http"), "^[[:alnum:][:punct:]]{10}", "")
  links <- links[grepl(url, links)]
  return(unique(links))
}

##############################
# ACTUAL WEB SCRAPER FUNCTION, 
# WHICH CALLS ALL PREVIOUS FUNCTIONS
##############################

scrape <- function (vats, urls, path, timeout = 20, verbose = F, parallel = T) 
{
  processedLinks <<- c()
  
  vatsurls <- mapply(vats, urls, FUN = function(x, y) {
    y <- stri_replace_all_regex(y, "^(//)", "http://")
    string <- paste(x, y, sep = ":", collapse = "")
    string <- stri_replace_all_fixed(string, ":", "-#-")
    string <- stri_replace_all_fixed(string, "http-#-//", 
                                     "http://")
    string <- stri_replace_all_fixed(string, "https-#-//", 
                                     "https://")
    string <- stri_replace_all_fixed(string, "https:/", "https://")
    string <- stri_replace_all_regex(string, "(https:/)(?=[:alnum:])", 
                                     "https://")
    string <- stri_replace_all_regex(string, "(http:/)(?=[:alnum:])", 
                                     "http://")
    string <- stri_replace_all_fixed(string, "https://http://www.", 
                                     "http://www.")
    return(string)
  })
   
    require(parallel)
    options(mc.cores = detectCores() )
    lapply(vatsurls, FUN = function(x) {
      
      if (x != "NA") {
        tryCatch({
          x <- stri_split_fixed(x, "-#-")
           write(paste0("Working on: ", x[[1]][2]), stderr())
          links <- url_links(x[[1]][2], timeout = timeout)
          links <- links[!is.null(links)]
          links <- links[links != ""]
          domain <- stri_replace_all_fixed(x[[1]][2], "http://www.", "")
          links <- stri_replace_all_fixed(links, paste0(domain,"/", domain), domain)
          links <- unique(links)
          secondLevel <- c()
          thirdLevel <- c()
          fourthLevel <- c()
          
          if (length(links) > 0) {
            
            secondLevel <- unlist(lapply(links, FUN=function(a) {
              write(a, stderr())
              if(!a %in% globalenv()$processedLinks) {
              try( url_download(x[[1]][1], a, path, timeout = timeout, verbose = verbose))
              processedLinks<<-c(globalenv()$processedLinks, a)
              link <- try(url_links(a, 15))
              if("try-error" %in% class(link)) {
                return("")
              } else {
                return(link)
              }  
              } else {
                return("")
              }
            }))
            secondLevel <- secondLevel[!is.null(secondLevel)]
            secondLevel <- secondLevel[secondLevel!=""]
            secondLevel <- stri_replace_all_fixed(secondLevel, paste0(domain, domain), domain)
            secondLevel <- unique(secondLevel)

          }
 
        })
        }
        })
        }


##############################
# READ INPUT
##############################

input <- read_csv("vat,url
                  AR_clarin,http://www.clarin.com/
                  AR_lanacion,http://www.lanacion.com.ar
                  AR_diariopopular,https://www.diariopopular.com.ar/
                  AR_cronica,https://www.cronica.com.ar/
                  CH_lanacion,http://www.lanacion.cl
                  CH_latercera,http://www.latercera.com/
                  ES_elpais,https://elpais.com/
                  ES_elmundo,http://www.elmundo.es/
                  ES_abc,https://www.abc.es/
                  ES_elperiodico,https://www.elperiodico.com/es/
                  CO_eltiempo,http://www.eltiempo.com/
                  CO_elespectador,https://www.elespectador.com/
                  CO_Bogotaextra,http://bogota.extra.com.co/
                  MX_universal,http://www.eluniversal.com.mx/
                  MX_laprensa,https://www.la-prensa.com.mx/
                  PR_elnuevodia,https://www.elnuevodia.com/
                  PR_primerahora,http://www.primerahora.com/
                  CU_granma,http://www.granma.cu/
                  CU_juventudRebelde,http://www.juventudrebelde.cu/
                  DO_listindiario,www.listin.com.do
                  DO_eldia,http://eldia.com.do/")


input <- read_csv("vat,url
                  CO_Bogotaextra,http://bogota.extra.com.co/")



##############################
# APPLY SCRAPE FUNCTION TO INPUT
##############################
rotateIP()
scrape(input$vat, input$url, path="~/Desktop/scraping/html/", timeout = 60)
