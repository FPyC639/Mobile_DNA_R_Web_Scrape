library(httr)  # will be use to make HTML GET and POST requests
library(rvest) # will be used to parse HTML
library(methods)
library(tidyr) #will be used to remove NA
library(tidyverse)

webscraper_class <- setRefClass("webscraper_class",
                                fields = list(
                                  base_url = "character"
                                ),
                                methods = list(
                                  in_var_read = function(){
                                    cat("Enter Topic:")
                                    query <- readline(prompt = "")
                                    cat("Enter a year between ", 2010, " through ", 2023)
                                    volume <-readline(prompt = "")
                                    sq_1 <- as.integer(2010:2023)
                                    sq_2 <- 1:14
                                    names(sq_2) <- as.character(sq_1)
                                    volume <-  sq_2[volume]
                                    in_var <- list(query=query,volume=volume)
                                    return(in_var)
                                  },
                                  process_webPage = function(){
                                    in_var <- in_var_read()
                                    query_params <- list(query =paste0(in_var$query), volume=paste0(in_var$volume), tab="keyword")
                                    doc <- httr::GET(base_url,query=query_params)
                                    html <- xml2::read_html(httr::content(doc, "text"))
                                    c_listing_title <- html_elements(html,"h3.c-listing__title")
                                    a_element <- rvest::html_node(c_listing_title,"a")
                                    a_href <- as.list(html_attr(a_element,"href"))
                                    a_text <- lapply(a_element,html_text)
                                    lapply(1:length(a_text),function(x){
                                      paste0(x," ", a_text[x])
                                    })
                                  }
                                  
                                ))
titles <- webscraper_class(base_url = "https://mobilednajournal.biomedcentral.com/articles")
titles$process_webPage()
