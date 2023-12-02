library(httr)  # will be use to make HTML GET and POST requests
library(rvest) # will be used to parse HTML
library(methods)
library(tidyr) #will be used to remove NA
library(tidyverse)

webscraper_class <- setRefClass("webscraper_class",
                                fields = list(
                                  base_url = "character",
                                  in_var = "character"
                                ),
                                methods = list(
                                  in_var_read = function(){
                                    in_var <<- readline(prompt = "Enter Year: ")
                                  },
                                  process_webPage = function(){
                                    query_params <- list(query =paste0(in_var), tab="keyword")
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
titles$in_var_read()
titles$process_webPage()
