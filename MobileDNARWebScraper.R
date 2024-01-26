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
                                  title_obtainer = function(x){
                                    c_listing_title <- rvest::html_elements(x,"h3.c-listing__title")
                                    a_element <- rvest::html_node(c_listing_title,"a")
                                    a_href <- as.list(rvest::html_attr(a_element,"href"))
                                    a_text <- lapply(a_element,rvest::html_text)
                                  },
                                  write_to_csv = function(x){
                                    write.csv(data.frame(unlist(x)), 
                                    file = paste0("MobileDNA", ".csv"),
                                    row.names = TRUE)
                                  },
                                  access_counts_grapher = function(x){
                                      c_listing_title <- html_elements(x,"h3.c-listing__title")
                                      a_element <- rvest::html_node(c_listing_title,"a")
                                      a_href <- as.list(rvest::html_attr(a_element,"href"))
                                      a_text <- lapply(a_element,html_text)
                                      
                                      ##################### 2 Page Depth #######################
                                      merge_strings <- function(x){
                                        paste0("https://mobilednajournal.biomedcentral.com",x)
                                      }
                                      sub_pages <- lapply(a_href,merge_strings)
                                      
                                      ######################## Function Read Sub Pages #####################
                                      
                                      read_page_1 <- function(x){
                                        webpages <- httr::GET(x)
                                        html <- rvest::read_html(httr::content(webpages, "text"))
                                        return(html)
                                      }
                                      
                                      collection_html_sub_pages <- lapply(sub_pages,read_page_1)
                                      p_reader <- function(x){
                                        ul_element <- rvest::html_node(x, ".c-article-metrics-bar.u-list-reset")
                                        if (!is.null(ul_element)) {
                                          li_elements <- rvest::html_elements(ul_element, "li.c-article-metrics-bar__item")
                                          if (length(li_elements) > 0) {
                                            p_elements <- rvest::html_elements(li_elements[[1]], "p.c-article-metrics-bar__count")
                                            if (length(p_elements) > 0) {
                                              temp <- str_split(rvest::html_text(p_elements[[1]])," ")[[1]][1]
                                              first_p_value <- as.numeric(gsub("k", "000", temp))
                                              return(first_p_value)
                                            }
                                          }
                                        }
                                        return(NULL)  # Return NULL if no matching elements found
                                      }  
                                      # p_reader <- function(a){
                                      #   c <- rvest::html_node(a,"ul.c-article-metrics-bar u-list-reset")
                                      #   print(c)
                                      #   d <- rvest::html_elements(c,"li")
                                      #   b <- rvest::html_elements(d,"p.c-article-metrics-bar__count")
                                      #   return(b)
                                      # }
                                      p_list <- lapply(collection_html_sub_pages,p_reader)
                                      data_plot <- data.frame(Value = unlist(p_list))
                                      ggplot2::ggplot(data_plot, aes(x = Value)) +
                                        ggplot2::geom_histogram(binwidth = 0.125, fill = "blue", color = "black") +
                                        scale_x_log10()+
                                        labs(title = "Access Counts Histogram", y = "Frequency")
                                      # Check if p_list is not empty
                                    },
                                  process_webPage = function(){
                                    in_var <- in_var_read()
                                    query_params <- list(query =paste0(in_var$query), volume=paste0(in_var$volume), tab="keyword")
                                    doc <- httr::GET(base_url,query=query_params)
                                    html <- xml2::read_html(httr::content(doc, "text"))
                                    #a_text <- title_obtainer(html)
                                    ##write_to_csv(a_text) #Uncomment to write to csv
                                    access_counts_grapher(html)
                                  }
                                  
                                ))
titles <- webscraper_class(base_url = "https://mobilednajournal.biomedcentral.com/articles")
titles$process_webPage()
