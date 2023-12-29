Mobile DNA R WebScraper
================

``` r
library(httr)  # will be use to make HTML GET and POST requests
library(rvest) # will be used to parse HTML
library(methods)
library(tidyr) #will be used to remove NA
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ purrr     1.0.2
    ## ✔ forcats   1.0.0     ✔ readr     2.1.4
    ## ✔ ggplot2   3.4.3     ✔ stringr   1.5.0
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter()         masks stats::filter()
    ## ✖ readr::guess_encoding() masks rvest::guess_encoding()
    ## ✖ dplyr::lag()            masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

The libraries that are being used in this project are httr which is used
for GET and POST requests. Meanwhile, rvest will be used to parse the
HTML documents that will be obtained from httr. Another library that
will be used is the methods library that includes a pseudo-formulation
of Object Oriented Programming using a refClass. Instead of using the S3
class creation method. Tidyr and tidyverse are used for data clean up
and plotting.

``` r
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
```

In the code above I created the class webscraper_class that has
functions that can be used to scraper the Mobile DNA website. However,
now I will show additional features that can be done using this methods
library.
