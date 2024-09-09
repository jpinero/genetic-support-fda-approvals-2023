
get_data_api <- function( url = new_url, verbose = FALSE, warnings = TRUE, n_pags=100, api_key = key ) {

  cat("\n")
  resp_format <- "application/json"

  result <- NULL
  r <- httr::GET(url ,
                 add_headers(.headers = c('Authorization'= api_key,
                                          'accept' = resp_format)))

  if (r$status_code == 200) {
    res <- jsonlite::fromJSON(rawToChar(r$content))
    n <- ceiling(res$paging$totalElements/100) -1
    mydata <- res$payload
  }else{
    print(httr::http_status(r))
    print(httr::content(r, "text"))
  }

  if(n > n_pags){
    cat(paste0("number of pages ", n))
    n = n_pags

  }

  for (nn in c(1:n)){
    Sys.sleep(1)
    url2 <- paste0(url, "&page_number=", nn)
    r <- httr::GET(url2, httr::add_headers(Authorization=api_key))
    res <- fromJSON(rawToChar(r$content))
    mydata <- rbind(mydata,  res$payload)
  }
  result <- data.table::data.table(unique(mydata))
  return( result )
}
