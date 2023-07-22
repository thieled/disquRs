get_thread_list <- function(thread = NULL,
                            forum = NULL,
                            key,
                            verbose = TRUE,
                            n = 500,
                            ...){

  ressource = "threads"
  option = "list"

  url <- generateURL(ressource = ressource,
                     option = option,
                     thread = thread,
                     forum = forum,
                     key = key,
                     ...)

  # Call API
  res <- callAPI(url = url)
  con <- res$response
  len <- length(con); if (verbose) cat(len, "threads ")


  # Check if there's more
  while (len<n && res$cursor$hasNext == TRUE){
    # waiting before making next API call...
    Sys.sleep(0.33)

    cursor <- res$cursor$`next`

    nxt_url <- generateURL(ressource = ressource,
                           option = option,
                           thread = thread,
                           forum = forum,
                           key = key,
                           cursor = cursor,
                           ...)

    res <- callAPI(url=nxt_url)
    nxt_con  <- res$response

    len <- len + length(nxt_con)
    if (length(nxt_con)>0){ if (verbose) cat(len, "threads ") }

    # Append content lists
    con <- append(con,
                  nxt_con)
  }

  # Tidy up
  suppressWarnings(
    con <- con %>%
      data.table::rbindlist(fill = T) %>%
      dplyr::as_tibble()
  )

  return(con)
}
