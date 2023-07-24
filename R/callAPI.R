#' Generate a URL for the Disqus API call.
#'
#' This function generates the URL for making API calls to the Disqus platform.
#' It constructs the URL based on the specified resource, option, and various parameters provided.
#' The generated URL can then be used to retrieve data from the Disqus API.
#'
#' @param ressource Character vector specifying the type of resource to access through the API.
#' @param option Character vector specifying the specific API operation to perform.
#' @param forum Character vector containing the forum identifier or shortname.
#' @param thread Character vector containing the thread identifier or link.
#' @param user Character vector containing the user identifier.
#' @param post Character vector containing the post identifier.
#' @param author Character vector containing the author identifier.
#' @param category Character vector containing the category identifier.
#' @param count Numeric value specifying the number of items to retrieve.
#' @param depth Numeric value specifying the depth of the query.
#' @param interval Character vector specifying the time interval for data retrieval.
#' @param query Character vector containing the query string for filtering data.
#' @param range Character vector specifying the time range for data retrieval.
#' @param since Character vector specifying the starting date for data retrieval.
#' @param since_id Character vector specifying the starting identifier for data retrieval.
#' @param start Character vector specifying the starting point for data retrieval.
#' @param end Character vector specifying the ending point for data retrieval.
#' @param priority Character vector specifying the sorting type for data retrieval.
#' @param attach Character vector specifying additional attachments for the query.
#' @param filters Character vector specifying filters to apply to the query.
#' @param include Character vector specifying additional data to include in the response.
#' @param related Character vector specifying related data to include in the response.
#' @param limit Numeric value specifying the maximum number of items to return in the response.
#' @param order Character vector specifying the order of the items in the response.
#' @param cursor Character vector specifying the cursor for pagination.
#' @param type Character vector specifying the data type for the response (e.g., "json", "xml").
#' @param version Character vector specifying the version of the Disqus API to use (default is "3.0").
#' @param key Character vector containing the API key. If not provided, a placeholder is used.
#'
#' @return A character vector representing the API call URL, based on the provided parameters.
#'
#' @examples
#' # Generate the URL to retrieve 10 posts from the "my_forum" forum in JSON format
#' generateURL(ressource = "posts", option = "list", forum = "my_forum", thread = "XYZ")
#'
#' @export
generateURL <- function(
    ressource = c("forums", "pages", "posts", "threads", "trends", "users"),
    option = NULL,
    forum = NULL,
    thread = NULL,
    user = NULL,
    post = NULL,
    author = NULL,
    category = NULL,
    count = NULL,
    depth = NULL,
    interval = NULL,
    query = NULL,
    range = NULL,
    since = NULL,
    since_id = NULL,
    start = NULL,
    end = NULL,
    priority = NULL,
    attach = NULL,
    filters = NULL,
    include = NULL,
    related = NULL,
    limit = 25,
    order = "desc",
    cursor = NULL,
    type = "json",
    version = "3.0",
    key = NULL
) {
  # Stop if no option
  if (is.null(option)){
    stop("Error: No option called.")
  }

  # Set base URL
  url <- "https://disqus.com/api/"

  # Add API version
  url <- paste0(url, version, "/")

  # Add API endpoint ("ressource")
  url <- paste0(url, ressource, "/")

  # Add option and type
  url <- paste0(url, option, ".", type)


  # Add key placeholder if none provided
  if (is.null(key)){
    url <- paste0(url, "?api_key=YOURAPPKEY", key)
  }

  # Stop if key not provided as string
  if (!is.null(key) && !is.character(key)){
    stop("Error: Please provide app key as string.")
  }

  if (!is.null(key) && is.character(key)){
    url <- paste0(url, "?api_key=", key)
  }

  ### Object parameters

  if (!is.null(forum)) {
    url <- paste0(url, "&forum=", forum)
  }

  if (!is.null(thread)) {

    # Switch: thread provided by link or ID:
    if (grepl("^(http|https)", thread)) {
      url <- paste0(url,
                    "&thread=link:",
                    utils::URLencode(thread, reserved = TRUE))
    } else {
      url <- paste0(url, "&thread=", thread)
    }

  }

  if (!is.null(user)) {
    url <- paste0(url, "&user=", user)
  }

  if (!is.null(post)) {
    url <- paste0(url, "&post=", post)
  }

  ### Other parameters:
  if (!is.null(author)) {
    url <- paste0(url, "&author=", author)
  }

  if (!is.null(category)) {
    url <- paste0(url, "&category=", category)
  }

  if (!is.null(count)) {
    url <- paste0(url, "&count=", count)
  }

  if (!is.null(depth)) {
    url <- paste0(url, "&depth=", depth)
  }

  if (!is.null(interval)) {
    url <- paste0(url, "&interval=", interval)
  }

  if (!is.null(query)) {
    url <- paste0(url, "&query=", query)
  }

  if (!is.null(range)) {
    url <- paste0(url, "&range=", range)
  }

  if (!is.null(since)) {
    url <- paste0(url, "&since=", since)
  }

  if (!is.null(since_id)) {
    url <- paste0(url, "&since_id=", since_id)
  }

  if (!is.null(start)) {
    url <- paste0(url, "&start=", start)
  }

  if (!is.null(end)) {
    url <- paste0(url, "&end=", end)
  }

  if (!is.null(priority)) { # default is "date"
    url <- paste0(url, "&sortType=", "priority")
  }

  ### Parameters that allow multiple inputs

  if (!is.null(attach)) {
    url <- paste0(url, paste0("&attach=", attach, collapse = ""))
  }

  if (!is.null(filters)) {
    url <- paste0(url, paste0("&filters=", filters, collapse = ""))
  }

  if (!is.null(related)) {
    url <- paste0(url, paste0("&related=", related, collapse = ""))
  }

  if (!is.null(include)) {
    url <- paste0(url, paste0("&include=", include, collapse = ""))
  }

  # Cursor
  if (!is.null(cursor)) {
    url <- paste0(url, "&cursor=", cursor)
  }

  # Limit
  if (limit != 25) {
    url <- paste0(url, "&limit=", limit)
  }

  # Order
  if (order!="desc") {
    url <- paste0(url, "&order=", order)
  }

  ## Print URL
  # cat("URL: ", url, "\n")

  return(url)

}





#' Call the Disqus API.
#'
#' This function makes a call to the Disqus API using the provided URL and API key.
#' It replaces the API key in the URL with the one provided and retrieves the data from the API.
#' The response is parsed and returned as an R object.
#'
#' @param url A character vector representing the complete URL for the Disqus API call.
#' @param key A character vector containing the API key to authenticate the API call.
#' @param verbose Logical value indicating whether to display informative messages (default is TRUE).
#'
#' @return The response from the Disqus API call, parsed as a list.
#'
#' @examples
#' # Call the API to retrieve the data from the specified URL with the provided API key
#' # result <- callAPI(url = url, key = "YOURAPPKEY")
#'
#' @export
callAPI <- function(url,
                    key,
                    verbose = TRUE){

  # Stop if no url
  if (is.null(url)){
    stop("Error: No URL specified.")
  }

  # Replace api key
  newkey <- paste0("json?api_key=", key)
  url <- gsub("json.+(?=\\&)", newkey, url, perl = TRUE)

  # Call API
  tryCatch(
    {
      raw <- httr::GET(url)
      limit_count <- raw$headers$`x-ratelimit-remaining`
      if (verbose) cat(limit_count, "calls left this hour. \n")

      content <- httr::content(raw)
    },
    error = function(e) {
      message(paste0("An error occured while calling API: ", e))
      NA
    }
  )

  # Check if hourly limit has been reached
  if(!is.null(content$code) && content$code == "13"){

    # Print message that hourly API limit is reached.
    cat(content$response)
    diff <- round(lubridate::ceiling_date(Sys.time(), "hours") - Sys.time(), 0)
    cat("Please wait", diff, "min. until API limit resets or use second key. \n")

  }

  return(content)

}







#' Call the Disqus API using spare key.
#'
#' This function makes a call to the Disqus API using the provided URL and primary API key.
#' If the hourly API limit is reached (error code 13), and a second API key is provided,
#' the function will make a second API call using the second key.
#' The response from the API call is parsed and returned as a list.
#'
#' @param url A character vector representing the complete URL for the Disqus API call.
#' @param key A character vector containing the primary API key to authenticate the first API call.
#' @param second_key A character vector containing a second API key to use if the hourly API limit is reached with the primary key (default is NULL).
#' @param verbose Logical value indicating whether to display informative messages (default is TRUE).
#'
#' @return The response from the Disqus API call, parsed as a list.
#'
#' @examples
#' # Call the API to retrieve the data from the specified URL using the primary key,
#' # with a fallback to the second key if the hourly API limit is reached.
#' #result <- leechAPI(url = url,
#' #                   key = "YOURAPPKEY",
#' #                   second_key = "YOURSECONDAPPKEY")
#'
#' @export
leechAPI <- function(url,
                     key,
                     second_key = NULL,
                     verbose = TRUE){

  # First API call
  content <- callAPI(url = url, key = key, verbose = verbose)

  # Check if hourly limit has been reached
  if(!is.null(content$code) && content$code == "13" && !is.null(second_key)){
    if (verbose) cat("Using second key: \n")
    content <- callAPI(url = url, key = second_key, verbose = verbose)
  }

  return(content)
}






#' Call the Disqus API using pagination and optionally a second API key.
#'
#' This function makes API calls to the Disqus API using the provided URL and primary API key. If a second API key is provided, and the hourly API limit is reached with the primary key, the function will use the second key for subsequent API calls. The function implements pagination to retrieve multiple pages of data if available and combines the results into a single R object.
#'
#' @param url A character vector representing the complete URL for the initial Disqus API call.
#' @param key A character vector containing the primary API key to authenticate the first API call.
#' @param second_key A character vector containing a second API key to use if the hourly API limit is reached with the primary key (default is NULL).
#' @param verbose Logical value indicating whether to display informative messages during API calls (default is TRUE).
#' @param n Numeric value specifying the maximum number of observations to retrieve (default is Inf, meaning all available observations).
#'
#' @return A list or vector containing the combined data retrieved from multiple API calls, based on pagination and API keys.
#'
#' @examples
#' # Call the API to retrieve all available data from the specified URL using the primary key,
#' # with a fallback to the second key if the hourly API limit is reached.
#' #all_data <- paginateAPI(url = url,
#' #                         key = "YOURAPPKEY",
#' #                         second_key = "YOURSECONDAPPKEY",
#' #                         verbose = TRUE,
#' #                         n = Inf)
#'
#' @export
paginateAPI <- function(url,
                        key,
                        second_key = NULL,
                        verbose = TRUE,
                        n = Inf){

  if(is.null(second_key)){
    # First API call
    content <- callAPI(url = url, key = key, verbose = verbose)
  }else{
    # First API call
    content <- leechAPI(url = url, key = key, second_key = second_key, verbose = verbose)
  }

  # Check amount of data
  con <- content$response
  len <- length(con); if (verbose) cat(len, "obs. ")

  # Check if there's more
  while (len<n && content$cursor$hasNext == TRUE){
    # waiting before making next API call...
    Sys.sleep(0.1)

    cursor <- content$cursor$`next`

    # Replace old cursor if in url
    if(grepl("&cursor=", url)){
      newcursor <- paste0("&cursor=", cursor)
      nxt_url <- gsub("(&cursor=.+(?=\\&)|&cursor=.+$)", newcursor, url, perl = TRUE)
    }

    if(!grepl("&cursor=", url) == TRUE){
      nxt_url <- paste0(url, "&cursor=", cursor)
    }


    if(is.null(second_key)){
      # First API call
      nxt_content <- callAPI(url = nxt_url, key = key, verbose = verbose)
    }else{
      # First API call
      nxt_content <- leechAPI(url = nxt_url, key = key, second_key = second_key, verbose = verbose)
    }

    nxt_con  <- nxt_content$response

    len <- len + length(nxt_con)
    if (length(nxt_con)>0){ if (verbose) cat(len, "obs. ") }

    # Append content lists
    con <- base::append(con, nxt_con)
  }

  return(con)

}




#' Generic function to retrieve data from the Disqus API in different formats.
#'
#' This function is a wrapper around other functions in the package, providing a generic interface to interact with the Disqus API. It allows users to retrieve data from the API in either a 'tidy', unnested format or 'raw' (JSON) format. The function generates the appropriate URL for the API call using the provided resource, option, and additional arguments, and then makes paginated API calls to retrieve the data. Optionally, a second API key can be provided to handle the hourly API limit.
#'
#' @param ressource Character vector specifying the type of resource to access through the API.
#' @param option Character vector specifying the specific API operation to perform.
#' @param key Character vector containing the primary API key to authenticate the API call(s).
#' @param second_key Character vector containing a second API key to use if the hourly API limit is reached with the primary key (default is NULL).
#' @param verbose Logical value indicating whether to display informative messages during the API calls (default is TRUE).
#' @param n The maximum number of observations to retrieve (default is 100).
#' @param limit Numeric value specifying the maximum number of items to return in each API response (default is 25).
#' @param format Character vector specifying the desired output format: 'tidy' (unnested) or 'raw' (JSON) (default is 'tidy').
#' @param ... Additional arguments to be passed to the `generateURL` function.
#'
#' @return The retrieved data from the Disqus API in the specified format.
#'
#' @seealso `generateURL`, `paginateAPI`, `unnest_recursively`
#'
#' @examples
#' # Retrieve data in 'tidy' format using the primary key
#' # tidy_data <- get_generic(ressource = "forums", option = "listThreads", key = "YOURAPPKEY")
#'
#'
#' @export
get_generic <- function(ressource,
                        option,
                        key,
                        second_key = NULL,
                        verbose = TRUE,
                        n = 100,
                        limit = 25,
                        format = c("tidy", "raw"),
                        ...){

  # Generate url, pass additional arguments
  url <- generateURL(ressource = ressource,
                     option = option,
                     limit = limit,
                     ...)

  # Make paginated API call, optionally using a second API key
  con <- paginateAPI(url = url,
                     key = key,
                     second_key = second_key,
                     verbose = verbose,
                     n = n)

  # Check which output format
  format <- match.arg(format, c("tidy", "raw"))

  if(format == "tidy"){
    con <- do.call(rbind, con) |> tibble::as_tibble()
    con <- unnest_recursively(con)
    return(con)
  }

  if(format == "raw"){
    return(con)
  }
}







