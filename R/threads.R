#' Get a list of threads from the Disqus API.
#'
#' This function retrieves a list of threads from the Disqus API for the specified thread identifier and forum (optional).
#' The data is returned in either a 'tidy', unnested format or 'raw' (JSON) format.
#'
#' @param thread Character vector containing the thread identifier.
#' @param forum Character vector containing the forum identifier or shortname (default is NULL).
#' @param key Character vector containing the primary API key to authenticate the API call(s).
#' @param second_key Character vector containing a second API key to use if the hourly API limit is reached with the primary key (default is NULL).
#' @param verbose Logical value indicating whether to display informative messages during the API calls (default is TRUE).
#' @param n The maximum number of observations to retrieve (default is 100).
#' @param limit Numeric value specifying the maximum number of items to return in each API response (default is 25).
#' @param format Character vector specifying the desired output format: 'tidy' (unnested) or 'raw' (JSON) (default is 'tidy').
#' @param ... Additional arguments to be passed to the `generateURL` function for URL generation.
#'
#' @return The retrieved list of threads from the Disqus API in the specified format.
#'
#' @seealso `generateURL`, `paginateAPI`, `get_generic`, `unnest_recursively`
#'
#' @examples
#' # Retrieve a list of threads in 'tidy' format using the primary key
#' # tidy_threads <- get_thread_list(thread = "thread_id", key = "YOURAPPKEY")
#'
#' @export
get_thread_list <- function(thread,
                            forum = NULL,
                            key,
                            second_key = NULL,
                            verbose = TRUE,
                            n = 100,
                            limit = 25,
                            format = c("tidy", "raw"),
                            ...){

  format <- match.arg(format, c("tidy", "raw"))

  res <- get_generic(ressource = "threads",
                     option = "list",
                     thread = thread,
                     forum = forum,
                     key = key,
                     second_key = second_key,
                     verbose = verbose,
                     n = n,
                     limit = limit,
                     format = format,
                     ...)

  return(res)
}



