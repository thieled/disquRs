

callAPI <- function(url){

  # Stop if no url
  if (is.null(url)){
    stop("Error: No URL specified.")
  }

  cat("URL: ", url, "\n")

  # Call API
  content <- tryCatch(
    {
      httr::content(
        httr::GET(url)
      )
    },
    error = function(e) {
      message(paste0("An error occured while calling API: ", e))
      NA
    }
  )

  if (length(content$error)>0){
    stop(content$error$message)
  }

  return(content)

}


# URL generator

generateURL <- function(

  ressource = c("forums",
                "pages",
                "posts",
                "threads",
                "trends",
                "users"
  ),

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

  key = NULL){

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


  # Add access token

  # Stop if no app key
  if (is.null(key) | !is.character(key)){
    stop("Error: Please provide app key as string.")
  }

  if (!is.null(key) & is.character(key)){
    url <- paste0(url, "?api_key=", key)
  }


  # Object parameters

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

  # Other parameters:
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

  # Parameters that allow multiple inputs

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
