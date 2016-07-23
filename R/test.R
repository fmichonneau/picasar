##' @importFrom httr GET
pic_GET <- function(...) {
    res <- httr::GET("https://picasaweb.google.com/", path = "data/feed/api/user/francois.michonneau")
}

pic_album_list <- function(user = "francois.michonneau") {
    res <- pic_GET(path = paste0("data/feed/api/user", user))
    content(res, as = "text", encoding = "utf-8")
}
