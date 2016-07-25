##' @importFrom httr GET
pic_GET <- function(...) {
    res <- httr::GET("https://picasaweb.google.com/", ...)
}



##' @importFrom httr content
##' @importFrom xml2 read_xml xml_ns_rename xml_ns xml_find_all xml_text
pic_album_list <- function(user = "francois.michonneau", ...) {
    res <- pic_GET(path = paste0("data/feed/api/user/", user), ...)
    xml_res <- httr::content(res, as = "raw", encoding = "utf-8")
    xml_res <- xml2::read_xml(xml_res)
    ns_res <- xml2::xml_ns_rename(xml2::xml_ns(xml_res), d1 = "atom")
    full_names <- xml2::xml_find_all(xml_res, ".//atom:entry/atom:title", ns = ns_res)
    google_names <- xml2::xml_find_all(xml_res, ".//atom:entry/gphoto:name", ns = ns_res)
    id_names <- xml2::xml_find_all(xml_res, ".//atom:entry/gphoto:id", ns = ns_res)
    res <- data.frame(id = xml2::xml_text(id_names),
                      full_name = xml2::xml_text(full_names),
                      google_name = xml2::xml_text(google_names),
                      stringsAsFactors = FALSE)
    attr(res, "user_id") <- xml2::xml_text(xml2::xml_find_first(xml_res, ".//gphoto:user", ns = ns_res))
    attr(res, "uris") <- paste0("https://picasaweb.google.com/data/feed/api/user/",
                               attr(res, "user_id"),
                               "/albumid/", res$id)
    attr(res, "thumbs") <- xml2::xml_text(xml2::xml_find_all(xml_res, ".//media:group/media:content/@url"))
    res
}


pic_photo_list <- function(album_name, user = "francois.michonneau", ...) {
    all_alb <- pic_album_list(user, ...)
    album_name <- match.arg(album_name, all_alb$google_name)
    pht_res <- httr::GET(attr(all_alb, "uris")[match(album_name, all_alb$google_name)], ...)
    pht_con <- httr::content(pht_res, as = "raw", encoding = "utf-8")
    pht_xml <- xml2::read_xml(pht_con)
    ns_res <- xml2::xml_ns_rename(xml2::xml_ns(pht_xml), d1 = "atom")
    img_url <- xml2::xml_find_all(pht_xml, ".//media:group/media:content/@url")
    img_url <- xml2::xml_text(img_url)
    captions <- xml2::xml_find_all(pht_xml, ".//media:group/media:description")
    captions <- xml2::xml_text(captions)
    album_title <- xml2::xml_text(xml2::xml_find_first(pht_xml, "//atom:feed/atom:title", ns = ns_res))
    album_subtitle <- xml2::xml_text(xml2::xml_find_first(pht_xml, "//atom:feed/atom:subtitle", ns = ns_res))
    res <- data.frame(img_url = img_url,
                      caption = captions,
                      stringsAsFactors = FALSE)
    attr(res, "title") <- album_title
    attr(res, "subtitle") <- album_subtitle
    attr(res, "thumb") <-  xml2::xml_text(xml2::xml_find_first(pht_xml, "//d1:icon"))
    res
}

jekyll_album <- function(photo_list, file) {

    thumb <- attr(photo_list, "thumb")
    teaser <- gsub("s160-c", "w800-o", thumb)
    header <- gsub("s160-c", "s0", thumb)
    cat("--- \n",
        "layout: single \n",
        "title: \"", attr(photo_list, "title"), "\"\n",
        "date: ", format(Sys.Date(), "%Y-%m-%d"), "\n",
        "type: page \n",
        "status: publish \n",
        "header: \n",
        "  image: ", header, "\n",
        "  teaser: ", teaser, "\n",
        "excerpt: \"", attr(photo_list, "subtitle"), "\"\n",
        "gallery: \n", file = file, sep = "")
    apply(photo_list, 1, function(x) {
        full_url <- paste0(dirname(x[1]), "/s0/", basename(x[1]))
        thumb_url <- paste0(dirname(x[1]), "/w800-o/", basename(x[1]))
        caption <- x[2]
        cat("  - url: ", full_url, "\n",
            "    image_path: ", thumb_url, "\n",
            "    alt: \"", caption, "\"\n",
            "    title: \"", caption, "\"\n",
            sep = "", file = file, append = TRUE)
    })
    cat("---\n\n\n", file = file, append = TRUE)

    cat("{% include gallery caption=\"", attr(photo_list, "subtitle"),
        "\" %}", sep = "", file = file, append = TRUE)
    invisible(file)
}


jekyll_galleries <- function(album_list = c("5312413807524136849",
                                            "5312422113847760993",
                                            "5312418895206881137",
                                            "5505966842882076545",
                                            "Ectoprocta",
                                            "Crinoidea",
                                            "5946493914529070097",
                                            "Cnidaria", "Mollusca",
                                            "Annelida",
                                            "DaintreeRainforestNationalPark",
                                            "FloridaWildlife",
                                            "MarineArthropods",
                                            "AxolotlAmbystomaMexicanum",
                                            "5946495710840975121",
                                            "6310635656974481025",
                                            "TerrestrialArthropods"),
                             pages = c("fish", "holothuroidea",
                                       "echinoidea", "urochordata",
                                       "ectoprocta", "crinoidea",
                                       "ophiuroidea", "cnidaria",
                                       "mollusca", "annelida",
                                       "daintree-rainforest",
                                       "florida-wildlife",
                                       "marine-arthropods",
                                       "axolotl",
                                       "worms-not-annelida",
                                       "capitella-telata",
                                       "terrestrial-arthropods"),
                             path_photos = "~/Documents/fm.net_minimal/_photos") {

    if (!identical(length(album_list), length(pages)))
        stop(sQuote("album_list"), " and ", sQuote("pages"),
             " must have the same length.")
    else {
        res <- mapply(function(alb, pg) {
            jekyll_album(photo_list = pic_photo_list(alb),
                         file = file.path(path_photos, paste0(pg, ".html")))
        }, album_list, pages)
        res
    }
}
