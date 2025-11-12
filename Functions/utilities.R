#Null selector
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

add_links <- function(col){
  sapply(col, FUN=function(x){
    if(str_detect(x, '\\[([^\\]]+)\\]\\(([^ "]+?)(?: "(.+)")?\\)')) { #std markdown regex string
      link_text <- str_match(x, '\\[([^\\]]+)\\]\\(([^ "]+?)(?: "(.+)")?\\)')
      as.character(tags$a(link_text[[2]],
                          href=link_text[[3]],
                          target="_blank"))
    } else if(str_detect(x, "^https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,4}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)$")) {
      as.character(tags$a(x, href=x, target="_blank"))
    } else if(str_detect(x, "^10\\.\\d{4,9}/[-._;()/:A-Z0-9]+$")) {
      as.character(tags$a(x, href=paste0("https://doi.org/", x), target="_blank"))
    } else {
      x
    }
  }
  )
}