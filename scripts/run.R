# load packages -------------------------------------------------
# require(lubridate)
# require(magrittr)
require(xml2)
require(dplyr)
require(jsonlite)


# functions -----------------------------------------------------

## get one month from r devel or r help --------------------------
get_rdevel <- function(ym, site = 'r-devel'){
  Month <- format(ym, '%Y-%m')
  ym <- format(ym, '%Y-%B')

  url = paste0("https://stat.ethz.ch/pipermail/", site, "/", ym, "/thread.html")

  dir = dirname(url)

  ## all main thread
  if (class(try(read_html(url), silent = TRUE))[1] == 'try-error') {
    return(NULL)
  } else {
    tmpl =
      read_html(url) %>%
      xml_find_all("//body/ul[2]/li")

    title=
      tmpl %>%
      xml_find_first(".//a") %>%
      xml_contents() %>%
      as.character()

    Title <- gsub('\\n|^\\[Rd\\] *|^R-alpha: *|^\\[R\\] *|^R-beta: *', '', title)

    author <- tmpl %>%
      xml_find_all(".//i") %>%
      xml_contents() %>%
      as.character()
    Author <- gsub('\\n', '', author)


    url =
      tmpl %>%
      xml_find_first(".//a") %>%
      xml_attr("href") %>%
      paste0(dir,"/",.)

    Replies =
      sapply(tmpl, function(node) {
        return(node %>% xml_find_all(".//li") %>% length())
      })

    return(list(
      replies = data.frame(Month, Replies, Title, url, stringsAsFactors = FALSE),
      authors = data.frame(Author, Month, stringsAsFactors = FALSE) %>%
        group_by(Month, Author) %>%
        summarise(Replies = n())
    ))
  }
}

## get multiple months from r devel or r help --------------------------
get_rdeveln <- function(yms, site){
  rdevel <- lapply(yms, get_rdevel, site = site)
  list(replies = dplyr::bind_rows(lapply(rdevel, function(x) x[1][[1]])),
       authors = dplyr::bind_rows(lapply(rdevel, function(x) x[2][[1]])))
}

# extract dataframe from js
get_cosdf <- function(l) {
  df = l$data$attributes
  df$link = l$data$id
  return(df)
}

calc_df <- function(df, add_url = TRUE){
  if (add_url) df$Title <- paste0('<a href="', df$url, '">', df$Title , '</a>')
  df <- df[order(-df$Replies), ]
  df$time <- as.Date(paste0(df$Month, '-01'))
  return(df)
}

calc_df_cos <- function(df){
  df_new <- data.frame(time = as.Date(substr(df$createdAt, 1, 10)),
                       Month = substr(df$createdAt, 1, 7),
                       Title = paste0('<a href=https://d.cosx.org/d/', df$link, '>', df$title , '</a>'),
                       Replies = df$commentCount,
                       Participants = df$participantCount,
                       Update = as.Date(substr(df$lastPostedAt, 1, 10)),
                       hour = as.numeric(substr(df$createdAt, 12, 13)) + as.numeric(substr(df$createdAt, 12, 13))/60,
                       stringsAsFactors = FALSE)
  df_new$Active_Days <- as.numeric(df_new$Update - df_new$time)
  df_new[order(-df_new$Replies), ]
}


# # Get the complete database ---------------------------------------------------------------
#
# ## R dev ------
# sysdate <- Sys.Date()
# ym_seq <- seq(1997 + 1 / 12 * 3, as.numeric(format(sysdate, "%Y")) + 1 / 12 * (as.numeric(format(sysdate, "%m")) - 1), 1 / 12)
# ym <- paste0(floor(ym_seq), "-", round((ym_seq - floor(ym_seq)) * 12 + 1), "-1")
# ym <- as.Date(ym)
# rdevel <- get_rdeveln(ym, site = 'r-devel')
# rdevel_r <- rdevel$replies
# rdevel_r$Title <- paste0('<a href=', rdevel_r$url, '>', rdevel_r$Title , '</a>')
# rdevel_r <- rdevel_r[, - which(names(df_rdevelcsv) == "url")]
# saveRDS(rdevel_r, 'rdevel.RDS')
#
# ## R help ------
#
# rhelp <- get_rdeveln(ym, site = 'r-help')
# rhelp_r <- rhelp$replies
# rhelp_r$Title <- paste0('<a href=', rhelp_r$url, '>', rhelp_r$Title , '</a>')
# rhelp_r <- rhelp_r[, - which(names(df_rhelpcsv) == "url")]
# saveRDS(rhelp_r, 'rhelp.RDS')
#
# ## COSX ------
# # get the max page (1422 pages on  2019-06-18, 1471 pages on 2021-03-29)
# get_maxpage <- function(page_range = 1421:1500){
#   for (i in page_range) {
#     print(paste(Sys.time(), i))
#     COS_link <- xml2::read_html(paste0('https://d.cosx.org/all?page=', i))
#     url_vector= rvest::html_attr(rvest::html_nodes(COS_link, "a"), "href")
#     last_link = url_vector[length(url_vector)]
#     last_number <- as.numeric(gsub("[https://d.cosx.org/all?page=]", "",last_link) )
#     if(last_number <= i - 1){
#       message('There are ', i, ' pages with 20 posts on each.')
#       return(i)
#     }
#   }
# }
#
# # get json from cos
# get_js <- function(url){
#   # url <- cos_url[1]
#   print(paste(Sys.time(), url))
#   mytry <- try(jsonlite::fromJSON(url))
#   if(class(mytry) == 'try-error') return(NULL)
#   jsonlite::fromJSON(url)
# }
#
# # extract dataframe from js
# get_cosdf <- function(l) {
#   df = l$data$attributes
#   df$link = paste0("https://d.cosx.org/d/", l$data$id)
#   return(df)
# }
#
# get_cosauthor <- function(l){
#   authors <- unlist(l$included$attributes$username)
#   authors <- authors[!is.na(authors)]
# }
#
# maxpage <- get_maxpage(1470:2100)
# cos_url <- paste0('https://d.cosx.org/api/discussions?page%5Blimit%5D=50&page%5Boffset%5D=', seq(0, maxpage * 20, 50) + 50)
#
# cos_js <- lapply(cos_url, get_js)
# # saveRDS(cos_js, 'cos_js.RDS')
# cos_ls <- lapply(cos_js, get_cosdf)
# cos_author <- unlist(sapply(cos_js, get_cosauthor))
# cos_author_df <- as.data.frame(table(cos_author))
# cos_author_df <- cos_author_df[order(-cos_author_df$Freq), ]
#
# cpc <- c('title', 'commentCount', 'participantCount', 'createdAt', 'lastPostedAt', 'link')
# cos_df <- dplyr::bind_rows(cos_ls)
# cos_df <- cos_df[, cpc]
# cos_df <- calc_df_cos(cos_df)
# saveRDS(cos_df, 'cos.RDS')
# # paste(names(cos_df), collapse = "', '")

# update the database -----------------------------
## load old data ----
message('reading db.Rdata')

RDSfile <- 'data/db.RData'
load(RDSfile)
# con = url('https://github.com/pzhaonet/rchive_travis/raw/master/db.RData')
# load(con)

## read new data ----
message('update db.Rdata...')
# class(df_coscsv$Month) <- as.character(df_coscsv$Month)
# last_update <- difftime(Sys.Date(), as.Date(file.info(RDSfile)$mtime))
# newmonth <- 1 #ceiling(as.numeric(as.duration(last_update),  'months'))
newmonth <- Sys.Date()
# # download the data that are not in db.RData
rdevel_new <- get_rdeveln(yms = newmonth, site = 'r-devel')
rhelp_new <- get_rdeveln(yms = newmonth, site = 'r-help')

cos_js <- fromJSON('https://d.cosx.org/api/discussions?page%5Blimit%5D=50&page%5Boffset%5D')
cpc <- c('title', 'commentCount', 'participantCount', 'createdAt', 'lastPostedAt')
cos_new = cos_js$data$attributes[, cpc]
cos_new$link = cos_js$data$id
cos_new <- calc_df_cos(cos_new)

### merge data
if (length(rdevel_new$replies) > 0) {
  rdevel_new$Title <- paste0('<a href=', rdevel_new$url, '>', rdevel_new$Title , '</a>')
  rdevel_new <- rdevel_new[, - which(names(rdevel_new) == "url")]
  df_rdevelcsv <- bind_rows(rdevel_new$replies, df_rdevelcsv)
  url_rdevel <- gsub('^.*href=([^>]+).*$', '\\1', df_rdevelcsv$Title)
  df_rdevelcsv <- df_rdevelcsv[!duplicated(url_rdevel), ]
}

if (length(rhelp_new$replies) > 0) {
  rhelp_new$Title <- paste0('<a href=', rhelp_new$url, '>', rhelp_new$Title , '</a>')
  rhelp_new <- rhelp_new[, - which(names(rhelp_new) == "url")]
  df_rhelpcsv <- bind_rows(rhelp_new$replies, df_rhelpcsv)
  url_rhelp <- gsub('^.*href=([^>]+).*$', '\\1', df_rhelpcsv$Title)
  df_rhelpcsv <- df_rhelpcsv[!duplicated(url_rhelp), ]
}
df_coscsv <- bind_rows(cos_new, df_coscsv)
url_cos <- gsub('^.*href=([^>]+).*$', '\\1', df_coscsv$Title)
df_coscsv <- df_coscsv[!duplicated(url_cos), ]

# write the data file -------------------------------------------
message('Writing db.Rdata...')
save(file = RDSfile, list = c('df_coscsv', 'df_rdevelcsv', 'df_rhelpcsv', 'msg'))
message('Well done!')

