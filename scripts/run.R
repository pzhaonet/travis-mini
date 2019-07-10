require(lubridate)
require(magrittr)
require(xml2)
require(dplyr)
require(jsonlite)

get_rdevel <- function(ym, site = 'r-devel'){
  # ym <e- today()
  Month <- format(ym, '%Y-%m')
  ym <- format(ym, '%Y-%B')
  
  url = paste0("https://stat.ethz.ch/pipermail/", site, "/", ym, "/thread.html")
  
  dir = dirname(url)
  
  ## all main thread
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
  
  Time = paste0(Month, '-01')
  
  
  return(list(
    replies = data.frame(Month, Replies, Title, url, Time, stringsAsFactors = FALSE),
    authors = data.frame(Author, Month, stringsAsFactors = FALSE) %>% 
      group_by(Month, Author) %>% 
      summarise(Replies = n())
    ))
  
}

get_rdeveln <- function(n, site){
  rdevel <- lapply(today() - months(0:n), get_rdevel, site = site)
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

# update: new version

message('reading db.Rdata')

RDSfile <- 'data/db.RData'
load(RDSfile)

# con = url('https://github.com/pzhaonet/rchive_travis/raw/master/db.RData')
# load(con)
message('update db.Rdata...')
df_coscsv$Month <- as.character(df_coscsv$Month)
#last_update <- difftime(Sys.Date(), as.Date(file.info(RDSfile)$mtime))
newmonth <- 1 #ceiling(as.numeric(as.duration(last_update),  'months'))
# # download the data that are not in db.RData
rdevel_new <- get_rdeveln(n = newmonth, site = 'r-devel')
rhelp_new <- get_rdeveln(n = newmonth, site = 'r-help')
cos_js <- fromJSON('https://d.cosx.org/api/discussions?page%5Blimit%5D=50&page%5Boffset%5D')
cpc <- c('title', 'commentCount', 'participantCount', 'createdAt', 'lastPostedAt')
cos_new = cos_js$data$attributes[, cpc]
cos_new$link = cos_js$data$id
cos_new <- calc_df_cos(cos_new)

df_rdevelcsv <- bind_rows(df_rdevelcsv, rdevel_new$replies)
df_rdevelcsv <- df_rdevelcsv[!duplicated(df_rdevelcsv$url), ]

df_rhelpcsv <- bind_rows(df_rhelpcsv, rhelp_new$replies)
df_rhelpcsv <- df_rhelpcsv[!duplicated(df_rhelpcsv$url), ]

df_coscsv <- bind_rows(df_coscsv, cos_new)
df_coscsv <- df_coscsv[!duplicated(df_coscsv$Title), ]


# RDSfile <- 'db_new.RData'
message('Writing db.Rdata...')
save(file = RDSfile, list = c('df_coscsv', 'df_rdevelcsv', 'df_rhelpcsv', 'msg'))

message('Well done!')

