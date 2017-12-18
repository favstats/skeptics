#### Read TXT & extract URLs #####

extract_video_ids <- function(video_list) {
  
  video_list %<>% 
    stri_replace_all_fixed(" ", "") %>% 
    stri_replace_all_charclass("\\p{WHITE_SPACE}", "")
  
  url <- rm_between(video_list, "URL:", "Description", extract = TRUE)
  
  imp_vec <- as.vector(url[[1]]) %>% 
    str_replace(pattern = "(.*v\\=)", "")
  
  return(imp_vec)
}

get_video_ids <- function(textfile) {
  readr::read_file(textfile) %>% 
    extract_video_ids()
}

#### GetAllComments #####

get_all_comments2 <- function(video_id = NULL, ...) {
  yt_check_token <- function() {
    app_token <- getOption("google_token")
    if (is.null(app_token)) stop("Please get a token using yt_oauth().\n")
  }
  
  tuber_GET <- function(path, query, ...) {
    yt_check_token()
    
    req <- GET(
      "https://www.googleapis.com", path = paste0("youtube/v3/", path),
      query = query, config(token = getOption("google_token")), ...
    )
    
    tuber_check <- function(req) {
      if (req$status_code < 400) return(invisible())
      
      stop("HTTP failure: ", req$status_code, "\n", call. = FALSE)
    }
    
    tuber_check(req)
    res <- content(req)
    
    res
  }
  
  querylist <- list(
    videoId = video_id, part = "id,replies,snippet",
    maxResults = 100
  )
  res <- tuber_GET("commentThreads", querylist, ...)
  simple_res <- lapply(res$items, function(x) {
    unlist(x$snippet$topLevelComment$snippet)
  })
  simpler_res <- ldply(simple_res, rbind)
  simpler_res$parentId <- NA
  n_replies <- sapply(res$items, function(x) {
    unlist(x$snippet$totalReplyCount)
  })
  if (sum(n_replies) > 1) {
    replies <- lapply(res$items[n_replies > 0], function(x) {
      unlist(x$replies$comments)
    })
    simpler_rep <- ldply(replies, rbind)
    names(simpler_rep) <- gsub("snippet.", "", names(simpler_rep))
    simpler_rep <- subset(simpler_rep, select = -c(
      kind,
      etag, id
    ))
    agg_res <- plyr::rbind.fill(simpler_res, simpler_rep)
  }
  agg_res <- simpler_res
  page_token <- res$nextPageToken
  while (is.character(page_token)) {
    querylist$pageToken <- page_token
    a_res <- tuber_GET("commentThreads", querylist, ...)
    simple_res <- lapply(a_res$items, function(x) {
      unlist(x$snippet$topLevelComment$snippet)
    })
    simpler_res <- ldply(simple_res, rbind)
    simpler_res$parentId <- NA
    n_replies <- sapply(a_res$items, function(x) {
      unlist(x$snippet$totalReplyCount)
    })
    if (sum(n_replies) > 1) {
      replies <- lapply(a_res$items[n_replies > 0], function(x) {
        unlist(x$replies$comments)
      })
      simpler_rep <- ldply(replies, rbind)
      names(simpler_rep) <- gsub("snippet.", "", names(simpler_rep))
      simpler_rep <- subset(simpler_rep, select = -c(
        kind,
        etag, id
      ))
      agg_res <- plyr::rbind.fill(simpler_res, simpler_rep, agg_res)
      page_token <- a_res$nextPageToken
    }
    agg_res <- plyr::rbind.fill(simpler_res, agg_res)
    page_token <- a_res$nextPageToken
  }
  agg_res %>%
    as.data.frame()
}



#### Scraper Function ####

readkey <- function() {
  line <- readline(prompt = "Are you there? Press [enter]")
}

authentication <- function() {
  beepr::beep(5)
  readkey()
  auth_token <- ".httr-oauth"
  if (file.exists(auth_token)) file.remove(auth_token)
  
  client_id <- "580511698053-p8mu77kktkcvb503g9q737svu9gcq101.apps.googleusercontent.com"
  
  client_key <- "Y0eW96yy-WRg90RZjVJS615o"
  
  tuber::yt_oauth(client_id, client_key)
}

extract_comments <- function(ids, n, youtuber) {
  comments <- list()
  cat("Starting loop. Sit back and get a tea. This might take a while...  ( ͡° ͜ʖ ͡°)\n")
  for (jj in n:length(ids)) {
    #    svMisc::progress(jj)
    outcome <- tryCatch({
      get_all_comments2(ids[jj])
    }, 
    error = function(e){
      e
    })
    if (is.data.frame(outcome)) { #checking if an error occured
      comments[[jj]] <- outcome
    } else if (str_detect(outcome$message, "401")) { #if Error Message include 401
      cat("\n Loop stopped on iteration ", jj)
      cat("\n", outcome$message)
      message("\n YouTube Authentication needed!")
      cat("\n Saving data for safety...")
      save(comments, file = paste0("data/", youtuber, "_comments_",  n, "_", jj, ".Rdata"))   
      cat("\t Done! \n")
      authentication()
      cat("\n Waiting for 10 seconds...")
      Sys.sleep(10)                
      cat("\t Done! \n Continuing...")
      comments[[jj]] <- get_all_comments2(ids[jj])
    } else if (str_detect(outcome$message, "400")) { #if Error Message include 400
      beepr::beep(9)
      cat("\n", outcome$message)
      cat("\n Encountered problem at iteration ", jj)
      cat("\n Saving data for safety...")
      save(comments, file = paste0("data/", youtuber, "_comments_",  n, "_", jj, ".Rdata"))   
      cat("\t Done!")
      cat("\n Waiting for 20 seconds and..")
      Sys.sleep(20)
      cat("\t trying again\n")
      comments[[jj]] <- get_all_comments2(ids[jj])
      cat("\n Puuh... this worked. Deleting file.\n")
      file.remove(paste0("data/", youtuber, "_comments_",  n, "_", jj, ".Rdata"))
    } else if (str_detect(outcome$message, "403")) { #if Error Message include 403
      beepr::beep(11)      
      cat("\n", outcome$message)
      cat("\n Encountered problem at iteration ", jj)
      Sys.sleep(10)
      cat("\n Skipping", jj, "\n")
      next
    } # TODO(favstats): failsafe what happens if another error happens?
    cat(paste0("\t", jj)) 
  }   # End of the for loop
  cat("\n Binding rows and transforming data. Might take while.. \t")
  comments <- suppressWarnings(bind_rows(comments))
  
  comments %<>% 
    mutate(comment = str_to_lower(textOriginal)) %>% 
    mutate(date_posted = as_date(publishedAt)) %>% 
    mutate(id = videoId) %>% 
    mutate(author = authorDisplayName) %>% 
    mutate(author_id = authorChannelId.value) %>% 
    mutate(likes = as.numeric(likeCount)) %>% 
    select(id, author, author_id, likes, date_posted, comment)
  
  cat("Done!\n ")
  cat("\n Saving data one last time...")
  save(comments, file = paste0("data/", youtuber, "_comments_",  n, "_", jj, ".Rdata"))
  cat("\t Done!\n")
  if (jj == length(ids)) beepr::beep(8)
  cat(paste("\n Congratulations! You just succesfully mined", nrow(comments), "comments from", jj, "YouTube Videos"))
  return(comments)
}


#### Video Details ####

extract_details <- function(video_ids, youtuber) {
  cat("Getting the details now.. \n")
  video_ids_details <- purrr::map(video_ids, get_video_details)
  
  cat("1) Extracting Youtuber \n")
  
  channel <- video_ids_details[[1]]$items[[1]]$snippet$channelTitle
  
  cat("2) Extracting Dates \n")
  
  publishedAt <- vector()
  for (jj in seq_along(video_ids)) {
    publishedAt[[jj]] <- video_ids_details[[jj]]$items[[1]]$snippet$publishedAt # time
  }
  
  
  date <- publishedAt %>% 
    as_date()
  
  cat("3) Extracting Titles \n")
  
  titles <- vector()
  for (jj in seq_along(video_ids)) {
    titles[[jj]] <- video_ids_details[[jj]]$items[[1]]$snippet$title # titles
  }
  
  
  cat("4) Extracting Tags \n")
  
  
  tags <- list()
  for (jj in seq_along(video_ids)) {
    tags[[jj]] <- video_ids_details[[jj]]$items[[1]]$snippet$tags # tags
  }
  
  cat("5) Extracting Video IDs \n")
  
  id <- vector()
  for (jj in seq_along(video_ids)) {
    id[[jj]] <- video_ids_details[[jj]][["items"]][[1]][["id"]]
  }
  
  
  details <- tibble(channel, id, date, titles, tags)
  
  cat("6) Extracting Video Statistics \n")
  
  
  stats <- map_df(video_ids, get_stats) %>% 
    mutate_at(vars(viewCount, likeCount, 
                   dislikeCount, favoriteCount,
                   commentCount),
              as.numeric)
  
  details <- left_join(details, stats, by = "id")
  
  save(details, file = paste0("data/", youtuber, "details.Rdata"))
  
  cat("Done! \n")
  

  
  return(details)
}



