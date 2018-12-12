library(RedditExtractoR)
library(dplyr)
library(readxl)
library(httr)
library(rjson)
library(readxl)
library("RCurl")

comments <- c()
postStats <- c()
didntWork <- c()
noComments <- c()
for(i in 1:length(titles$`Movie Title`)){
  urls_unfiltered <- reddit_urls(search_terms=paste(titles[i,],"trailer",sep = " "),subreddit = "movies")
  
  if(!is.null(urls_unfiltered)){
    urls_1 <- urls_unfiltered %>% filter(grepl("trailer",tolower(title)))
    urls <- urls_1 %>% filter(grepl(tolower(titles$'Movie Title'[i]),tolower(title)))
    if(!is.null(urls)){
      maxComments <- max(urls$num_comments)
      urlToUse <- urls %>% filter(num_comments == maxComments) %>% select(URL)
      reddit_data <- reddit_content(URL = urlToUse$URL[1],wait_time = 5)
      if(length(reddit_data$id)==0){noComments <- c(noComments,titles$'Movie Title'[i])}
      currentComments <- reddit_data %>% select(title,comment)
      currentComments$MovieTitle <- rep(titles[i,],length(currentComments$comment))
      currentPostStats <- reddit_data %>%select(title,post_score,upvote_prop,num_comments)  %>% slice(1)
      comments <- rbind(comments,currentComments)
      postStats <- rbind(postStats,currentPostStats)
      print(titles$'Movie Title'[i])
    }
    else{ print(titles$'Movie Title'[i])}
    
  }
  else{ print(titles$'Movie Title'[i])
    didntWork <- c(didntWork,titles$'Movie Title'[i])}
}

write.csv(comments,file = "comments.csv")
write.csv(postStats,file = "postStats.csv")



#IBM watson
url      <- "https://gateway.watsonplatform.net/natural-language-understanding/api/v1/analyze"
#username <- "412c1b9f-a8a2-4bc4-b911-aca61d01b9a5"
#password <- "7aEKF65lpGVf"

username <- "557f8036-e67d-4aa1-b174-2d6ffd8e1732"
password <- "L1gaaTo7kU75"

commentsCSV <- read.csv("comments.csv")
comments <- commentsCSV$comment
comments <- as.vector(comments)
movieTitles <- commentsCSV$MovieTitle
movieTitles <- as.character(movieTitles)


sentiment <- data.frame("score" = character(),"title"=character())
emotion <- data.frame("emotion"=character(),"score"=numeric(),"title"=character())



for(i in 42063:length(comments)){
  watsonUrl <- URLencode(comments[i])
  
  response <-GET(url, query = list(version="2017-02-27", text = watsonUrl, features = "sentiment,emotion"),authenticate(username, password))
  response <-fromJSON(content(response, "text"))
  
  if(!is.null(response$sentiment$document$score)){
    titleScore <- cbind(response$sentiment$document$score,movieTitles[i])
    
  }
  else{
    titleScore <- cbind(0,movieTitles[i])
  }
  colnames(titleScore)<-c("score","title")
  sentiment <- rbind(sentiment,titleScore)
  
  
  
  if(!is.null(response$emotion$document$emotion)){
    emotionVec <-  unlist(response$emotion$document$emotion)
    emotionDF <- data.frame(emotion = names(emotionVec), score = emotionVec,row.names = NULL)
    titleEmotion <- cbind(emotionDF,movieTitles[i])
    colnames(titleEmotion)<- c("emotion","score","title")
  }
  else{
    titleEmotion <- c("NA",0,movieTitles[i])
  }
  
  emotion <- rbind(emotion,titleEmotion)
  
  
  print(c(i,movieTitles[i],response$sentiment$document$score))
}



#aggregating different values from Watson
sentiment1 <- read.csv("sentiment1.csv")
sentiment2 <- read.csv("sentiment2.csv")
sentiment4 <- read.csv("sentiment4.csv")

sentiment <- rbind(sentiment1,sentiment2,sentiment4)

emotion1 <- read.csv("emotion1.csv")
emotion2 <- read.csv("emotion2.csv")
emotion3 <- read.csv("emotion4.csv")

emotion <- rbind(emotion1,emotion2,emotion3)

write.csv(emotion,file = "emotion.csv")


sentiment <- sentiment %>% select(score,title)

avgSentiment <- sentiment %>% group_by(title) %>% summarise(avgScore = mean(score))

avgEmotion <- read.csv("wideEmotion.csv")
avgEmotion <- avgEmotion %>% select(title,anger,fear,joy,sadness)

#Combining the separate data files into one for the model

postStatsTop50 <- read_excel("PostStatsTop50.xlsx")
allPostStats <- read_excel("AllPostStats.xlsx")


#Create data frame for all movies
combined <- left_join(avgEmotion,allPostStats,by="title")
allMovies <- inner_join(combined,avgSentiment,by="title")

#create data frame for top 50 box office movies to be consistent with other group members' data
top50 <- inner_join(postStatsTop50,avgEmotion,by="title") %>% inner_join(avgSentiment,by="title")

write.csv(top50,"top50.csv")
write.csv(allMovies,"allMovies.csv")




#IMdB Web Scraping for Box office totals and predictions
scrap_imbd_2017 <- function(url) {
  html_code <- getURL(url)
  html_code <- gsub(x = html_code, pattern = "[[:blank:]]+", replacement = " ")
  
  
  pos_beg_movie_title <- gregexpr(pattern = "> <img alt=\".+?\"", text = html_code)
  pos_beg_movie_title <- pos_beg_movie_title[[1]]
  pos_beg_movie_title <- pos_beg_movie_title[-1]
  
  pos_end_movie_title <- gregexpr(pattern = "class=\"loadlate\"", text = html_code)
  pos_end_movie_title <- pos_end_movie_title[[1]]
  
  movie_title <- c()
  for(i in 1:length(pos_beg_movie_title)) {
    movie_title <- c(movie_title, substr(html_code, start = pos_beg_movie_title[i] + 12, stop = pos_end_movie_title[i] - 3))
  }
  movie_title
  
  pos_beg_movie_gross <- gregexpr(pattern = "Gross:", text = html_code)
  pos_beg_movie_gross <- pos_beg_movie_gross[[1]]
  
  pos_end_movie_gross <- gregexpr(pattern = "\">[$]", text = html_code)
  pos_end_movie_gross <- pos_end_movie_gross[[1]]
  
  movie_gross <- c()
  for(i in 1:length(pos_beg_movie_gross)) {
    movie_gross <- c(movie_gross, substr(html_code, start = pos_beg_movie_gross[i] + 43, stop = pos_end_movie_gross[i] - 1))
    movie_gross <- gsub(",", "", movie_gross)
  }
  
  movie_data <- data.frame(movie_title, movie_gross)
}


df1 <- scrap_imbd_2017("https://www.imdb.com/search/title?year=2017,2017&title_type=feature&sort=boxoffice_gross_us,desc")
df2 <- scrap_imbd_2017("https://www.imdb.com/search/title?year=2017,2017&title_type=feature&sort=boxoffice_gross_us,desc&page=2&ref_=adv_nxt")
df3 <- scrap_imbd_2017("https://www.imdb.com/search/title?year=2017,2017&title_type=feature&sort=boxoffice_gross_us,desc&page=3&ref_=adv_nxt")
df4 <- scrap_imbd_2017("https://www.imdb.com/search/title?year=2017,2017&title_type=feature&sort=boxoffice_gross_us,desc&page=4&ref_=adv_nxt")
df5 <- scrap_imbd_2017("https://www.imdb.com/search/title?year=2017,2017&title_type=feature&sort=boxoffice_gross_us,desc&page=5&ref_=adv_nxt")
df6 <- scrap_imbd_2017("https://www.imdb.com/search/title?year=2017,2017&title_type=feature&sort=boxoffice_gross_us,desc&page=6&ref_=adv_nxt")
df7 <- scrap_imbd_2017("https://www.imdb.com/search/title?year=2017,2017&title_type=feature&sort=boxoffice_gross_us,desc&page=7&ref_=adv_nxt")
df8 <- scrap_imbd_2017("https://www.imdb.com/search/title?year=2017,2017&title_type=feature&sort=boxoffice_gross_us,desc&page=8&ref_=adv_nxt")

#Create on data frame from the 17 to represent the entire season
scrap_merge <- rbind(df1, df2, df3, df4, df5, df6, df7, df8,
                     deparse.level = 1)

#movie_titles <- scrap_merge[,-2]
#write.csv(movie_titles, "movie titles.csv")

#---------------------------------------------------------------------------------------------------
scrap_imbd_2017_1 <- function(url) {
  html_code <- getURL(url)
  html_code <- gsub(x = html_code, pattern = "[[:blank:]]+", replacement = " ")
  
  
  pos_beg_movie_title <- gregexpr(pattern = "> <img alt=\".+?\"", text = html_code)
  pos_beg_movie_title <- pos_beg_movie_title[[1]]
  pos_beg_movie_title <- pos_beg_movie_title[-1]
  
  pos_end_movie_title <- gregexpr(pattern = "class=\"loadlate\"", text = html_code)
  pos_end_movie_title <- pos_end_movie_title[[1]]
  
  movie_title <- c()
  for(i in 1:length(pos_beg_movie_title)) {
    movie_title <- c(movie_title, substr(html_code, start = pos_beg_movie_title[i] + 12, stop = pos_end_movie_title[i] - 3))
  }
  movie_title
  
  pos_beg_movie_gross <- gregexpr(pattern = "Worldwide Total: ", text = html_code)
  pos_beg_movie_gross <- pos_beg_movie_gross[[1]]
  
  pos_end_movie_gross <- gregexpr(pattern = "illion<br/>", text = html_code)
  pos_end_movie_gross <- pos_end_movie_gross[[1]]
  
  movie_gross <- c()
  movie_gross_m <- movie_gross
  for(i in 1:length(pos_beg_movie_gross)) {
    movie_gross_m <- c(movie_gross_m, substr(html_code, start = pos_beg_movie_gross[i] + 17, stop = pos_end_movie_gross[i] - 1))
    movie_gross_m <- gsub(",", "", movie_gross_m)
  }
  movie_gross_m
  movie_gross_m <- as.integer(as.character(gsub(" M", "", movie_gross_m))) * 1000000
  
  movie_gross <- c()
  movie_gross_b <- movie_gross
  for(i in 1:length(pos_beg_movie_gross)) {
    movie_gross_b <- c(movie_gross_b, substr(html_code, start = pos_beg_movie_gross[i] + 17, stop = pos_end_movie_gross[i] - 1))
    movie_gross_b <- gsub(",", "", movie_gross_b)
  }
  movie_gross_b
  movie_gross_b <- as.numeric(as.character(gsub(" B", "", movie_gross_b))) * 1000000000
  
  movie_gross <- pmin(movie_gross_m, movie_gross_b, na.rm = TRUE)
  
  movie_title <- as.data.frame(movie_title)
  no.r <- nrow(movie_title)
  l.v <- length(movie_gross)
  difer <- no.r-l.v
  movie_title <- cbind(movie_title,c(movie_gross,rep(NA,difer)))
  colnames(movie_title) <- c("movie_title", "movie_gross")
  
  movie_title <- movie_title[!(is.na(movie_title$movie_gross)), ]
}


df1 <- scrap_imbd_2017_1("https://www.imdb.com/list/ls066226082/")

#---------------------------------------------------------------------------------------------------
scrap_imbd_2017_2 <- function(url) {
  html_code <- getURL(url)
  html_code <- gsub(x = html_code, pattern = "[[:blank:]]+", replacement = " ")
  
  
  pos_beg_movie_title <- gregexpr(pattern = "> <img alt=\".+?\"", text = html_code)
  pos_beg_movie_title <- pos_beg_movie_title[[1]]
  pos_beg_movie_title <- pos_beg_movie_title[-1]
  
  pos_end_movie_title <- gregexpr(pattern = "class=\"loadlate\"", text = html_code)
  pos_end_movie_title <- pos_end_movie_title[[1]]
  
  movie_title <- c()
  for(i in 1:length(pos_beg_movie_title)) {
    movie_title <- c(movie_title, substr(html_code, start = pos_beg_movie_title[i] + 12, stop = pos_end_movie_title[i] - 3))
  }
  movie_title
  
  pos_beg_movie_gross <- gregexpr(pattern = "Total: ", text = html_code)
  pos_beg_movie_gross <- pos_beg_movie_gross[[1]]
  
  pos_end_movie_gross <- gregexpr(pattern = "[$]<", text = html_code)
  pos_end_movie_gross <- pos_end_movie_gross[[1]]
  
  movie_gross <- c()
  for(i in 1:length(pos_beg_movie_gross)) {
    movie_gross <- c(movie_gross, substr(html_code, start = pos_beg_movie_gross[i] + 7, stop = pos_end_movie_gross[i] - 1))
    movie_gross <- gsub("[.]", "", movie_gross)
  }
  movie_gross
  
  
  movie_title <- as.data.frame(movie_title)
  no.r <- nrow(movie_title)
  l.v <- length(movie_gross)
  difer <- no.r-l.v
  movie_title <- cbind(movie_title,c(movie_gross,rep(NA,difer)))
  colnames(movie_title) <- c("movie_title", "movie_gross")
  
  movie_title <- movie_title[!(is.na(movie_title$movie_gross)), ]
}

df2 <- scrap_imbd_2017_2("https://www.imdb.com/list/ls066145123/")


#---------------------------------------------------------------------------------------------------
scrap_imbd_2017_3 <- function(url) {
  html_code <- getURL(url)
  html_code <- gsub(x = html_code, pattern = "[[:blank:]]+", replacement = " ")
  
  
  pos_beg_movie_title <- gregexpr(pattern = "> <img alt=\".+?\"", text = html_code)
  pos_beg_movie_title <- pos_beg_movie_title[[1]]
  pos_beg_movie_title <- pos_beg_movie_title[-1]
  
  pos_end_movie_title <- gregexpr(pattern = "class=\"loadlate\"", text = html_code)
  pos_end_movie_title <- pos_end_movie_title[[1]]
  
  movie_title <- c()
  for(i in 1:length(pos_beg_movie_title)) {
    movie_title <- c(movie_title, substr(html_code, start = pos_beg_movie_title[i] + 12, stop = pos_end_movie_title[i] - 3))
  }
  movie_title
  
  pos_beg_movie_gross <- gregexpr(pattern = "Worldwide -", text = html_code)
  pos_beg_movie_gross <- pos_beg_movie_gross[[1]]
  
  pos_end_movie_gross <- gregexpr(pattern = "eekend -", text = html_code)
  pos_end_movie_gross <- pos_end_movie_gross[[1]]
  
  movie_gross <- c()
  movie_gross_m <- movie_gross
  for(i in 1:length(pos_beg_movie_gross)) {
    movie_gross_m <- c(movie_gross_m, substr(html_code, start = pos_beg_movie_gross[i] + 12, stop = pos_end_movie_gross[i] - 4))
    movie_gross_m <- gsub(",", "", movie_gross_m)
  }
  movie_gross_m
  movie_gross_m <- as.integer(as.character(gsub("m", "", movie_gross_m))) * 1000000
  
  movie_gross <- c()
  movie_gross_b <- movie_gross
  for(i in 1:length(pos_beg_movie_gross)) {
    movie_gross_b <- c(movie_gross_b, substr(html_code, start = pos_beg_movie_gross[i] + 12, stop = pos_end_movie_gross[i] - 4))
    movie_gross_b <- gsub(",", "", movie_gross_b)
  }
  movie_gross_b
  movie_gross_b <- as.integer(as.character(gsub("b", "", movie_gross_b))) * 1000000000
  
  movie_gross <- pmin(movie_gross_m, movie_gross_b, na.rm = TRUE)
  
  
  movie_title <- as.data.frame(movie_title)
  no.r <- nrow(movie_title)
  l.v <- length(movie_gross)
  difer <- no.r-l.v
  movie_title <- cbind(movie_title,c(movie_gross,rep(NA,difer)))
  colnames(movie_title) <- c("movie_title", "movie_gross")
  
  movie_title <- movie_title[!(is.na(movie_title$movie_gross)), ]
}

df3 <- scrap_imbd_2017_3("https://www.imdb.com/list/ls062184767/")


#---------------------------------------------------------------------------------------------------
scrap_imbd_2017_4 <- function(url) {
  html_code <- getURL(url)
  html_code <- gsub(x = html_code, pattern = "[[:blank:]]+", replacement = " ")
  
  
  pos_beg_movie_title <- gregexpr(pattern = "> <img alt=\".+?\"", text = html_code)
  pos_beg_movie_title <- pos_beg_movie_title[[1]]
  pos_beg_movie_title <- pos_beg_movie_title[-1]
  
  pos_end_movie_title <- gregexpr(pattern = "class=\"loadlate\"", text = html_code)
  pos_end_movie_title <- pos_end_movie_title[[1]]
  
  movie_title <- c()
  for(i in 1:length(pos_beg_movie_title)) {
    movie_title <- c(movie_title, substr(html_code, start = pos_beg_movie_title[i] + 12, stop = pos_end_movie_title[i] - 3))
  }
  movie_title
  
  pos_beg_movie_gross <- gregexpr(pattern = "Worldwide: ", text = html_code)
  pos_beg_movie_gross <- pos_beg_movie_gross[[1]]
  
  pos_end_movie_gross <- gregexpr(pattern = "</p></div> </div>", text = html_code)
  pos_end_movie_gross <- pos_end_movie_gross[[1]]
  
  movie_gross <- c()
  for(i in 1:length(pos_beg_movie_gross)) {
    movie_gross <- c(movie_gross, substr(html_code, start = pos_beg_movie_gross[i] + 12, stop = pos_end_movie_gross[i] - 1)) 
    movie_gross <- gsub(",", "", movie_gross)
  }
  movie_gross
  
  
  movie_title <- as.data.frame(movie_title)
  no.r <- nrow(movie_title)
  l.v <- length(movie_gross)
  difer <- no.r-l.v
  movie_title <- cbind(movie_title,c(movie_gross,rep(NA,difer)))
  colnames(movie_title) <- c("movie_title", "movie_gross")
  
  movie_title <- movie_title[!(is.na(movie_title$movie_gross)), ]
}

df4 <- scrap_imbd_2017_4("https://www.imdb.com/list/ls066024328/")


#---------------------------------------------------------------------------------------------------
scrap_imbd_2017_5 <- function(url) {
  html_code <- getURL(url)
  html_code <- gsub(x = html_code, pattern = "[[:blank:]]+", replacement = " ")
  
  
  pos_beg_movie_title <- gregexpr(pattern = "> <img alt=\".+?\"", text = html_code)
  pos_beg_movie_title <- pos_beg_movie_title[[1]]
  pos_beg_movie_title <- pos_beg_movie_title[-1]
  
  pos_end_movie_title <- gregexpr(pattern = "class=\"loadlate\"", text = html_code)
  pos_end_movie_title <- pos_end_movie_title[[1]]
  
  movie_title <- c()
  for(i in 1:length(pos_beg_movie_title)) {
    movie_title <- c(movie_title, substr(html_code, start = pos_beg_movie_title[i] + 12, stop = pos_end_movie_title[i] - 3))
  }
  movie_title
  
  pos_beg_movie_gross <- gregexpr(pattern = "Total: ", text = html_code)
  pos_beg_movie_gross <- pos_beg_movie_gross[[1]]
  
  pos_end_movie_gross <- gregexpr(pattern = "[$]<", text = html_code)
  pos_end_movie_gross <- pos_end_movie_gross[[1]]
  
  movie_gross <- c()
  for(i in 1:length(pos_beg_movie_gross)) {
    movie_gross <- c(movie_gross, substr(html_code, start = pos_beg_movie_gross[i] + 7, stop = pos_end_movie_gross[i] - 1)) 
    movie_gross <- gsub("[.]", "", movie_gross)
  }
  movie_gross
  
  
  movie_title <- as.data.frame(movie_title)
  no.r <- nrow(movie_title)
  l.v <- length(movie_gross)
  difer <- no.r-l.v
  movie_title <- cbind(movie_title,c(movie_gross,rep(NA,difer)))
  colnames(movie_title) <- c("movie_title", "movie_gross")
  
  movie_title <- movie_title[!(is.na(movie_title$movie_gross)), ]
}

df5 <- scrap_imbd_2017_5("https://www.imdb.com/list/ls066145123/")

#--------------------------------------------------------------------------------------------------
IMBD_movie_predictions <- rbind(df1, df2, df3, df4, df5)
IMBD_movie_predictions$movie_gross <- as.integer(as.character(IMBD_movie_predictions$movie_gross))
IMBD_movie_predictions <- aggregate(IMBD_movie_predictions[, 2], list(IMBD_movie_predictions$movie_title), mean)
colnames(IMBD_movie_predictions) <- c("movie_title", "pred_movie_gross")


#--------------------------------------------------------------------------------------------------
IMBD <- merge(x = scrap_merge, y = IMBD_movie_predictions, by = "movie_title", all.y = TRUE)
IMBD <- IMBD[complete.cases(IMBD), ]

imdb <- read.csv("movies.csv")


# Facebook data collection
install.packages("devtools", "httr")
library("devtools")
library("httr")

install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")

require(Rfacebook)

# load in data about movies
movies = read.csv("movie_titles.csv")

# get from https://developers.facebook.com/tools/explorer
token <- "EAACEdEose0cBAKXwh2uDbDMySyEzkTkEQ3ZAyxZC5mNftR42ZCjZBh8jLslDnjfZCF7MA18qhH1eB3DkZA3CQa85k4Tvh4ZBGZAXtJtPXkZBA36j8s5AXYEwn99q51ZCnrDUowPIp6PUEW6CteA7T6QSg9ZC8fHSzzOq9gKmOy5mEzsWdzFZCFbshikN79eKZCc4SAKMZD"

# ibm watson credentials
url      <- "https://gateway.watsonplatform.net/natural-language-understanding/api/v1/analyze"
username <- "48e79aed-0756-43d1-9c6a-11f7b9bf5e10"
password <- "tyTHOhIe2sHw"

# convert date into unix timestamp for until parameter in getpage
# as.numeric(as.POSIXct("2013-09-16 2:13:46 EST"))  UNTIL flag

movie_data <- data.frame(Title = character(),
                         avg_sentiment = double(),
                         tot_comments = double(),
                         tot_shares = double(),
                         tot_likes = double(),
                         avg_sadness = double(),
                         avg_joy = double(),
                         avg_fear = double(),
                         avg_disgust = double(),
                         avg_anger = double(),
                         bad_comments = double())

#movie_data <- read.csv("final_movie_data.csv")
nicknames = movies$Facebook.Handle
titles = movies$Movie.Title
release_year = movies$Year
release_month = movies$Month
release_day = movies$Day
# Finished: 1, 20 - 50 + 40 skipped #45, 31, 11, 35, 38 (no posts), possibly 2, 22, 24, 8
# Start: 2
# End: 50
# i = 31
for(i in 63:63) { # loop through each movie
  print(paste("getting page for ", titles[i], " (", i, ")", "...", sep = ""))
  
  # convert date into unix timestamp for until parameter in getpage
  page <- getPage(nicknames[i], token, n = 20, until = as.numeric(as.POSIXct(paste(release_year[i], "-", release_month[i], "-", release_day[i], " 00:00:00 EST", sep = ""))))
  posts <- list()
  tot_sentiment <- double()
  tot_sadness <- double()
  tot_joy <- double()
  tot_fear <- double()
  tot_disgust <- double()
  tot_anger <- double()
  tot_comments <- double()
  tot_shares <- double()
  tot_likes <- double()
  
  print(paste("getting posts for ", titles[i], "...", sep = ""))
  for(k in 1:nrow(page)) {  # get posts from page
    posts[[k]] <- x <- getPost(page[k,7], token, n = 10000, likes = TRUE, comments = TRUE)
  }
  
  sum = 0
  for(t in 1:length(posts)){
    sum = sum + posts[[t]]$post$comments_count
  }
  print(paste("Movie has", sum, "total comments"))
  if(sum < 15000) {
    print(paste("getting sentiment and emotion for", titles[i], "posts..."))
    # get sentiment and other metrics from posts
    for(k in 1:length(posts)) {  # loop through each post and find sentiment value
      
      print(paste("Analyzing post #", k, "... (total: ", posts[[k]]$post$comments_count, ")", sep = ""))
      comments = posts[[k]]$comments$message
      tot_likes = append(tot_likes, posts[[k]]$post$likes_count)
      tot_comments = append(tot_comments, posts[[k]]$post$comments_count)
      tot_shares = append(tot_shares, posts[[k]]$post$shares_count)
      bad_comments = 0
      if(length(comments) > 0) {
        for(j in 1:length(comments)) { # find sentiment of each comment
          
          response <- GET(url,
                          query = list(version="2017-02-27",
                                       text = comments[j],
                                       features = "sentiment,emotion"),
                          authenticate(username, password))
          if(response$status_code == 200) {
            response <- fromJSON(content(response, "text"))
            
            tot_sentiment = append(tot_sentiment, response$sentiment$document$score)
            tot_sadness = append(tot_sadness, response$emotion$document$emotion$sadness)
            tot_joy = append(tot_joy, response$emotion$document$emotion$joy)
            tot_fear = append(tot_fear, response$emotion$document$emotion$fear)
            tot_disgust = append(tot_disgust, response$emotion$document$emotion$disgust)
            tot_anger = append(tot_anger, response$emotion$document$emotion$anger)
          } else if(response$status_code == 400) {
            bad_comments = bad_comments + 1
          }
        }
      }
    }
    print(paste("aggregating for ", titles[i], "...", sep = ""))
    avg_sentiment = mean(tot_sentiment)
    tot_comments = sum(tot_comments)
    tot_shares = sum(tot_shares)
    tot_likes = sum(tot_likes)
    avg_sadness = mean(tot_sadness)
    avg_joy = mean(tot_joy)
    avg_fear = mean(tot_fear)
    avg_disgust = mean(tot_disgust)
    avg_anger = mean(tot_anger)
    tmp <- data.frame(Title = titles[i],
                      avg_sentiment,
                      tot_comments,
                      tot_shares,
                      tot_likes,
                      avg_sadness,
                      avg_joy,
                      avg_fear,
                      avg_disgust,
                      avg_anger,
                      bad_comments)
    movie_data <- rbind(movie_data, tmp)
    print("Saving...")
    write.csv(x = movie_data, file = "final_movie_data.csv", row.names = FALSE)
  }
}

View(movie_data)

# save data
write.csv(x = movie_data, file = "final_movie_data.csv", row.names = FALSE)


#combining reddit, imdb, and facebook data
colnames(IMBD) <- c("title","movie_gross","pred_movie_gross")
IMBD$title <- as.character(IMBD$title)
top50 <- read.csv("top50.csv")
top50$title <- as.character(top50$title)

movieData <- top50 %>% inner_join(IMBD,by="title")

facebook <- read.csv("final_movie_data.csv")
facebook$Title <- as.character(facebook$Title)
colnames(facebook)[1] <- "title" 

allMovies <- read.csv("allMovies.csv")
allMovies$title <- as.character(allMovies$title)

movieData <- allMovies %>% inner_join(IMBD,by="title")%>% inner_join(facebook,by="title")

movieData <- movieData %>% select(title,anger,fear,joy,sadness,post_score,upvote_prop,num_comments,avgScore,
                                  movie_gross,pred_movie_gross,avg_sentiment,avg_sadness,avg_anger,avg_fear,
                                  avg_disgust,avg_joy,tot_comments,tot_shares,tot_likes,bad_comments)

colnames(movieData)<- c("title","avg_anger_red","avg_fear_red","avg_joy_red","avg_sadness_red","post_score_red",
                             "upvote_prop_red","num_comments_red","avgSentiment_red","movie_gross","pred_movie_gross",
                             "avg_sentiment_FB","avg_sadness_FB","avg_anger_FB","avg_fear_FB",
                                  "avg_disgust_FB","avg_joy_FB","tot_comments_FB","tot_shares_FB","tot_likes_FB","bad_comments_FB")

write.csv(movieData,"movieData.csv")

#Models

##Multiple Regression
splitDataFrame <-function(df, pct){
  number_rows  <-nrow(df)
  number_rows_df1    <-round( pct*number_rows )
  rows_df_1  <-sample(1:number_rows,  number_rows_df1,replace= FALSE )
  df_1 <- df[rows_df_1, ]
  df_2 <- df[-rows_df_1, ]
  returned_dfs  <-list(df_1, df_2)
  return(returned_dfs)
  }

split_data <- splitDataFrame(movieData,.80) 
training <- split_data[[1]]
training$movie_gross <- as.integer(as.character(training$movie_gross))
validation <- split_data[[2]]
validation$movie_gross <- as.integer(as.character(validation$movie_gross))

regression <- lm(movie_gross~avg_anger_red+avg_fear_red+avg_joy_red+avg_sadness_red+post_score_red+
                 upvote_prop_red+num_comments_red+avgSentiment_red+avg_sentiment_FB+avg_sadness_FB+avg_anger_FB+
                avg_fear_FB+avg_disgust_FB+avg_joy_FB+tot_comments_FB+tot_shares_FB+tot_likes_FB+bad_comments_FB,
                 data = training)

regression2 <- lm(movie_gross~avg_anger_red+avg_fear_red+avg_sadness_red+post_score_red+
                    num_comments_red+avg_sentiment_FB+avg_anger_FB+
                    avg_disgust_FB+avg_joy_FB+tot_likes_FB+bad_comments_FB,
                  data = training)

summary(regression)
summary(regression2)

prediction <- predict(regression,validation)
prediction2 <- predict(regression2,validation)


rmse <- rmse(validation$movie_gross,prediction)
mae <- mae(validation$movie_gross,prediction)

rmse2 <- rmse(validation$movie_gross,prediction2)
mae_regression <- mae(validation$movie_gross,prediction2)

movieData$movie_gross <- as.numeric(as.character(movieData$movie_gross))
movieData$pred_movie_gross <- as.numeric(as.character(movieData$pred_movie_gross))

usersRmse <- rmse(validation$movie_gross,validation$pred_movie_gross)
userMAE <- mae(validation$movie_gross,validation$pred_movie_gross)


##Random Forest
install.packages ("randomForest")
library("randomForest")


forest <- randomForest (movie_gross~avg_anger_red+avg_fear_red+avg_joy_red+avg_sadness_red+post_score_red+
                          upvote_prop_red+num_comments_red+avgSentiment_red+avg_sentiment_FB+avg_sadness_FB+avg_anger_FB+
                          avg_fear_FB+avg_disgust_FB+avg_joy_FB+tot_comments_FB+tot_shares_FB+tot_likes_FB+bad_comments_FB,  
                        data = training, ntree =200,type="regression")

regression_forest <- predict (forest, validation, "response")

mae_forest <- mae(actual = validation$movie_gross, predicted = regression_forest)

accuracy <- cbind(mae_forest,mae_regression,userMAE)

