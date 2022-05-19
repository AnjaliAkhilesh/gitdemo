#Import Library
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(DT)
library(ggthemes)
library(wordcloud)
library(tm)
library(SnowballC)
#India_Video$publish_time <- ydm(substr(India_Video$publish_time, start = 1, stop = 10))

#a.Read the YouTube stat from locations = CA, FR, GB, IN, US and prepare the data. 
CAvideo <- tail(read.csv("C:/Users/ANJALI BABU/Desktop/CAvideos.csv",encoding = "UTF-8"),20000)
INvideo <- tail(read.csv("C:/Users/ANJALI BABU/Desktop/INvideos.csv",encoding = "UTF-8"),20000)
GBvideo <- tail(read.csv("C:/Users/ANJALI BABU/Desktop/GBvideos.csv",encoding = "UTF-8"),20000)
FRvideo <- tail(read.csv("C:/Users/ANJALI BABU/Desktop/FRvideos.csv",encoding = "UTF-8"),20000)
USvideo <- tail(read.csv("C:/Users/ANJALI BABU/Desktop/USvideos.csv",encoding = "UTF-8"),20000)


INvideo$trending_date <- ydm(INvideo$trending_date)
INvideo$publish_time <- ydm(substr(INvideo$publish_time, 
                                   start = 0, stop = 9))
#tail(INvideo)

GBvideo$trending_date <- ydm(GBvideo$trending_date)
GBvideo$publish_time <- ydm(substr(GBvideo$publish_time, 
                                   start = 1, stop = 10))
#tail(GBvideo)

USvideo$trending_date <- ydm(USvideo$trending_date)
USvideo$publish_time <- ydm(substr(USvideo$publish_time, 
                                   start = 0, stop = 9)) 
#tail(USvideo)

CAvideo$trending_date <- ydm(CAvideo$trending_date)
CAvideo$publish_time <- ydm(substr(CAvideo$publish_time, 
                                   start = 0, stop = 9))
#tail(CAvideo)

FRvideo$trending_date <- ydm(FRvideo$trending_date)
FRvideo$publish_time <- ydm(substr(FRvideo$publish_time, 
                                   start = 0, stop = 9)) 
#tail(FRvideo)


youtube_videos <- rbind(USvideo,FRvideo,CAvideo,INvideo,GBvideo)
head(youtube_videos)

#b.Display the correlation plot between category_id,
#views, likes, dislikes, comment_count. Which two have stronger and weaker correlation

YouTube_df <- youtube_videos[, 8:11]
groups <- youtube_videos[,5]
head(YouTube_df)


pairs(YouTube_df, labels = colnames(YouTube_df),
      pch = 21,
      bg = rainbow(3)[groups],
      col = rainbow(3)[groups])
library(corrplot)

corrplot(cor(YouTube_df), method = 'number')
corrplot(cor(YouTube_df), method = 'color')
corrplot(cor(YouTube_df), method = 'pie')

#c.Display Top 10 most viewed videos of YouTube.

mostviewed <- head(youtube_videos %>%
                     group_by(video_id,title)%>%
                     dplyr::summarise(Total = sum(views)) %>%
                     arrange(desc(Total)),10)
datatable(mostviewed)


ggplot(mostviewed, aes(video_id, Total)) +
  geom_bar( stat = "identity", fill = "blue") + 
  ggtitle("Top 10 most viewed videos")

#d.Show Top 10 most liked videos on YouTube.
mostliked <- head(youtube_videos %>%
                    group_by(video_id,title)%>%
                    dplyr::summarise(Total = sum(likes)) %>%
                    arrange(desc(Total)),10)
datatable(mostliked)


ggplot(mostliked, aes(video_id, Total)) +
  geom_bar( stat = "identity", fill = "red") + 
  ggtitle("Top 10 most liked videos")


#e.Show Top 10 most disliked videos on YouTube

mostdisliked <- head(youtube_videos %>%
                       group_by(video_id,title)%>%
                       dplyr::summarise(Total = sum(dislikes)) %>%
                       arrange(desc(Total)),10)
datatable(mostdisliked)


ggplot(mostdisliked, aes(video_id, Total)) +
  geom_bar( stat = "identity", fill = "red") + 
  ggtitle("Top 10 most disliked videos")

#f.Show Top 10 most commented video of YouTube

mostcommented <- head(youtube_videos %>%
                        group_by(video_id,title)%>%
                        dplyr::summarise(Total = sum(comment_count)) %>%
                        arrange(desc(Total)),10)
datatable(mostcommented)


ggplot(mostcommented, aes(video_id, Total)) +
  geom_bar( stat = "identity", fill = "red") + 
  ggtitle("Top 10 most commented videos")

#g.Show Top 15 videos with maximum percentage (%) of Likes on basis of views on video. 
#Hint: round (100* max (likes, na.rm = T)/ max (views, na.rm = T), digits = 2)) 

mostliked2 <- head(youtube_videos %>%
                     group_by(video_id,title) %>%
                     dplyr::summarise(per_likes =round (100* max (likes, na.rm = T)/ max (views, na.rm = T))) %>%
                     arrange(desc(per_likes)),15)
datatable(mostliked2)

ggplot(mostliked2, aes(video_id, per_likes)) +
  geom_bar( stat = "identity", fill = "red") + 
  ggtitle("Top 10 most commented videos")

#h.Show Top 15 videos with maximum percentage (%) of Dislikes on basis of views on video.

mostdisliked2 <- head(youtube_videos %>%
                        group_by(video_id,title) %>%
                        dplyr::summarise(per_dislikes =round (100* max (dislikes, na.rm = T)/ max (views, na.rm = T))) %>%
                        arrange(desc(per_dislikes)),15)
datatable(mostdisliked2)

ggplot(mostdisliked2, aes(video_id, per_dislikes)) +
  geom_bar( stat = "identity", fill = "red") + 
  ggtitle("Top 10 most commented videos")

#i.Show Top 15 videos with maximum percentage (%) of Comments on basis of views on video.
mostcommented2 <- head(youtube_videos %>%
                         group_by(video_id,title) %>%
                         dplyr::summarise(per_comment =round (100* max (comment_count, na.rm = T)/ max (views, na.rm = T))) %>%
                         arrange(desc(per_comment)),15)
datatable(mostcommented2)

ggplot(mostcommented2, aes(video_id, per_comment)) +
  geom_bar( stat = "identity", fill = "red") + 
  ggtitle("Top 10 most commented videos")


#j.Top trending YouTube channels in all countries

trending_channel <- head(youtube_videos %>%
                           group_by(channel_title,video_id)%>%
                           dplyr::summarise(trending = sum(views)) %>%
                           arrange(desc(trending)),10)
datatable(trending_channel)


ggplot(trending_channel, aes(video_id,trending)) +
  geom_bar( stat = "identity", fill = "red") + 
  ggtitle("Trending  youtube channels in all countries")


#k.Top trending YouTube channels in India.

trending_INchannel <- head(INvideo %>%
                             group_by(channel_title,video_id)%>%
                             dplyr::summarise(trending = sum(views)) %>%
                             arrange(desc(trending)),10)
datatable(trending_INchannel)


ggplot(trending_INchannel, aes(video_id,trending)) +
  geom_bar( stat = "identity", fill = "red") + 
  ggtitle("Trending  youtube channels in India")


#l. Create a YouTube Title WordCloud.
library(wordcloud)

wordcloud(words = youtube_videos$title,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))


#m. Show Top Category ID

top_category <- head(youtube_videos %>%
                       #group_by(category_id)%>%
                       dplyr::count(category_id) %>%
                       arrange(desc(n)),1)
datatable(top_category)

#n. How much time passes between published and trending?
time_passes <- head(youtube_videos %>%
                      group_by(video_id)%>%
                      dplyr::summarise(Total = difftime(publish_time,
                                                        trending_date,units = "days")) %>%
                      arrange(Total),10)
head(time_passes)
datatable(top_category)

ggplot(time_passes, aes(x=video_id,y=Total)) +
  geom_bar( stat = "identity" , fill = "navyblue") + 
  ggtitle(" time passes between published and trending")

#o. Show the relationship plots between Views Vs. Likes on Youtube.

ggplot(youtube_videos, aes(x=views , y=likes)) +
  geom_point()
#p. Top Countries In total number of Views in absolute numbers
countries <- c("IN","FR","GB","US","CA")
views_in <- sum(INvideo$views)
views_fr <- sum(FRvideo$views)
views_gb <- sum(GBvideo$views)
views_us <- sum(USvideo$views)
views_ca <- sum(CAvideo$views)
sum_views <- rbind(views_in,views_fr,views_gb,views_us,views_ca)
views_df <- data.frame(countries,sum_views)
views_df    
top_views <- head(views_df %>%
                    arrange(desc(sum_views)))
datatable(top_views)

ggplot(top_views, aes(x=countries,y=sum_views,fill=countries)) +
  geom_bar( stat = "identity") + 
  ggtitle(" Countries In total number of Views in absolute numbers")

#q. Top Countries In total number of Likes in absolute numbers
likes_in <- sum(INvideo$likes)
likes_fr <- sum(FRvideo$likes)
likes_gb <- sum(GBvideo$likes)
likes_us <- sum(USvideo$likes)
likes_ca <- sum(CAvideo$likes)
sum_likes <- rbind(likes_in,likes_fr,likes_gb,likes_us,likes_ca)
likes_df <- data.frame(countries,sum_likes)
likes_df    
top_likes <- head(likes_df %>%
                    arrange(desc(sum_likes)))
datatable(top_likes)
ggplot(top_likes, aes(x=countries,y=sum_likes,fill=countries)) +
  geom_bar( stat = "identity") + 
  ggtitle(" Countries In total number of likes in absolute numbers")
#r. Top Countries In total number of Dislikes in absolute numbers
dislikes_in <- sum(INvideo$dislikes)
dislikes_fr <- sum(FRvideo$dislikes)
dislikes_gb <- sum(GBvideo$dislikes)
dislikes_us <- sum(USvideo$dislikes)
dislikes_ca <- sum(CAvideo$dislikes)
sum_dislikes <- rbind(dislikes_in,dislikes_fr,dislikes_gb,dislikes_us,dislikes_ca)
dislikes_df <- data.frame(countries,sum_dislikes)
dislikes_df    
top_dislikes <- head(dislikes_df %>%
                       arrange(desc(sum_dislikes)))
datatable(top_dislikes)

ggplot(top_dislikes, aes(x=countries,y=sum_dislikes,fill=countries)) +
  geom_bar( stat = "identity") + 
  ggtitle(" Countries In total number of dislikes in absolute numbers")
#s. Top Countries In total number of Comments in absolute numbers
cmm_in <- sum(INvideo$comment_count)
cmm_fr <- sum(FRvideo$comment_count)
cmm_gb <- sum(GBvideo$comment_count)
cmm_us <- sum(USvideo$comment_count)
cmm_ca <- sum(CAvideo$comment_count)
sum_comments <- rbind(cmm_in,cmm_fr,cmm_gb,cmm_us,cmm_ca)
cmm_df <- data.frame(countries,sum_comments)
cmm_df    
top_cmm <- head(cmm_df %>%
                  arrange(desc(sum_comments)))
datatable(top_cmm)

ggplot(top_cmm, aes(x=countries,y=sum_comments,fill=countries)) +
  geom_bar( stat = "identity") + 
  ggtitle(" Countries In total number of comments in absolute numbers")
#t. Title length words Frequency Distribution 

corpus = Corpus(VectorSource(list(sample(youtube_videos $title, size = 3000))))

corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, stopwords('english'))

dtm_us = TermDocumentMatrix(corpus)
matrix <- as.matrix(dtm_us)

words <- sort(rowSums(matrix), decreasing = TRUE)
df <- data.frame(word = names(words), freq = words)

head(df)

wordcloud(words = df$word, freq = df$freq, min.freq = 5, 
          random.order = FALSE, colors = brewer.pal(6, "Dark2"))


