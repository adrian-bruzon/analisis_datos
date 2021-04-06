library(readr)
library(dplyr)
library(jsonlite)
library(anytime)
library(stringr)
library(ggplot2)
library(GGally)
library(igraph)
library(ggraph)
library(leaps)
library(reshape2)
library(widyr)
library(wordcloud)
library(RColorBrewer)
library (tm)

ted_main <- read_csv("C:/ted_data/ted_main.csv")

id <- seq(1,2550)

dat <- cbind(id,ted_main)

# Transformar las fechas de formato unix a formato fecha. 

date_fm<- dat %>% select(film_date) %>% mutate(anydate(film_date))

dat$film_date <- date_fm$`anydate(film_date)`

date_pub <- dat %>% select(published_date) %>% mutate(anydate(published_date))

dat$published_date <- date_pub$`anydate(published_date)`

# Imputar los valores de idioma = 0 que realmente corresponden a charlas en las que no se habla. Música o Danza. 

dat <- dat %>% mutate(languages = ifelse(languages == 0 & str_detect(tags, "music|dance|live music", negate = TRUE), 1, languages))

# Crear una variable con el número de veces que un speaker ha hablado en una charla Ted incluida en la BD

count_speaker <- NULL
i <- 1
for (i in 1:2550){
  cd <- str_count(dat$main_speaker, dat [i,8])
  cd <- sum(cd)
  count_speaker <- rbind(count_speaker,cd)
}
count_speaker <- as.data.frame(cbind(id,count_speaker))
colnames(count_speaker) <- c("id","count_speaker")
dat<-merge(x=dat,y=count_speaker,by="id",all.x=TRUE)


#Crear variables en función de los ratings  
  
df_rating <- NULL
i <- 1
for (i in 1:2550){
  
  rating <- dat[i,]$ratings
  
  rating <- gsub("'",'"',rating)
  
  result <- fromJSON(rating)
  
  result <- result %>% select("name", "count") %>% arrange(desc(name))
  
  df_result <- data.frame(i,t(result[-1]),row.names = NULL)
  col <- c("id",result[, 1])
  colnames(df_result) <- col
  
  df_rating <- rbind(df_rating,df_result)

}

dat<-merge(x=dat,y=df_rating,by="id",all.x=TRUE)


#Contar los tags diferentes y su frecuencia. 

df_tags <- NULL
i <- 1
for (i in 1:2550){
  
  tags <- dat[i,]$tags
  
  tags <- gsub("r's","r",tags)
  
  tags <- gsub("'",'"',tags)
  
  result <- fromJSON(tags)
  
  df_result <- data.frame(i,t(result),row.names = NULL)
  
  df_result <- melt(df_result, id.vars='i')
  
  df_tags <- rbind(df_tags,df_result)
  
}

df_tags <-  select(df_tags, -variable)
tags_count <- data.frame(table(unlist(strsplit(tolower(df_tags$value), ","))))


#Creación del test y el train para trabajar sobre el dataset

set.seed(123)

train_index <- sample(1:nrow(dat), 0.8 * nrow(dat))
test_index <- setdiff(1:nrow(dat), train_index)

dat_train <- dat[train_index, -1]
dat_test <- dat[test_index, -1]

# Descubrir si existe una relación entre el número de visitas y los diferentes ratings. Utilizando un proceso Hitters. 

sub_rating <- dat_train %>% select(views, 20:32)

regfit_full <- leaps::regsubsets(views~., sub_rating)

for (metric in c("r2", "adjr2", "Cp", "bic")){
  
  plot(regfit_full, scale=metric)
  }



# Regresion multiple con las variables seleciconadas en las anteriores partes del estudio

regresion <- lm(views~ languages + OK + Inspiring + Funny + Fascinating, dat_train)


summary(regresion)



# Agrupacion de las palabras topics por temas

# Wordcloud visual
wordcloud(words = tags_count$Var1, freq = tags_count$Freq,
          max.words = 416, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))


# Wordcloud seria. La distancia es una variable, las líneas dan información sobre la relación fuerte o debil entre los tags. 
topic_word_pairs <- df_tags %>% 
  pairwise_count(value, i, sort = TRUE, upper = FALSE)

topic_word_pairs

set.seed(1234)
topic_word_pairs %>%
  filter(n >= 50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# Correlación entre las palabras incluidas en los tags. 
topic_cors <- df_tags %>% 
  group_by(value) %>%
  filter(n() >= 10) %>%
  pairwise_cor(value, i, sort = TRUE, upper = FALSE)

topic_cors


set.seed(1234)
  topic_cors %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()


 




  
  


