#------------------------------------
#     ESTRUCTURA DE LA PRÁCTICA
#------------------------------------
 
# 2. Definición de objetivos
# 
# ?. Preprocesado del dataset
#       A) Variables cualitativas: ratings, tags --> Almacenadas en formato json. Deshacer formato
#       B) Variable cuantitativas: film_date, published_date --> Transformar las fechas de formato unix a formato fecha 
# 
# 3. Análisis exploratorio inicial
#       A) Visualizacion de variables: 
#             histogramas y boxplots para variables numéricas
#             diagramas de barras y otros para variables categóricas
#             etc
#       B) Separacion de train y test
# 
# 4. Detección, tratamiento e imputación de datos faltantes
#       A) Imputar los valores de idioma = 0 
# 
# 5. Transformaciones de variables cuantitativas
#       A) Aplicar transformación a las variables que, según su distribución, lo necesiten
#       B) Ajustar los rangos de las variables numéricas (reescalado) para no contaminar el modelo
# 
# 6. Procesado de variables cualitativas
#       A) Convertir los rating en variable categórica --> general_opinion: good, neutral, bad, none
#       B) Categorizar la influencia de un speaker en función del nº de charlas: --> speaker_type: popular, not_popular, ¿más categor??as?
#       C) Convertir los nombres de eventos a: TED, TEDX, ..., others 
# 
# 7. Selección de variables
#       A) Estudiar correlaciones entre variables para ver "posibles sospechosos" de ser descartados en apartado 8
# 
# 8. Ajuste, interpretación y diagnosis del m. de regresión lineal múltiple
#       A) Iterar e iterar hasta encontrar el modelo más ótimo
#       B) Entender modelo y explicarlo


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
library(cowplot)
library (tm)
library(modeest)

#ted_main <- read.csv("C:/ted_data/ted_main.csv")
ted_main <- read.csv("/home/rober/Escritorio/Master_Data_Science/Asignaturas/Fundamento_ de_analisis_de_Datos/3-Modelos/Practica/ted_main.csv")
n_observations <- dim(ted_main)[1]     # number of observations
n_vars <- dim(ted_main)[2]
id <- seq(1,n_observations)
dat <- cbind(id,ted_main)



########################    2. Definición de objetivos ###########################################################################
# El objetivo proncipal del proyecto es diseñar un modelo de regresión lineal que estime las visualizaciones que tendrá un
# video subido a la plataforma TED Talks en función de aquellas variables que se hayan considerado útiles para este propósito.
# 
# Adicionalmente se han considerado algunos objetivos que podr??an desarrollarse como trabajos futuros:
#   -
#   -
#   -
#   -
#   -
#   
#   
# Resumen del conjunto de datos:
# Se trata de un dataset descargado de la plataforma Kaggle que aporta información a cerca de las charlas TED publicadas entre
# los años AAAA y BBBB. Algunas de las variables contenidas en este cojunto de datos son: 
#   - views: numero de visualizaciones del v??deo. Será la variable respuesta
#   - duration: duracion del video
#   - publish_date: fecha de publicación
#   - comments: numero de comentarios
#   - speaker: ponente
#   - event: evento TED donde se desarrolló la charla
#   - rating: votos para las distintas categor??as (p.e: funny=19600, ingenious=10000, unconvincing=300, etc)
#   - tags: temáticas del v??deo (p.e: culture, creativity, alternative energy, etc)
#   - otras



########################    Preprocesado del dataset #############################################################################

# A) Variables cualitativas:
#     ratings, tags --> Almacenadas en formato json. Deshacer formato

# B) Variable cuantitativas: 
#   film_date, published_date --> Transformar las fechas de formato unix a formato fecha 

#.................................................................................................................................


# A) Preprocesado variable "categórica": ratings 
# Se preprocesa la variable y se añade el output al dataframe de trabajo (dat)

df_rating <- NULL
for (i in 1:n_observations){
  rating <- dat[i,]$ratings
  rating <- gsub("'",'"',rating)
  result <- fromJSON(rating)
  result <- result %>% select("name", "count") %>% arrange(desc(name))
  
  df_result <- data.frame(i,t(result[2]),row.names = i)
  colnames(df_result) = c("id", t(result[1]))
  df_rating <- rbind(df_rating, df_result)
}

dat <- merge(x=dat,y=df_rating,by="id",all.x=TRUE)



# A) Preprocesar variable categórica: Tags
# Extraer Tags, contar los diferentes y su frecuencia. 

# REVISAR ESTE CÓDIGO: lanza warning!
# El warning se lanza porque los tags al no tener atributos n?mericos, los ratings ten?an un count, los convierte a factores cuyo nivel es el n?mero de palabras
# Lo que hace que cada l?nea tenga un nivel de factor diferente en funci?n del n?mero de tags.
# Cuando haces melt como cada linea tiene un nivel de factor diferente lanza el warning, diciendo que quita los atributos.
# Adem?s el warning es de la segunda columna que quitamos porque no da informaci?n. 
df_tags <- NULL
for (i in 1:n_observations){
  tags <- dat[i,]$tags
  tags <- gsub("r's","r",tags)
  tags <- gsub("'",'"',tags)
  result <- fromJSON(tags)
  df_result <- data.frame(i,t(result),row.names = NULL)
  df_result <- melt(df_result, id.vars='i')     # esta sentencia lanza un warning! "attributes are not identical across measure variables; they will be dropped "
  df_tags <- rbind(df_tags,df_result)         
}

df_tags <-  select(df_tags, -variable)
tags_count <- data.frame(table(unlist(strsplit(tolower(df_tags$value), ","))))



# B) Preprocesado variable cuantitativas: film_date y published_date
month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

num2dates <- function (dates_raw){
  date <- anydate(dates_raw)
  year <- as.numeric(substring(date, 1, 4))
  month <- as.numeric(substring(date, 6, 7))
  #month <- month_names[month]
  dates_out <- data.frame(date, year, month)
}

# convert published_date
converted_dates <- num2dates(dat$published_date)
dat$published_date <- converted_dates$date
dat$published_year <- converted_dates$year
dat$published_month <- converted_dates$month
# reoder dataframe
dat <- dat %>%  select(id:published_date, published_year, published_month, ratings:Beautiful)

# convert film_date
converted_dates <- num2dates(dat$film_date)
dat$film_date <- converted_dates$date
dat$film_year <- converted_dates$year
dat$film_month <- converted_dates$month
# reoder dataframe
dat <- dat %>%  select(id:film_date, film_year, film_month, languages:Beautiful)


# Reordenamos dataframe (aquí se descartan ya algunas variables: url, related_talks, ratings(x ya estar incluida, num_speaker,
# name, speaker_occupation, description))
dat <- dat %>% select(id, views, comments, duration, languages, event, film_date:film_month, published_date:published_month,
                      title, main_speaker, tags, Unconvincing:Beautiful)

# Ordenamos las observaciones por fecha y actualizamos los ID
dat <- dat %>% arrange(film_date)
dat$id <- seq(1, n_observations)



########################   3. Análisis exploratorio inicial  #####################################################################

# A) Visualizacion de variables: 
#     histogramas y boxplots para variables numéricas
#     diagramas de barras y otros para variables categóricas
#     etc

# B) Separacion de train y test

#.................................................................................................................................

vars <- colnames(dat)

# A) EDA

#Realizamos visualizacion de los histogramas de las variables numéricas. En caso de que sus distribuciones no sean
#normales, dichas variables se almacenarán para una transformacion posterior
#Adicionalmente se muestran los boxplots para verificar si hay o no posibles espurios
#Finalmente se almacenará los valores máximo y mínimo de cada variable para aplicar posteriormente un reescalado
#a las variables de modo que no se penalicen en el modelo, al tener rangos diferentes.

numeric_vars <- c(2, 3, 4, 5, 8, 9, 11, 12)

# Visualizacion variables numéricas (1/2) --> Histogramas
p1 <- ggplot(dat, aes(x=views)) + geom_histogram(fill="white", colour="black")
p2 <- ggplot(dat, aes(x=comments)) + geom_histogram(fill="white", colour="black")
p3 <- ggplot(dat, aes(x=duration)) + geom_histogram(fill="white", colour="black")
p4 <- ggplot(dat, aes(x=languages)) + geom_histogram(fill="white", colour="black")
plot_grid(p1, p2, p3, p4)

# Almacenamos las variables sobre las que se realizará una conversión posteriormente
numvars2convert = c("views", "comments", "duration")

# Visualizacion variables numéricas (1/2) --> Boxplots
b1 <- ggplot(dat, aes(x="views", y=views)) + geom_boxplot()
b2 <- ggplot(dat, aes(x="comments", y=comments)) + geom_boxplot()
b3 <- ggplot(dat, aes(x="duration", y=duration)) + geom_boxplot()
b4 <- ggplot(dat, aes(x="languages", y=languages)) + geom_boxplot()

plot_grid(b1, b2, b3, b4) 
# Como era de esperar, los boxplots requiren que las transformaciones de las variables se apliquen 
# para poder aportar información de interés. Tras aplicar las transofrmaciones, se volverán a revisar


# Adicionalmente se revisan los valores mínimos y máximos para verificar que no hay nulos o valores atípicos (a primera vista)
for (var_col in numeric_vars){
  cat(vars[var_col], " --> Min:", min(dat[,var_col]), ",  Max:", max(dat[,var_col]), "\n")
}
# Los valores de todas las variables numéricas están dentro de los valores "esperados" salvo en el caso de la variable languages
# Esta variable se tratará más adelante pues, estos valores, no son correctos


# visualizacion variables numericas (2/2) (fechas: años y meses de grabacion y publicacion)
labels_angle <- theme(axis.text.x = element_text(angle=45))
scale_month <- scale_x_continuous(breaks=seq(1,12), labels=month_names)

p5 <- ggplot(dat, aes(x=film_year)) + geom_histogram(fill="white", colour="black", bins=50)
p6 <- ggplot(dat, aes(x=film_month)) + geom_histogram(fill="white", colour="black", bins=length(month_names)) + scale_month + labels_angle
p7 <- ggplot(dat, aes(x=published_year)) + geom_histogram(fill="white", colour="black", bins=12) 
p8 <- ggplot(dat, aes(x=published_month)) + geom_histogram(fill="white", colour="black", bins=length(month_names)) + scale_month + labels_angle 

plot_grid(p5, p6, p7, p8, align="h")

# De la visualización anterior se extraen algunas conclusiones interesantes, útiles para poner en contexto el dataset:
#- Las charlas TED, a pesar de que comenzaron a publicarse a partir de 2005, se empezaron a impartir en 1970
#- El número de charlas publicadas aumentó desde 2005 hasta 2012
#- Las fechas de publicachión están repartidas a lo largo del año de una forma uniforme, con un tramo decreciente en los ultimos meses del año


# Sobre estas variables no se aplicará ninguna transformación, debida a la naturaleza de las mismas (contienen valores temporales). Además,
# estas variables se usarán para dividir el dataset en train y test; en concreto las variables published_year y published_month





# B) Separacion de train y test

set.seed(123)

train_index <- sample(1:nrow(dat_ord), 0.8 * nrow(dat_ord))
test_index <- setdiff(1:nrow(dat_ord), train_index)

dat_train <- dat_ord[train_index, -1]
dat_test <- dat_ord[test_index, -1]


#Separacion en train test con split temporal

dat_train_ini <- dat %>% filter(published_year < 2015)
dat_validation <-  dat %>% filter(published_year >= 2015)

dat_train <- dat_train_ini %>% filter(published_year < 2013)
dat_test <- dat_train_ini %>% filter(published_year >= 2013)



########################  4. Detección, tratamiento e imputación de datos faltantes  #############################################

# A) Imputar los valores de i"dioma"languages" = 0 que realmente corresponden a charlas en las que no se habla. Música o Danza. 

#.................................................................................................................................

# La variable languages indica el número de lenguajes al que se ha traducido una charla. Al tratarse de charlas, no parece lógico
# que este valor sea 0. 
# Se ha observado que el dataset contiene, además de charlas, actuaciones musicales o artísticas en las que no hay un ponente que de 
# una charla. Para aquellas muestras en las que sí se trata de una charla, y el valor de "languages" es 0, este valor se corregirá
# mediante imputación de valores. Se van a probar diferentes métodos de imputación, eligiendo el que mejores rersultados ofrezca.


# Histograma de los lenguajes de la variable original. Se usará para validar el método de imputacion
p_orig <- ggplot(dat, aes(x=languages)) + geom_histogram(fill="white", colour="black", bins=max(dat$languages) - min(dat$languages)) + scale_y_continuous(limits=c(0, 250)) + ggtitle("Languages: Original data")


# Valoramos el numero de ocurrencias de datos erróneos.
ids_not_ok <- (dat$languages == 0 & str_detect(dat$tags, "music|dance|live music", negate = TRUE))
n_not_ok <- length (ids_not_ok[ids_not_ok == TRUE])
frec_nok <- n_not_ok/n_observations
cat("Porcentaje de observaciones de la variable languages a imputar: ", 100*frec_nok, "%")
# Al tratarse de un 2.5% intentaremos inputarlos para evitar perder información que pudiera ser relevante


#------------------------
# Metodo 1 de imputación --> Moda
#------------------------

# Calculo de la moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
languages_moda = getmode(dat$languages)

# imputamos con la moda los 0 que corresponda
languages_corrected_1 <- dat %>% select(languages) %>% mutate(languages = ifelse(languages == 0 & str_detect(tags, "music|dance|live music", negate = TRUE), languages_moda, languages))

# visualizamos cómo ha cambiado la distribución al imputar los ceros con la moda de la variable
p_corrected_1 <- ggplot(languages_corrected_1, aes(x=languages)) + geom_histogram(fill="white", colour="black", bins=max(dat$languages) - min(dat$languages)) + scale_y_continuous(limits=c(0, 250)) + ggtitle("Imputation: Method 1")
plot_grid(p_orig, p_corrected_1)

# Se observa que imputar con la moda genera un máximo en la función de distribución que la ensucia. Se decide aplicar otra lógica.


#------------------------
# Método 2 de imputacion --> Distribucion normal 
#------------------------

# Miramos si la variable languages correla con alguna de las variables numéricas. Si así fuera, una posibilidad sería imputar
# utilizando el método knn, asociando los idiomas en función de su vecino/s mas cercanos.
library(corrplot)
lang_correlations <- cor(dat[,numeric_vars])
corrplot(lang_correlations)
cat("Correlaciones con languages: ", lang_correlations[, 4])

# Los valores de correlacion mayores son con views:0.38 (variable respuesta: no se puede usar para imputar) y comments:0.31. 
# Aplicar un método como knn para imputar, con una correlacion tan baja, no parece una solución aceptable

# Se decide imputar con muestras aleatorias correspondientes a una normal con media y desviacion estandar iguales a las de la
# distribución de la variable "languages" (excluyendo los valores erróneos)

# Obtenemos los valores a imputar
languages_col <- 5
lang_mean <- mean(dat[!ids_not_ok, languages_col])
lang_sd <- sd(dat[!ids_not_ok, languages_col])
vals <- rnorm(n_not_ok, mean=lang_mean, sd=lang_sd)

# "languages" es una variable que toma valores enteros. Por lo tanto, rendondeamos los valores de la distribucion para imputar 
# valores acorde a la naturaleza de la variable (valores enteros).
vals <- round(vals, 0)

# imputamos los valores
languages_corrected_2 <- dat %>% select(languages) %>% mutate(languages = ifelse(languages == 0 & str_detect(tags, "music|dance|live music", negate = TRUE), vals, languages))
p_corrected_2 <- ggplot(languages_corrected_2, aes(x=languages)) + geom_histogram(fill="white", colour="black", bins=max(dat$languages) - min(dat$languages)) + scale_y_continuous(limits=c(0, 250))  + ggtitle("Imputation: Method 2")
plot_grid(p_orig, p_corrected_1, p_corrected_2, ncol=3)

# Se observa mejoría a la hora de imputar la variable languages con el método 2. 
# Aceptamos esta transformación y actualizamos el dataframe.
dat$languages <- languages_corrected_2$languages



########################  5. Transformaciones de variables cuantitativas  ########################################################

# A) Aplicar transformación a las variables que, según su distribución, lo necesiten
# B) Ajustar los rangos de las variables numéricas (reescalado) para no contaminar el modelo

#.................................................................................................................................


# Las variables numéricas que, tras hacer el análisis exploratorio, podrían necesitar transformaciones son: "views", "comments" y "duration"

# Aplicamos logaritmos y observamos el resultado:
p1_mod <- ggplot(dat, aes(x=log10(views))) + geom_histogram(fill="white", colour="black")
p2_mod <- ggplot(dat, aes(x=log10(comments))) + geom_histogram(fill="white", colour="black")
p3_mod <- ggplot(dat, aes(x=log10(duration))) + geom_histogram(fill="white", colour="black")

plot_grid(p1, p1_mod, p2, p2_mod, p3, p3_mod, ncol=2)


# Obtenemos los boxplot de las variables modificadas
b1_mod <- ggplot(dat, aes(x="views", y=log10(views))) + geom_boxplot()
b2_mod <- ggplot(dat, aes(x="comments", y=log10(comments))) + geom_boxplot()
b3_mod <- ggplot(dat, aes(x="duration", y=log10(duration))) + geom_boxplot()

plot_grid(b1, b1_mod, b2, b2_mod, b3, b3_mod, ncol=2) 

# Se observa que tanto las distribuciones como los boxplots han mejorado notablemente. Se aceptan por lo tanto dichas transformaciones
# sobre las variables y se actualizan en el dataframe de trabajo

# actualizamos dataframe
dat_old <- dat
dat$views <- log10(dat_old$views)
dat$comments <- log10(dat_old$comments)
dat$duration <- log10(dat_old$duration)

# Pintamos las variables numéricas resultantes almacenadas en el dataframe
p1 <- ggplot(dat, aes(x=views)) + geom_histogram(fill="white", colour="black")
p2 <- ggplot(dat, aes(x=comments)) + geom_histogram(fill="white", colour="black")
p3 <- ggplot(dat, aes(x=duration)) + geom_histogram(fill="white", colour="black")
p4 <- ggplot(dat, aes(x=languages)) + geom_histogram(fill="white", colour="black", bins=max(dat$languages)-min(dat$languages))
p5 <- ggplot(dat, aes(x=film_year)) + geom_histogram(fill="white", colour="black", bins=50)
p6 <- ggplot(dat, aes(x=film_month)) + geom_histogram(fill="white", colour="black", bins=12) + scale_month + labels_angle
p7 <- ggplot(dat, aes(x=published_year)) + geom_histogram(fill="white", colour="black", bins=12) 
p8 <- ggplot(dat, aes(x=published_month)) + geom_histogram(fill="white", colour="black", bins=12) + scale_month + labels_angle 
plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, align="h", ncol=4)



########################  6. Procesado de variables cualitativas  ################################################################

# A) Convertir los rating en variable categórica --> general_opinion: good, neutral, bad, none
# B) Categorizar la influencia de un speaker en función del nº de charlas: --> speaker_type: popular, not_popular, ¿más categor??as?
# C) Convertir los nombres de eventos a: TED, TEDX, ..., others 

#.................................................................................................................................

# A) Convertir los rating en variable categórica --> general_opinion: good, neutral, bad, none

opin_fun = function(x) {
  opinion <- NULL
  positive <- x %>% select(`Jaw-dropping`, Inspiring, Funny, Fascinating, Beautiful) %>% sum
  neutral <- x %>% select(OK, Informative, Ingenious, Persuasive, Courageous) %>% sum
  negative <- x %>% select(Obnoxious, Unconvincing, Longwinded, Confusing) %>% sum
  
  if (positive > neutral & positive > negative){
    opinion = "positive"
  } else if (negative > neutral & negative > positive){
    opinion = "negative"
  } else {opinion = "neutral"}
  
  return(opinion)
}

general_opinion <- NULL
for (i in 1:dim(dat_train)[1]){
  general_opinion[i] <- opin_fun(dat_train[i,])
}

dat_train$general_opinion <- general_opinion

dat_train <- select(dat_train, -(Unconvincing:Beautiful))

dat_train$general_opinion <- factor(dat_train$general_opinion, ordered = TRUE, levels = c("negative", "neutral", "positive"))

# B) Categorizar la influencia de un speaker en función del nº de charlas: --> speaker_type: popular, not_popular, ¿más categor??as?

#Primero se hace un count de las veces que un speaker a hablado en una charla incluida en el dataset

count_speaker <- NULL

dat_train$main_speaker <- gsub("[[:punct:]]","",dat_train$main_speaker)
dat_train$main_speaker <- as.character(dat_train$main_speaker)
for (i in 1:dim(dat_train)[1]){
  cd <- str_count(dat_train$main_speaker, dat_train[i,"main_speaker"])
  cd <- sum(cd)
  count_speaker <- rbind(count_speaker,cd)
}
count_speaker <- as.data.frame(cbind(dat_train$main_speaker,count_speaker))
colnames(count_speaker) <- c("main_speaker","speaker_pop")

# Segundo se categorizan seg?n su popularidad
count_speaker$speaker_pop <- as.integer(count_speaker$speaker_pop)
c <- count_speaker$speaker_pop[count_speaker$speaker_pop>1]
not_popular <- median(c)
popular <- c[quantile(c, probs = 0.75)]

# Tercero se asignan valores a los ponentes segun su popularidad

pop_fun = function(x) {
  
  if (x < not_popular){
    x = "not_popular"
    
  } else if (x > popular){
    x = "very_popular"
    
  } else {x = "popular"}
}

count_speaker$speaker_pop <- (lapply(count_speaker$speaker_pop, FUN = pop_fun))

#Se incluye la nueva columna al dataset

count_speaker<- count_speaker[!duplicated(count_speaker$main_speaker), ]
dat_train <-left_join(dat_train, count_speaker,by="main_speaker")

dat_train$speaker_pop <- factor(dat_train$speaker_pop, ordered = TRUE, levels = c("not_popular", "popular", "very_popular"))

# C) Convertir los nombres de eventos a: TED, TEDX, ..., others 

# Se hacen los cambios necesarios en la tabla. Quitando fechas, puntuaciones y otros. 
dat_train$event <- gsub('[0-9]+', "", dat_train$event)
dat_train$event <- gsub("[[:punct:]]","", dat_train$event)
dat_train$event <- gsub('TEDx.*', "TEDx", dat_train$event)
dat_train$event <- gsub('TEDGlobal.*', "TEDGlobal", dat_train$event)
dat_train$event <- gsub('TED@.*', "TED@", dat_train$event)

#Se hace un count de los eventos para saber c?ales son los que m?s se reptien. 
event_count <- count(distinct(dat_train), event)

# Se transforma la variable incluyendo cuatro categorias. TED, TEDx, TEDGlobal y Other. 
dat_train <- dat_train %>% mutate(event = ifelse(str_detect(dat_train$event, "TED$|TEDx$|TEDGlobal$", negate = TRUE), "Other", event))
dat_train$event <- factor(dat_train$event)
levels(dat_train$event)

#Al converir la variable eventos, comprobamos que existen cinco categorias, en vez de las cuatro que querriamos. Por lo que para solucionarlo debemos cambiar los valores erroneos por TEDGlobal y luego volver a facotrizarlo. 


dat_train$event <- gsub('TED Senior Fellows at TEDGlobal', "TEDGlobal", dat_train$event)
dat_train$event <- factor(dat_train$event)
levels(dat_train$event)
########################  7. Selección de variables  #############################################################################

# A) Estudiar correlaciones entre variables para ver "posibles sospechosos" de ser descartados en apartado 8

#.................................................................................................................................




########################  8. Ajuste, interpretación y diagnosis del m. de regresión lineal múltiple ##############################

# A) Iterar e iterar hasta encontrar el modelo más ótimo
# B) Entender modelo y explicarlo

#.................................................................................................................................

# Regresion multiple con las variables seleciconadas en las anteriores partes del estudio
regresion <- lm(views~ languages + OK + Inspiring + Funny + Fascinating, dat_train)
summary(regresion)









#**************** Como nos dijo Rubén, esto sin preprocesado de las variables no ser??a correcto ********************************

## Descubrir si existe una relación entre el número de visitas y los diferentes ratings. Utilizando un proceso Hitters. 
#
#sub_rating <- dat_train %>% select(views, 20:32)
#regfit_full <- leaps::regsubsets(views~., sub_rating)
#for (metric in c("r2", "adjr2", "Cp", "bic")){
#  plot(regfit_full, scale=metric)
#}
#*******************************************************************************************************************************



# ***************** Esto podemos dejarlo y comentar que no se usa por blablabla o directamente quitarlo *************************

# Agrupacion de los tags por temas
# Wordcloud visual
wordcloud(words = tags_count$Var1, freq = tags_count$Freq,
          max.words = 416, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))


# Wordcloud seria. La distancia es una variable, las l??neas dan información sobre la relación fuerte o debil entre los tags. 
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
#*********************************************************************************************************************************
