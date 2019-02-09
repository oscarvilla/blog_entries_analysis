library(here)
library(tidyverse)

# Importamos el set de datos que ya contiene los sentimientos de cada texto calculados con NLTK
df <- read_csv("~/Documents/NLP-Python/blog_entries_analysis/data/blogtext_sent.csv")

# Damos un vistazo a las variables
glimpse(df)

# Al set de datos
df

# Algunas variables necesitan ser tratadas para ser convertidas a factores. Tal es el caso de:
# - gender
# - topic
# - sign
# - factor

df <- df %>% 
  mutate(gender = gender %>% factor(), 
         topic = topic %>% factor(), 
         sign = sign %>% factor(), 
         id = id %>% factor())

# Chequeamos que no hay errores en los factores (categorías erroneas)
levels(df$gender)
levels(df$topic)
levels(df$sign)
levels(df$id)

# Revisemos la distribución de los sentimientos
# por género
df %>% 
  group_by(id) %>% 
  ggplot(aes(compound, fill = gender)) +
  geom_density(alpha = 0.4) + 
  ggtitle("Distribución de Sentimientos por género") + 
  xlab("polaridad (- / +)")

# vemos que no hay diferencias marcadas; salvo que las mujeres tienden a manifestar más emociones 
# positivas en sus blogs y los hombres más neutras. Mientras que manifiestan emociones negativas por 
# igual

# por signo del zodiaco
df %>%
  group_by(id, sign) %>% 
  ggplot(aes(compound, colour = sign)) +
  geom_density() +
  ggtitle("Distribución de sentimientos por signo del zodiaco") +
  xlab("polaridad (- / +)")

# No se encuentran tendencias notorias. Una ligera tendencia de los cancer a ser los más neutrales

# Vamos a revisar por edades. Pero antes debemos discretizar la variable: de edades como valores 
# contínuos pasamos a 5 intervalos
# Veamos cómo están distribuidas las edades
df %>% 
  group_by(id) %>% 
  ggplot(aes(age, fill = ..count..)) +
  geom_histogram(bins = 50) +
  ggtitle("Disribución de edades") + 
  xlab("edad") +
  ylab("cuántos")

# Dada esta distribución, vamos a repartir de manera balanceada los grupos de edad en 4 grupos
# Para proceder con su análisis, necesitamos que la altura esté dada por la frecuencia y no por el
# conteo, para que no se confunda tendencia con número. Ya que tenemos claro que el grupo etáreo
# que más entradas de blog escribe
df %>% 
  mutate(age = cut_interval(age, n = 4)) %>% 
  group_by(id) %>% 
  ggplot(aes(compound, y = ..density..)) + 
  geom_histogram(aes(fill = age), alpha = 0.8, position = "dodge") +
  ggtitle("Distribución de sentimiento por grupo etáreo", subtitle = "por frecuencia") +
  xlab("polaridad (- / +)")

# En general, la mayor parte de las entradas de blog expresan emociones positivas.
# Los bloggers de los 13 a los 21 tienden a expresar emociones extremas: más positivas y negativas
# y menos neutras con respecto al resto. 
# Por su parte, los bloggers entre los 30 y los 39 años son los más neutrales.
# Mientras que los de 21 a 30 tienden a ser tan positivos como los más jóvenes, pero son más neutrales
# y menos negativos: son como los más jóvenes, pero menos negativos y más neutros.
# Por último, los mayores (de 39 a 48 años de edad)
df %>% 
  mutate(age = cut_interval(age, n = 4)) %>% 
  group_by(id) %>% 
  ggplot(aes(age, compound, colour = age)) +
  geom_violin() +
  ggtitle("Comportamiento de sentimientos por edad", subtitle = "agrupado por edades") + 
  xlab("grupo etáreo") + 
  ylab("sentimiento")

# Para poder llevar a cabo el análisis a través de tiempo, necesitamos mejorar las fechas
# Como tenemos el problema de meses escritos en otros idiomas, vamos a tener que mapearlos antes; 
# reemplazando a cada mes en otro idioma por su escritura en inglés, y llevando todo a minúscula.
df_split <- df %>% 
  separate(date, c("day", "month", "year"), sep = ",") %>% 
  mutate(month = month %>% factor())

summary(df_split$month)

# Como no cuento con el tiempo para esto, voy a sacrificar el análisis de las series de tiempo
# y me voy a quedar tan sólo con el análisis de tendencias anuales.
# Retiramos aquellas entradas de blog sin fecha y el año 2006, por tener una muestra muy pequeña
# (quizá incompleta).

# Tenemos una dificultad con el número de blogs por año
df %>% 
  separate(date, c("day", "month", "year"), sep = ",") %>% 
  filter(year != "", year != 2006) %>% 
  group_by(year) %>%
  count(year) %>% 
  ggplot(aes(year, n, fill = n)) +
  geom_bar(stat = "identity") + 
  ggtitle("Blogs por año", subtitle = "removidos sin fecha y 2006") + 
  xlab("años") +
  ylab("cuenta")

# Esto a lo mejor confirma lo planteado por la ley de Moore. En todo caso, cada vez hay más entradas 
# de blog

# A pesar de ello, revisemos cómo se varían los sentimientos expresados en los blogs
df %>% 
  separate(date, c("day", "month", "year"), sep = ",") %>% 
  filter(year != "", year != 2006) %>% 
  group_by(year) %>%
  mutate(n = n()) %>% 
  ggplot(aes(year, compound, colour = n)) + 
  geom_violin()

# 