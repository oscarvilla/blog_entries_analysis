library(here)
library(tidyverse)
library(lubridate)

# Importamos el set de datos que ya contiene los sentimientos de cada texto calculados con NLTK
df <- read_csv(here::here("data", "blogtext_sent.csv"))

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

# Chequeamos que no haya errores en los factores (categorías erroneas)
# Géneros
levels(df$gender)
# Tópicos
levels(df$topic)
# Signos del zodiaco
levels(df$sign)
# qué tantos id's o bloggers únicos tenemos
length(levels(df$id))
# Qué tanto escribe cada uno de ellos
df %>% 
  group_by(id) %>% 
  count(id) %>% 
  ggplot(aes(n)) +
  geom_density() +
  ggtitle("distribución de blogs por escritor", 
          subtitle = "qué tantos escriben un cierto número de entradas") +
  xlab("número de entradas de blog") + 
  ylab("frecuencia")

# lo más probable es que un autor hay escrito menos de 50 entradas

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

# Vamos a revisar por edades.
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
df <- df %>% 
  separate(date, c("day", "month", "year"), sep = ",") %>% 
  mutate(month = month %>% str_to_lower() %>% factor())

summary(df$month)

# Tenemos que traducir los meses a inglés: todos.

df <- df %>% 
  mutate(month = case_when(
    month %in% c("januar", "janvier", "ianuarie", "jaanuar", "enero", "janeiro") ~ "january",
    month %in% c("februar", "febrero", "februarie", "fevereiro") ~ "february",
    month %in% c("mars", "marzo", "maart") ~ "march", 
    month %in% c("abril", "avril", "aprill") ~ "april",
    month %in% c("mai", "mayo", "mei", "maj", "maio", "toukokuu") ~ "may", 
    month %in% c("juin", "junho", "junio", "juni", "giugno", "juuni", "czerwiec", "lipanj") ~ "june", 
    month %in% c("juillet", "julho", "juli", "luglio", "iulie", "juuli", "lipiec", "julio") ~ "july", 
    month %in% c("agosto", "augusti", "augustus", "avgust", "kolovoz", "elokuu") ~ "august",
    month %in% c("septembre", "septembrie", "setembro", "septiembre") ~ "september",
    month %in% c("octubre", "octobre", "ottobre", "outubro") ~ "october", 
    month %in% c("novembre", "noiembrie", "novembro", "noviembre") ~ "november", 
    month %in% c("dezember", "diciembre", "dezembro", "desember") ~ "december", 
    TRUE ~ as.character(month)
  ) %>% factor()) 

# Verificamos que queden todos los meses en inglés
unique(df$month)

df <- df %>% 
  mutate(day = day %>% as.character(), 
         year = year %>% as.character()) %>% 
  unite("date", c("day", "month", "year"), sep = "-") %>% 
  mutate(date = date %>% lubridate::dmy())

# Hay un warning de fechas con fallos
df %>% 
  filter(is.na(date))

# Para los análisis de series de tiempo debemos prescindir de ellos o imputar las fechas.
# Voy a prescindir por lo poco que aporta a la muestra (24 / 681284)
df_clean <- df %>% 
  filter(!is.na(date))

# Revisamos la serie de tiempo más básica
df_clean %>% 
  group_by(date) %>% 
  summarise(sent = mean(compound))

# De inmediato es notorio que faltan días. Vamos a completarlos
df_clean %>% 
  mutate(date = as.Date(date)) %>% 
  complete(date = seq.Date(min(df_clean$date), max(df_clean$date), by = "day")) %>% 
  group_by(date) %>% 
  summarise(sentiment = mean(compound)) %>% 
  fill(sentiment) %>% 
  ggplot(aes(date, sentiment)) +
  geom_line()

# Definitivamente después de 2005 los registros están muy poco frecuentes.
# Dado que la tendencia era de crecimiento; puede tratarse de un problema.
df_clean %>% 
  group_by(year(date), month(date)) %>% 
  unite("periodo", c(`year(date)`, `month(date)`)) %>% 
  group_by(periodo) %>% 
  count() %>% 
  ggplot(aes(periodo, n)) +
  geom_bar(stat = "identity") +
  ggtitle("Blogs por mes") +
  coord_flip()

# Lo mejor es mantener la muestra hasta 2003-12, para cerrar por año hasta donde la muestra sea completa
df_sub_2004 <- df_clean %>% 
  filter(year(date) < 2004)

# Pasamos de 681260 a 681231: perdimos 29 registros por un año.
df_sub_2004 %>% 
  group_by(date) %>% 
  summarise(sent = mean(compound)) %>% 
  ggplot(aes(date, sent, colour = sent)) +
  geom_point() +
  geom_line()

# Si bien al principio puede pensarse que hay una estabilización o neutralización debida al tiempo,
# de lo que realmente se trata es de la regresión a la media y teorema del límite central, dado que
# estoy obteniendo el promedio de los sentimientos para cada día: mientras mayor sea el número de 
# entradas de blog, más tenderá el promedio hacia un punto y habrá menos varianza.

# Vamos a tomarlo por grupos
df_sub_2004 %>% 
  ggplot(aes(date, compound, colour = gender)) + 
  geom_point()

df_sub_2004 %>% 
  ggplot(aes(date, compound, colour = sign)) + 
  geom_point()

df_sub_2004 %>% 
  ggplot(aes(date, compound, colour = topic)) + 
  geom_point()

# Time series
df_sub_2004 %>%
  group_by(gender, date) %>% 
  summarise(sent = mean(compound)) %>% 
  ggplot(aes(date, sent)) + 
  geom_line() +
  facet_grid(~gender) +
  ggtitle("Serie de tiempo por género") + 
  xlab("año") + 
  ylab("polaridad")
