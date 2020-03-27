#### Práctica ####

remove(list = ls(all.names = T))
wd = "D:\\OneDrive\\Documentos\\Profesional\\CURSOS\\El horror cósmico - Beyond the walls of NLP\\"
setwd(wd)
load("NecronomiconDF.RData")

#### Librerías ####
library(tidyverse)
library(tidytext)
library(RColorBrewer)
library(kableExtra)
library(formattable)
library(readxl)
library(stopwords)
library(scales)
library(udpipe)
library(tm)
library(gridExtra)
library(wordcloud2)
library(topicmodels)
library(ggrepel)

#### Funciones ####

tema_principal = theme(legend.position = "none"
                       , text = element_text(size = 20, family = "serif")
                       , axis.text.x = element_text(angle = 90, vjust = -0.25)
                       , plot.title = element_text(face = "bold.italic"))

#### Variables extras ####

NecronomiconDF$ExtensionHistoria = NecronomiconDF$PaginaFinal-
  NecronomiconDF$PaginaInicial+1

NecronomiconDF %>% 
  mutate(DecadaCreacion = 
           ifelse(AnoCreacion %in% 1890:1899, "1890s",
           ifelse(AnoCreacion %in% 1900:1909, "1900s",
           ifelse(AnoCreacion %in% 1910:1919, "1910s",
           ifelse(AnoCreacion %in% 1920:1929, "1920s",
           ifelse(AnoCreacion %in% 1930:1939, "1930s", "N/E")))))) -> NecronomiconDF

#### Análisis preliminar ####

str(NecronomiconDF, nchar.max = 50)

Colores = colorRampPalette(brewer.pal(8,"Dark2"))(n_distinct(NecronomiconDF$AnoCreacion))
NecronomiconDF %>% 
  group_by(AnoCreacion) %>% 
  summarise(N = n_distinct(Titulo)) %>% 
  ggplot()+
  geom_col(aes(x = as.factor(AnoCreacion), y = N, fill = as.factor(AnoCreacion)))+
  geom_text(aes(x = as.factor(AnoCreacion), y = N, label = N), vjust = -0.5, family = "serif")+
  labs(x = "Año de creación", y = "Número de historias")+
  ggtitle("Cronología - Trabajos en solitario")+
  scale_fill_manual(values = Colores)+
  tema_principal

Colores = brewer.pal(5,"Dark2")
NecronomiconDF %>% 
  group_by(DecadaCreacion) %>% 
  summarise(N = n_distinct(Titulo)) %>% 
  ggplot()+
  geom_col(aes(x = as.factor(DecadaCreacion), y = N, fill = as.factor(DecadaCreacion)))+
  geom_text(aes(x = as.factor(DecadaCreacion), y = N, label = N), vjust = -0.5, family = "serif")+
  labs(x = "Año de creación", y = "Número de historias")+
  ggtitle("Cronología - Trabajos en solitario")+
  scale_fill_manual(values = Colores)+
  tema_principal

NecronomiconDF %>% 
  group_by(DecadaCreacion,Tipo) %>% 
  summarise(N = n_distinct(Titulo)) %>% 
  ggplot()+
  geom_col(aes(x = DecadaCreacion, y = N, fill = Tipo))+
  labs(x = "Año de creación", y = "Número de historias")+
  ggtitle("Cronología - Trabajos en solitario")+
  scale_fill_manual(values = Colores)+
  tema_principal+
  theme(legend.position = "right")

NecronomiconDF %>%
  filter(Tipo == "Relato") %>% 
  select("Año de creación" = AnoCreacion
         ,"Título en inglés" = TituloIngles
         ,"Título en español" = Titulo) %>%
  arrange(`Año de creación`) %>%
  mutate(`Año de creación` = color_tile("orange", "firebrick1")(`Año de creación`)) %>%
  kable("html", escape = F, align = "c"
        , caption = "Narrativa Completa en Solitario de HPL"
        , table.attr = "style = \"color: black;\"") %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered"), 
                full_width = T)

NecronomiconDF %>% 
  filter(Tipo == "Relato") %>% 
  select("Autor", "AnoCreacion", "Titulo", "TituloIngles", "Tipo", "PaginaInicial", 
         "PaginaFinal", "Historia", "DecadaCreacion","ExtensionHistoria") -> NecronomiconDF

#### Tokenización ####

tidy_necronomicon <- NecronomiconDF %>%
  unnest_tokens(output = Palabra,input = Historia)

tidy_necronomicon_ngram3 <- NecronomiconDF %>%
  unnest_tokens(output = Palabra,input = Historia, token = "ngrams", n = 3)

# La función se encarga de descartar puntuación y caracteres especiales, todo en minúsculas
str(NecronomiconDF$Historia[NecronomiconDF$Titulo=="La Botellita De Cristal"], nchar.max = 132)
head(tidy_necronomicon$Palabra, n = 20)

#### Extensión de las historias ####

# En este conteo se incluyen palabras repetidas
tidy_necronomicon %>% 
  group_by(AnoCreacion, Titulo) %>% 
  summarise(NumeroPalabras=n()) %>% 
  arrange(desc(NumeroPalabras)) -> extension_historias

extension_historias %>%
  ungroup(NumeroPalabras, Titulo, AnoCreacion) %>%
  mutate(NumeroPalabras = color_bar("cyan")(NumeroPalabras)) %>%
  kable("html", escape = F, align = "c", caption = "Historias según su conteo de palabras"
        , table.attr = "style = \"color: black;\"") %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered"), 
                full_width = T)

#### Remover stopwords ####

stopwords_es_1 = read_excel("CustomStopWords.xlsx")
stopwords_es_2 = tibble(Palabra=tm::stopwords(kind = "es"), Fuente="tm")
stopwords_es_3 = tibble(Palabra=stopwords::stopwords(language = "es", source = "stopwords-iso")
                        , Fuente="stopwords-iso")
stopwords_es_4 = tibble(Palabra=stopwords::stopwords(language = "es", source = "snowball")
                        , Fuente="snowball")
stopwords_es = rbind(stopwords_es_1, stopwords_es_2, stopwords_es_3, stopwords_es_4)
stopwords_es = stopwords_es[!duplicated(stopwords_es$Palabra),]
remove(stopwords_es_1, stopwords_es_2, stopwords_es_3, stopwords_es_4)

tidy_necronomicon$Palabra[grepl("^?l",tidy_necronomicon$Palabra)]
tidy_necronomicon$Palabra[grepl("^aquel",tidy_necronomicon$Palabra)]

tidy_necronomicon <- tidy_necronomicon %>%
  anti_join(stopwords_es)

tidy_necronomicon$Palabra[grepl("^?l",tidy_necronomicon$Palabra)]
tidy_necronomicon$Palabra[grepl("^aquel",tidy_necronomicon$Palabra)]

#### Stemming ####

tidy_necronomicon$Stem = stemDocument(tidy_necronomicon$Palabra, language="spanish")
View(tidy_necronomicon[tidy_necronomicon$Palabra=="hombres",])
tidy_necronomicon[tidy_necronomicon$Palabra=="hombre",]

#### Lematización ####

# Lematizacion de google
load("RAW_Necronomicon_Lematizacion_Google.RData")
LemasGoogle = map(RAW_Necronomicon_Lematizacion_Google, function(x)x$tokensDF)
LemasGoogle = bind_rows(LemasGoogle, .id = 'Titulo')
head(LemasGoogle)
LemasGoogle$content = tolower(LemasGoogle$content)
LemasGoogle$value = tolower(LemasGoogle$value)

LemasGoogle %>%
  unnest_tokens(output = Palabra, input = content) %>% 
  anti_join(stopwords_es) -> LemasGoogle

View(LemasGoogle[LemasGoogle$Palabra=="hablaban",])
View(LemasGoogle[LemasGoogle$Palabra=="extraña",])

#### Nube de palabras ####

LemasGoogle %>%
  count(value, sort = TRUE) -> conteos_lemas_google

wordcloud2(conteos_lemas_google, minSize = 10, size = .5)

LemasGoogle %>%
  filter(Titulo == "La Llamada De Cthulhu") %>% 
  count(value, sort = TRUE) -> conteos_lemas_google_cthulu

wordcloud2(conteos_lemas_google_cthulu, size = .5)

LemasGoogle %>%
  filter(Titulo == "La Sombra Sobre Insmouth") %>% 
  count(value, sort = TRUE) -> conteos_lemas_google_insmouth

wordcloud2(conteos_lemas_google_insmouth, size = .5)

#### Búsqueda de palabras más comunes ####

tidy_necronomicon %>% 
  distinct() -> tidy_necronomicon_unicas

tidy_necronomicon %>%
  count(Palabra, sort = TRUE) -> conteos_necronomicon

tidy_necronomicon %>%
  group_by(Titulo) %>% 
  count(Palabra, sort = TRUE) -> conteos_historia_necronomicon

wordcloud2(conteos_necronomicon)
wordcloud2(conteos_historia_necronomicon[conteos_historia_necronomicon$Titulo=="La Llamada De Cthulhu",c("Palabra","n")])

tidy_necronomicon_unicas = merge.data.frame(tidy_necronomicon_unicas, conteos_necronomicon
                                            , by = "Palabra")
tidy_necronomicon_unicas = merge.data.frame(tidy_necronomicon_unicas, conteos_historia_necronomicon
                                            , by = c("Palabra","Titulo")
                                            , suffixes = c("_general","_historia"))

tidy_necronomicon_unicas %>% 
  filter(Palabra == "noche") %>%
  select(Palabra, Titulo, AnoCreacion, DecadaCreacion, Frecuencia=n_historia) %>%
  arrange() %>%
  top_n(10,Frecuencia) %>%
  mutate(Titulo = color_tile("darkcyan","darkcyan")(Titulo)) %>%
  mutate(Palabra = color_tile("black","black")(Palabra)) %>%
  kable("html", escape = F, align = "c", caption = "Historias que más usan la palabra: noche"
        , table.attr = "style = \"color: white;\"") %>%
  kable_styling(bootstrap_options = c("condensed", "bordered"), 
                full_width = T) %>% 
  column_spec(1:5, background = "darkgray")

tidy_necronomicon_unicas %>% 
  filter(Palabra == "muerte") %>%
  select(Palabra, Titulo, AnoCreacion, DecadaCreacion, Frecuencia=n_historia) %>%
  arrange() %>%
  top_n(10,Frecuencia) %>%
  mutate(Titulo = color_tile("darkcyan","darkcyan")(Titulo)) %>%
  mutate(Palabra = color_tile("black","black")(Palabra)) %>%
  kable("html", escape = F, align = "c", caption = "Historias que más usan la palabra: muerte"
        , table.attr = "style = \"color: white;\"") %>%
  kable_styling(bootstrap_options = c("condensed", "bordered"), 
                full_width = T) %>% 
  column_spec(1:5, background = "darkgray")

tidy_necronomicon_unicas %>% 
  filter(Palabra %in% c("cthulhu","azathoth","nyarlathotep")) %>%
  select(Palabra, Titulo, AnoCreacion, DecadaCreacion, Frecuencia=n_historia) %>%
  arrange() %>%
  mutate(Titulo = color_tile("darkcyan","darkcyan")(Titulo)) %>%
  mutate(Palabra = color_tile("black","black")(Palabra)) %>%
  kable("html", escape = F, align = "c", caption = "Historias que nombran a los Dioses"
        , table.attr = "style = \"color: white;\"") %>%
  kable_styling(bootstrap_options = c("condensed", "bordered"), 
                full_width = T) %>% 
  column_spec(1:5, background = "darkgray")

tidy_necronomicon_unicas %>% 
  filter(Palabra %in% c("randolph","carter")) %>%
  select(Palabra, Titulo, AnoCreacion, DecadaCreacion, Frecuencia=n_historia) %>%
  arrange() %>%
  mutate(Titulo = color_tile("darkcyan","darkcyan")(Titulo)) %>%
  mutate(Palabra = color_tile("black","black")(Palabra)) %>%
  kable("html", escape = F, align = "c", caption = "Historias que nombran a Randolph Carter"
        , table.attr = "style = \"color: white;\"") %>%
  kable_styling(bootstrap_options = c("condensed", "bordered"), 
                full_width = T) %>% 
  column_spec(1:5, background = "darkgray")

tidy_necronomicon %>%
  filter(nchar(Palabra)>3) %>% 
  count(Palabra, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(Palabra = reorder(Palabra, n)) %>%
  ggplot() +
  geom_col(aes(Palabra, n), fill = "black") +
  geom_text(aes(Palabra, n, label=n)
            , family="serif", color="white", hjust=1.5, face="bold", size=6)+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("Palabra") + 
  ylab("Conteo de palabras") +
  ggtitle("Top 10 de palabras más usadas en las historias de HPL") +
  coord_flip()+
  tema_principal -> g0

tidy_necronomicon %>%
  filter(nchar(Stem)>3) %>% 
  count(Stem, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(Stem = reorder(Stem, n)) %>%
  ggplot() +
  geom_col(aes(Stem, n), fill = "darkblue") +
  geom_text(aes(Stem, n, label=n)
            , family="serif", color="white", hjust=1.5, face="bold", size=6)+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("Stem") + 
  ylab("Conteo de palabras") +
  ggtitle("Top 10 stems (Snowball)") +
  coord_flip()+
  tema_principal -> g1

LemasGoogle %>% 
  filter(nchar(value)>3) %>% 
  count(value, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(value = reorder(value, n)) %>%
  ggplot() +
  geom_col(aes(value, n), fill = "purple") +
  geom_text(aes(value, n, label=n)
            , family="serif", color="white", hjust=1.5, face="bold", size=6)+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("Lema (Google)") + 
  ylab("Conteo de lemas") +
  ggtitle("Top 10 lemas (Google)") +
  coord_flip()+
  tema_principal -> g2

grid.arrange(g0,g1,g2,ncol=2,nrow=2)

#### Correlación entre la historias de Randolph Carter ####

LlavePlata <- bind_rows(LemasGoogle %>% filter(Titulo=="La Llave De Plata")
                       , LemasGoogle %>% filter(Titulo=="A Través De Las Puertas De La Llave De Plata")) %>% 
  count(Titulo, value) %>%
  group_by(Titulo) %>%
  mutate(Proporcion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(Titulo, Proporcion)

ggplot(LlavePlata, aes(x = `La Llave De Plata`
                       , y = `A Través De Las Puertas De La Llave De Plata`)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = value), check_overlap = TRUE, vjust = 1.5, family = "serif") +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  labs(x = "La Llave De Plata", y = "A Trav?s De Las Puertas De La Llave De Plata")+
  theme(text = element_text(family = "serif", size = 16))

#### Correlación entre la historias de los Dioses ####

Dioses <- bind_rows(LemasGoogle %>% filter(Titulo=="El Caso De Charles Dexter Ward")
                    , LemasGoogle %>% filter(Titulo=="El Que Susurra En La Oscuridad")
                    , LemasGoogle %>% filter(Titulo=="La Búsqueda En Sueños De La Ignota Kadath")) %>% 
  count(Titulo, value) %>%
  group_by(Titulo) %>%
  mutate(Proporcion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(Titulo, Proporcion) %>% 
  gather(Titulo, Proporcion, `El Que Susurra En La Oscuridad`, `El Caso De Charles Dexter Ward`)

ggplot(Dioses, aes(x = Proporcion
                       , y = `La Búsqueda En Sueños De La Ignota Kadath`
       , color = abs(`La Búsqueda En Sueños De La Ignota Kadath` - Proporcion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = value), check_overlap = TRUE, vjust = 1.5
            , family = "serif", color = "black") +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.03), low = "orange", high = "firebrick1")+
  facet_wrap(~Titulo, ncol = 2)+
  labs(x = "La Búsqueda En Sueños De La Ignota Kadath", y = "")+
  theme(text = element_text(family = "serif", size = 16)
        , legend.position = "none")

#### Correlación entre historias independientes ####

Independientes <- bind_rows(LemasGoogle %>% filter(Titulo=="Polaris")
                            , LemasGoogle %>% filter(Titulo=="El Alquimista")) %>% 
  count(Titulo, value) %>%
  group_by(Titulo) %>%
  mutate(Proporcion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(Titulo, Proporcion)

ggplot(Independientes, aes(x = Polaris
                   , y = `El Alquimista`)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = value), check_overlap = TRUE, vjust = 1.5
            , family = "serif", color = "black") +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.03), low = "orange", high = "firebrick1")+
  labs(x = "Polaris", y = "El Alquimista")+
  theme(text = element_text(family = "serif", size = 16)
        , legend.position = "none")

#### Análisis de sentimientos ####

DiccionarioPositivo = read.delim("sentiment-lexicons/positive_words_es.txt")
names(DiccionarioPositivo) = "value"
DiccionarioPositivo$Sentimiento = "Positivo"
DiccionarioNegativo = read.delim("sentiment-lexicons/negative_words_es.txt")
names(DiccionarioNegativo) = "value"
DiccionarioNegativo$Sentimiento = "Negativo"
DiccionarioSentimientos = rbind.data.frame(DiccionarioPositivo, DiccionarioNegativo)

LemasGoogle %>%
  inner_join(DiccionarioSentimientos) %>%
  count(value, Sentimiento, sort = TRUE) %>% 
  ungroup() -> SentimentAnalysis1

SentimentAnalysis1 %>%
  #filter(!value %in% c("parecer","tierra","piedra","negro","sentir","ir","resultar")) %>% 
  #filter(!value %in% c("deber","llevar","pasar","esperar")) %>% 
  group_by(Sentimiento) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(value = reorder(value, n)) %>%
  ggplot(aes(value, n, fill = Sentimiento)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Sentimiento, scales = "free_y") +
  labs(y = "Frecuencia", x = "Lema") +
  coord_flip() +
  tema_principal

DiccionarioSentimientos2 = read.csv("lexico_afinn.en.es.csv")
DiccionarioSentimientos2 %>% 
  mutate(Sentimiento = ifelse(Puntuacion > 0, "Positiva", "Negativa")) -> DiccionarioSentimientos2
names(DiccionarioSentimientos2) = c("Palabra","Puntuacion","Word","Sentimiento")

LemasGoogle %>%
  inner_join(DiccionarioSentimientos2) %>%
  count(value, Sentimiento, sort = TRUE) %>% 
  ungroup() -> SentimentAnalysis2

SentimentAnalysis2 %>%
  group_by(Sentimiento) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(value = reorder(value, n)) %>%
  ggplot(aes(value, n, fill = Sentimiento)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Sentimiento, scales = "free_y") +
  labs(y = "Frecuencia", x = "Lema") +
  coord_flip()+
  tema_principal

LemasGoogle %>%
  inner_join(DiccionarioSentimientos2) %>%
  count(value, Sentimiento, Titulo, sort = TRUE) %>% 
  ungroup() -> SentimentAnalysis3

SentimentAnalysis3 %>%
  #filter(!value %in% c("parecer","tierra","piedra","negro","sentir","ir","resultar")) %>% 
  #filter(!value %in% c("deber","llevar","pasar","esperar")) %>% 
  filter(Titulo %in% c("La Sombra Sobre Insmouth", "La Llamada De Cthulhu"
                       , "El Horror De Dunwich", "El Horror De Red Hook")) %>% 
  group_by(Titulo, Sentimiento) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(value = reorder(value, n)) %>%
  ggplot(aes(value, n, fill = Sentimiento)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(Titulo~Sentimiento, scales = "free_y", ncol = 4) +
  labs(y = "Frecuencia", x = "Lema") +
  coord_flip() +
  tema_principal

SentimentAnalysis3 %>%
  filter(Titulo %in% c("La Búsqueda En Sueños De La Ignota Kadath"
                       ,"El Que Susurra En La Oscuridad"
                       ,"El Caso De Charles Dexter Ward")) %>% 
  group_by(Titulo, Sentimiento) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(value = reorder(value, n)) %>%
  ggplot(aes(value, n, fill = Sentimiento)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(Titulo~Sentimiento, scales = "free_y", ncol=2) +
  labs(y = "Frecuencia", x = "Lema") +
  coord_flip() +
  tema_principal

#### TF-IDF ####

LemasGoogle %>%
  count(value, Titulo, sort = TRUE) -> conteos_lemas_google_historias
head(conteos_lemas_google_historias)

conteos_lemas_google_historias <- conteos_lemas_google_historias %>%
  bind_tf_idf(value, Titulo, n)

conteos_lemas_google_historias
length(unique(conteos_lemas_google_historias$Titulo))
length(unique(conteos_lemas_google_historias$Titulo[conteos_lemas_google_historias$value=="carter"]))
log(76/6)*349/20999

conteos_lemas_google_historias %>%
  filter(Titulo %in% c("La Sombra Sobre Insmouth", "La Llamada De Cthulhu"
                       , "El Horror De Dunwich", "El Horror De Red Hook")) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(value = factor(value, levels = rev(unique(value)))) %>% 
  group_by(Titulo) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(value, tf_idf, fill = Titulo)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~Titulo, ncol = 2, scales = "free") +
  coord_flip()+
  tema_principal

conteos_lemas_google_historias %>%
  filter(Titulo %in% c("La Búsqueda En Sueños De La Ignota Kadath"
                       ,"El Que Susurra En La Oscuridad"
                       ,"El Caso De Charles Dexter Ward"
                       ,"Dag?n")) %>% 
  arrange(desc(tf_idf)) %>%
  mutate(value = factor(value, levels = rev(unique(value)))) %>% 
  group_by(Titulo) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(value, tf_idf, fill = Titulo)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~Titulo, ncol = 2, scales = "free") +
  coord_flip()+
  tema_principal

#### Análisis de clusters ####

conteos_lemas_google_historias %>%
  cast_dtm(Titulo, value, n) -> DTM_lema_google

DTM_lema_google

LDA_Result <- LDA(DTM_lema_google, k = 5, control = list(seed = 123))
LDA_Result

LDA_Result_tidy <- tidy(LDA_Result, matrix = "beta")
LDA_Result_tidy

LDA_Result_tidy %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

LDA_Result_tidy %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>% 
  arrange(desc(topic1))

top_terms_per_topic <- function(lda_model, num_words) {
  
  top_terms <- lda_model %>%
    group_by(topic) %>%
    arrange(topic, desc(beta)) %>%
    #get the top num_words PER topic
    slice(seq_len(num_words)) %>%
    arrange(topic, beta) %>%
    #row is required for the word_chart() function
    mutate(row = row_number()) %>%
    ungroup() %>%
    #add the word Topic to the topic labels
    mutate(topic = paste("Topic", topic, sep = " "))
  #create a title to pass to word_chart
  #title <- paste("T?rminos top por cada t?pico")
  #call the word_chart function you built in prep work
  #word_chart(top_terms, top_terms$term, title)
  return(top_terms)
}

lda_top_terms = top_terms_per_topic(LDA_Result_tidy, 15)

lda_top_terms %>%
  ggplot(aes(x=as.factor(row), 1, label = lda_top_terms$term, fill = factor(topic) )) +
  geom_point(color = "transparent") +
  geom_label_repel(direction = "y",
                   box.padding = 0.1,
                   size = 6, family = "serif") +
  facet_grid(~topic) +
  labs(x = NULL, y = NULL, title = "Top de palabras por tópico")+
  coord_flip() +
  tema_principal+
  theme(axis.text.x = element_blank())



