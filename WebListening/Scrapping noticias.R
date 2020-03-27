remove(list=ls(all=T))

# Librerías ----------
library(dplyr) 
library(rvest) 
library(stringr) 
library(purrr)

# Lectura de noticias y obtención de links ----------

# Funciones
obtieneNoticiasData = function(noticia){
  news_pag = "https://news.google.com/"
  titular = noticia %>% html_node("h3") %>% html_text()
  fecha = noticia %>% html_node("time") %>% html_attr("datetime")
  diario = noticia %>% html_node("a.wEwyrc.AVN2gc.uQIVzc.Sksgp") %>% html_text()
  link_enmascarado = noticia %>% html_node("h3 a") %>% html_attr("href")
  link_enmascarado = paste0(news_pag,substring(link_enmascarado,3))  
  link_enmascarado = read_html(link_enmascarado)
  link = link_enmascarado %>% 
    html_node(css='c-wiz div div c-wiz div a') %>% 
    html_attr("href")
  noticiaDF = data.frame(Titular=titular, Fecha=fecha, Diario=diario, Link=link, stringsAsFactors = F)
  return(noticiaDF)
}

obtieneNoticiasBusqueda = function(busqueda){
  news_pag = "https://news.google.com/"
  html_dir = paste0(news_pag,"search?q=",gsub(" ","+",busqueda),"&hl=es-419&gl=US&ceid=US:es-419")
  google_news = read_html(html_dir)
  noticias = google_news %>% 
    html_nodes(css = "c-wiz div div div div main c-wiz div div div article")
  noticiasDF = map(noticias,obtieneNoticiasData)
  noticiasDF = bind_rows(noticiasDF)
  noticiasDF = noticiasDF[!is.na(noticiasDF$Titular),]
  return(noticiasDF)
}

# Dataframes de titulares ----------
noticiasYundaDF = obtieneNoticiasBusqueda(busqueda="Jorge Yunda Quito when:7d")
noticiasViteriDF = obtieneNoticiasBusqueda("Cynthia Viteri Guayaquil when:7d")
noticiasSonnenholznerDF = obtieneNoticiasBusqueda("Otto Sonnenholzner vicepresidente Ecuador when:7d")
noticiasMorenoDF = obtieneNoticiasBusqueda("Lenin Moreno presidente Ecuador when:7d")
noticiasYundaDF$Diario = gsub(" \\(por eliminar\\)","",noticiasYundaDF$Diario)
save.image("RadarNoticias.RData")

# Lectura de links de noticias ----------
# comercio ec: '.paragraphs'
# universo: '.field-name-body'

load("RadarNoticias.RData")

Diarios = unique(c(noticiasMorenoDF$Diario,noticiasSonnenholznerDF$Diario,
       noticiasViteriDF$Diario, noticiasYundaDF$Diario))
Estructura = data.frame(Diario=Diarios)
Estructura$CSS = NA
Estructura$Class = NA
Estructura[Estructura$Diario=='El Comercio (Ecuador)',c("CSS","Class")] = c('.paragraphs','NA')
Estructura[Estructura$Diario=='El Universo',c("CSS","Class")] = c('.field-name-body','NA')
Estructura[Estructura$Diario=='Metro Ecuador',c("CSS","Class")] = c('.resumen','NA')
Estructura[Estructura$Diario=='El Telégrafo',c("CSS","Class")] = c('div div div article','story-body-block')
Estructura[Estructura$Diario=='La Hora (Ecuador)',c("CSS","Class")] = c('div','content')
Estructura[Estructura$Diario=='expreso.ec',c("CSS","Class")] = c('div','paragraph ')
Estructura[Estructura$Diario=='Primicias',c("CSS","Class")] = c('#entry-content-inarticle','NA')
Estructura[Estructura$Diario=='Teleamazonas',c("CSS","Class")] = c('.entry-content','NA')
Estructura[Estructura$Diario=='Portal Extra',c("CSS","Class")] = c('.template-33','')
Estructura[Estructura$Diario=='Hoy en Imbabura',c("CSS","Class")] = c('div','has-text-align-justify')
Estructura[Estructura$Diario=='El Diario Ecuador',c("CSS","Class")] = c('.contenido_nota','NA')
Estructura[Estructura$Diario=='Diario La Opinión',c("CSS","Class")] = c('.border-right','p-contenido')
# 3 casos

obtenerNoticiaNacional = function(link_noticia,diario){
  
  #link_noticia = noticiasYundaDF$Link[13]
  #diario = noticiasYundaDF$Diario[13]
  noticia = read_html(link_noticia)
  css = Estructura$CSS[Estructura$Diario==diario]
  class = Estructura$Class[Estructura$Diario==diario]
  
  if(!is.na(css)){
    
    text_nodes = noticia %>% 
      html_nodes(css = css) %>% 
      html_children()
    
    if (class=="NA"){
      text_nodes = text_nodes[is.na(sapply(text_nodes,function(x)html_attr(x,'class')))]  
    } else if (class!=""){
      text_nodes = text_nodes[sapply(text_nodes,function(x)html_attr(x,'class'))==class & 
                                !is.na(sapply(text_nodes,function(x)html_attr(x,'class')))]
    }
    
    text = text_nodes %>% 
      html_text()
    
    text = paste0(text, collapse = " ")
    
    return(text)
  }
  
  return(NA)
  
}

rownames(noticiasYundaDF) = 1:nrow(noticiasYundaDF)
news = c()
for (i in 1:37){
  print(i)
  new = obtenerNoticiaNacional(noticiasYundaDF$Link[i], noticiasYundaDF$Diario[i])
  news = c(news,new)
}
noticiasYundaDF$Noticia = news

rownames(noticiasViteriDF) = 1:nrow(noticiasViteriDF)
news = c()
for (i in 1:55){
  print(i)
  new = obtenerNoticiaNacional(noticiasViteriDF$Link[i], noticiasViteriDF$Diario[i])
  news = c(news,new)
}
noticiasViteriDF$Noticia = news

save(noticiasYundaDF, noticiasViteriDF, file="noticiasDF.RData")
