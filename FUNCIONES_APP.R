preproc.text <- function(x){
  library(dplyr)
  library(tidytext)
  library(rstudioapi)
  library(janeaustenr)
  library(tm)
  library(readxl)
  library(tidyr)
  library(wordcloud)
  library(ggplot2)
  library(igraph)
  library(ggraph)
  library(tictoc)
  library(Rtsne)
  library(factoextra)
  library(ClusterR)
  library(tidyverse)
  library(syuzhet)
  
  y <- x %>%
    gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", .) %>% 
    iconv(.,from="utf-8",to="ASCII//TRANSLIT") %>%
    gsub("[^[:print:]]", " ", .) %>%
    tolower %>% 
    gsub("[^[:lower:]^[:space:]]", " ", .) %>%
    gsub("[[:space:]]{1,}", " ", .) %>%
    trimws
  return(y)
}

removeManyWords <- function(texto,palabras){
  require(tm)
  nsplits <- ceiling(length(palabras)/2000)
  for (i in 1:nsplits){
    primera<-(i-1)*2000+1
    ultima<-min(i*2000,length(palabras))
    texto<-removeWords(texto,palabras[primera:ultima])  
  }
  gsub("[[:space:]]{1,}", " ", texto)
}

lematizador_diccionario<-function(vector_texto,diccionario){
  for(i in 1:length(vector_texto)){
    q<-unlist(strsplit(vector_texto[i]," "))
    r<-vector_texto[i]
    for (j in q) {
      if (j%in%diccionario$palabras_originales) {
        ind<-which(diccionario$palabras_originales==j)[1]
        r<-gsub(j,diccionario$palabras_lematizadas[ind], r)
      } 
    } 
    vector_texto[i]<-r 
  }
  return(vector_texto)
}


proc_rtas<-function(rtas_sondeos){
  
  load("objetos_sondeos_app.Rda")
  tic(msg="Tiempo de procesamiento")
  
  #### limpieza del vector de texto
  rtas_sondeos<-rtas_sondeos[is.na(rtas_sondeos)==F,] # quito respuestas en blanco
  
  colnames(rtas_sondeos)<-c("rta")
  rtas_sondeos$limpio<-preproc.text(rtas_sondeos$rta)
  
  ## nombres propios que se quitan
  nom.propios<-table(unlist(strsplit(rtas_sondeos$limpio," "))[unlist(strsplit(rtas_sondeos$limpio," "))%in%nombres.propios])
  rtas_sondeos$limpio<-removeManyWords(rtas_sondeos$limpio,nombres.propios)
  
  ## stopwords que se quitan
  sw.quitan<-sort(table(unlist(strsplit(rtas_sondeos$limpio," "))[unlist(strsplit(rtas_sondeos$limpio," "))%in%stopwords]),decreasing = T)
  rtas_sondeos$limpio<-removeManyWords(rtas_sondeos$limpio,stopwords)
  rtas_sondeos<-rtas_sondeos[!rtas_sondeos$limpio%in%c(""," "),]# quito respuestas vacias despues de la limpieza
  
  rtas_sondeos$limpio<-lematizador_diccionario(vector_texto = rtas_sondeos$limpio,diccionario = diccionario_lematizacion)
  rtas_sondeos$id_rta<-paste("rta",c(1:nrow(rtas_sondeos)))
  rtas_sondeos<-rtas_sondeos[!is.na(rtas_sondeos$limpio),]
  
  #  rtas_sondeos$limpio<-preproc.text(rtas_sondeos$limpio)
  
  ## analisis de palabras
  palabras<-rtas_sondeos%>%
    unnest_tokens(palabra,limpio)%>%
    group_by(palabra)%>%
    summarise(freq=n())%>%
    arrange(desc(freq))%>%
    mutate(palabra=factor(palabra, levels = palabra[order(freq)]))
  
  ###### BIGRAMAS
  ### creando bigramas
  ### contando bigramas
  cuenta_bigramas<- rtas_sondeos%>%
    mutate(n.pal=sapply(strsplit(limpio, " "), length))%>%
    filter(n.pal>1)%>%
    unnest_tokens(bigrams,limpio,token="ngrams",n=2)%>%
    count(bigrams,sort = T)%>%
    filter(is.na(bigrams)==F)%>%
    mutate(bigrams=factor(bigrams,levels = bigrams[order(n)]))
  
  ### trigramas
  ### creando trigramas
  ### contando bigramas
  cuenta_trigramas<- rtas_sondeos%>%
    mutate(n.pal=sapply(strsplit(limpio, " "), length))%>%
    filter(n.pal>2)%>%
    unnest_tokens(trigrams,limpio,token="ngrams",n=3)%>%
    count(trigrams,sort = T)%>%
    filter(is.na(trigrams)==F)%>%
    mutate(trigrams=factor(trigrams,levels = trigrams[order(n)]))
  
  mensajede_tiempo<-toc(func.toc = toc.outmsg)
  
  out<-list()
  out$rtas_sondeos<-rtas_sondeos
  out$unapal<-palabras
  out$dospal<-cuenta_bigramas
  out$trespal<-cuenta_trigramas
  out$tiempo_msg<-mensajede_tiempo
  
  #message(mensajede_tiempo)
  return(out)
  
}

palabras.despues.de<-function(palabra,rtas_sondeos){
  palabras_despues <-rtas_sondeos%>%
    mutate(n.pal=sapply(strsplit(rtas_sondeos.limpio, " "), length))%>%
    filter(n.pal>1)%>%
    unnest_tokens(bigrams,rtas_sondeos.limpio,token="ngrams",n=2) %>%
    separate(bigrams,c("palabra1","palabra2"),sep=" ")%>%
    group_by(palabra1,palabra2)%>%
    summarise(n=n())%>%
    filter(palabra1==palabra)%>%
    arrange(desc(n))
  palabras_despues$palabra2<-factor(palabras_despues$palabra2,
                                    levels =palabras_despues$palabra2[order(palabras_despues$n)] )
  
  return(palabras_despues)
}

palabras.antes.de<-function(palabra,rtas_sondeos){
  palabras_antes <-rtas_sondeos%>%
    mutate(n.pal=sapply(strsplit(rtas_sondeos.limpio, " "), length))%>%
    filter(n.pal>1)%>%
    unnest_tokens(bigrams,rtas_sondeos.limpio,token="ngrams",n=2) %>%
    separate(bigrams,c("palabra1","palabra2"),sep=" ")%>%
    group_by(palabra1,palabra2)%>%
    summarise(n=n())%>%
    filter(palabra2==palabra)%>%
    arrange(desc(n))
  palabras_antes$palabra1<-factor(palabras_antes$palabra1,
                                  levels =palabras_antes$palabra1[order(palabras_antes$n)] )
  
  return(palabras_antes)
}

data_red<-function(rtas_sondeos,n_pal){
  bigramas_red <-rtas_sondeos%>%
    unnest_tokens(bigrams,rtas_sondeos.limpio,token="ngrams",n=2) %>%
    separate(bigrams,c("palabra1","palabra2"),sep=" ") %>%
    count(palabra1, palabra2, sort = TRUE) %>%
    filter(is.na(palabra1)==F,n>n_pal)%>%
    graph_from_data_frame()
  
  return(bigramas_red)
  
}


reducir_dim<- function(rtas_sondeos){
  # creacion del bow
  bow<-rtas_sondeos%>%
    #mutate(id_rta=paste("rta",c(1:nrow(rtas_sondeos))))%>%
    unnest_tokens(palabra,rtas_sondeos.limpio)%>%
    count(rtas_sondeos.id_rta,palabra) %>% 
    mutate(n=as.integer(n))
  
  bow.total<-bow%>%
    group_by(rtas_sondeos.id_rta)%>%
    summarise(total=as.integer(sum(n)))
  
  bow1<-left_join(bow,bow.total,by = "rtas_sondeos.id_rta")%>%
    mutate(bow.freq=n/total)%>%
    select(-n,-total)%>%
    spread(palabra,bow.freq)
  
  bow1[is.na(bow1)]<- 0
  
  bow.bi<-rtas_sondeos%>%
    mutate(#id_rta=paste("rta",c(1:nrow(rtas_sondeos))),
      n.pal=sapply(strsplit(rtas_sondeos.limpio, " "), length))%>%
    filter(n.pal>1)%>%
    unnest_tokens(bigrams,rtas_sondeos.limpio,token = "ngrams",n=2)%>%
    count(rtas_sondeos.id_rta,bigrams)%>%
    filter(is.na(bigrams)==F)
  
  bow.bi.total<-bow.bi%>%
    group_by(rtas_sondeos.id_rta)%>%
    summarise(total=sum(n))
  
  bow.bi.1<-left_join(bow.bi,bow.bi.total,by = "rtas_sondeos.id_rta")%>%
    mutate(bow.freq=n/total)%>%
    select(-n,-total)%>%
    spread(bigrams,bow.freq)
  
  bow.bi.1[is.na(bow.bi.1)]<-0
  
  bow.t<-left_join(bow1,bow.bi.1,by="rtas_sondeos.id_rta")
  bow.t[is.na(bow.t)]<-0
  
  
  # # Aplicamos el t-SNE.
  # # En este caso, representamos los productos en un espacio de 2 dimensiones.
  # perp<-round(ncol(bow.t)*0.05)
  set.seed(9)
  
  perp<-(dim(bow.t)[1])*0.1
  
  tsne.model.BoW <- Rtsne(bow.t[,-1], check_duplicates=FALSE, pca=T, perplexity=0.5, theta=0.65, dims=3,max_iter = 3000,normalize = T)
  productos.en.3D = as.data.frame(tsne.model.BoW$Y)
  
  opt = Optimal_Clusters_KMeans(productos.en.3D[,1:3], max_clusters = 10, plot_clusters = T,
                                
                                criterion = 'distortion_fK', fK_threshold = 0.85,
                                
                                initializer = 'optimal_init', tol_optimal_init = 0.2)
  
  n_grupos_opt= which.min(opt)
  
  out_dim<-list()
  out_dim$productos.en.3D<-productos.en.3D
  out_dim$n_grupos_opt<-n_grupos_opt
  out_dim$bow.t<-bow.t
  return(out_dim) 
}

clusterizar<-function(productos.en.3D,n_grupos,npal_importantes=2,bow.t,rtas_sondeos){
  set.seed(34)
  kmeans.BoW <- kmeans(productos.en.3D, centers = n_grupos,nstart = 25)
  
  productos.en.3D$clus<-as.factor(kmeans.BoW$cluster)
  bow.t$cluster<-kmeans.BoW$cluster
  productos.en.3D$id_rta<-bow.t$rtas_sondeos.id_rta
  rtas_sondeos<-rtas_sondeos%>%left_join(select(bow.t,rtas_sondeos.id_rta,cluster),by="rtas_sondeos.id_rta")
  
  productos.en.3D<-productos.en.3D%>%left_join(select(rtas_sondeos,rtas_sondeos.rta,id_rta=rtas_sondeos.id_rta),
                                               by="id_rta")
  # # Compute hierarchical clustering
  # res.hc <- productos.en.2D %>%
  #   dist(method = "euclidean") %>% 
  #   hclust(method = "ward.D2")     
  
  #clusterCut <- cutree(res.hc,k = n_grupos)
  #prop.table(table(clusterCut))
  #rtas_sondeos$cluster.hc<-clusterCut
  
  
  # pal.hc<-rtas_sondeos%>%
  #   unnest_tokens(palabra,rtas_sondeos.limpio)%>%
  #   group_by(cluster.hc,palabra)%>%
  #   summarise(freq=n())%>%
  #   arrange(cluster.hc,desc(freq))
  
  pal.kme<-rtas_sondeos%>%
    unnest_tokens(palabra,rtas_sondeos.limpio)%>%
    group_by(cluster,palabra)%>%
    summarise(freq=n()) %>% 
    bind_tf_idf(palabra,cluster,freq)%>%
    arrange(cluster,desc(tf_idf))
  
  palabras.frecuentes<-data.frame(cluster=c(1:n_grupos))
  for (i in unique(pal.kme$cluster)) {
    # palabras.frecuentes$palabras.hc[i]<-paste0(pal.hc$palabra[pal.hc$cluster.hc==i][1:npal_importantes],collapse = " ")
    palabras.frecuentes$palabras.kme[i]<-paste0(pal.kme$palabra[pal.kme==i][1:npal_importantes],collapse = " ")
  }
  
  clust.obj<-list()
  clust.obj$rtas_sondeos<-rtas_sondeos
  clust.obj$productos.en.3D<-productos.en.3D
  clust.obj$palabras.frecuentes<-pal.kme
  return(clust.obj)
}
