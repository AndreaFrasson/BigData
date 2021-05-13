conta_bigrammi<-function(dataset){
  dataset %>% 
    unnest_tokens(bigram,text,token = "ngrams",n=2)%>%
    separate(bigram,c("word1","word2"),sep = " ") %>%
    count(word1,word2,sort = T) %>%
    filter( n >12) %>%
    na.omit()
}


conta_trigrammi<-function(dataset){
  dataset %>% 
    unnest_tokens(trigram,text,token = "ngrams",n=3)%>%
    separate(trigram,c("word1","word2","word3"),sep = " ") %>%
    count(word1,word2,word3,sort = T) %>%
    filter( n >10) %>%
    na.omit()
}


visualizza_bigrammi<-function(bigram){
  set.seed(2021)
  bigram %>% 
    graph_from_data_frame()%>%
    ggraph(layout = "fr")+
    geom_edge_link(aes(edge_alpha = n,edge_width=n),edge_colour = "red")+
    geom_node_point(size = 1)+
    geom_node_text(aes(label = name),repel = T,point.padding = unit(0.05,"lines"))+
    theme_void()
}

visualizza_trigrammi<-function(trigram){
  set.seed(2021)
  trigram %>% 
    graph_from_data_frame()%>%
    ggraph(layout = "fr")+
    geom_edge_link(aes(edge_alpha = n,edge_width=n),edge_colour = "red")+
    geom_node_point(size = 1)+
    geom_node_text(aes(label = name),repel = T,point.padding = unit(0.05,"lines"))+
    theme_void()
}
