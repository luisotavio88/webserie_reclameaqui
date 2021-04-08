library(readr)
info_basicas <- read_delim("info_basicas.txt", 
                           "\t", escape_double = FALSE, trim_ws = TRUE)

info_reclamacoes <- read_delim("info_reclamacoes.txt", 
                               "\t", escape_double = FALSE, trim_ws = TRUE)

info_reclamacoes_avaliadas <- read_delim("info_reclamacoes_avaliadas.txt", 
                                         "\t", escape_double = FALSE, trim_ws = TRUE)


info_reclamacoes<-unique(info_reclamacoes)
str(info_basicas)
info_reclamacoes_avaliadas<-unique(info_reclamacoes_avaliadas)
str(info_reclamacoes_avaliadas)


# Acima podemos ver que algumas variáveis numéricas estão como ```character```. 
# Isso aconteceu porque estão com o símbolo de %.
# 
# A função ```gsub``` localiza um texto padrão e o substitui pelo texto definido.
# 
# Iremos substituir qualquer sinal de % por vazio.

info_basicas$indice_solucao <- gsub("[\\%,]", "", info_basicas$indice_solucao)
info_basicas$percent_voltariam <- gsub("[\\%,]", "", info_basicas$percent_voltariam)

#agora podemos transformar a variável da classe texto em numérica.
info_basicas$indice_solucao<- as.numeric(info_basicas$indice_solucao)
info_basicas$percent_voltariam<- as.numeric(info_basicas$percent_voltariam)

head(info_basicas)

######### info_reclamacoes

glimpse(info_reclamacoes)
str(info_reclamacoes)

View(info_reclamacoes)

# Precisamos transformar a variável 'tempo_reclamacao' para numérica. 
# 
# Dificuldade: temos respostas em minutos e outras em horas.
# 
# Solução: criei uma função ```ifelse```. 
# 
# Iremos avaliar se cada linha do conjunto tem o texto 'horas' 
# ou 'hora' para a variável 'tempo_reclamacao'.
# 
# Caso seja verdadeiro, iremos extrair apenas os números e 
# multiplicar por 60. Assim deixaremos todos os valores em **minutos**.
# 
# Caso seja falso, ou seja, o tempo esteja medido em minutos, 
# iremos apenas extrair os números e eliminar o texto.
# 
# O resultado foi armazenado em uma nova coluna, chamada 'minutos'.

info_reclamacoes$minutos<-
  ifelse(
    grepl(pattern = paste(c("horas","hora"),collapse = "|"), x = info_reclamacoes$tempo_reclamacao),
    #se o valor tiver em horas (ou hora) irá transformar para minutos.
    #busca apenas os números da coluna 'tempo_reclamacao' e multiplica por 60.
    parse_number(info_reclamacoes$tempo_reclamacao)*60, 
    ##### VERIFICAR SE ESTÁ EM DIA OU MINUTOS
    ifelse(
      grepl(pattern = paste(c("minuto","minutos"),collapse = "|"), x = info_reclamacoes$tempo_reclamacao),
      #busca apenas os números da coluna 'tempo_reclamacao'
      parse_number(info_reclamacoes$tempo_reclamacao),
      #busca apenas os números da coluna 'tempo_reclamacao' e transforma pra minutos
      parse_number(info_reclamacoes$tempo_reclamacao)*24*60)
    
  )

############# Mesmo procedimento para info_reclamacoes_avaliadas


info_reclamacoes_avaliadas$minutos<-
  ifelse(
    grepl(pattern = paste(c("horas","hora"),collapse = "|"), x = info_reclamacoes_avaliadas$tempo_reclamacao),
    #se o valor tiver em horas (ou hora) irá transformar para minutos.
    #busca apenas os números da coluna 'tempo_reclamacao' e multiplica por 60.
    parse_number(info_reclamacoes_avaliadas$tempo_reclamacao)*60, 
    ##### VERIFICAR SE ESTÁ EM MESES, DIA OU MINUTOS
    ifelse(
      grepl(pattern = paste(c("minuto","minutos"),collapse = "|"), x = info_reclamacoes_avaliadas$tempo_reclamacao),
      #busca apenas os números da coluna 'tempo_reclamacao'
      parse_number(info_reclamacoes_avaliadas$tempo_reclamacao),
      ifelse(
        grepl(pattern = paste(c("mês","meses"),collapse = "|"), x = info_reclamacoes_avaliadas$tempo_reclamacao),
        #se o valor tiver em meses (ou mês) irá transformar para minutos.
        #busca apenas os números da coluna 'tempo_reclamacao' e multiplica por 60*24*30.
        parse_number(info_reclamacoes_avaliadas$tempo_reclamacao)*24*60*30,
        #se o valor tiver em dias (ou dia) irá transformar para minutos.
        #busca apenas os números da coluna 'tempo_reclamacao' e multiplica por 60*24.
        parse_number(info_reclamacoes_avaliadas$tempo_reclamacao)*24*60
      )
    )
  )

# Cabeçalho do conjunto ```info_reclamacoes``` após limpeza dos dados:

#info_reclamacoes

# View(info_reclamacoes)
# View(info_reclamacoes_avaliadas)

########## parte "censurada"

info_reclamacoes$titulo_reclamacao<-
  gsub('Editado pelo Reclame Aqui','editado_RA',info_reclamacoes$titulo_reclamacao)

info_reclamacoes_avaliadas$titulo_reclamacao<-
  gsub('Editado pelo Reclame Aqui','editado_RA',info_reclamacoes_avaliadas$titulo_reclamacao)


############ INÍCIO DA ANÁLISE
library(tidyverse)
library(magrittr)

## gráficos comparativos
library(ggplot2)
theme_set(theme_classic())

# ÍNDICE DE SOLUÇÃO DOS PROBLEMAS
g <- ggplot(info_basicas, aes(empresa, indice_solucao))
g + geom_col(width = 0.5,size = 1, color="darkblue", fill = "white") + 
  labs(title="Índice de Solução por Empresa") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


# % RECLAMAÇÕES NÃO RESPONDIDAS

info_basicas$per_nao_respondidas<- round(info_basicas$nao_respondidas/info_basicas$qnt_reclamacoes,3)*100

g <- ggplot(info_basicas, aes(empresa, per_nao_respondidas))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="% de reclamações não respondidas vs Nota") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  # Adicionando a nota
  geom_line(aes(x = empresa, y = nota), size = 1.5, color="red", group = 1)



# Quantidade de reclamações avaliadas
g <- ggplot(info_basicas, aes(empresa, avaliadas))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Quantidade de reclamações avaliadas") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

# Nota do Consumidor
g <- ggplot(info_basicas, aes(empresa, nota_consumidor))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Nota do Consumidor") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


############ ANÁLISE DO TEMPO DAS RECLAMAÇÕES

ggplot(info_reclamacoes, aes(x=empresa, y=minutos, fill=status_reclamacao)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
  





###################   ANÁLISE DAS RECLAMAÇÕES
library(tm)
library(wordcloud2)
library(tidytext)

#Cria um id para cada Título
info_reclamacoes_avaliadas %<>% mutate(reclamacao_id = row_number()) 

# Criar uma linha para cada palavra do título
avaliadas_token<-info_reclamacoes_avaliadas %>% 
  select(titulo_reclamacao,reclamacao_id) %>% 
  unnest_tokens(word,titulo_reclamacao) 
  
# library(corpus)
# avaliadas_token$word<-
#   unlist(text_tokens(avaliadas_token$word,
#                       stemmer = "pt"))
  

#remover stopwords
stop_words_pt<-data.frame(word=(stopwords(kind = "pt")))
avaliadas_token <- avaliadas_token %>% 
  anti_join(stop_words_pt)

#remover palavras com duas letras
avaliadas_token %<>% 
  filter(sapply(avaliadas_token$word,nchar)>2)

#### HÁ RELAÇÃO ENTRE AS PALAVRAS DO TÍTULO 
# E A SOLUÇÃO DA RECLAMAÇÃO?

avaliadas_token <-join(avaliadas_token,
                                       info_reclamacoes_avaliadas[,c("reclamacao_id","status_reclamacao")])
analise_palavras<-avaliadas_token %>%
  group_by(word) %>%
  mutate(percentual_solucao = sum(status_reclamacao=="Resolvido")/n(),
         qnt_palavra = n()) %>%
  select(word,percentual_solucao,qnt_palavra) %>%
  unique()%>%
  ungroup()

analise_palavras %<>% filter(qnt_palavra >50)

####### EXISTE DIFERENÇA ENTRE AS EMPRESAS?

empresas_stem <- join(avaliadas_token,
                      info_reclamacoes_avaliadas[,c("reclamacao_id","empresa")])

#### Percentual geral de solução por empresa

analise_empresas <-empresas_stem %>%
  group_by(empresa) %>%
  mutate(percentual_solucao = sum(status_reclamacao=="Resolvido")/n()) %>%
  select(percentual_solucao,empresa) %>%
  unique() %>%
  ungroup()

##### Percentual geral de solução por empresa
##### Por palavras no título

analise_palavras_empresas <-empresas_stem %>%
  group_by(word,empresa) %>%
  mutate(percentual_solucao = sum(status_reclamacao=="Resolvido")/n(),
         qnt_palavra = n()) %>%
  select(word,percentual_solucao,qnt_palavra,empresa) %>%
  unique()%>%
  ungroup()

analise_palavras_empresas %<>% filter(qnt_palavra >50)

########## CORRELAÇÃO ENTRE PALAVRAS
########## QUAIS AS PALAVRAS QUE MAIS APARECERAM JUNTAS?
library(widyr)
library(igraph)
library(ggraph)
correlacao <- avaliadas_token %>%
  group_by(word) %>%
  filter(n() > 20) %>%
  pairwise_cor(word, reclamacao_id, sort = TRUE,upper=F)%>%
  ungroup()

correlacao %<>% filter(correlation >0.3)


correlacao %>%
  arrange(-correlation) %>%
  top_n(10) %>% #Filtrar as 10 maiores
  graph_from_data_frame() %>%
  ggraph(layout = 'fr') + 
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2")) +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) + 
  geom_node_point(color = 'lightblue', size = 5) + 
  geom_node_text(aes(label = name), repel = TRUE) + 
  theme_graph() +
  labs(title = "Palavras que geralmente apareceram juntas")


########### palavras que mais aparecem 

dados_grafico<- avaliadas_token %>% count(word, sort = T) %>% top_n(5) %>%
  mutate(word = reorder(word,n))

  ggplot(dados_grafico,aes(x = word, y=n)) + 
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "",
       y = "Qnt. aparições",
       title = paste0("Palavras que aparecem nas reclamações"))

##### NUVEM DE PALAVRAS
  library(wordcloud2)
  wordcloud_dados <- analise_palavras %>% 
    select(word,qnt_palavra) %>%
    rename(freq = qnt_palavra)
  
  wordcloud2(data = wordcloud_dados)
  
  
########### ANÁLISE DOS SENTIMENTOS
  
library(tidytext)
  sentimento <- avaliadas_token %>% 
    inner_join(get_sentiments("afinn")) 
    
  sentimento<- sentimento %>%
    group_by(reclamacao_id) %>%
    summarise(score = sum(value))%>%
    ungroup()

  sentimento<- join(sentimento,info_reclamacoes_avaliadas)
  