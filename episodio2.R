library(readr)
info_basicas <- read_delim("info_basicas_bruto.txt", 
                           "\t", escape_double = FALSE, trim_ws = TRUE)


info_reclamacoes_avaliadas <- read_delim("info_reclamacoes_avaliadas_bruto.txt", 
                                         "\t", escape_double = FALSE, trim_ws = TRUE)


str(info_basicas)
info_reclamacoes_avaliadas<-unique(info_reclamacoes_avaliadas)
str(info_reclamacoes_avaliadas)


# Acima podemos ver que algumas vari�veis num�ricas est�o como ```character```. 
# Isso aconteceu porque est�o com o s�mbolo de %.
# 
# A fun��o ```gsub``` localiza um texto padr�o e o substitui pelo texto definido.
# 
# Iremos substituir qualquer sinal de % por vazio.

info_basicas$indice_solucao <- gsub("[\\%,]", "", info_basicas$indice_solucao)
info_basicas$percent_voltariam <- gsub("[\\%,]", "", info_basicas$percent_voltariam)

#agora podemos transformar a vari�vel da classe texto em num�rica.
info_basicas$indice_solucao<- as.numeric(info_basicas$indice_solucao)
info_basicas$percent_voltariam<- as.numeric(info_basicas$percent_voltariam)

head(info_basicas)

######### info_reclamacoes

glimpse(info_reclamacoes_avaliadas)
str(info_reclamacoes_avaliadas)

View(info_reclamacoes_avaliadas)

# Precisamos transformar a vari�vel 'tempo_reclamacao' para num�rica. 
# 
# Dificuldade: temos respostas em minutos e outras em horas.
# 
# Solu��o: criei uma fun��o ```ifelse```. 
# 
# Iremos avaliar se cada linha do conjunto tem o texto 'horas' 
# ou 'hora' para a vari�vel 'tempo_reclamacao'.
# 
# Caso seja verdadeiro, iremos extrair apenas os n�meros e 
# multiplicar por 60. Assim deixaremos todos os valores em **minutos**.
# 
# Caso seja falso, ou seja, o tempo esteja medido em minutos, 
# iremos apenas extrair os n�meros e eliminar o texto.
# 
# O resultado foi armazenado em uma nova coluna, chamada 'minutos'.


info_reclamacoes_avaliadas$minutos<-
  ifelse(
    grepl(pattern = paste(c("horas","hora"),collapse = "|"), x = info_reclamacoes_avaliadas$tempo_reclamacao),
    #se o valor tiver em horas (ou hora) ir� transformar para minutos.
    #busca apenas os n�meros da coluna 'tempo_reclamacao' e multiplica por 60.
    parse_number(info_reclamacoes_avaliadas$tempo_reclamacao)*60, 
    ##### VERIFICAR SE EST� EM MESES, DIA OU MINUTOS
    ifelse(
      grepl(pattern = paste(c("minuto","minutos"),collapse = "|"), x = info_reclamacoes_avaliadas$tempo_reclamacao),
      #busca apenas os n�meros da coluna 'tempo_reclamacao'
      parse_number(info_reclamacoes_avaliadas$tempo_reclamacao),
      ifelse(
        grepl(pattern = paste(c("m�s","meses"),collapse = "|"), x = info_reclamacoes_avaliadas$tempo_reclamacao),
        #se o valor tiver em meses (ou m�s) ir� transformar para minutos.
        #busca apenas os n�meros da coluna 'tempo_reclamacao' e multiplica por 60*24*30.
        parse_number(info_reclamacoes_avaliadas$tempo_reclamacao)*24*60*30,
        #se o valor tiver em dias (ou dia) ir� transformar para minutos.
        #busca apenas os n�meros da coluna 'tempo_reclamacao' e multiplica por 60*24.
        parse_number(info_reclamacoes_avaliadas$tempo_reclamacao)*24*60
      )
    )
  )

# Cabe�alho do conjunto ```info_reclamacoes``` ap�s limpeza dos dados:

#info_reclamacoes

# View(info_reclamacoes)
# View(info_reclamacoes_avaliadas)

########## parte "censurada"

info_reclamacoes_avaliadas$titulo_reclamacao<-
  gsub('Editado pelo Reclame Aqui','editado_RA',info_reclamacoes_avaliadas$titulo_reclamacao)


###### salvar os dados
write.table(info_basicas,"info_basicas.txt",
            sep = "\t",fileEncoding = "utf8",row.names = F)

write.table(info_reclamacoes_avaliadas,"info_reclamacoes_avaliadas.txt",
            sep = "\t",fileEncoding = "utf8",row.names = F)
############ IN�CIO DA AN�LISE
library(tidyverse)
library(magrittr)

## gr�ficos comparativos
library(ggplot2)
theme_set(theme_classic())

# �NDICE DE SOLU��O DOS PROBLEMAS
g <- ggplot(info_basicas, aes(empresa, indice_solucao))
g + geom_col(width = 0.5,size = 1, color="darkblue", fill = "white") + 
  labs(title="�ndice de Solu��o por Empresa") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


# % RECLAMA��ES N�O RESPONDIDAS

info_basicas$per_nao_respondidas<- round(info_basicas$nao_respondidas/info_basicas$qnt_reclamacoes,3)*100

g <- ggplot(info_basicas, aes(empresa, per_nao_respondidas))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="% de reclama��es n�o respondidas vs Nota") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  # Adicionando a nota
  geom_line(aes(x = empresa, y = nota), size = 1.5, color="red", group = 1)



# Quantidade de reclama��es avaliadas
g <- ggplot(info_basicas, aes(empresa, avaliadas))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Quantidade de reclama��es avaliadas") +
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


############ AN�LISE DO TEMPO DAS RECLAMA��ES

ggplot(info_reclamacoes, aes(x=empresa, y=minutos, fill=status_reclamacao)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
  





###################   AN�LISE DAS RECLAMA��ES
library(tm)
library(wordcloud2)
library(tidytext)


#Cria um id para cada T�tulo
info_reclamacoes_avaliadas %<>% mutate(reclamacao_id = row_number()) 

# Criar uma linha para cada palavra do t�tulo
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

#### H� RELA��O ENTRE AS PALAVRAS DO T�TULO 
# E A SOLU��O DA RECLAMA��O?

avaliadas_token <-join(avaliadas_token,
                                       info_reclamacoes_avaliadas[,c("reclamacao_id","status_reclamacao")])

avaliadas_token_empresa <- join(avaliadas_token,
                                info_reclamacoes_avaliadas[,c("reclamacao_id","empresa")])

analise_empresa_palavra <-avaliadas_token_empresa %>%
  group_by(word,empresa) %>%
  mutate(percentual_solucao = sum(status_reclamacao=="Resolvido")/n(),
         qnt_palavra = n()) %>%
  select(percentual_solucao,empresa,word,qnt_palavra) %>%
  unique() %>%
  ungroup()

analise_geral_palavra <-avaliadas_token_empresa %>%
  group_by(word) %>%
  mutate(percentual_solucao = sum(status_reclamacao=="Resolvido")/n(),
         qnt_palavra = n()) %>%
  select(percentual_solucao,word,qnt_palavra) %>%
  unique() %>%
  ungroup()

#filtrar apenas palavras que apareceram mais de 20x 
analise_geral_palavra %<>% filter(qnt_palavra >20)

#filtrar os resultados para as empresas somente 
# se estiverem no resultado geral
analise_empresa_palavra <-
  analise_empresa_palavra[analise_empresa_palavra$word %in%
                            analise_geral_palavra$word,]

analise_empresa_palavra <-filter(analise_empresa_palavra,
                                 qnt_palavra>9)


## salvar essas informa��es para usar na visualiza��o de dados
write.table(analise_empresa_palavra,"analise_empresa_palavras.txt",
            row.names = F,fileEncoding = "utf8",sep = "\t")

write.table(analise_geral_palavra,"analise_geral_palavra.txt",
            row.names = F,fileEncoding = "utf8",sep = "\t")

####### EXISTE DIFEREN�A ENTRE AS EMPRESAS?

empresas_stem <- join(avaliadas_token,
                      info_reclamacoes_avaliadas[,c("reclamacao_id","empresa")])

#### Percentual geral de solu��o por empresa

analise_empresas <-empresas_stem %>%
  group_by(empresa) %>%
  mutate(percentual_solucao = sum(status_reclamacao=="Resolvido")/n()) %>%
  select(percentual_solucao,empresa) %>%
  unique() %>%
  ungroup()

##### Percentual geral de solu��o por empresa
##### Por palavras no t�tulo

analise_palavras_empresas <-empresas_stem %>%
  group_by(word,empresa) %>%
  mutate(percentual_solucao = sum(status_reclamacao=="Resolvido")/n(),
         qnt_palavra = n()) %>%
  select(word,percentual_solucao,qnt_palavra,empresa) %>%
  unique()%>%
  ungroup()

analise_palavras_empresas %<>% filter(qnt_palavra >50)

########## CORRELA��O ENTRE PALAVRAS
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
       y = "Qnt. apari��es",
       title = paste0("Palavras que aparecem nas reclama��es"))

##### NUVEM DE PALAVRAS
  library(wordcloud2)
  wordcloud_dados <- analise_palavras %>% 
    select(word,qnt_palavra) %>%
    rename(freq = qnt_palavra)
  
  wordcloud2(data = wordcloud_dados)
  
  
########### AN�LISE DOS SENTIMENTOS
  
library(tidytext)
  sentimento <- avaliadas_token %>% 
    inner_join(get_sentiments("afinn")) 
    
  sentimento<- sentimento %>%
    group_by(reclamacao_id) %>%
    summarise(score = sum(value))%>%
    ungroup()

  sentimento<- join(sentimento,info_reclamacoes_avaliadas)
  