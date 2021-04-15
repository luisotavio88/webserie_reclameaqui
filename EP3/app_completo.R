
### BOAS VINDAS

### MELHOR DIA - SHINY É FODA - MACETE DA ENTREVISTA

### QUEM PARTICIPOU DO EPISÓDIO #1 OU #2?
# DEIXA O COMENTÁRIO AÍ NO CHAT

# pra quem tá chegando agora:

# NÃO TEM PRÉ-REQUISITO DE TER ASSISTIDO OS OUTROS

# MOSTRAR MATERIAIS COMPLEMENTAR

# WEBSCRAPING, LIMPEZA DOS DADOS, ANÁLISE EXPLORATÓRIA, 
# ANÁLISE DE TEXTO, GRÁFICOS COMPARATIVOS, 
# VISUALIZAÇÃO DE DADOS EM PAINEL INTERATIVO...

# PROMESSA: HOJE VOCÊ VAI APRENDER A CONSTRUIR UM 
# PAINEL INTERATIVO QUE IMPRESSIONA

# PEDIR PARA SE INSCREVER NO CANAL
# E ATIVAR SINO

### -> TODA SEGUNDA FEIRA ÀS 20Hs tem live!!!!
# Faça sua sugestão de tema no meu instagram

# vaga de estágio

# galera que tá perguntando das lives 1 e 2.
# Desafio: 10 compartilhamentos de Stories

# Conteúdo aqui nunca vi nem em curso pago.

# como eu prometi no começo, 100% de conteúdo e tudo gratuito.

# Não sejam egoístas

# Se bater eu libero as 3 aulas por 1 semana
# (será vendido depois disso)


# O que é o Shiny? 
# Qual é a estrutura do Shiny???


# Executar app pronto

# _____________________________________________________________
# MOSTRAR TELA
# _____________________________________________________________


# Mostrar app pronto


# Mostrar estrutura do app = ui,server e runApp

#   MATERIAL COMPLEMENTAR TERÁ SENHA
#   COMPROMETIMENTO COM A GALERA QUE ESTÁ LEVANDO A SÉRIO

### Carregar bibliotecas
library(shiny)
library(plotly)
library(plyr)
library(dplyr)
library(magrittr)
library(wordcloud2)

#carregar os dados
load("~/ESTATISTICA NA PRATICA/Conteudo/webserie_RA/EP3/.RData")

# menu de comparação
menu_comparacao<-c("Índice de Solução"="indice_solucao",
                   "Não respondidas"="nao_respondidas",
                   "Qnt. Avaliadas"="avaliadas",
                   "Nota RA"="nota",
                   "Nota do Consumidor"="nota_consumidor",
                   "% que voltaria"="percent_voltariam",
                   "Qnt. Reclamações"="qnt_reclamacoes")

# Define UI 
ui <- navbarPage("Websérie Reclame Aqui com Ciência de Dados",
                 tabPanel("Início",
                          class = "text-center",
                          imageOutput("logo", 
                                      height = "200px"),
                          h1("Episódio #3"),
                          wordcloud2Output('wordcloud2')),
                 tabPanel("Comparação Geral",
                          column(4,
                                 selectInput(inputId = "comparacao_v1", 
                                             label = "Quais resultados você quer comparar?", 
                                             choices = menu_comparacao,
                                             width = "100%"),
                                 selectInput(inputId = "comparacao_v2", 
                                             label = "Quais resultados você quer comparar?", 
                                             choices = menu_comparacao,
                                             selected = menu_comparacao[2],
                                             width = "100%")
                          ),
                          column(8,
                                 plotly::plotlyOutput(outputId = "comparacao_graf", width = "100%") 
                          )
                 ),
                 tabPanel("Título vs Solução",
                          column(4,
                                 selectInput(inputId = "relacao_empresa", 
                                             label = "Qual empresa você deseja avaliar?", 
                                             choices = info_basicas$empresa,
                                             width = "100%")
                          ),
                          column(8,
                                 plotly::plotlyOutput(outputId = "relacao_graf_maiores", width = "100%"),
                                 plotly::plotlyOutput(outputId = "relacao_graf_menores", width = "100%")
                          )
                 ),
                 tabPanel("Reclamações mais frequentes",
                          column(4,
                                 selectInput(inputId = "mais_aparecem_empresa", 
                                             label = "Qual empresa você deseja avaliar?", 
                                             choices = info_basicas$empresa,
                                             width = "100%")
                          ),
                          column(8,
                                 plotly::plotlyOutput(outputId = "mais_aparecem_graf", width = "100%"),
                          )
                          
                 )
)



server <- function(input, output,session) {
  
  #### Logo do projeto
  output$logo <- renderImage({
    list(src = "logo.jpeg",
         alt = "")
  }, deleteFile = FALSE)
  
  ######## NÚVEM DE PALAVRAS
  wordcloud_dados <- analise_geral_palavra %>% 
    select(word,qnt_palavra) %>%
    rename(freq = qnt_palavra)
  
  
  output$wordcloud2 <- renderWordcloud2({
    wordcloud2(data = wordcloud_dados)
  })
  
  #######comparação entre as empresas
  # Cruzamento de variáveis
  output$comparacao_graf <- plotly::renderPlotly({
    ay <- list(
      overlaying = "y",
      side = "right"
    )
    
    plot_ly(data = info_basicas) %>%
      add_lines(x = ~empresa, y = ~get(input$comparacao_v1), name = names(menu_comparacao)[menu_comparacao==input$comparacao_v1]) %>%
      add_lines(x = ~empresa, y = ~get(input$comparacao_v2), name = names(menu_comparacao)[menu_comparacao==input$comparacao_v2], yaxis = "y2") %>%
      layout(
        font = list(color = "black"),
        title = "Comparação entre empresas", 
        yaxis = list(title=""),
        yaxis2 = ay,
        xaxis = list(title="", tickangle = 60)
      )
  })
  
  # COMPARTILHAR A SENHA DO MATERIAL COMPLEMENTAR:
  # shinycabuloso
  
  # vaga de estágio
  
  
  ##### Relação entre palavras usadas no título
  # e a solução da reclamação
  
  relacao_dados <- reactive({
    #filtrar pela empresa escolhida
    relacao_palavra_empresa <- analise_empresa_palavras %>%
      select(-qnt_palavra) %>%
      filter(empresa == input$relacao_empresa)
    #filtrar pelos MAIORES % de solução
    relacao_palavra_empresa_maiores <- relacao_palavra_empresa %>%
      slice_max(n = 5,order_by = percentual_solucao,with_ties=F)
    
    #filtrar pelos MENORES % de solução
    relacao_palavra_empresa_menores <- relacao_palavra_empresa %>%
      slice_min(n = 5,order_by = percentual_solucao,with_ties=F)
    
    #Considerando todas as empresas
    relacao_palavra_geral<- analise_geral_palavra %>%
      mutate(empresa = "geral") %>%
      select(-qnt_palavra)
    
    relacao_palavra_geral_maiores <-
      relacao_palavra_geral[relacao_palavra_geral$word %in%
                              relacao_palavra_empresa_maiores$word,]
    
    relacao_palavra_geral_menores <-
      relacao_palavra_geral[relacao_palavra_geral$word %in%
                              relacao_palavra_empresa_menores$word,]
    
    
    ### resultado final: juntar o resultado geral + empresa
    concatenado_maiores <-  data.frame(rbind(relacao_palavra_empresa_maiores,relacao_palavra_geral_maiores))
    concatenado_maiores <- reshape(concatenado_maiores, 
                                   idvar = "word", timevar = "empresa", direction = "wide")
    concatenado_menores <- data.frame(rbind(relacao_palavra_empresa_menores,relacao_palavra_geral_menores))
    concatenado_menores <- reshape(concatenado_menores, 
                                   idvar = "word", timevar = "empresa", direction = "wide")
    names(concatenado_maiores) <-c("palavra",
                                   "percentual_loja",
                                   "percentual_geral")
    names(concatenado_menores) <-c("palavra",
                                   "percentual_loja",
                                   "percentual_geral")
    
    list(concatenado_maiores,concatenado_menores)
  })
  
  ## Gráfico relação entre palavras do título e solução
  
  output$relacao_graf_maiores <- plotly::renderPlotly({
    
    plot_ly(data = relacao_dados()[[1]],x = ~palavra, y = ~percentual_loja, type = 'bar', name = '% Empresa') %>%
      add_trace(y = ~percentual_geral, name = '% geral') %>%
      layout(title = "Pontos Fortes da Empresa",
             yaxis = list(title = '% Casos Resolvidos'),
             xaxis = list(title = ''),barmode = 'group')
    
  })
  
  output$relacao_graf_menores <- plotly::renderPlotly({
    
    plot_ly(data = relacao_dados()[[2]],x = ~palavra, y = ~percentual_loja, type = 'bar', name = '% Empresa') %>%
      add_trace(y = ~percentual_geral, name = '% geral') %>%
      layout(title = "Pontos Fracos da Empresa",
             yaxis = list(title = '% Casos Resolvidos'),
             xaxis = list(title = ''),barmode = 'group')        
  })
  
  
  ####### Dados - Palavras que mais aparecem
  mais_aparecem <-reactive({
    # filtrar por empresa que o usuário escolheu
    qnt_palavras_empresa_filtrado <- qnt_palavras_empresa %>%
      # filter(empresa == 'Carrefour -  Loja Online') %>%
      filter(empresa == input$mais_aparecem_empresa) %>%
      slice_max(n = 5,order_by = qnt_palavra,with_ties=F) 
    
    qnt_palavras_geral <- 
      qnt_palavras_geral[qnt_palavras_geral$word %in%
                           qnt_palavras_empresa_filtrado$word,]
    qnt_palavras_geral$empresa<-"geral"
    
    concatenado_palavras <-  data.frame(rbind(qnt_palavras_empresa_filtrado,qnt_palavras_geral))
    
    concatenado_palavras <- reshape(concatenado_palavras, 
                                    idvar = "word", timevar = "empresa", direction = "wide")
    names(concatenado_palavras)<-c("palavra","qnt_loja","qnt_geral")
    concatenado_palavras
  })
  
  # COMPARTILHAR A SENHA DO MATERIAL COMPLEMENTAR:
  # shinycabuloso
  
  output$mais_aparecem_graf <- plotly::renderPlotly({
    
    plot_ly(data = mais_aparecem(),x = ~qnt_loja, y = ~palavra, 
            type = 'bar', name = 'Quantidade Empresa', orientation = 'h') %>%
      add_trace(x = ~qnt_geral, name = 'Quantidade geral') %>%
      layout(title = "Palavras mais usadas nas reclamações da Empresa",
             yaxis = list(title = ''),
             xaxis = list(title = 'Quantidade de vezes'),barmode = 'group')        
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
