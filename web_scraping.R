# 3 - configurações necessárias

library(plyr)
library(dplyr)
library(rvest)
library(RSelenium)

binman::list_versions("chromedriver")

# iniciar um SERVIDOR SELENIUM E O NAVEGADOR
rD <- rsDriver(browser="chrome", port=4546L, chromever = "89.0.4389.23")

remDr <- rD[["client"]] #NAVEGADOR


################# Página Inicial da empresa (informações básicas)
info_basicas<-data.frame()

link_lojas<-c("https://www.reclameaqui.com.br/empresa/americanas-com-loja-online/",
              "https://www.reclameaqui.com.br/empresa/casas-bahia-loja-online/",
              "https://www.reclameaqui.com.br/empresa/ponto-frio-loja-online/")


for(link_loja in link_lojas){
  
  remDr$navigate(link_loja)
  
  #ler todo o código
  codigo<- read_html(remDr$getPageSource()[[1]])
  
  empresa <- codigo %>% 
    html_nodes(".short-name") %>%
    html_text()
  
  nota <- codigo %>% 
    html_nodes(".score b") %>%
    html_text()
  
  qnt_reclamacoes <- codigo %>% 
    html_nodes("a:nth-child(1) .stats") %>%
    html_text()
  
  percent_resp <- codigo %>% 
    html_nodes(".jFIGdy:nth-child(2) span") %>%
    html_text()
  
  percent_voltariam <- codigo %>% 
    html_nodes(".jFIGdy:nth-child(4) span") %>%
    html_text()
  
  indice_solucao <- codigo %>% 
    html_nodes(".jFIGdy:nth-child(6) span") %>%
    html_text()
  
  nota_consumidor <- codigo %>% 
    html_nodes(".jFIGdy:nth-child(8) span") %>%
    html_text()
  
  nao_respondidas <- codigo %>% 
    html_nodes(".col-sm-6:nth-child(1) b") %>%
    html_text()
  
  avaliadas <- codigo %>% 
    html_nodes(".col-sm-6+ .col-sm-6 b") %>%
    html_text()
  
  info_basicas <-rbind(
    data.frame(empresa,indice_solucao,nao_respondidas,
               avaliadas,nota,nota_consumidor,percent_voltariam,
               qnt_reclamacoes,link_loja), info_basicas)
  
}

############## FIM DAS INFORMAÇÕES BÁSICAS ######


############## COMEÇO DA BUSCA POR RECLAMAÇÕES #####

link_reclamacoes<-paste0(link_lojas,"lista-reclamacoes/")

info_reclamacoes <-data.frame()

# for(link_pagina in link_reclamacoes){
#   
#   for(pagina in 1:2){    
    remDr$navigate("https://www.reclameaqui.com.br/empresa/americanas-com-loja-online/lista-reclamacoes/?pagina=1")
    
    # remDr$navigate(paste0(link_pagina,"?pagina=",pagina))
    
    #ler todo o código
    codigo_reclamacoes<- read_html(remDr$getPageSource()[[1]])
    
    empresa<-codigo_reclamacoes %>% 
      html_nodes(".company-title .ng-binding") %>%
      html_text()
    
    titulo_reclamacao<-codigo_reclamacoes %>% 
      html_nodes(".text-title") %>%
      html_text()
    
    status_reclamacao<-codigo_reclamacoes %>% 
      html_nodes(".status-text") %>%
      html_text()
    
    local_reclamacao<-codigo_reclamacoes %>% 
      html_nodes("#complains-anchor-top .hidden-xs.ng-binding") %>%
      html_text()
    
    
    tempo_reclamacao<-codigo_reclamacoes %>% 
      html_nodes(".hourAgo") %>%
      html_text()
    
    info_reclamacoes <-rbind(data.frame(empresa,titulo_reclamacao,
                                        status_reclamacao,local_reclamacao,
                                        tempo_reclamacao)
      ,info_reclamacoes)
#   }
# }
############## FIM DA BUSCA POR RECLAMAÇÕES ######


remDr$close()
rD$server$stop()
rm(rD)


### SUBIR PARA O GITHUB

# https://www.youtube.com/watch?v=zksMz0lIH_Q

