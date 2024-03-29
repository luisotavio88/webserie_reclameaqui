# 3 - configura��es necess�rias

library(plyr)
library(dplyr)
library(rvest)
library(RSelenium)


# iniciar um SERVIDOR SELENIUM E O NAVEGADOR
#colocar a mesma vers�o que fez download
rD <- rsDriver(browser="chrome", port=4546L, chromever = "89.0.4389.23")

remDr <- rD[["client"]] #NAVEGADOR


################# P�gina Inicial da empresa (informa��es b�sicas)
info_basicas<-data.frame()

link_lojas<-c("https://www.reclameaqui.com.br/empresa/americanas-com-loja-online/",
              "https://www.reclameaqui.com.br/empresa/casas-bahia-loja-online/",
              "https://www.reclameaqui.com.br/empresa/ponto-frio-loja-online/",
              "https://www.reclameaqui.com.br/empresa/magazine-luiza-loja-online/",
              "https://www.reclameaqui.com.br/empresa/carrefour-loja-online/")


for(link_loja in link_lojas){
  # link_loja<-link_lojas[1]  
  remDr$navigate(link_loja)
  
  # remDr$navigate("https://www.reclameaqui.com.br/empresa/americanas-com-loja-online/")
  
  
  #ler todo o c�digo
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
  
  tabela_empresa <- data.frame(empresa,indice_solucao,nao_respondidas,
                               avaliadas,nota,nota_consumidor,percent_voltariam,
                               qnt_reclamacoes,link_loja)
  
  info_basicas <-rbind(tabela_empresa,info_basicas)
  
}

############## FIM DAS INFORMA��ES B�SICAS ######


############## COME�O DA BUSCA POR RECLAMA��ES #####

link_reclamacoes<-paste0(link_lojas,"lista-reclamacoes/")
# 
info_reclamacoes <-data.frame()

for(link_pagina in link_reclamacoes){
  
  for(pagina in 1:100){
    # remDr$navigate("https://www.reclameaqui.com.br/empresa/americanas-com-loja-online/lista-reclamacoes/?pagina=1")
    
    remDr$navigate(paste0(link_pagina,"?pagina=",pagina))
    
    #ler todo o c�digo
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
    
    info_pagina<-data.frame(empresa,titulo_reclamacao,
                            status_reclamacao,local_reclamacao,tempo_reclamacao)
    info_reclamacoes <-rbind(info_pagina,info_reclamacoes)
    
  }
}

############## FIM DA BUSCA POR RECLAMA��ES ######

############## INICIO RECLAMA��ES - AVALIADAS #############

info_reclamacoes_avaliadas <-data.frame()

for(link_pagina in link_reclamacoes){
  
  for(pagina in 1:100){
    teste_logico <-FALSE
    while(teste_logico == FALSE){
      # remDr$navigate("https://www.reclameaqui.com.br/empresa/americanas-com-loja-online/lista-reclamacoes/?pagina=1&status=EVALUATED")
      
      remDr$navigate(paste0(link_pagina,"?pagina=",pagina,"&status=EVALUATED"))
      
      #ler todo o c�digo
      codigo_reclamacoes_avaliadas<- read_html(remDr$getPageSource()[[1]])
      
      empresa<-codigo_reclamacoes_avaliadas %>% 
        html_nodes(".company-title .ng-binding") %>%
        html_text()
      
      titulo_reclamacao<-codigo_reclamacoes_avaliadas %>% 
        html_nodes(".text-title") %>%
        html_text()
      
      status_reclamacao<-codigo_reclamacoes_avaliadas %>% 
        html_nodes(".status-text") %>%
        html_text()
      
      local_reclamacao<-codigo_reclamacoes_avaliadas %>% 
        html_nodes("#complains-anchor-top .hidden-xs.ng-binding") %>%
        html_text()
      
      
      tempo_reclamacao<-codigo_reclamacoes_avaliadas %>% 
        html_nodes(".hourAgo") %>%
        html_text()
      
      teste_logico <- length(empresa)>0 & length(titulo_reclamacao)>0 &
        length(status_reclamacao)>0 & length(local_reclamacao)>0 &
        length(tempo_reclamacao)>0
    }
    info_pagina<-data.frame(empresa,titulo_reclamacao,
                            status_reclamacao,local_reclamacao,tempo_reclamacao)
    info_reclamacoes_avaliadas <-rbind(info_pagina,info_reclamacoes_avaliadas)
  }
}

############ FIM DA BUSCA AVALIADAS ###################


remDr$close()
rD$server$stop()
rm(rD)

write.table(info_basicas,"info_basicas_bruto.txt",sep = "\t",fileEncoding = "utf8",row.names = F)

write.table(info_reclamacoes,"info_reclamacoes_bruto.txt",sep = "\t",fileEncoding = "utf8",row.names = F)

write.table(info_reclamacoes_avaliadas,"info_reclamacoes_avaliadas_bruto.txt",sep = "\t",fileEncoding = "utf8",row.names = F)

### SUBIR PARA O GITHUB
