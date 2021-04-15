library(readr)

info_basicas <- read_delim("~/ESTATISTICA NA PRATICA/Conteudo/webserie_RA/info_basicas.txt", 
                           "\t", escape_double = FALSE, trim_ws = TRUE)


info_reclamacoes_avaliadas <- read_delim("~/ESTATISTICA NA PRATICA/Conteudo/webserie_RA/info_reclamacoes_avaliadas.txt", 
                                         "\t", escape_double = FALSE, trim_ws = TRUE)

analise_empresa_palavras <- read_delim("~/ESTATISTICA NA PRATICA/Conteudo/webserie_RA/analise_empresa_palavras.txt", 
                                       "\t", escape_double = FALSE, trim_ws = TRUE)

analise_geral_palavra <- read_delim("~/ESTATISTICA NA PRATICA/Conteudo/webserie_RA/analise_geral_palavra.txt", 
                                    "\t", escape_double = FALSE, trim_ws = TRUE)

qnt_palavras_empresa <- analise_empresa_palavras %>%
  select(-percentual_solucao )

qnt_palavras_geral <- analise_geral_palavra %>%
  select(-percentual_solucao ) %>%
  mutate(qnt_palavra = qnt_palavra/5)

save.image(".RData")
