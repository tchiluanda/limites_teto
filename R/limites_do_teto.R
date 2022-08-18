library(ckanr)
library(tabulizer)
library(janitor)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)





processa_tabela<- function(.data){

  colunas<- .data[1,]

  .data <- .data[-1,]

  names(.data)<- colunas

  .data<- janitor::clean_names(.data)

  .data<-
    .data %>%
    pivot_longer(cols = -(1:2), names_to = "referencia", values_to = "valor") %>%
    mutate(valor = str_replace_all(valor,"[.]",""),
           valor = str_replace_all(valor,",","."),
           valor = as.numeric(valor),
           data_processamento = data_atualizacao)

  .data
}

processa_teto<- function(){

  tb_ckan<-ckanr::resource_show(id="08e935cd-70df-45e4-9d72-a5eb616f71c4",url="https://www.tesourotransparente.gov.br/ckan/")


  URL_add<- tb_ckan$url

  data_atualizacao<- tb_ckan$last_modified

  tmp <- tempfile(fileext = ".pdf")
  download.file(URL_add,mode = "wb", destfile = tmp)


  tabela_teto<- tabulizer::extract_tables(tmp)

  limites_atualizados<-
    as_tibble(tabela_teto[[1]]) %>%
    processa_tabela()

  limites_efetivos<-
    as_tibble(tabela_teto[[2]]) %>%
    processa_tabela()

  list(limites_atualizados = limites_atualizados, limites_efetivos= limites_efetivos)

}

limites_teto<- processa_teto()


limites_teto$limites_atualizados%>%
  write_csv("limites_teto_atualizados.csv")


limites_teto$limites_efetivos %>%
  write_csv("limites_teto_efetivos.csv")




