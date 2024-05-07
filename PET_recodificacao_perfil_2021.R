# Tratamento banco de dados Perfil 2021

install.packages("tidyverse")
install.packages("here")
install.packages("data.table")
install.packages("readxl")
install.packages("dplyr")

library(dplyr)
library(readxl)
library(data.table)
library(here)
library(tidyverse)

perfil21_original <- read_excel("PET/Pesquisa.Ingressantes.2021.xlsx")
# visualizar o CSV
View(perfil21_original)

getwd()
# setwd("path/do/wd")

# tratamento => selecionar todas as headers
perfil21_intermediario <- perfil21_original %>%
  select(`Nr. USP`,
         `P1. Data de Nascimento`,
         `P2. Cor (Critério IBGE)`,
         `P3a. Grau de escolaridade de sua mãe (biológica ou, se não cresceu com ela, a de criação)`,
         `P3b. Grau de escolaridade de seu pai (biológico ou, se não cresceu com ele, o de criação)`,
         `P4a. Qual gênero foi atribuído a você ao nascer?`,
         `P4b. Qual é a sua identidade de gênero?`,
         `P5. Qual é a sua orientação sexual?`,
         `P6. Seu estado conjugal (situação de fato)`,
         `P7. Filhos?`)

#2.2 Renomear as perguntas para o ID delas
perfil21_intermediario <- perfil21_intermediario %>%
  rename(NUSP = `Nr. USP`,
         P1 = `P1. Data de Nascimento`,
         P2 = `P2. Cor (Critério IBGE)`,
         P3a = `P3a. Grau de escolaridade de sua mãe (biológica ou, se não cresceu com ela, a de criação)`,
         P3b = `P3b. Grau de escolaridade de seu pai (biológico ou, se não cresceu com ele, o de criação)`,
         P4a = `P4a. Qual gênero foi atribuído a você ao nascer?`,
         P4b = `P4b. Qual é a sua identidade de gênero?`,
         P5 = `P5. Qual é a sua orientação sexual?`,
         P6 = `P6. Seu estado conjugal (situação de fato)`,
         P7 = `P7. Filhos?`)

#P2
perfil21_intermediario <- perfil21_intermediario %>%
  mutate(P2_codificado = case_when(
    P2 == "Branca" ~ 1,
    P2 == "Preta" ~ 2,
    P2 == "Parda" ~ 3,
    P2 == "Amarela" ~ 4,
    P2 == "Indígena" ~ 5,
    TRUE ~ 6)) %>%
  mutate(P2_outra = ifelse(
                           P2 == "Branca" | P2 == "Preta" | P2 == "Parda" | P2 == "Amarela" | P2 == "Indígena",
                           "",
                           P2
                          )
  )

#P3a
perfil21_intermediario <- perfil21_intermediario%>%
  mutate(P3a_codificado = case_when(
    P3a == "Não frequentou escola" ~ 1,
    P3a == "de 1a. a 4a. do Fundamental" ~ 2,
    P3a == "de 5a. a 8a. do Fundamental" ~ 3,
    P3a == "Médio incompleto (1o. ou 2o. col.)" ~ 4,
    P3a == "Médio completo" ~ 5,
    P3a == "Superior incompleto" ~ 6,
    P3a == "Superior completo" ~ 7,
    P3a == "Pós-graduada" ~ 8,
    P3a == "Não sabe" ~ 9))

#P3b
perfil21_intermediario <- perfil21_intermediario %>%
  mutate(P3b_codificado = case_when(
    P3b == "Não frequentou escola" ~ 1,
    P3b == "de 1a. a 4a. do Fundamental" ~ 2,
    P3b == "de 5a. a 8a. do Fundamental" ~ 3,
    P3b == "Médio incompleto (1o. ou 2o. col.)" ~ 4,
    P3b == "Médio completo" ~ 5,
    P3b == "Superior incompleto" ~ 6,
    P3b == "Superior completo" ~ 7,
    P3b == "Pós-graduado" ~ 8,
    P3b == "Não sabe" ~ 9))

#P4a
perfil21_intermediario <- perfil21_intermediario %>%
  mutate(P4a_codificado = case_when(
    P4a == "Masculino" ~ 1,
    P4a == "Feminino" ~ 2))

#P4b
perfil21_intermediario <- perfil21_intermediario %>%
  mutate(P4b_codificado = case_when(
    P4b == "Homem CIS" ~ 1,
    P4b == "Homem TRANS" ~ 2,
    P4b == "Mulher CIS" ~ 3,
    P4b == "Mulher TRANS" ~ 4,
    P4b == "Não-binário" ~ 5,
    TRUE ~ 6)) %>%
  mutate(P4b_outra = ifelse(
                            P4b == "Homem CIS" | P4b == "Homem TRANS" | P4b == "Mulher CIS" | P4b == "Mulher TRANS" | P4b == "Não-binário",
                            "",
                            P4b
        )
  )

#P5
perfil21_intermediario <- perfil21_intermediario %>%
  mutate(P5_codificado = case_when(
    P5 == "Assexual" ~ 1,
    P5 == "Bissexual" ~ 2,
    P5 == "Heterossexual" ~ 3,
    P5 == "Homossexual" ~ 4,
    P5 == "Pansexual" ~ 5,
    TRUE ~ 6)) %>%
  mutate(P5_outra = ifelse(
                           P5 == "Assexual" | P5 == "Bissexual" | P5 == "Heterossexual" | P5 == "Homossexual" | P5 == "Pansexual",
                           "",
                           P5
                           )
  )

#P6
perfil21_intermediario <- perfil21_intermediario %>%
  mutate(P6_codificado = case_when(
    P6 == "Solteiro(a)" ~ 1,
    P6 == "Casado(a) ou mora junto" ~ 2,
    P6 == "Separado(a)" ~ 3,
    P6 == "Viúvo(a)" ~ 4))

#P7
perfil21_intermediario <- perfil21_intermediario %>%
  mutate(P7_codificado = case_when(
    P7 == "Não" ~ 9,
    P7 == "2.0" ~ 2))

#3. Criação do banco final
perfil21_codificado <- perfil21_intermediario %>%
  select(`NUSP`, `P1`, `P2_codificado`, `P2_outra`, `P3a_codificado`, `P3b_codificado`, `P4a_codificado`,
         `P4b_codificado`, `P4b_outra`, `P5_codificado`, `P5_outra`, `P6_codificado`, `P7_codificado`)
view(perfil21_codificado)
#
# #3.1 Exportação do banco final em excel
write.csv(perfil21_codificado, file = "perfil21codificado.csv")
