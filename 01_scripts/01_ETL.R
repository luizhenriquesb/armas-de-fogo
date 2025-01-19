
rm(list = ls())

# 1. Pacotes --------------------------------------------------------------

library(tidyverse)
library(haven)
library(janitor)
library(labelled)
library(glue)

# 2. Importação -----------------------------------------------------------

p01601 <- read_sav("00_dados/01601.zip") |> clean_names()
p02528 <- read_sav("00_dados/02528.zip") |> clean_names() 
p03372 <- read_sav("00_dados/03372.zip") |> clean_names()
p03836 <- read_sav("00_dados/03836.zip") |> clean_names()
p03925 <- read_sav("00_dados/03925.zip") |> clean_names()
p03926 <- read_sav("00_dados/03926.zip") |> clean_names()
p04405 <- read_sav("00_dados/04405.zip") |> clean_names()
p04422 <- read_sav("00_dados/04422.zip") |> clean_names()
p04572 <- read_sav("00_dados/04572.zip") |> clean_names()
p04696 <- read_sav("00_dados/04696.zip") |> clean_names()
p04714 <- read_sav("00_dados/04714.zip") |> clean_names()
p04715 <- read_sav("00_dados/04715.zip") |> clean_names()
p04716 <- read_sav("00_dados/04716.zip") |> clean_names()
p04745 <- read_sav("00_dados/04745.zip") |> clean_names()
p04843 <- read_sav("00_dados/04843.zip") |> clean_names()
lapop2019 <- read_dta("00_dados/lapop2018_2019/Brazil LAPOP AmericasBarometer 2019 v1.0_W.dta") |> clean_names()
lapop2012 <- read_dta("00_dados/lapop_2012/54861031Brazil LAPOP AmericasBarometer 2012 Rev1_W.dta") |> clean_names()
lat_barometro2011 <- read_dta("00_dados/F00004339-Latinobarometro_2011_dta/Latinobarometro_2011_esp.dta") |> clean_names()

# 3. Seleção das colunas --------------------------------------------------

p01601 <- p01601 |> 
  select(p39, p40, p41) |>
  mutate(
    id_pesquisa = "01601",
    instituto = "Data Folha",
    tipo = NA_character_,
    nome_pesquisa = "AVALIAÇÃO PRESIDENTE FERNANDO HENRIQUE - 
    INTENÇÃO DE VOTO PARA PRESIDENTE - 
    CRISE NA SEGURANÇA PÚBLICA",
    data_inicio = "02-2002",
    data_fim = "02-2002",
    tam_amostra = 3857,
    universo = "Eleitores de 16 anos ou mais",
    local= "Brasil"
  )

p02528 <- p02528 |> 
  select(p3, p4, p5, p6, p9, p29, p30) |>
  mutate(
    id_pesquisa = "02528",
    instituto = "Data Folha",
    tipo = NA_character_,
    nome_pesquisa = "
    AVALIAÇÃO DO PRESIDENTE LULA - 
    REFERENDO ARMAS DE FOGO - 
    INTENÇÃO DE VOTO PRESIDENTE 2006 - 
    CORRUPÇÃO NO GOVERNO LULA - 
    LULA E O PT - 
    AVALIAÇÃO DOS SENADORES E DEPUTADOS FEDERAIS –
    CONGRESSO - 
    PROGRAMAS SOCIAIS DO GOVERNO LULA",
    data_inicio = "10-2005",
    data_fim = "10-2005",
    tam_amostra = 2537,
    universo = "Eleitores de 16 anos ou mais",
    local= "Brasil"
  )

p03372 <- p03372 |> 
  select(p16a) |>
  mutate(
    id_pesquisa = "03372",
    instituto = "Data Folha",
    tipo = NA_character_,
    nome_pesquisa = "AVALIAÇÃO DA PRESIDENTE DILMA APÓS DOIS ANOS DE GOVERNO",
    data_inicio = "12-2012",
    data_fim = "12-2012",
    tam_amostra = 2588,
    universo = "Eleitores de 16 anos ou mais",
    local= "Brasil"
  )

p03836 <- p03836 |> 
  select(p15a) |>
  mutate(
    id_pesquisa = "03836",
    instituto = "Data Folha",
    tipo = NA_character_,
    nome_pesquisa = "INTENÇÃO DE VOTO PARA PRESIDENTE AVALIAÇÃO DA PRESIDENTE DILMA",
    data_inicio = "09-2014",
    data_fim = "09-2014",
    tam_amostra = 10054,
    universo = "Eleitores de 16 anos ou mais",
    local= "Brasil"
  )

p03925 <- p03925 |> 
  select(p18a) |>
  mutate(
    id_pesquisa = "03925",
    instituto = "Data Folha",
    tipo = NA_character_,
    nome_pesquisa = "AVALIAÇÃO DA PRESIDENTE DILMA",
    data_inicio = "10-2013",
    data_fim = "10-2013",
    tam_amostra = 2517,
    universo = "Eleitores de 16 anos ou mais",
    local= "Brasil"
  )

p03926 <- p03926 |> 
  select(p15a) |>
  mutate(
    id_pesquisa = "03926",
    instituto = "Data Folha",
    tipo = NA_character_,
    nome_pesquisa = "AVALIAÇÃO DA PRESIDENTE DILMA",
    data_inicio = "11-2013",
    data_fim = "11-2013",
    tam_amostra = 4557,
    universo = "Eleitores de 16 anos ou mais",
    local= "Brasil"
  )

p04405 <- p04405 |> 
  select(p28) |>
  mutate(
    id_pesquisa = "04405",
    instituto = "Data Folha",
    tipo = NA_character_,
    nome_pesquisa = "INTENÇÃO DE VOTO PARA PRESIDENTE/AVALIAÇÃO DO PRESIDENTE TEMER - 1 ANO E 6 MESES",
    data_inicio = "11-2017",
    data_fim = "11-2017",
    tam_amostra = 2761,
    universo = "Eleitores de 16 anos ou mais",
    local= "Brasil"
  )

p04422 <- p04422 |> 
  select(p20a) |>
  mutate(
    id_pesquisa = "04422",
    instituto = "Data Folha",
    tipo = NA_character_,
    nome_pesquisa = "INTENÇÃO DE VOTO PARA PRESIDENTE/AVALIAÇÃO DO PRESIDENTE TEMER - 1 ANO E 1 MÊS",
    data_inicio = "06-2017",
    data_fim = "06-2017",
    tam_amostra = 2771,
    universo = "Eleitores de 16 anos ou mais",
    local= "Brasil"
  )

p04572 <- p04572 |> 
  select(p23) |>
  mutate(
    id_pesquisa = "04572",
    instituto = "Data Folha",
    tipo = NA_character_,
    nome_pesquisa = "INTENÇÃO DE VOTO PARA PRESIDENTE",
    data_inicio = "08-2018",
    data_fim = "08-2018",
    tam_amostra = 8433,
    universo = "Eleitores de 16 anos ou mais",
    local= "Brasil"
  )

p04696 <- p04696 |> 
  select(p23a) |>
  mutate(
    id_pesquisa = "04696",
    instituto = "Data Folha",
    tipo = NA_character_,
    nome_pesquisa = "INTENÇÃO DE VOTO PARA PRESIDENTE - 2º TURNO",
    data_inicio = "10-2018",
    data_fim = "10-2018",
    tam_amostra = 9173,
    universo = "População adulta do Brasil",
    local= "Brasil"
  )

p04714 <- p04714 |> 
  select(p40) |>
  mutate(
    id_pesquisa = "04714",
    instituto = "Data Folha",
    tipo = NA_character_,
    nome_pesquisa = "Expectativa do governo Bolsonaro",
    data_inicio = "12-2018",
    data_fim = "12-2018",
    tam_amostra = 2077,
    universo = "População acima de 16 anos",
    local= "Brasil"
  )

p04715 <- p04715 |> 
  select(p34, p42c, p55, p56, p57) |>
  mutate(
    id_pesquisa = "04715",
    instituto = "Data Folha",
    tipo = NA_character_,
    nome_pesquisa = "AVALIAÇÃO DO PRESIDENTE JAIR BOLSONARO 100 DIAS",
    data_inicio = "04-2019",
    data_fim = "04-2019",
    tam_amostra = 2086,
    universo = "População acima de 16 anos",
    local= "Brasil"
  )

p04716 <- p04716 |> 
  select(p32, p33) |>
  mutate(
    id_pesquisa = "04716",
    instituto = "Data Folha",
    tipo = NA_character_,
    nome_pesquisa = "AVALIAÇÃO DO PRESIDENTE JAIR BOLSONARO 6 MESES",
    data_inicio = "06-2019",
    data_fim = "06-2019",
    tam_amostra = 2086,
    universo = "População acima de 16 anos",
    local= "Brasil"
  )

p04745 <- p04745 |> 
  select(p1b, p2b01, p2b02, p2b03) |>
  mutate(
    id_pesquisa = "04745",
    instituto = "IPEC",
    tipo = NA_character_,
    nome_pesquisa = "
    AVALIAÇÃO DO GOVERNO JAIR BOLSONARO -
    ARMA DE FOGO NO BRASIL -
    AUXILIO EMERGENCIAL -
    PANDEMIA DO CORONAVIRUS",
    data_inicio = "02-2021",
    data_fim = "02-2021",
    tam_amostra = 2002,
    universo = "População acima de 16 anos",
    local= "Brasil"
  )

p04843 <- p04843 |> 
  select(p901a) |>
  mutate(
    id_pesquisa = "04843",
    instituto = "Data Folha",
    tipo = NA_character_,
    nome_pesquisa = "PERFIL IDEOLÓGICO",
    data_inicio = "06-2023",
    data_fim = "06-2023",
    tam_amostra = 2010,
    universo = "Eleitores de 16 anos ou mais",
    local= "Brasil"
  )

lapop2019 <- lapop2019 |> 
  select(arm2) |>
  mutate(
    id_pesquisa = "Lapop-2019",
    instituto = "IBOPE",
    tipo = "face-to-face interviews",
    nome_pesquisa = "AmericasBarometer 2018/19: Brazil",
    data_inicio = "29-01-2019",
    data_fim = "03-03-2019",
    tam_amostra = 2010,
    universo = "Eleitores de 16 anos ou mais",
    local= "Brasil"
  )

lapop2012 <- lapop2012 |> 
    select(arm1, arm2) |>
    mutate(
      id_pesquisa = "Lapop-2012",
      instituto = "Vox Populi",
      tipo = "face-to-face interviews",
      nome_pesquisa = "AmericasBarometer 2012: Brazil",
      data_inicio = "01-03-2012",
      data_fim = "18-04-2012",
      tam_amostra = 1500,
      universo = "Eleitores de 16 anos ou mais",
      local= "Brasil"
    )

lat_barometro2011 <- lat_barometro2011 |> 
    select(p82n) |>
    mutate(
      id_pesquisa = "Latino Barómetro-2011",
      instituto = "IBOPE Inteligencia Brasil",
      tipo = "face-to-face interviews",
      nome_pesquisa = "Latino Barómetro-2011",
      data_inicio = "04-09-2011",
      data_fim = "06-10-2011",
      tam_amostra = 1204,
      universo = "Eleitores de 16 anos ou mais",
      local= "Brasil"
    )

# 4. Consolidação ---------------------------------------------------------

# Pega todos os dfs
dfs <- mget(ls())

# Consolida os dfs
consolidada <- bind_rows(dfs)
# Nota: ao consolidar, um conflito de labels é identificado (rotulos diferentes
# em uma mesma label). Mas isso não será problema para os nossos propósitos

# Cria o vetor de IDs únicos
vetor_ids <- consolidada |> 
  distinct(id_pesquisa) |> 
  pull()

# Inicializa a lista vazia
lista <- list()

# Itera sobre cada ID no vetor
for (id in vetor_ids) {
  # Filtra, seleciona  e transforma o data frame
  df <- consolidada |> 
    filter(id_pesquisa == id) |> 
    select(where(~ !all(is.na(.)))) |> 
    mutate(
      across(
        .cols = -c(
          id_pesquisa, instituto, nome_pesquisa, data_inicio, data_fim, tam_amostra, universo, local
        ),
        .fns = ~ attr(., "label")
      )
    ) |> 
    distinct_all() |> 
    pivot_longer(
      cols = -c(id_pesquisa, instituto, nome_pesquisa, data_inicio, data_fim, tam_amostra, universo, local),
      names_to = "cod_pergunta",
      values_to = "pergunta"
    )
  
  # Adiciona o data frame resultante à lista
  lista[[as.character(id)]] <- df
}

# Consolida tudo de novo
lista_total <- bind_rows(lista)

consolidada2 <- consolidada |> 
  mutate(
    across(
      .cols = -c(id_pesquisa, instituto, nome_pesquisa, data_inicio, data_fim, tam_amostra, universo, local),
      .fns = as_factor
    )
  ) |> 
  pivot_longer(
    cols = -c(id_pesquisa, instituto, nome_pesquisa, data_inicio, data_fim, tam_amostra, universo, local),
    names_to = "cod_pergunta",
    values_to = "alternativas"
  ) |> 
  count(
    id_pesquisa, 
    instituto, 
    nome_pesquisa, 
    data_inicio, 
    data_fim, 
    tam_amostra, 
    universo, 
    local, 
    cod_pergunta, alternativas
  )
  
final <- left_join(
  x = lista_total,
  y = consolidada2,
  by = c(
    "id_pesquisa",
    "instituto",
    "nome_pesquisa",
    "data_inicio", 
    "data_fim",
    "tam_amostra",
    "universo",
    "local",
    "cod_pergunta"
  )
) |> 
  rename(
    tam_amostra_informada = tam_amostra,
    qtde_respostas = n
  ) |> 
  group_by(id_pesquisa, cod_pergunta) |> 
  mutate(tam_amostra_calculada = sum(qtde_respostas)) |> 
  ungroup()

final <- final |> 
  mutate(
    across(
      .cols = everything(),
      .fns = ~ str_replace_all(., "\\n", " ")
    )
  )

final <- final |> 
  mutate(
    across(
      .cols = everything(),
      .fns = ~ str_trim(str_squish(.))
    )
  )

# Vamos ver como ficou
final |> 
  mutate(
    ano = str_sub(data_inicio, -4),
    mes = str_sub(data_inicio, -7, -6)
  ) |> 
  distinct(instituto, nome_pesquisa, ano, mes) |> 
  arrange(ano, mes)

# 5. Controle de qualidade ------------------------------------------------

id_pesquisa_aleatorio <- final |> 
  ungroup() |> 
  distinct(id_pesquisa, cod_pergunta) |> 
  slice_sample(n=1) |> 
  pull(id_pesquisa)

cod_pergunta_aleatorio <- final |> 
  ungroup() |> 
  filter(id_pesquisa == id_pesquisa_aleatorio) |> 
  distinct(cod_pergunta) |> 
  slice_sample(n=1) |> 
  pull(cod_pergunta)

final |> 
  select(id_pesquisa, cod_pergunta, alternativas, qtde_respostas) |> 
  filter(
    id_pesquisa == id_pesquisa_aleatorio, 
    cod_pergunta == cod_pergunta_aleatorio
    ) |> 
  mutate(qtde_respostas = as.numeric(qtde_respostas)) |> 
  arrange(qtde_respostas)

get(paste0("p", id_pesquisa_aleatorio)) |>
  filter(id_pesquisa == id_pesquisa_aleatorio) |> 
  count(get(cod_pergunta_aleatorio)) |> 
  arrange(n)

# 5. Exportação -----------------------------------------------------------

writexl::write_xlsx(
  x = final,
  path = "02_outputs/pesquisas-infos.xlsx"
)

write_csv(
  x = final,
  file = "02_outputs/pesquisas-infos.csv"
)