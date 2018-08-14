install.packages("magrittr")
install.packages("dplyr")

library(magrittr)
library(dplyr)

# mude esse diretorio- ele deve informar onde estão os arquivos csv
setwd('C:/Users/logonrmlocal/Documents/trabalho_geoanalise_r/warehouse')

DM_IES <- read.csv(file = 'DM_IES.csv', 
                   header = TRUE, 
                   stringsAsFactors = FALSE, 
                   fileEncoding = "latin1", 
                   sep = ";" )

DM_IES[is.na(DM_IES)] == 0

View(DM_IES)


DM_IES_FILTERED <- subset.data.frame(x = DM_IES,
                                     subset = DM_IES$SGL_UF_IES == "RS") %>% select('CO_IES', 
                                                                                    'SGL_UF_IES', 
                                                                                    'NO_IES',
                                                                                    'NO_MUNICIPIO_IES')
DM_DOCENTE <- read.csv(file = 'DM_DOCENTE.csv',
                       header = TRUE,
                       stringsAsFactors = FALSE,
                       fileEncoding = "latin1",
                       sep = ";" ) %>% select('CO_IES', 
                                              'DS_COR_RACA_DOCENTE', 
                                              'DS_SEXO_DOCENTE', 
                                              'DS_ORGANIZACAO_ACADEMICA',
                                              'DS_ESCOLARIDADE_DOCENTE')

DM_CURSO <- read.csv(file = 'DM_CURSO.csv',
                     header = TRUE,
                     stringsAsFactors = FALSE,
                     fileEncoding = "latin1",
                     sep = ";") %>% select('CO_IES',
                                           'NO_CURSO', 
                                           'DT_INICIO_FUNCIONAMENTO',
                                           'QT_INSCRITOS_PRINCIPAL_EAD',
                                           'QT_VAGAS_PRINCIPAL_EAD',
                                           'QT_VAGAS_PRINCIPAL_INTEGRAL',
                                           'QT_VAGAS_PRINCIPAL_MATUTINO',
                                           'QT_VAGAS_PRINCIPAL_NOTURNO',
                                           'QT_VAGAS_PRINCIPAL_VESPERTINO',
                                           'QT_INSCRITOS_PRINCIPAL_MATU',
                                           'QT_INSCRITOS_PRINCIPAL_VESP',
                                           'QT_INSCRITOS_PRINCIPAL_NOTURNO',
                                           'QT_INSCRITOS_PRINCIPAL_INTE')


DM_IGC <- setNames(read.csv(file = 'DM_IGC.csv',
                            header = TRUE,
                            stringsAsFactors = FALSE,
                            fileEncoding = "latin1",
                            sep = ";") %>% select('co_ies',
                                                  'IGC_FAIXA'), c("CO_IES", 
                                                                  "IGC_FAIXA"))

mergedDF1 <- merge(x = DM_IES_FILTERED, 
                   y = DM_DOCENTE, 
                   by = "CO_IES")

mergedDF2 <- merge(x = mergedDF1, 
                   y = DM_CURSO, 
                   by = "CO_IES")

mergedDF3 <- merge(x = mergedDF2, 
                   y = DM_IGC, 
                   by = "CO_IES")

View(mergedDF3)

#Changed NA to 0
mergedDF3 <- data.frame(mergedDF3)
mergedDF3[is.na(mergedDF3)] <- 0

# KPI 1 - Percentual de professores por sexo

kpi1_qtd_docente_por_sexo <- count(mergedDF3, c(DS_SEXO_DOCENTE))
kpi1_qtd_docente_por_sexo$percent <- prop.table(kpi1_qtd_docente_por_sexo$n) * 100
View(kpi1_qtd_docente_por_sexo)

# KPI 2 - Percentual de professores por escolaridade

kpi1_qtd_docente_por_escolaridade <- count(mergedDF3, c(mergedDF3$DS_ESCOLARIDADE_DOCENTE))
kpi1_qtd_docente_por_escolaridade$percent <- prop.table(kpi1_qtd_docente_por_escolaridade$n) * 100
View(kpi1_qtd_docente_por_escolaridade)

# KPI 3 - Percentual de professores por raça

kpi1_qtd_docente_por_raca <- count(mergedDF3, c(mergedDF3$DS_COR_RACA_DOCENTE))
kpi1_qtd_docente_por_raca$percent <- prop.table(kpi1_qtd_docente_por_raca$n) * 100
View(kpi1_qtd_docente_por_raca)

# KPI 4 - Total de alunos (SUGESTÃO)

kp1_qtd_aluno_vespertino_ead_por_curso <- sum(mergedDF3, c(mergedDF3$NO_IES,
                                                             mergedDF3$QT_VAGAS_PRINCIPAL_EAD)))

View(kp1_qtd_aluno_vespertino_ead_por_curso)

