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
                                           'QT_INSCRITOS_PRINCIPAL_NOTURNO'
                                           'QT_INSCRITOS_PRINCIPAL_INTE',
)

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
# ...

kpi1_relacao_candidato_x_vaga_por_curso <- count(DM_CURSO, c(DM_CURSO$NO_CURSO, 
                                                             DM_CURSO$NO_IES))

                                                             # KPI 4 - Total de alunos (SUGESTÃO)
# ...
MEDIA_INSCRITOS_EAD <- round(mean(DM_CURSO$QT_INSCRITOS_PRINCIPAL_EAD, na.rm = TRUE))
MEDIA_INSCRITOS_INTEGRAL <- round(mean(DM_CURSO$QT_VAGAS_PRINCIPAL_INTEGRAL, na.rm = TRUE))
MEDIA_INSCRITOS_MATUTINO <- round(mean(DM_CURSO$QT_VAGAS_PRINCIPAL_MATUTINO, na.rm = TRUE))
MEDIA_INSCRITOS_VESPERTINO <- round(mean(DM_CURSO$QT_VAGAS_PRINCIPAL_VESPERTINO, na.rm = TRUE))
MEDIA_INSCRITOS_VESPERTINO <- round(mean(DM_CURSO$QT_VAGAS_PRINCIPAL_NOTURNO, na.rm = TRUE))

kpi1_relacao_candidato_x_vaga_por_curso <- count(mergedDF3, c(mergedDF3$NO_CURSO, 
                                                              mergedDF3$NO_IES))




