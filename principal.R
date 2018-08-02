install.packages("magrittr")
install.packages("dplyr")

library(magrittr)
library(dplyr)

# mude esse diretorio- ele deve informar onde estão os arquivos csv
setwd('/home/flavio/trabalho-geoanalise/projeto-r/warehouse/')

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
                                           'DT_INICIO_FUNCIONAMENTO')

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
