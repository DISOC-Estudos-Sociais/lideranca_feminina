
##############################################
##############################################
###Coleta de dados do mercado de trabalho e ocupações entre mulheres PNADC
#(21/01/2024)



#Limpando o diretório
rm(list = ls()) #Libera memória, apagando os objetos abertos no R.
gc() #Libera a memoria RAM que não esta sendo utilizada pelo computador:
options(scipen=999) #Remove nota??o cient?fica.

#Checagem do diretório
getwd()




#Instalando pacote de plotagem do Excel

install.packages("clipr")

library(clipr)

#carregando pacote que baixa arquivo direto para excel

library(writexl)

#instalando pacote para mexer com data frames

install.packages("dplyr")

### Carregando os pacotes #
library(PNADcIBGE)
library(tidyverse)
library(survey)
library(srvyr)
library(dplyr)

### Vetor para baixar apenas variáveis selecionadas

variaveis = c("Ano",
              "Trimestre",
              "UF",
              "Capital",
              "RM_RIDE",
              "V1023",  # Interior e Região Metropolitana
              "V2007",  # sexo
              "V2010",  # cor
              "V2009",  # idade
              "V3001",  # sabe ler e escrever
              "V3002",  # frequenta escola
              "V3002A", # escola que frequenta (publica x privada)
              "V3003",  # curso que frequenta
              "V3003A", # curso que frequenta
              "V4010",  # Código de ocupação
              "V4012",  # Posição na ocupação
              "VD3004", # etapa de ensino concluída
              "VD3005", # anos de estudo
              "VD4001", # Condição em relação à força de trabalho
              "VD4002", # Condição de ocupação
              "VD4009", # Tipo de Ocupação
              "VD4010", # Setor da economia
              "VD4011", # Grupamento por cargos 
              "VD4019", # Rendimento mensal habitual de todos os trabalhos
              "VD4020") # Rendimento mensal efetivo de todos os trabalhos


### Carregando os dados direto do IBGE #

pnadc = get_pnadc(year = 2019,
                  quarter = 4,
                  vars = variaveis,
                  labels = FALSE,
                  deflator = TRUE,
                  design = TRUE)




###--------------------------------------------------------------------------###
# Função "as_survey" do pacote srvyr permite usar códigos tidyverse
pnadc24t4 <- as_survey(pnadc)

class(pnadc24t4)


###--------------------------------------------------------------------------###
### Filtrar faixa etária idade p/ trabalhar e pessoas ocupadas

mulheres <- pnadc24t4 |> srvyr::filter (V2009 >= 14 & VD4002=="Pessoas ocupadas")




### Criar variáveis auxiliares

mulheres <- mulheres %>% 
  mutate(BR = 1,
         NE = ifelse(UF %in% 21:29, 1, NA),
         CE = ifelse(UF == 23, 1, NA),
         
         # variáveis de faixa etária dentro do grupo de jovens
         fetaria = factor(case_when(V2009 %in% 15:17 ~ "15-17",
                                    V2009 %in% 18:24 ~ "18-24",
                                    V2009 %in% 25:29 ~ "25-29",
                                    V2009 %in% 15:29 ~ "15-29")),
         
         sexo = factor(case_when(V2007==1 ~ "homem",
                                 V2007==2 ~ "mulher")),
         
         cor = factor(case_when(V2010==1 ~ "branco ",
                                V2010==2 | V2010==4 ~ "preto ou pardo",
                                V2010==5 | V2010==3 ~ "indigena ou asiático")),
         
         rec_geo = factor(case_when(Capital==23 ~ "Capital",
                                    V1023==2 ~ "RMF",
                                    V1023==4 ~ "Interior")),
         
         
         ## variáveis de analise educacional
         
         estuda = factor(case_when(V3002==1 ~ "estuda",
                                   V3002==2 ~ "nao estuda")),
         
         analfab = ifelse(V3001=="2", 1, 0),
         
         escolaridade = as.factor(VD3004),
         
         anos_estudo = as.numeric (VD3005),
         
         ## variáveis de mercado de trabalho
         
         forcatrab = factor(case_when(VD4001==1 ~ "forca trabalho",
                                      VD4001==2 ~ "fora da forca")),
         
         #ocupado = factor(case_when(VD4002==1 ~ "ocupado",
                                    #VD4002==2 ~ "desocupado")),
         
         ativ_ocup = factor(case_when(VD4001==1 & VD4002==1 ~ "ocupado",
                                      VD4001==1 & VD4002==2 ~ "desocupado",
                                      VD4001==2 ~ "inativo")),
         
         condicao = factor(case_when(
           V3002==1 & (VD4001==1 & VD4002==1) ~ "estuda e trabalha",
           (V3002==1 & (VD4001==1 & VD4002==2)) | (V3002==1 & VD4001==2) ~ "apenas estuda",
           V3002==2 & VD4002==1 ~ "apenas trabalha",
           (V3002==2 & (VD4001==1 & VD4002==2)) | (V3002==2 & (VD4001==2)) ~ "nem-nem")),
         
         
         #Condição de informalidade
         
         informal = factor (ifelse(VD4009=="02"| VD4009=="04" | VD4009=="06" | VD4009=="09", 1, 0)),
         
         
         #Setor da economia
         
         
         setor_economia = factor(case_when(VD4010=="01" ~ "Agricultura",
                                           VD4010=="02" ~ "Indústria",
                                           VD4010=="03" ~ "Construção",
                                           VD4010=="04" ~ "Comércio, reparação de veículos",
                                           VD4010=="05" ~ "Transportador, armazenagem e correio",
                                           VD4010=="06" ~ "Alojamento e alimentação",
                                           VD4010=="07" ~ "Atividades financeiras, imobiliarias e adm",
                                           VD4010=="08" ~ "Administração publica, seguridade social",
                                           VD4010=="09" ~ "Educação, Saúde e serviços sociais",
                                           VD4010=="10" ~ "Outros Serviços",
                                           VD4010=="11" ~ "Serviços domésticos",
                                           VD4010=="12" ~ "Mal definidas")),
         
         # rendimentos
         rendim_trabh = ifelse(!is.na(VD4019), (VD4019 * Habitual), NA),
         rendim_trabe = ifelse(!is.na(VD4020), (VD4020 * Efetivo), NA)
  )


### Acrescentar a parte de tipo de ocupação 

mulheres <- mulheres %>% mutate (tipo_trab = factor(case_when( V4012==1 ~ "Trabalho Domestico",
                                                               V4012==2 ~ "Militar",
                                                               V4012==3 ~ "Empregado do setor privado",
                                                               V4012==4 ~ "Empregado do setor publico",
                                                               V4012==5 ~ "Empregador",
                                                               V4012==6 ~ "Conta propria",
                                                               V4012==7 ~ "Trabalhador familiar nao remunerado")))

###Verificando variáveis recém adicionadas 

mulheres[["variables"]][["cod_ocupacao"]]


###Cargos de diretores e Gerentes de acordo com a Classificação de Ocupações para Pesquisas Domiciliares
################

## separar os cargos de diretores e gerentes

mulheres<- mulheres %>% mutate (cod_ocupacao = factor(case_when(V4010 %in% 1111:1439 ~ "diretores e gerentes",
                                                                V4010 %in% 2111:2659 ~ "profissionais da ciencia",
                                                                V4010 %in% 3111:3522 ~ "profissionais lvl medio",
                                                                V4010 %in% 4110:4419 ~ "apoio administrativo",
                                                                V4010 %in% 5111:5419 ~ "comércio/serviços",
                                                                V4010 %in% 6111:6225 ~ "trab. qualificados agro",
                                                                V4010 %in% 7111:7549 ~ "construção, mecanicas e outros",
                                                                V4010 %in% 8111:8350 ~ "instalações, maquinas e montadores",
                                                                V4010 %in% 9111:9629 ~ "ocupações elementares",
                                                                V4010 %in% 0110:0512 ~ "forças armadas, policiais e bombeiros")))
                                                                                                                       



###################################################################################################
###################################################################################################
################ Cálculo de Indicadores de diferença de ocupações de cargos de chefia INFORME/IPECE  


#####Proporção de MULHERES por código de ocupação de acordo com a CNAE

#CE
svymean(x=~cod_ocupacao, design=subset(mulheres,sexo=="mulher" & UF==23), na.rm=TRUE) %>% write_clip(dec = "," , col.names = FALSE)

svymean(x=~cod_ocupacao, design=subset(mulheres,sexo=="mulher" & UF==23), na.rm=TRUE) 

#NE
svymean(x=~cod_ocupacao, design=subset(mulheres,sexo=="mulher" & UF %in% 21:29), na.rm=TRUE)

## O comando write_clip copia para o excel pelo crtl+v
svymean(x=~cod_ocupacao, design=subset(mulheres,sexo=="mulher" & UF %in% 21:29), na.rm=TRUE) %>% write_clip(dec = ",", row.names = TRUE)


#BR
svymean(x=~cod_ocupacao, design= subset(mulheres,sexo=="mulher"),na.rm=TRUE)

svymean(x=~cod_ocupacao, design= subset(mulheres,sexo=="mulher"),na.rm=TRUE) %>% write_clip(dec = ",", row.names = TRUE)





#####Proporção de HOMENS por código de ocupação de acordo com a CNAE

#CE
svymean(x=~cod_ocupacao, design=subset(mulheres,sexo=="homem" & UF==23), na.rm=TRUE) 

svymean(x=~cod_ocupacao, design=subset(mulheres,sexo=="homem" & UF==23), na.rm=TRUE) %>% write_clip(dec = ",", row.names = TRUE)


#NE
svymean(x=~cod_ocupacao, design=subset(mulheres,sexo=="homem" & UF %in% 21:29), na.rm=TRUE) 

svymean(x=~cod_ocupacao, design=subset(mulheres,sexo=="homem" & UF %in% 21:29), na.rm=TRUE) %>% write_clip(dec = "," , row.names = TRUE) 

#BR
svymean(x=~cod_ocupacao, design= subset(mulheres,sexo=="homem"),na.rm=TRUE)

svymean(x=~cod_ocupacao, design= subset(mulheres,sexo=="homem"),na.rm=TRUE) %>% write_clip(dec = "," , row.names = TRUE)




#####Proporção entre homens e mulheres ocupando cargo de diretores e gerentes


#CE
svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & UF==23), na.rm=TRUE) 

#NE
svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & UF %in% 21:29), na.rm=TRUE) 

#BR
svymean(x=~sexo, design= subset(mulheres,cod_ocupacao=="diretores e gerentes" & BR),na.rm=TRUE)



### Ta de acordo com o separado
## Proporção entre os sexos de cargos de gereência para CE, NE, BR

chefes_mulheres <- svybys(formula=~ sexo, 
                      bys=~BR+NE+CE, 
                      design=subset(mulheres,cod_ocupacao=="diretores e gerentes" ),
                      FUN=svymean, 
                      na.rm=TRUE)
print(chefes_mulheres)





#####
##Proporção de gerência entre mulheres e homens por setor da economia para o CEARÁ


#eSSE É O CORRETO!!!!

chefes_setores <- svybys(formula=~ sexo, 
                         bys=~VD4010, 
                         design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & UF == 23),
                         FUN=svymean, 
                         na.rm=TRUE)
print(chefes_setores)


#socooorrrrrr

#print como tabela para o excel

writexl::write_xlsx(chefes_setores, 'output/cargos_gerencia.xlsx')


#TESTE 3
#### O que aparentou melhor funcionar

#Agricultura, pecuária, produção florestal, pesca e aquicultura 
svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="01" & UF==23), na.rm=TRUE) 


#INDUSTRIA GERAL
svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="02" & UF==23), na.rm=TRUE) 

#Construção
svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="03" & UF==23), na.rm=TRUE) 

#Comércio, reparação de veículos automotores e motocicletas

svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="04" & UF==23), na.rm=TRUE) 


#Transporte, armazenagem e correio

svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="05" & UF==23), na.rm=TRUE) 


#Alojamento e alimentação

svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="06" & UF==23), na.rm=TRUE) 


#Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas
svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="07" & UF==23), na.rm=TRUE) 

#Administração pública, defesa e seguridade social
svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="08" & UF==23), na.rm=TRUE) 

#Educação, saúde humana e serviços sociais
svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="09" & UF==23), na.rm=TRUE) 

#Outros Serviços (Ver de acordo com a CNAE)
svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="10" & UF==23), na.rm=TRUE) 


#Serviços Domésticos TODOS CONTENDO NAS
svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="11" & UF==23), na.rm=TRUE) 

#Atividades Mal Definidas TODAS CONTENDO NAS tbm

svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="12" & UF==23), na.rm=TRUE) 



##########
### Desenvolver a média salarial para cargos de gerência entre os gêneros 

 
#BR
tab_rend_sexoBR <- svybys(formula=~rendim_trabe, 
                            bys=~sexo, 
                            design= subset(mulheres, cod_ocupacao=="diretores e gerentes"), 
                            FUN=svymean, 
                            na.rm=TRUE)

print(tab_rend_sexoBR)


#NE
tab_rend_sexoNE <- svybys(formula=~rendim_trabe, 
                            bys=~sexo, 
                            design= subset(mulheres, cod_ocupacao=="diretores e gerentes"& UF %in% 21:29), 
                            FUN=svymean, 
                            na.rm=TRUE)

print(tab_rend_sexoNE)



#CE
tab_rendimef_sexoCE <- svybys(formula=~rendim_trabe, 
                            bys=~sexo, 
                            design= subset(mulheres, cod_ocupacao=="diretores e gerentes"& UF==23), 
                            FUN=svymean, 
                            na.rm=TRUE)

print(tab_rendimef_sexoCE)


##!!
#######
#DIFERENÇA MÉDIA SALARIAL POR SETOR DE ECONOMIA P/ O CEARÁ


## MULHERES
tab_rendimef_sexoCE <- svybys(formula=~rendim_trabe, 
                              bys=~VD4010, 
                              design= subset(mulheres, cod_ocupacao=="diretores e gerentes"& sexo == "mulher" & UF==23), 
                              FUN=svymean, 
                              na.rm=TRUE)

print(tab_rendimef_sexoCE)


#printando pro excel
writexl::write_xlsx(tab_rendimef_sexoCE, 'output/cargos_gerencia_renda.xlsx')


## HOMENS

tab_rendimef_sexoCE <- svybys(formula=~rendim_trabe, 
                              bys=~VD4010, 
                              design= subset(mulheres, cod_ocupacao=="diretores e gerentes"& sexo == "homem" & UF==23), 
                              FUN=svymean, 
                              na.rm=TRUE)

print(tab_rendimef_sexoCE)

#printando pro excel
writexl::write_xlsx(tab_rendimef_sexoCE, 'output/cargos_gerencia_renda.xlsx')


#Agricultura, pecuária, produção florestal, pesca e aquicultura 
svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="01" & UF==23), na.rm=TRUE) 


#INDUSTRIA GERAL 

tab_rendimef_ind <- svybys(formula=~rendim_trabe, 
                              bys=~sexo, 
                              design= subset(mulheres, cod_ocupacao=="diretores e gerentes" & VD4010=="02" & UF==23), 
                              FUN=svymean, 
                              na.rm=TRUE)

print(tab_rendimef_ind)


#Construção

tab_rendimef_construcao <- svybys(formula=~rendim_trabe, 
                           bys=~sexo, 
                           design= subset(mulheres, cod_ocupacao=="diretores e gerentes" & VD4010=="03" & UF==23), 
                           FUN=svymean, 
                           na.rm=TRUE)

print(tab_rendimef_construcao)



#Comércio, reparação de veículos automotores e motocicletas


tab_rendimef_comercio <- svybys(formula=~rendim_trabe, 
                                  bys=~sexo, 
                                  design= subset(mulheres, cod_ocupacao=="diretores e gerentes" & VD4010=="04" & UF==23), 
                                  FUN=svymean, 
                                  na.rm=TRUE)

print(tab_rendimef_comercio)



#Transporte, armazenagem e correio


tab_rendimef_transporte <- svybys(formula=~rendim_trabe, 
                                bys=~sexo, 
                                design= subset(mulheres, cod_ocupacao=="diretores e gerentes" & VD4010=="05" & UF==23), 
                                FUN=svymean, 
                                na.rm=TRUE)

print(tab_rendimef_transporte)



#Alojamento e alimentação

svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="06" & UF==23), na.rm=TRUE) 


#Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas
svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="07" & UF==23), na.rm=TRUE) 

#Administração pública, defesa e seguridade social
svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="08" & UF==23), na.rm=TRUE) 

#Educação, saúde humana e serviços sociais
svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="09" & UF==23), na.rm=TRUE) 

#Outros Serviços (Ver de acordo com a CNAE)
svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="10" & UF==23), na.rm=TRUE) 


#Serviços Domésticos TODOS CONTENDO NAS
svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="11" & UF==23), na.rm=TRUE) 

#Atividades Mal Definidas TODAS CONTENDO NAS tbm

svymean(x=~sexo, design=subset(mulheres,cod_ocupacao=="diretores e gerentes" & VD4010=="12" & UF==23), na.rm=TRUE) 





#####Análise utilizando a variável empregador
###########################################################################################################


##Proporção de MULHERES em situação de Empregador

#CE
svymean(x=~tipo_trab, design=subset(mulheres,sexo=="mulher" & UF==23), na.rm=TRUE) 

#NE
svymean(x=~tipo_trab, design=subset(mulheres,sexo=="mulher" & UF %in% 21:29), na.rm=TRUE) 

#BR
svymean(x=~tipo_trab, design= subset(mulheres,sexo=="mulher"),na.rm=TRUE)



##Proporção de HOMENS em situação de Empregador

#CE
svymean(x=~tipo_trab, design=subset(mulheres,sexo=="homem" & UF==23), na.rm=TRUE) 

#NE
svymean(x=~tipo_trab, design=subset(mulheres,sexo=="homem" & UF %in% 21:29), na.rm=TRUE) 

#BR
svymean(x=~tipo_trab, design= subset(mulheres,sexo=="homem"),na.rm=TRUE)




#Análise dos cargos de empregador por sexo 


#CE
svymean(x=~sexo, design=subset(mulheres,tipo_trab=="Empregador" & UF==23), na.rm=TRUE) 

#NE
svymean(x=~sexo, design=subset(mulheres,tipo_trab=="Empregador" & UF %in% 21:29), na.rm=TRUE) 

#BR
svymean(x=~sexo, design= subset(mulheres,tipo_trab=="Empregador" & BR),na.rm=TRUE)







