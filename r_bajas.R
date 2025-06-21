# ─────────────────────────────────────────────────────────────────────────────
# ANÁLISIS DE BAJAS LABORALES EN ESPAÑA (2016–2023)
# ─────────────────────────────────────────────────────────────────────────────

# 📦 Cargar librerías
library(tidyverse)
library(readxl)
library(data.table)
library(googlesheets4)
library(lubridate)
library(ggplot2)
library(DatawRappr)
library(Hmisc)
library(zoo)

# ─────────────────────────────────────────────────────────────────────────────
# 📁 Carga de datos
# (Los archivos deben estar en el mismo directorio del proyecto)
# ─────────────────────────────────────────────────────────────────────────────

datos_CIE10 <- read_excel("transp_CIE10.xlsx")
colnames(datos_CIE10) <- c("anio", "mes", "cprov", "prov", "sexo", "cod_edad", 
                           "r_edad", "codcie", "causa", "n_bajas", "durmedia") 

datos_CIE10 <- datos_CIE10 %>% 
  mutate(
    mes = case_when(
      str_starts(mes, "ENE") ~ str_replace(mes, "ENE", "JAN"),
      str_starts(mes, "ABR") ~ str_replace(mes, "ABR", "APR"),
      str_starts(mes, "AGO") ~ str_replace(mes, "AGO", "AUG"),
      str_starts(mes, "DIC") ~ str_replace(mes, "DIC", "DEC"),
      TRUE ~ mes
    ),
    fecha = my(mes)
  )

datos_CNO <- read_excel("transp_CNO.xlsx")
colnames(datos_CNO) <- c("anio", "mes", "cprov", "prov", "sexo", "cod_edad", 
                         "r_edad", "codcno", "prof", "n_bajas", "durmedia")

datos_CNO <- datos_CNO %>% 
  mutate(
    mes = case_when(
      str_starts(mes, "ENE") ~ str_replace(mes, "ENE", "JAN"),
      str_starts(mes, "ABR") ~ str_replace(mes, "ABR", "APR"),
      str_starts(mes, "AGO") ~ str_replace(mes, "AGO", "AUG"),
      str_starts(mes, "DIC") ~ str_replace(mes, "DIC", "DEC"),
      TRUE ~ mes
    ),
    fecha = my(mes)
  )

afiliados_provincia <- read.csv("afiliados_provincia.csv", fileEncoding = "UTF-8-BOM", 
                                colClasses = c("numeric", "character", "numeric"),
                                sep=";", header = TRUE)

tot_afiliados <- read_excel("afiliados_total_mes.xlsx")
nom_provs <- read_excel("Copia de Cods_provincias.xlsx")
mes_afiliados_prov <- read_excel("mes_afiliados_prov.xlsx", sheet = 1)
afiliados_sexo <- read_excel("afiliados_sexo.xlsx", sheet = 3)
afiliados_sexo_mensuales <- read_excel("afiliados_sexo_mensuales.xlsx")
afiliados_prof <- read_excel("afiliados_prof.xlsx", sheet = 1)

# 🔗 Fuente oficial: Seguridad Social https://w6.seg-social.es/PXWeb/

# ─────────────────────────────────────────────────────────────────────────────
# 📈 Análisis principal 
# ─────────────────────────────────────────────────────────────────────────────

#Evolución tasa de bajas

evol_tot <- datos_CIE10 %>% 
  group_by(fecha, mes) %>%
  summarise(num_bajas = sum(n_bajas)) %>%
  left_join(tot_afiliados, by = c("mes" = "Periodo")) %>%
  mutate(tasa_pormil = (num_bajas / tot_afiliados)*1000) %>%
  arrange(desc(tasa_pormil))

      #gráfico
      dw_data_to_chart(evol_tot, "TzjAV")

evol_anual <- datos_CIE10 %>% 
  group_by(anio) %>%
  summarise(num_bajas = sum(n_bajas)) 

#Evol total bajas / tasa

evol_mes_anio <- evol_tot %>%
  mutate(mes = month(fecha),
    anio = year(fecha)) %>%
  group_by(mes, anio) %>%
  select(mes, anio, tasa_pormil) %>%
  pivot_wider(names_from = anio, values_from = tasa_pormil) %>%
  arrange(mes)

dw_data_to_chart(evol_mes_anio, "d1EEV")

#Evolucion de las 10 causas con más bajas en 2022

evol_top10_bajas <- datos_CIE10 %>% 
  mutate(causa = case_when(causa != "CÓDIGOS SUPUESTOS ESPECIALES" & 
                    causa != "ENFERMEDADES DEL APARATO MUSCULOESQUELÉTICO Y DEL TEJIDO CONECTIVO (M00-M99)" &
                     causa != "ENFERMEDADES DEL APARATO RESPIRATORIO (J00-J99)" & 
                     causa != "CIERTAS ENFERMEDADES INFECCIOSAS Y PARASITARIAS (A00-B99)" &
                     causa != "LESIONES TRAUMÁTICAS, ENVENENAMIENTOS Y OTRAS CONSECUENCIAS DE CAUSAS EXTERNAS (S00-T88)" &
                     causa != "SÍNTOMAS, SIGNOS Y RESULTADOS ANORMALES DE PRUEBAS COMPLEMENTARIAS, NO CLASIFICADOS BAJO OTRO CONCEPTO (R00-R99)" &
                     causa != "TRASTORNOS MENTALES Y DE COMPORTAMIENTO (F01-F99)"&
                     causa != "ENFERMEDADES DEL APARATO DIGESTIVO (K00-K95)" ~ "otros",
                     TRUE ~ causa)) %>%
  group_by(fecha, causa) %>% summarise(num_bajas_tot = sum(n_bajas)) %>% ungroup() %>%
  group_by(causa) %>% mutate(total_bajas=sum(num_bajas_tot)) %>% arrange(desc(total_bajas)) %>%
  select(-total_bajas) %>%
  pivot_wider(names_from = causa, values_from = num_bajas_tot)

      #gráfico
      dw_data_to_chart(evol_top10_bajas, "y94d6")


#Tasa de variación años contando solo hasta julio 
tasa_variacion_en_jul <- datos_CIE10 %>%
  group_by(anio) %>%
  summarise(num_bajas = sum(n_bajas[month(fecha) <=7])) %>%
  arrange(anio) %>%
  mutate(varinteranual=((num_bajas-lag(num_bajas,1))/lag(num_bajas,1)*100))

      #gráfico
      dw_data_to_chart(tasa_variacion_en_jul, "BQE8H")

#Para tabla: numero bajas 2023, tasa de variación solo hasta julio 22-23, año entero 2016-22

tv_16_22 <- datos_CIE10 %>%
  group_by(anio, codcie, causa) %>%
  summarise(num_bajas = sum(n_bajas)) %>%
  pivot_wider(names_from = anio, values_from = num_bajas) %>%
  mutate(tv_16_22 = ((`2022`-`2016`)/`2016`)* 100, values_fill=0) %>%
  filter(causa != "NA", `2023` >= 6000) %>%
  select(codcie, causa, tv_16_22)

tv_23 <- datos_CIE10 %>%
  group_by(anio, codcie, causa) %>%
  summarise(num_bajas_jul = sum(n_bajas[month(fecha) <=7])) %>%
  pivot_wider(names_from = anio, values_from = num_bajas_jul) %>%
  mutate(tvtot = ((`2023`-`2016`)/`2016`)* 100,
         tv_23_22 = ((`2023`-`2022`)/`2022`)* 100) %>%
  filter(causa != "NA", `2023` >= 6000) %>%
  select(codcie, `2023`, tv_23_22)

tabladw <- left_join(tv_16_22, tv_23, by = "codcie")

    #gráfico
    dw_data_to_chart(tabladw, "obwKp")


#Evolución de las bajas de salud mental 
    
evol_mental <- datos_CIE10 %>% 
      group_by(anio, causa) %>%
      summarise(num_bajas_tm = sum(n_bajas)) %>%
      filter(causa == "TRASTORNOS MENTALES Y DE COMPORTAMIENTO (F01-F99)") %>%
      arrange(desc(num_bajas_tm))

    #gráfico
    dw_data_to_chart(evol_mental, "G2Fyo")
    
evol_mes_mental <- datos_CIE10 %>% 
      group_by(mes, causa) %>%
      summarise(num_bajas_tm = sum(n_bajas)) %>%
      filter(causa == "TRASTORNOS MENTALES Y DE COMPORTAMIENTO (F01-F99)") %>%
      arrange(desc(num_bajas_tm))
    
#Evolucion tasa tmc 

evol_tasa_tmc <- datos_CIE10 %>% 
  group_by(fecha, mes) %>%
  summarise(num_bajas = sum(n_bajas[causa == "TRASTORNOS MENTALES Y DE COMPORTAMIENTO (F01-F99)"], na.rm=TRUE)) %>%
  left_join(tot_afiliados, by = c("mes" = "Periodo")) %>%
  mutate(tasa_pormil = (num_bajas / tot_afiliados)*1000) %>%
  arrange(desc(tasa_pormil))

#Unión para gráfico

evol_tmc <- evol_mes_mental %>%
  left_join(evol_tasa_tmc, by = "mes") 

    #gráfico 
    dw_data_to_chart(evol_tmc, "GZavg")
    
    

#Evolución del porcentaje debajas por tmc hasta julio

evol_ptm <- datos_CIE10 %>% 
  group_by(mes) %>%
  summarise(num_bajas = sum(n_bajas),
            tm = sum(n_bajas[causa=="TRASTORNOS MENTALES Y DE COMPORTAMIENTO (F01-F99)"], na.rm=TRUE)) %>%
  mutate( p_tm = (tm / num_bajas)*100) %>%
  arrange(desc(p_tm))

evol_ptm_anio <- datos_CIE10 %>% 
  group_by(anio) %>%
  summarise(num_bajas = sum(n_bajas),
            tm = sum(n_bajas[causa=="TRASTORNOS MENTALES Y DE COMPORTAMIENTO (F01-F99)"], na.rm=TRUE)) %>%
  mutate( p_tm = (tm / num_bajas)*100) %>%
  arrange(desc(p_tm))

    #gráfico
    dw_data_to_chart(evol_ptm_anio, "uh370")


#Evolución del porcentaje de tmc cada año teniendo en cuenta los datos solo hasta el mes de julio

evol_año_mes <- datos_CIE10 %>%
  group_by(anio) %>%
  summarise(num_bajas_jul = sum(n_bajas[month(fecha) <=7]),
            tm_julio = sum(n_bajas[causa == "TRASTORNOS MENTALES Y DE COMPORTAMIENTO (F01-F99)" & (month(fecha) <=7)], na.rm=TRUE)) %>%
  mutate(ptm = tm_julio / num_bajas_jul * 100)  


#Bajas por TMC en cada provincia -tasa por mil afiliados-

provs_afiliación <- datos_CIE10 %>%
  group_by(cprov, causa, anio, mes) %>%
  filter(anio==2022, causa=="TRASTORNOS MENTALES Y DE COMPORTAMIENTO (F01-F99)") %>%
  summarise(num_bajas = sum(n_bajas)) %>%
  left_join(mes_afiliados_prov, by = c("cprov", "mes")) %>%
  left_join(nom_provs, by = c("cprov" = "codprov")) %>%
  group_by(cprov, nom, nom_eldiario, causa) %>%
  summarise(m_bajas = mean(num_bajas),
            m_afiliados = mean(afiliados_prov)) %>%
  mutate(m_mental = (m_bajas / m_afiliados) * 1000) %>%
  select(cprov, nom, nom_eldiario, m_bajas, m_mental, m_afiliados) %>%
  arrange(desc(m_mental))
    
      #mapa 
      dw_data_to_chart(provs_afiliación, "C6juk")

#Tasa de bajas por provincia

provstot_afiliación <- datos_CIE10 %>%
  group_by(cprov, anio, mes) %>%
  filter(anio==2022) %>%
  summarise(num_bajas = sum(n_bajas)) %>%
  left_join(mes_afiliados_prov, by = c("cprov", "mes")) %>%
  left_join(nom_provs, by = c("cprov" = "codprov")) %>%
  group_by(cprov, nom, nom_eldiario) %>%
  summarise(m_bajas = mean(num_bajas),
            m_afiliados = mean(afiliados_prov)) %>%
  mutate(tasa_bajas = (m_bajas / m_afiliados) * 1000) %>%
  select(cprov, nom, nom_eldiario, m_bajas, tasa_bajas, m_afiliados) %>%
  arrange(desc(tasa_bajas))

      #mapa
      dw_data_to_chart(provstot_afiliación, "UOEyf")
      
#Porcentaje bajas TMC por provincia 
provs_tmc <- datos_CIE10 %>%
  group_by(cprov) %>%
  summarise(num_bajas = sum(n_bajas[anio==2023 | anio==2022]),
            tmc = sum(n_bajas[causa == "TRASTORNOS MENTALES Y DE COMPORTAMIENTO (F01-F99)" & (anio==2023 | anio==2022)], na.rm = TRUE)) %>%
  mutate(p_bajas = tmc/num_bajas * 100) %>%
  left_join(nom_provs, by = c("cprov" = "codprov")) %>%
   select(cprov, nom, nom_eldiario, p_bajas, tmc) %>%
   arrange(desc(p_bajas))

  #mapa
  dw_data_to_chart(provs_tmc, "F93in")

#Porcentaje bajas TMC por CCAA 
ccaa_tmc <- datos_CIE10 %>%
  left_join(nom_provs, by = c("cprov" = "codprov")) %>%
  group_by(codcaa, ccaa, anio) %>%
  filter(anio==2022) %>%
  summarise(num_bajas = sum(n_bajas),
            tmc = sum(n_bajas[causa == "TRASTORNOS MENTALES Y DE COMPORTAMIENTO (F01-F99)"], na.rm = TRUE)) %>%
  mutate(p_bajas = tmc/num_bajas * 100) %>%
  select(ccaa, p_bajas) %>%
  arrange(desc(p_bajas))

#Bajas tmc por sexo y edad

bajas_sexo_mental <- datos_CIE10 %>% 
  group_by(causa, sexo) %>%
  summarise(num_bajas = sum(n_bajas)) %>%
  pivot_wider(names_from = sexo, values_from = num_bajas) %>%
  mutate(p_bajas_mujeres = MUJER / (HOMBRE + MUJER) * 100,
         p_bajas_hombres = HOMBRE /(HOMBRE + MUJER) * 100 ) %>%
  filter(causa == "TRASTORNOS MENTALES Y DE COMPORTAMIENTO (F01-F99)")

bajas_sexo_edad_mental <- datos_CIE10 %>% 
  group_by(causa, sexo, cod_edad) %>%
  summarise(num_bajas = sum(n_bajas)) %>%
  pivot_wider(names_from = sexo, values_from = num_bajas) %>%
  mutate(p_bajas_mujeres = MUJER / (HOMBRE + MUJER) * 100,
         p_bajas_hombres = HOMBRE /(HOMBRE + MUJER) * 100) %>%
  filter(causa == "TRASTORNOS MENTALES Y DE COMPORTAMIENTO (F01-F99)", cod_edad != 6) %>%
  mutate(cod_edad = case_when(cod_edad == 1 ~ "de 16 a 25",
                   cod_edad == 2 ~ "de 26 a 35",
                   cod_edad == 3 ~ "de 36 a 45",
                   cod_edad == 4 ~ "de 46 a 55",
                   cod_edad == 5 ~ "de 56 a 65",
                   cod_edad == 6 ~ "otros"),
        p_m_sobre_total = MUJER / (sum(bajas_sexo_edad_mental$HOMBRE) + sum(bajas_sexo_edad_mental$MUJER)) *100,
        p_h_sobre_total = HOMBRE / (sum(bajas_sexo_edad_mental$HOMBRE) + sum(bajas_sexo_edad_mental$MUJER)) *100)

#Evol edad
evol_edad <- datos_CIE10 %>% 
  mutate(cod_edad = case_when(cod_edad == 1 ~ "de 16 a 25",
                              cod_edad == 2 ~ "de 26 a 35",
                              cod_edad == 3 ~ "de 36 a 45",
                              cod_edad == 4 ~ "de 46 a 55",
                              cod_edad == 5 ~ "de 56 a 65",
                              cod_edad == 6 ~ "otros")) %>%
  group_by(cod_edad, fecha) %>%
  summarise(num_bajas = sum(n_bajas[causa == "TRASTORNOS MENTALES Y DE COMPORTAMIENTO (F01-F99)"], na.rm=TRUE)) %>%
  pivot_wider(names_from = cod_edad, values_from = num_bajas) %>%
  mutate(media1625 = round(rollmean(`de 16 a 25`, 12, fill = NA, align = "right"), digits = 0),
         media2635 = round(rollmean(`de 26 a 35`, 12, fill = NA, align = "right"), digits = 0),
         media3645 = round(rollmean(`de 36 a 45`, 12, fill = NA, align = "right"), digits = 0),
         media4655 = round(rollmean(`de 46 a 55`, 12, fill = NA, align = "right"), digits = 0),
         media5665 = round(rollmean(`de 56 a 65`, 12, fill = NA, align = "right"), digits = 0)) %>%
  filter(year(fecha) >= 2017 )


tv_edad <- datos_CIE10 %>% 
  mutate(cod_edad = case_when(cod_edad == 1 ~ "de 16 a 25",
                              cod_edad == 2 ~ "de 26 a 35",
                              cod_edad == 3 ~ "de 36 a 45",
                              cod_edad == 4 ~ "de 46 a 55",
                              cod_edad == 5 ~ "de 56 a 65",
                              cod_edad == 6 ~ "otros")) %>%
  group_by(cod_edad, fecha) %>%
  summarise(num_bajas = sum(n_bajas[causa == "TRASTORNOS MENTALES Y DE COMPORTAMIENTO (F01-F99)"], na.rm=TRUE)) %>%
  mutate(media = round(rollmean(num_bajas, 12, fill = NA, align = "right"), digits = 0)) %>%
  filter(fecha == "2019-12-01" | fecha == "2023-07-01", cod_edad != "otros") %>%
  select(fecha, cod_edad, media) %>%
  pivot_wider(names_from = fecha, values_from = media) %>%
  mutate(tv= (`2023-07-01`-`2019-12-01`)/`2019-12-01`*100) 
  


    #gráfico
    dw_data_to_chart(evol_edad, "zI0YC")

#Evol sexo
evol_sexo <- datos_CIE10 %>%
  group_by(fecha, mes, sexo) %>%
  summarise(num_bajas = sum(n_bajas[causa == "TRASTORNOS MENTALES Y DE COMPORTAMIENTO (F01-F99)"], na.rm = TRUE)) %>%
  pivot_wider(names_from = sexo, values_from = num_bajas)

  #gráfico
  dw_data_to_chart(evol_sexo, "yuraJ")

#Tasa bajas por TMC sobre afiliados por edad y sexo

bajas_sexo_edad_incidencia_mensual <- datos_CIE10  %>%
  filter(causa == "TRASTORNOS MENTALES Y DE COMPORTAMIENTO (F01-F99)", cod_edad != 6) %>%
  mutate(grupo_edad = case_when(cod_edad == 1 ~ "de 16 a 25",
                              cod_edad == 2 ~ "de 26 a 35",
                              cod_edad == 3 ~ "de 36 a 45",
                              cod_edad == 4 ~ "de 46 a 55",
                              cod_edad == 5 ~ "de 56 a 65")) %>%
  group_by(fecha, mes, grupo_edad, sexo) %>% summarise(n_bajas=sum(n_bajas)) %>%
  pivot_wider(names_from = sexo, values_from = n_bajas) %>%
  left_join(afiliados_sexo_mensuales,  by = c("grupo_edad", "mes")) %>%
  group_by(grupo_edad) %>%
  filter(year(fecha) >= 2022) %>%
  summarise(HOMBRE=mean(HOMBRE), MUJER=mean(MUJER),
            afiliados_hombre=mean(afiliados_hombre), afiliados_mujer=mean(afiliados_mujer)) %>%
  mutate(tasa_1000mujeres = round( MUJER / afiliados_mujer *1000, 2),
         tasa_1000hombres = round( HOMBRE / afiliados_hombre *1000, 2))

      #gráfico
      dw_data_to_chart(bajas_sexo_edad_incidencia_mensual, "SXLKH")


#Evolución de las bajas por profesión

evol_prof <- datos_CNO %>% 
  group_by(anio, prof) %>%
  summarise(num_bajas = sum(n_bajas)) %>%
  pivot_wider(names_from = prof, values_from = num_bajas, values_fill=0)

mes_prof <- datos_CNO %>% 
  group_by(anio, mes, prof) %>%
  summarise(num_bajas = sum(n_bajas)) %>%
  pivot_wider(names_from = prof, values_from = num_bajas, values_fill=0)

#Que profesiones tienen más bajas (2022)

num_prof_22 <- datos_CNO %>% 
  group_by(anio, prof, codcno) %>%
  summarise(num_bajas = sum(n_bajas)) %>% ungroup() %>%
  mutate (p_sobre_total = num_bajas / sum(num_bajas)*100) %>%
  filter(anio ==2022, prof != "No Informado") %>%
  arrange(desc(num_bajas)) 

#Bajas profesión sobre afiliación

prof_22 <- datos_CNO %>% 
  group_by(mes, anio, prof, codcno) %>%
  filter(anio ==2022) %>%
  summarise(n_bajas = sum(n_bajas)) %>% ungroup() %>%
  group_by(anio, prof, codcno) %>%
  summarise(m_bajas = mean(n_bajas)) %>%
  left_join(afiliados_prof, by = "codcno")  %>%
  mutate (m_ocupados=m_ocupados*1000,
          p_sobre_total = m_bajas / m_ocupados *1000) %>%
  filter(prof != "No Informado") %>%
  arrange(desc(p_sobre_total)) 

    #gráfico
    dw_data_to_chart(prof_22, "r7K7e")


#Duración media de las bajas por causa

duracion_por_causa <- datos_CIE10 %>% 
  #filter(anio == 2022) %>%
  select(n_bajas, durmedia, causa) %>%
  group_by(causa) %>%
  summarise(num_bajas = sum(n_bajas),
    mediaponderada = round(wtd.mean(durmedia, weights=n_bajas), digits= 0)) %>%
  ungroup() %>%
  mutate(porc_bajas = round(num_bajas/sum(num_bajas)*100, digits=1)) %>%
  arrange(desc(mediaponderada)) %>%
  filter(num_bajas >= 10000, causa != "No Informado")

    #gráfico
    dw_data_to_chart(duracion_por_causa, "ccS3J")


#Y por porfesiones cuánto dura cada baja

duracion_por_prof <- datos_CNO %>% 
  select(n_bajas, durmedia, prof) %>%
  group_by(prof) %>%
  summarise(mediaponderada = round(wtd.mean(durmedia, weights=n_bajas), digits= 0)) %>%
  arrange(desc(mediaponderada)) %>% 
  filter( prof != "No Informado", mediaponderada >= 42)











