# 📉 Análisis de las Bajas Laborales en España (2016–2023)

Este proyecto explora y visualiza la evolución de las bajas laborales en España, con énfasis especial en los **trastornos mentales y del comportamiento (TMC)** como causa creciente de incapacidad temporal.

🔍 El análisis se realiza a partir de datos obtenidos mediante una solicitud de información pública, amparada en la Ley de Transparencia, al Instituto Nacional de la Seguridad Social (INSS) y la afiliación a la Seguridad Social, utilizando el lenguaje **R** y herramientas del ecosistema `tidyverse`.

---

## 📰 Publicación relacionada

Este análisis sirvió como base para el artículo periodístico en elDiario.es:

👉 **[Las bajas por salud mental se duplican y ganan peso en el total](https://www.eldiario.es/economia/bajas-trabajadores-salud-mental-duplican-ganan-peso-total_1_10480188.html)**

---

## ⚙️ ¿Qué hace el código?

El script `analisis_bajas_laborales_limpio.R` realiza los siguientes procesos:

### 📁 1. Importación y limpieza de datos
- Datos de bajas laborales por causa (CIE-10)
- Datos de bajas por profesión (CNO)
- Datos de afiliación por provincia, sexo, grupo de edad y ocupación

### 📊 2. Cálculo de indicadores
- Número total de bajas y tasa por cada 1.000 afiliados
- Duración media de las bajas (ponderada)
- Distribución por sexo, edad, grupo profesional y comunidad autónoma
- Variaciones interanuales y mensuales

### 🧠 3. Análisis enfocado en salud mental (TMC)
- Evolución anual y mensual de las bajas por salud mental
- Participación en el total de bajas
- Análisis por sexo y grupo de edad
- Tasa de incidencia sobre la población afiliada

### 🌍 4. Visualizaciones
- Gráficos enviados directamente a [Datawrapper](https://www.datawrapper.de)
- Listos para publicación en medios o informes
