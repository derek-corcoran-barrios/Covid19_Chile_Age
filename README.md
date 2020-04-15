Organización de modelos para covid19 usando
================

## Paquetes necesarios para el uso completo de este repositorio

  - **tidyverse**
  - **readxl**

## Generación de bases de datos y estimación de parametros

### Cantidad de contactos diarios y matriz de contactos entre edades

Para determinar la cantidad de contactos diarios y la matriz de
contactos entre edades utilizaremos el sript
`Bases_de_datos/Matriz_de_Contacto_Edades.R`, utilizamos la base de
datos de Prem, Cook, and Jit (2017) como base recalculando para los
grupos de edad que usamos (0-25, 25-65 y 65+), las bases de datos
necesarias para la estimación de estos resultados son
`Bases_de_datos/Cantidad-de-Personas-por-Sexo-y-Edad.xlsx` y
`Bases_de_datos/contact_matrices_152_countries/MUestimates_all_locations_1.xlsx`.

El primer resultado, que encontramos en las lineas 31-37 del código y
guardadas en `Bases_de_datos/Contactos.rds` nos dice que para los grupos
antes establecidos en número de contactos diarios promedio es de 18.44,
14.3, 1.91 por persona respectivamente.

La matriz de probabilidad de contacto entre grupos etareos aparece en la
linea 64, y esta guardada como `Bases_de_datos/Age_Matrix.rds`. donde en
cada fila, se ve la probabilidad de que el grupo en cuestion este en
contacto con el de cada columna:

|             | Age\_0\_25 | Age\_25\_65 | Age\_65\_ |
| ----------- | ---------: | ----------: | --------: |
| Age\_0\_25  |       0.57 |        0.37 |      0.06 |
| Age\_25\_65 |       0.25 |        0.67 |      0.08 |
| Age\_65\_   |       0.18 |        0.37 |      0.45 |

### Estado inicial del modelo y probabilidad de contactos entre comunas

Para esto hay dos scripts generados, uno que permite generar un estado
inicial y matriz de probabilidad de viajes entre todas las comunas de
Chile `Bases_de_datos/Probabilidad_Viaje_Comunas`, y otro que lo hace
para alguna región particular de Chile. Para ambos scripts es necesario
utilzar las bases de datos `Bases_de_datos/Viajes_comunas.rds`(para los
viajes intercomunales),
`Bases_de_datos/Cantidad-de-Personas-por-Sexo-y-Edad.xlsx` (Para los
grupos de edad y poblacion por comunas) y `Bases_de_datos/Pais.rds`
(Para tener los Infectados, Acumulados y recuperados), esta última base
de datos puede ser actualizada con las primeras 17 lineas del código.

La bases de datos de Chile para el estado inicial de cada fecha se
guardará en `Bases_de_datos/Datos_Chile/df_out_2020-04-13.rds` con la
fecha

## Modelaciones

## Resultados

## Difusión

## Referencias

<div id="refs" class="references">

<div id="ref-prem2017projecting">

Prem, Kiesha, Alex R Cook, and Mark Jit. 2017. “Projecting Social
Contact Matrices in 152 Countries Using Contact Surveys and Demographic
Data.” *PLoS Computational Biology* 13 (9). Public Library of Science:
e1005697.

</div>

</div>
