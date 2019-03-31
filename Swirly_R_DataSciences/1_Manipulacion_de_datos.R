¿Qué es dplyr?
  dplyr es un poderoso paquete R para transformar y resumir datos tabulares con filas y columnas. Para otra explicación de dplyr vea la viñeta del paquete dplyr: Introducción a dplyr

¿Por qué es útil?
  El paquete contiene un conjunto de funciones (o "verbos") que realizan operaciones comunes de manipulación de datos como filtrar filas, seleccionar columnas específicas, reordenar filas, agregar nuevas columnas y resumir datos.

Además, dplyr contiene una función útil para realizar otra tarea común que es el concepto de "dividir aplicar-combinar". Vamos a discutir eso en un momento.

¿Cómo se compara con el uso de funciones base R?
  Si está familiarizado con R, probablemente esté familiarizado con las funciones básicas de R, como split (), subconjunto (), apply (), sapply (), lapply (), tapply () y aggregate (). En comparación con las funciones básicas en R, las funciones en dplyr son más fáciles de trabajar, son más consistentes en la sintaxis y están dirigidas al análisis de datos en torno a marcos de datos en lugar de solo vectores.

¿Cómo obtengo dplyr?
  
  Para instalar dplyr

install.packages("dplyr")

Cargar dplyr

library(dplyr)

Datos: los mamíferos duermen
El conjunto de datos msleep (los mamíferos duermen) contiene las horas de sueño y los pesos para un conjunto de mamíferos y está disponible en el repositorio de dagdata en github. Este conjunto de datos contiene 83 filas y 11 variables.

Descargue el conjunto de datos msleep en formato CSV desde aquí , y luego cárguelo en R:
  
  install.packages("downloader")

library(downloader)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- "msleep_ggplot2.csv"
if (!file.exists(filename)) download(url,filename)
msleep <- read.csv("msleep_ggplot2.csv")
head(msleep)

Descripcion de los datos
?msleep

Importantes verbos para recordar

verbos dplyr	Descripción
select()	seleccionar columnas
filter()	filtrar filas
arrange()	reordenar o organizar filas
mutate()	crear nuevas columnas
summarise()	resumir valores
group_by()	Permite operaciones de grupo en el concepto de "dividir-aplicar-combinar"

verbos dplyr en acción

Las dos funciones más básicas son select()y filter()que selecciona columnas y filas de filtros, respectivamente.

Seleccionando columnas usando select()

Seleccione un conjunto de columnas: el nombre y las columnas sleep_total.

sleepData <- select(msleep, name, sleep_total)
head(sleepData)

Para seleccionar todas las columnas excepto una columna específica, use el operador “-“ (resta) (también conocido como indexación negativa)

head(select(msleep, -name))

Para seleccionar un rango de columnas por nombre, use el operador “:” (dos puntos)

head(select(msleep, name:order))

Para seleccionar todas las columnas que comienzan con la cadena de caracteres "sl", use la función starts_with()

head(select(msleep, starts_with("sl")))

Algunas opciones adicionales para seleccionar columnas basadas en un criterio específico incluyen

ends_with() = Seleccionar columnas que terminan con una cadena de caracteres
contains() = Seleccionar columnas que contienen una cadena de caracteres
matches() = Seleccionar columnas que coincidan con una expresión regular
one_of() = Seleccionar nombres de columnas que sean de un grupo de nombres

Seleccionando filas usando filter()

Filtra las filas para los mamíferos que duermen un total de más de 16 horas.

filter(msleep, sleep_total >= 16)

Filtre las filas para los mamíferos que duermen un total de más de 16 horas y tienen un peso corporal de más de 1 kilogramo.

filter(msleep, sleep_total >= 16, bodywt >= 1)

Filtrar las filas de mamíferos en el orden taxonómico de Perissodactyla y Primates.

filter(msleep, order %in% c("Perissodactyla", "Primates"))

Puede usar los operadores booleanos (por ejemplo,>, <,> =, <=,! =,% En%) para crear las pruebas lógicas.

Operador de tubería:%>%
  
  Antes de continuar, presentemos el operador de tubería:%>%. dplyr importa este operador de otro paquete (magrittr). Este operador le permite canalizar la salida de una función a la entrada de otra función. En lugar de funciones de anidamiento (lectura desde adentro hacia afuera), la idea de la tubería es leer las funciones de izquierda a derecha.

Aquí hay un ejemplo que has visto:
  
  head(select(msleep, name, sleep_total))

Ahora, en este caso, canalizaremos el marco de datos msleep a la función que seleccionará dos columnas (nombre y total de sueño) y luego canalizaremos el nuevo marco de datos a la función head()que devolverá el encabezado del nuevo marco de datos.

msleep %>% 
  select(name, sleep_total) %>% 
  head()

Pronto verás cuán útil es el operador de tuberías cuando comencemos a combinar muchas funciones.

Volver a los verbos dplyr en acción.
Ahora que conoce el operador de tuberías (%>%), lo utilizaremos durante el resto de este tutorial.

Organizar o reordenar filas usando arrange()
Para organizar (o reordenar) filas por una columna particular, como el orden taxonómico, enumere el nombre de la columna en la que desea organizar las filas por

msleep %>% arrange(order) %>% head

Ahora, seleccionaremos tres columnas de msleep, organizaremos las filas por orden taxonómico y luego las organizaremos por sleep_total. Finalmente mostrar el encabezado del cuadro de datos final.

msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>% 
  head

Igual que el anterior, excepto que aquí filtramos las filas para los mamíferos que duermen durante 16 horas o más en lugar de mostrar el encabezado del marco de datos final

msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>% 
  filter(sleep_total >= 16)

Algo un poco más complicado: igual que arriba, excepto organizar las filas en la columna sleep_total en orden descendente. Para ello, utiliza la función.desc()

msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, desc(sleep_total)) %>% 
  filter(sleep_total >= 16)

Crear nuevas columnas usando mutate()
La mutate()función agregará nuevas columnas al marco de datos. Cree una nueva columna llamada rem_proportion que es la relación entre el sueño rem y la cantidad total de sueño.

msleep %>% 
  mutate(rem_proportion = sleep_rem / sleep_total) %>%
  head

Puede muchas columnas nuevas usando mutar (separadas por comas). Aquí agregamos una segunda columna llamada bodywt_grams que es la columna bodywt en gramos.

msleep %>% 
  mutate(rem_proportion = sleep_rem / sleep_total, 
         bodywt_grams = bodywt * 1000) %>%
  head

Crear resúmenes del marco de datos utilizando summarise()
La summarise()función creará estadísticas de resumen para una columna determinada en el marco de datos, como encontrar la media. Por ejemplo, para calcular el número promedio de horas de sueño, aplique la mean()función a la columna sleep_total y llame al valor de resumen avg_sleep.

msleep %>% 
  summarise(avg_sleep = mean(sleep_total))

Hay muchas otras estadísticas de resumen se podría considerar por ejemplo sd(), min(), max(), median(), sum(), n()(devuelve la longitud del vector), first()(devuelve primer valor en el vector), last()(devuelve el último valor en el vector) y n_distinct()(número de valores distintos en el vector).

msleep %>% 
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total),
            max_sleep = max(sleep_total),
            total = n())

Operaciones de grupo usando group_by()
El group_by()verbo es una función importante en dplyr. Como mencionamos anteriormente, está relacionado con el concepto de "dividir aplicar-combinar". Literalmente, queremos dividir el marco de datos por alguna variable (por ejemplo, orden taxonómico), aplicar una función a los marcos de datos individuales y luego combinar la salida.

Hagamos eso: divida el marco de datos msleep por el orden taxonómico, luego solicite las mismas estadísticas de resumen que arriba. Esperamos un conjunto de estadísticas de resumen para cada orden taxonómico.

msleep %>% 
  group_by(order) %>%
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total), 
            max_sleep = max(sleep_total),
            total = n())










