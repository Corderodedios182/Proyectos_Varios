#Este capitulo lo introduce en la manipulacion de cadenas en R.
#Aprendera los conceptos basicos de como funcionan las cadenas y como crearlas a mano,
#pero el enfoque de este capitulo estara en las expresiones regulares,
#o expresiones regulares para abreviar. 
#Las expresiones regulares son utiles porque las cadenas generalmente contienen datos no estructurados o semiestructurados, y las expresiones regulares son un lenguaje conciso para describir patrones en cadenas.
#Cuando vea por primera vez una expresion regular, pensara que un gato camino sobre su teclado,
#pero a medida que mejore su comprension, pronto comenzaran a tener sentido.

#Este capitulo se centrara en el paquete stringr para la manipulacion de cadenas.
#stringr no forma parte del nucleo tidyverse porque no siempre tiene datos de texto,
#por lo que necesitamos cargarlo explicitamente.

library(tidyverse)
library(stringr)

#Puede crear cadenas con comillas simples o dobles.
#A diferencia de otros idiomas, no hay diferencia en el comportamiento. 
#Recomiendo usar siempre comillas, a menos que desee crear una cadena que contenga multiples comillas

string1 <- "Esto es un texto"
string2 <- 'Si quiero introducir una "Cita" es mejor usa comilla simple'

#Para incluir una comilla simple o doble literal en una cadena, puede usarla \ para "escapar":

double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"

#Esto significa que si desea incluir una barra invertida, tendra que doblar hacia arriba: "\\".

x <- c("\"", "\\")
x

writeLines(x)

#Base R contiene muchas funciones para trabajar con cadenas,
#pero las evitaremos porque pueden ser inconsistentes, 
#lo que las hace dificiles de recordar. 
#En su lugar usaremos funciones de stringr.
#Estos tienen nombres mas intuitivos, y todos comienzan con str_.
#Por ejemplo, str_length() te dice el numero de caracteres en una cadena

str_length(c("Python", "Cientifico de Datos", NA))

#El str_prefijo comun es particularmente util si usas RStudio,
#porque la escritura str_ activara el autocompletado,
#permitiendote ver todas las funciones de stringr:

#Para combinar 2 cadenas usa str_c()

str_c("Cientifico","_de_", "Datos")

#Usa sep para controlar su separacion
str_c("Cientifico","de", "Datos", sep = "_")

#Como la mayoria de las otras funciones en R,
#los valores perdidos son contagiosos.

x <- c("abc", NA)

str_c("|-", x, "-|")

#Si quieres que se impriman como "NA", usa str_replace_na():

str_replace_na(x)

str_c("|-", str_replace_na(x), "-|")

#Como se muestra arriba, str_c() esta vectorizado,
#y recicla automaticamente vectores mas cortos a la misma longitud que el mas largo:
  
str_c("prefix-", c("a", "b", "c"), "-suffix")

#Para contraer un vector de cadenas en una sola cadena, use collapse:
  
str_c(c("Cientifico", "de", "Datos"), collapse = ",")

#Puedes extraer partes de una cadena usando str_sub(). Ademas de la cadena,
#str_sub() tomas start y end argumentos que dan la posición (incluida) de la subcadena:
  
x <- c("Apple", "Banana", "Pear")

str_sub(x, 3, 3)

#Parte negativa o final de un caracter
str_sub(x, -3, -1)

#Tenga en cuenta que str_sub() no fallara si la cadena es demasiado corta:
#simplemente devolvera tanto como sea posible:
  
str_sub("a", 1, 5)

#Tambien puede utilizar el formulario de asignacion de str_sub() para modificar cadenas:
  
str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))

x

#Arriba solia str_to_lower() cambiar el texto a minusculas.
#Tambien puede utilizar str_to_upper() o str_to_title(). 
#Sin embargo, cambiar el caso es mas complicado de lo que parece a primera vista porque los diferentes idiomas tienen diferentes reglas para cambiar el caso.
#Puede elegir que conjunto de reglas usar al especificar una configuracion regional:

str_to_upper(c("ciencia", "datos"))

#Ejercicios
#Funcion paste0 Base R vs str_c paquete string
str_c("Ciencia","Datos")
paste0("Ciencia", "Datos")

#Diferencia al tratar los NA
str_c(str_replace_na(NA),"Datos", sep = " ")
paste(NA, "Datos")

#Diferencia de argumentos
str_c("Ciencia","de","Datos", sep = "_")
str_c("Ciencia","de","Datos", collapse = "_")
str_c("Ciencia","de","Datos")

#Uso de str_length() y str_sub() para extraer el caracter medio de una cadena 

str_sub("seleccion de algun caracter por numero", 1,20)
str_sub("Extraer_el_caracter_medio",str_length("Ciencia de Datos")/2, str_length("Ciencia de Datos")/2)

#Funciones para acomodar espacios
str_wrap("Ciencia   de               datos   ")

str_trim("     Ciencia    de datos  ")

#Patrones con expresiones Regulares

#Las expresiones regulares son un lenguaje muy conciso que le permite describir patrones en cadenas.
#Toman un poco de tiempo para entenderlo, pero una vez que los entiendas,
#los encontraras extremadamente utiles.

#Para aprender expresiones regulares, 
#usaremos str_view() y str_view_all().
#Estas funciones toman un vector de caracteres y una expresion regular,
#y te muestran como coinciden.

#Los patrones mas simples coinciden con las cadenas exactas:

x <- tail(words)

str_view(x, "^y")
str_view(x, "y$")
str_view(x, ".e.") #Una a que se encuentra entre 2 caracteres
str_view(x, "e.")
str_view(x,"[^y]") #Coindice con cualquier cosa exepto

#Metacaracteres
str_view(c("a.c","f$a"), ".[$].") #[] dentro se especifica un metacaracter
str_view(c("a.c","f$a"), "a|f") #Contiene o a o f

str_view(words,"a|e|i|o|u") #Palabras que comienzen con alguna vocal
str_view(words,"[^a|e|i|o|u]")

str_view(words, "")



