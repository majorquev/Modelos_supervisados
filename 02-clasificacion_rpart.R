# Abre el proyecto antes de ejecutar este script

# pacman = gestor de paquetes de R
# rpart = paquete para generar árboles de clasifiación y regresión
# ISLR = introduction to statistical learning (versión intermedia) --> versión 2: Statistical Learning
# visNetwork = visualización de grafos
# dplyr = para ejecutar operaciones tipo SQL
# tidyr = para ordenar datos
# data.table = paquete que lee texto plano de forma eficiente
# skimr = permite generar resumenes de las variables en un data frame

# 1: Cargar/instalar paquetes ---------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(rpart, ISLR, visNetwork, dplyr, tidyr, data.table, skimr)

# 2: Descargar los datos --------------------------------------------------

heart_url <- "http://www-bcf.usc.edu/~gareth/ISL/Heart.csv"
heart_csv <- "heart.csv"

if (!file.exists(heart_csv)) {
  download.file(heart_url, heart_csv)
}

# 3: Leer datos -----------------------------------------------------------

heart <- fread("heart.csv") %>%
  as_tibble()

names(heart)

skim(heart)

heart <- heart %>%
  mutate(
    Sex = as.factor(Sex),
    ChestPain = as.factor(ChestPain),
    RestECG = as.factor(RestECG),
    ExAng = as.factor(ExAng),
    Slope = as.factor(Slope),
    Thal = as.factor(Thal)
  )

# 4: División de los datos ------------------------------------------------

# Recordar que este es un paso que de hoy en adelante deben considerar en sus modelos.
heart <- heart %>%
  drop_na() %>%
  drop_na(Ca, Thal)

N <- dim(heart)[1]

# no olvidar especificar la semilla para que sea reproducible
set.seed(6000)

# defino (aleatoriamente) los índices de las filas que formarán parte de la data de entrenamiento
id <- sample(1:N, size = round(0.7 * N))

train <- heart %>% filter(row_number() %in% id)
dim(train)

test <- heart %>% filter(!row_number() %in% id)
dim(test)

skim(heart)
skim(train)
skim(test)

# 5: Ajuste de un árbol --------------------------------------------------

# por defecto los "split" se basan en el índice de gini
arbol0 <- rpart(formula = AHD ~ ., data = train)

arbol0$frame
plot(arbol0)
text(arbol0, use.n = TRUE)

visTree(
  arbol0,
  height = "800px",
  nodesPopSize = TRUE,
  minNodeSize = 10,
  maxNodeSize = 30
)

# ramificando por ganancia de información
arbol1 <- rpart(
  formula = AHD ~ .,
  data = train,
  parms = list(split = 'information')
)

visTree(
  arbol1,
  height = "800px",
  nodesPopSize = TRUE,
  minNodeSize = 10,
  maxNodeSize = 30
)

summary(arbol1)

# 6: Importancia de las variables -----------------------------------------

names(arbol1)
arbol1$variable.importance

# 7: Predicciones ---------------------------------------------------------

# ojo que las predicciones se realizan en la data de test
p1 <- as_tibble(
  predict(
    arbol1, 
    newdata = test, 
    type = "prob"
  )
)

p2 <- tibble(
  clase_predicha = predict(arbol1, newdata = test, type = "class")
)

# juntar columnas
bind_cols(p1, p2)


A <- bind_cols(select(test, AHD), p2) %>% 
  rename(
    clase_real = AHD
  )

table(A)

visTree(
  arbol1,
  height = "800px",
  nodesPopSize = TRUE,
  minNodeSize = 10,
  maxNodeSize = 30
)

arbol1$cptable
plotcp(arbol1)

# sobreajuste
arbol2 <- rpart(
  formula = AHD ~ .,
  data = train,
  parms = list(split = 'information'),
  control = rpart.control(minsplit = 2, cp = 0.0001)
)

visTree(
  arbol2,
  height = "800px",
  nodesPopSize = TRUE,
  maxNodeSize = 30
)

arbol2$cptable

# Como criterio visual podemos podar el árbol al valor del cp a partir del cual el xerror 
# (error de test estimado por validación cruzada) no mejore significativamente.
# En breve se recordará el método de validación cruzada para la estimación del error de test asociado a un 
# modelo de aprendizaje supervisado.
plotcp(arbol2)

?prune.rpart

arbol2.pod <- prune(arbol2, cp = 0.048)

visTree(
  arbol2.pod,
  height = "800px",
  nodesPopSize = TRUE,
  maxNodeSize = 30
)

# 1. Formato RDS: Nos permite guradar info cargada en meroria preservando la estructura de la sesión.
# 2. saveRDS(): guarda en el directorio de proyecto los objetos definidos en la sesion, en formato RDS
saveRDS(arbol2, "arbol2.rds")
saveRDS(arbol2.pod, "arbol2_pod.rds")

rm(arbol2.pod)

# ¿Cómo cargar un archivo RDS en sesión?
arbol2 <- readRDS("arbol2.rds")
arbol2.pod <- readRDS("arbol2_pod.rds")
