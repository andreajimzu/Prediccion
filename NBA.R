
library(readr)
library(MASS)
library(carData)
library(car)
library(timeDate)
library(timeSeries)
library(fBasics)
library(akima)
library(gvlma)

nba <- read_csv("nba.csv")
nba <- na.omit(nba)
View(nba)

names(nba)[10] = "TS"
names(nba)[13] = "ORB"
names(nba)[14] = "DRB"
names(nba)[15] = "TRB"
names(nba)[16] = "AST"
names(nba)[17] = "STL"
names(nba)[18] = "BLK"
names(nba)[19] = "TDV"
names(nba)[20] = "USG"
names(nba)[11] = "Three_PAr"
names(nba)[24] = "WS48"
names(nba)


# Voy a hacer un modelo que incluya todas las variables de mi base de datos. Para 
# ello hago el fitting con lm()


model <- lm (Salary ~ NBA_DraftNumber+Age+G+MP+ PER+ TS+ Three_PAr+ FTr+
               ORB+ DRB+ TRB+ AST+ STL+ BLK+ TDV+ USG+OWS+DWS+ WS + WS48+ 
               OBPM+DBPM+ BPM+VORP, data=nba)
summary(model)

# Genero un nuevo modelo con AIC para que me de las variables que son mas significativas.



stepAIC(model, direction = "backward")

    ## Con esta formula obtengo el siguiente modelo unicamente con las variables 
    ## signifivativas:

model2 <- lm(formula = Salary ~ NBA_DraftNumber + Age + G + MP + PER + 
                 Three_PAr + ORB + TRB + USG + WS + OBPM, data = nba)
summary(model2)

# Hago otro stepAIC para ver si puedo eliminar algunas de las variables del modelo 2.

stepAIC(model2, direction = "backward")

    ## Aparece que no puedo eliminar las variables poco significativas del modelo 2.

# Voy a comparar ambos modelos con anova. 

anova(model, model2)
    ## Sale con un p = 0.9854. No es significativo el test, por lo que no aportan
    ## a la prediccion lineal por lo que se justifica que elimine esas variables
    ## de mi modelo inicial. 


# DETECCION DE LA MULTICOLINEALIDAD:

vif(model2)
sqrt(vif(model2))>2

# Hay multicolinealidad, hay factores que hinchan la variabilidad en mi 
# modelo. Hay variables cuya raiz cuadrada del vif es mayor que 2 y por 
# tanto tienen problemas de multicolinealidad, por lo que voy a eliminar 
# esas variables de mi modelo: 

model3 <- lm(formula = Salary ~ NBA_DraftNumber + Age + G+
              Three_PAr + ORB + TRB + USG + WS + OBPM , data = nba)
summary(model3)

vif(model3)
sqrt(vif(model3))>2

# Muestro el intervalo de confianza para cada uno de los coeficientes parciales
# de regresion:

confint(model3)

set.seed(1234)
n <- 10
muestra <- sample(1:nrow(nba), size = n, replace= FALSE)
nba10 <- nba[muestra,]
nba10

predmuestra <- predict( model3, newdata = nba10)
predmuestra

# Analisis Normalidad: 

qqPlot(model3, labels=row.names(nba), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

        # Me indica los valores atípicos más importantes que no te generan 
        # normalidad, por lo que hay que ver por qué. En este caso son el 
        # 326 y 112 

# HISTOGRAMA + DENSIDAD + NORMAL + RUG 

residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}

residplot(model3)

        # Se observa que mis datos son mas extremos que mi normal.


# Contraste de Normalidad de Jarque-Bera: 


vResid=resid(model3)
jbTest(vResid)

# Shapiro-Wilk Test: 

shapiro.test(vResid)
        # Como p-valor es menor que 0.05 se rechaza la hipotesis nula de que sigue
        # una distribucion normal.

# Linealidad: Componentes o Gráficos de residuos parciales

crPlots(model3)

        # Se observa que hay problemas de linealidad ya que se obtiene una recta
        # sobre las que se representan los puntos. 


# Varianza Constante. Homocedasticidad

ncvTest(model3)
spreadLevelPlot(model3)

        # Con un modelo explicativo esto es útil, pero no para un modelo predictivo 
        # ya que me he dado cuenta de que hay datos que no son homogéneos, por lo
        # que tengo que hacer modelos distintos


# VALIDACION GLOBAL: 

gvmodel <- gvlma(model3)
summary(gvmodel)

        # Hay problemas de simetria y de curtosis por lo que  se  rechaza  
        # la validacion global. 

# OUTLIERS: 

## Valores atipicos:

outlierTest(model3)

        # Nos identifica un valor atipico, el 326 y 112.

## Valores extremos: Para identificarlos se usa el hat statistic. 

hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(model3)

        # Aparecen 8 valores por encima de la segunda linea, esos son los valores
        # extremos. 

## Valores influyentes: Distancia de Cook.

# Los valores de D mayores que  4/(n-k-1) indican que son variables influtentes.
# Voy a identificar primero esos valores.

cutoff <- 4/(nrow(nba)-length(model3$coefficients)-2)
plot(model3, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

## Otra forma de identificar los valores influyentes es a traves de graficos
## added variable

avPlots(model3, ask=FALSE, id.method="identify")

## Represento el influence plot: 

influencePlot(model3, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )

        #Los análisis muestran varias observaciones influyentes (posición 141 y 223) 
        #que exceden los límites de preocupación para los valores de Leverages o 
        #Distancia Cook. Estudios más exhaustivos consistirían en rehacer el modelo 
        #sin las observaciones y ver el impacto.
        #Cuanto más lejos esten es que son mas extremos.


