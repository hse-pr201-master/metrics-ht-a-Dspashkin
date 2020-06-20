install.packages("tidyverse") # коллекция пакетов от Hadley Wickham

install.packages("knitr") # взаимодействие R-LaTeX и R-markdown
install.packages("rmarkdown") # взаимодействие R-markdown
install.packages("xtable") # перевод таблиц в LaTeX
install.packages("texreg") # сравнение моделей в LaTeX
install.packages("pander") # перевод таблиц в markdown
install.packages("memisc") # перевод таблиц в markdown
install.packages('huxtable') # красивые таблички для latex/markdown/html

install.packages("lmtest") # тесты в линейных моделях
install.packages("sandwich") # оценки ковариационной матрицы робастные к гетероскедастичности
install.packages("erer") # подборка пакетов для эмпирических исследований
install.packages("AUC") # подсчёт показателя AUC
install.packages("mfx") # для предельных эффектов в logit/probit
install.packages("estimatr") # модели с робастными стандартными ошибками

install.packages("multiColl")
install.packages("ggplot2")
install.packages("GGally") # матрица диаграмм рассеяния
install.packages("lattice") # конкурент ggplot2
install.packages("vcd") # мозаичный график
install.packages("hexbin") # график из шестиугольников
install.packages("sjPlot") # визуализация результатов МНК
install.packages("factoextra") # визуализация для метода главных компонент и не только

install.packages("reshape2") # длинные <-> широкие таблицы
install.packages("psych") # описательные статистики
install.packages("skimr") # описательные статистики

install.packages("glmnet") # LASSO
install.packages("HSAUR")
install.packages("sgof")
install.packages("car") # для тестирования линейных гипотез, подсчёта vif

install.packages("spikeslab") # байесовская регрессия пик-плато
install.packages("quantreg") # квантильная регрессия
install.packages("MCMCpack") # набор моделей с байесовским подходом

install.packages("devtools") # разработка пакетов

install.packages("caret") # подбор параметров с помощью кросс-валидации
install.packages("AER")
install.packages("ivpack") # интсрументальные переменные

install.packages("zoo") # нерегулярные временные ряды
install.packages("xts") # еще ряды
install.packages("forecast") # ARMA, экспоненциальное сглаживание
install.packages("rugarch") # не используется в курсе, хорош для GARCH

install.packages("quantmod") # загрузка с finance.google.com
install.packages("Quandl") # загрузка с Quandl
install.packages("sophisthse") # read data from sophist.hse.ru
install.packages("sandwich")
install.packages("broom")

library("multiColl")
library("psych")
library("dplyr")
library("ggplot2")
library("GGally")
library("tidyverse")
library("car")
library("lmtest")
library("sandwich")
library("dplyr")
library("broom")

t <- read.csv("forestfires.csv",dec = ".",header = TRUE)

# Задание 1

glimpse(t)
describe(t)
# Фильтр на нулевые значения area
t1 <- filter(t, area>0)
t1
qplot(data = t1, area)
# t1$month <- as.numeric(as.factor(t1$month))
# t1$day <- as.numeric(as.factor(t1$day))
ggpairs(t)
describe(t)
# По графику определили, что данные по area сильно скошены вправо, 
# поэтому используем логарифм
t1 <- mutate(t1, logarea = log(area))
hist(t1$FFMC, xlab = "Индекс Fine Fuel Moisture Code", ylab = "Число пожаров",
     main = "Данные по FFMC")
glimpse(t1)
# По графику определили, что данные по FFMC сильно скошены влево, 
# поэтому используем квадрат функции

# Задание 2

qplot(data = t1, FFMC, xlab = "Индекс Fine Fuel Moisture Code", ylab = "Число пожаров",
      main = "Данные по FFMC")
t1 <- mutate(t1, FFMC2 = FFMC^2)
qplot(data = t1, FFMC2, xlab = "Квадратные значения индекса Fine Fuel Moisture Code", ylab = "Число пожаров",
      main = "Данные по FFMC2")
# Вероятно, вторая степень недостаточна, попробуем возвести в куб
t1 <- mutate(t1, FFMC3 = FFMC^3)
qplot(data = t1, FFMC3, xlab = "Кубические значения индекса Fine Fuel Moisture Code", ylab = "Число пожаров",
      main = "Данные по FFMC3")
# У параметра rain абсолютное большинство значений нулевые, следовательно не будем использовать в модели
t1 <- mutate(t1, day = as.factor(day), month = as.factor(month))
# Ящик с усами. Сожженная площадь по месяцам
qplot(data = t1, x = month, y = area, geom = "boxplot", xlab = "Месяц", ylab = "Число пожаров",
      main = "сожженная площадь по месяцам")
# Ящик с усами. Сожженная площадь по месяцам. Измененная ось y для более понятной визуализации
ggplot(t1, aes(t1$month, t1$area)) + geom_boxplot() + coord_trans(y = "sqrt") + 
  labs(x = "Месяц", y = "Сожженная площадь", title ="Сожженная площадь по месяцам")
# Ящик с усами. Сожженная площадь по дням недели
qplot(data = t1, x = day, y = area, geom = "boxplot", xlab = "День недели", ylab = "Число пожаров",
      main = "сожженная площадь по месяцам")
# Ящик с усами. Сожженная площадь по дням недели. Измененная ось y для более понятной визуализации
ggplot(t1, aes(t1$day, t1$area)) + geom_boxplot() + coord_trans(y = "sqrt") + 
  labs(x = "День недели", y = "Сожженная площадь", title ="Сожженная площадь по дням недели")
ggpairs(t1)
qplot(data = t1)
# В модель войдут все признаки, кроме rain, day, X, Y, rain, DC - так как слишком большое количество нулевых значений,
# не имеет смысла модифицировать, day - так как необнаружена существенная зависимость между днем недедли и площадью
# пожара. X, Y - зависимость между координатами точки и площадью пожара не была выявлена графическим путем. Так как 
# DMC и DC отражают влагу в органических слоях, было решено убрать один из признаков, DC показывает содержание влаги
# в глубоких слоях, что предположительно оказывает меньшее влияние на площадь возгорания, следовательно этот признак убрали.
# FFMC будет модифицирован возведением в степень куба, так как наблюдался
# сильный скос влево, а вместо area будем использовать logarea - логарифмическая функция от area.
# При признаке temp ожидаем получить положительный знак, так как повышенная температура может способствовать
# возникновению пожара и влиять положительно на площадь. При признаке wind также ожидаем получить положительный 
# знак, так как сила ветра может способствовать распространению пожара. При признаке RH ожидаем получить 
# отрицательный знак, так как повышенная влажность негативно влияет на распространение пожара
# DMC, FFMC3 отражают содержание влаги, что должно негативно влиять на распространение огня, 
# ожидаем отрицательный знак. ISI отвечает за скорость распространения возгорания, ожидается
# положительный знак. У month ожидается положительный знак так большие площади могут быть подвержены пожарам летом и осенью.
# Выбросы присутствуют: редкие аномально большие площади, имеет смысл удалить наиболее выделяющиеся наблюдения из выборки.
# Удаление выбросов (2 наблюдения с самыми высокими площадями пожара):
t1 <- filter(t1, area<300)

# Описательные статистики: среднее, медиана, дисперсия, минимум, максимум
describe(t1)
# Гипотеза о ненормальности temp не отвергается (уровень значимости = 5%). p-value = 4.54e-05
shapiro.test(t1$temp)
# Гипотеза о ненормальности FFMC3 не отвергается (уровень значимости = 5%). p-value = 4.514e-15
shapiro.test(t1$FFMC3)
# Гипотеза о ненормальности wind не отвергается (уровень значимости = 5%). p-value = 1.997e-07
shapiro.test(t1$wind)
# Гипотеза о ненормальности RH не отвергается (уровень значимости = 5%). p-value = 2.061e-07
shapiro.test(t1$RH)
# Гипотеза о ненормальности DMC не отвергается (уровень значимости = 5%). p-value = 1.617e-06
shapiro.test(t1$DMC)
# Гипотеза о ненормальности ISI не отвергается (уровень значимости = 5%). p-value = 4.243e-07
shapiro.test(t1$ISI)
t1 <- filter(t1, area<300)
describe(t1)

# Гистограмма для температуры
qplot(data = t1, temp, xlab = "Температура (гр. Цельсия)", ylab = "Количество пожаров",
      main = "Зависимость количества пожаров от температуры")
# Гистограмма для FFMC в кубе
qplot(data = t1, FFMC3, xlab = "FFMC в кубе", ylab = "Количество пожаров",
      main = "Зависимость количества пожаров от FFMC в кубе")
# Гистограмма для скорости ветра
qplot(data = t1, wind, xlab = "Скорость ветра (км/ч)", ylab = "Количество пожаров",
      main = "Зависимость количества пожаров от скорости ветра")
# Гистограмма для относительной влажности
qplot(data = t1, RH, xlab = "Относительная влажность (мм/м^2)", ylab = "Количество пожаров",
      main = "Зависимость количества пожаров от относительной влажности")
# Гистограмма для DMC
qplot(data = t1, DMC, xlab = "DMC", ylab = "Количество пожаров",
      main = "Зависимость количества пожаров от DMC (содержание влаги в органических слоях)")
# Гистограмма для ISI 
qplot(data = t1, ISI, xlab = "ISI", ylab = "Количество пожаров",
      main = "Зависимость количества пожаров от ISI (скорость распространения возгорания)")

# Интерпретация самых важных графиков была сделана раннее, также были сделаны некоторые модификации данных,
# которые были основаны на интерпретации графических данных

# Задание 3

t1$month <- as.numeric(as.factor(t1$month))
model1 <- lm(data=t1, logarea~temp+FFMC3+wind+RH+DMC+ISI+month)
# Расчет VIF для регрессоров. VIF оказался менее 3.3 для всех признаков. VIF - не более четырех, а значит
# нет мультиколлинеарности
vif(model1)
# Расчет CN для регрессоров. CN = 23.8167, значение меньше 30, следовательно нет мультиколлинеарности
V <- cbind(t1$temp, t1$FFMC3, t1$wind, t1$RH, t1$DMC, t1$ISI, t1$month)
V
CN(V)
# Используя два способа определения мультиколлинеарности (VIF, CN), мультиколлинеарность не была выявлена

# Задание 4

# Линейная модель
model2 <- lm(data=t1, logarea~temp+FFMC3+wind+RH+DMC+ISI+month)
summary(model2)
# R-squared:  0.03843,	Adjusted R-squared:  0.01254
# Проверим нормальность остатков с помощью теста Шапиро-Уилка:
shapiro.test(resid(model2))
#Гипотеза о ненормальности остатков отвергается (p-value = 0.3674), то есть остатки распределены нормально
# Ни одна из переменных не оказалась значимой на уровне значимости 5%
# Для F-статистики p-value: 0.1729. Следовательно гипотеза о незначимости переменных в целом не отвергается
# Не совпали знаки при переменных temp, FFMC3, DMC, ISI в сравнении с предполагаемыми.
# Это может быть следствием низкого влияния этих факторов на площадь пожара.

# Задание 5

describe(t1)
# Средние для переменных. temp=19.26, FFMC3=757549.12, wind=4.11, RH=43.86, DMC=114.28, ISI=9.16, month=5.79)
d <- data.frame(temp=19.26, FFMC3=757549.12, wind=4.11, RH=43.86, DMC=114.28, ISI=9.16, month=5.79)
# Точеченый прогноз логарифма площади. Значение: 1.80793 
predict(model2,newdata = d)
# Точеченый прогноз площади. Значение: 6.097815 
exp(predict(model2,newdata = d))
# Доверительный интервал для среднего логарифма площади. Значение: [1.632166, 1.983695]
predict(model2,newdata = d, interval = "confidence")
# Доверительный интервал для среднего значения площади. Значение: [5.11494, 7.269557]
exp(predict(model2,newdata = d, interval = "confidence"))
# Доверительный интервал для индивидуального значения логарифма площади. Значение: [-1.074826, 4.690687]
predict(model2,newdata = d, interval = "prediction")
# Доверительный интервал для индивидуального значения площади. Значение: [0.3413571, 108.928]
exp(predict(model2,newdata = d, interval = "prediction"))

# Задание 6

# Гетероскедастичность ожидается при следующих регрессорах: FFMC3, DMC, RH. Следствие большого интервала значений регрессоров
# При увлеичении разброса этих регрессоров дисперсия ошибок должна увеличиваться

# Задание 7

# Cтандартизированные остатки
model2.st.resid <- rstandard(model2) 
# График. Зависимость остатков от FFMC3
ggplot(aes(x = FFMC3, y = abs(model2.st.resid)), data =t1) + geom_point(alpha = 0.2) + 
  labs(x = "Значение куба FFMC", y = "Стандартизированные остатки",
       title = "Зависимость остатков от FFMC3")

ggplot(aes(x = DMC, y = abs(model2.st.resid)), data =t1) + geom_point(alpha = 0.2) + 
  labs(x = "Значение DMC", y = "Стандартизированные остатки",
       title = "Зависимость остатков от DMC")

ggplot(aes(x = RH, y = abs(model2.st.resid)), data =t1) + geom_point(alpha = 0.2) + 
  labs(x = "Значение куба влажности", y = "Стандартизированные остатки",
       title = "Зависимость остатков от влажности")

# Тест Голдфельда-Квандта для регрессора FFMC3. Гипотеза о условной гомоскедастичности не отвергается (p-value = 0.3631)
gqtest(model2, order.by = ~FFMC3, data=t1, fraction=0.2)
# Тест Голдфельда-Квандта для регрессора DMC. Гипотеза о условной гомоскедастичности отвергается на уровне значимости 5% (p-value = 0.0377)
gqtest(model2, order.by = ~DMC, data=t1, fraction=0.2)
# Тест Голдфельда-Квандта для регрессора RH. Гипотеза о условной гомоскедастичности не отвергается (p-value = 0.6804)
gqtest(model2, order.by = ~RH, data=t1, fraction=0.2)
# Графически гетероскедастичность не была обнаружена, с помощью теста Голдфельда-Квандта гетероскедастичность была обнаружена только для
# регрессора DMC.

# Задание 8

# Вес
wts <- 1/fitted( lm(abs(residuals(model2))~fitted(model2)))^2
# Модель взвешенного МНК
model3 <- lm(data=t1, logarea~temp+FFMC3+wind+RH+DMC+ISI+month, weights=wts)
summary(model3)
# R-squared:  0.03891,	Adjusted R-squared:  0.01304 
#             Estimate   Std. Error  t value  Pr(>|t|)
# (Intercept)  2.037e+00  1.389e+00   1.466    0.144
# temp        -1.869e-02  2.314e-02  -0.808    0.420
# FFMC3        3.133e-07  1.918e-06   0.163    0.870
# wind         5.813e-02  5.301e-02   1.097    0.274
# RH          -9.341e-03  7.431e-03  -1.257    0.210
# DMC          2.497e-03  1.834e-03   1.361    0.175
# ISI         -4.743e-02  3.706e-02  -1.280    0.202
# month        3.688e-02  2.668e-02   1.382    0.168
# В сравнении с обычной моделью изменились коэффициенты (большинство коэффициентов меньше по модулю), вырос показатель R-squared
# Увеличилась значимость модели в целом p-value: 0.1661

# Задание 9

vcovHC(model2, type = "HC0")

coeftest(model2, vcov. = vcovHC(model2, type = "HC0"))
# t test of coefficients:
#   
#              Estimate    Std. Error  t value  Pr(>|t|)  
# (Intercept)  1.8167e+00  1.0887e+00  1.6686   0.0964 .
# temp        -1.6149e-02  2.3091e-02 -0.6994   0.4849  
# FFMC3        4.0254e-07  1.5447e-06  0.2606   0.7946  
# wind         6.8171e-02  4.9464e-02  1.3782   0.1693  
# RH          -7.6738e-03  6.8720e-03 -1.1167   0.2652  
# DMC          2.4251e-03  1.7083e-03  1.4196   0.1569  
# ISI         -4.8941e-02  3.6032e-02 -1.3583   0.1756  
# month        3.8839e-02  2.5483e-02  1.5241   0.1287  
# P-value для большинства кэффициентов уменьшились (исключение составляет только коэффициент перед ISI)
# Используем модифицированную ковариационную матрицу, устойчивую к гетероскедастичности, диагональные элементы матрицы в формуле - 
# квадраты оценок остатков модели

# Бонус 
vcovHC(model2, type = "HC3")
coeftest(model2, vcov. = vcovHC(model2, type = "HC3"))
# t test of coefficients:
#  
#              Estimate    Std. Error  t value  Pr(>|t|)  
# (Intercept)  1.8167e+00  1.1418e+00  1.5911   0.1128
# temp        -1.6149e-02  2.4183e-02 -0.6678   0.5049
# FFMC3        4.0254e-07  1.6328e-06  0.2465   0.8055
# wind         6.8171e-02  5.1298e-02  1.3289   0.1850
# RH          -7.6738e-03  7.1606e-03 -1.0717   0.2849
# DMC          2.4251e-03  1.7766e-03  1.3650   0.1734
# ISI         -4.8941e-02  3.7923e-02 -1.2905   0.1980
# month        3.8839e-02  2.6206e-02  1.4821   0.1395  

# Задание 10

# Матрица значений регрессоров
V <- cbind(t1$temp, t1$FFMC3, t1$wind, t1$RH, t1$DMC, t1$ISI, t1$month)
V
# Метод главных компонент
v.pca <- prcomp(V, scale = TRUE)
v.pca
# Первая главная компонента
pca1 <- v.pca$x[, 1]
pca1
# Вторая главная компонента
pca2 <- v.pca$x[, 2]
pca2
# Первые две главные компоненты объясняют 59.75% дисперсии
summary(v.pca)
# Линейная регрессия зависимой переменной на две первые главные компоненты
model4 <- lm(data=t1, logarea~pca1+pca2)
summary(model4)
# Coefficients:
#              Estimate   Std. Error t value  Pr(>|t|)    
#   (Intercept)  1.80782    0.08965  20.166   <2e-16 ***
#   pca1        -0.07230    0.05384  -1.343    0.180    
#   pca2        -0.08490    0.07593  -1.118    0.265
# R-squared:  0.01139,	Adjusted R-squared:  0.003929 
# R-squared снизился, переменные незначимы. Для F-статистики p-value вырос. Объясняющая способность модели снизилась

# Задание 11
nll <- function(theta0,theta1,theta2,theta3,theta4,theta5,theta6,theta7) {
  y <- t1$logarea[-idx]
  x1 <- t1$temp[-idx]
  x2 <- t1$FFMC3[-idx]
  x3 <- t1$wind[-idx]
  x4 <- t1$RH[-idx]
  x5 <- t1$DMC[-idx]
  x6 <- t1$ISI[-idx]
  x7 <- t1$month[-idx]
  mu = exp(theta0 + x1*theta1 + x2*theta2 + x3*theta3 + x4*theta4 + x5*theta5 + x6*theta6 + x7*theta7)
  -sum(y*(log(mu)) - mu)
}
opt.res <- optim(nll ,par=c(3,3),y.in=logarea,x.in=V,hessian=T)
set.seed(200)
idx <- createDataPartition(t1$logarea, p=0.25,list=FALSE)
# Дальше не успел :)


