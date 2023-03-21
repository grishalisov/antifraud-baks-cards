
# ---------------- Подгрузка требущихся библиотек------------------------------
library(caret)
library(rpart)
library(dplyr)
library(ROSE)
library(randomForest)
library(rpart.plot)
library(ggplot2)

# --------------------------------------------------------------------------

# --------------------- Чтение датасета --------------------------------
credit_card = read.csv(file = 'creditcard.csv')

# --------------------- Просмотр датасета --------------------------------
View(credit_card)

# --------------------- Отображение столбцов -----------------------------
names(credit_card)

# --------------------- Отображение структуры датасета ------------------
str(credit_card)


head(credit_card)

# - Преобразование класса в фактор, тк не фрод = 0, а фрод = 1--
credit_card$Class = as.factor(credit_card$Class)

# -------------- Суммирование фрод и не фрод операций --------
summary(credit_card$Class)

# --------------------- Проверка на наличие NA значений -------------------------
sum(is.na(credit_card))

# -------------- Разделение фрод и не фрод на новые dfs  ---------
credit_card.true = credit_card[credit_card$Class == 0,]
credit_card.false = credit_card[credit_card$Class == 1,]


# --------- Круговая диаграмма для сравнения колличества фрод и не фрод операций ------------

labels = c("Не является фродом","Фрод")
labels = paste(labels,round(prop.table(table(credit_card$Class))*100,2))
labels = paste0(labels,"%")
pie(table(credit_card$Class),labels,col = c("blue","red"),
    main = "Круговая диаграмма транзакций по кредитным картам")

# ---------------------- Разделение данных -------------------------------------
rows = nrow(credit_card)
cols = ncol(credit_card)

set.seed(39)
credit_card = credit_card[sample(rows),1:cols]
ntr = as.integer(round(0.8*rows))

credit_card.train = credit_card[1:ntr,1:cols] # для обучения
credit_card.test = credit_card[(ntr+1):rows,-cols] # для тестового ввода
credit_card.testc = credit_card[(ntr+1):rows,cols] # для тестовых классов данных

credit_card.testc = as.data.frame(credit_card.testc)
colnames(credit_card.testc)[1] = c("Class")


# -------------------- Алгоритм Random Forest -----------------------------
samp = as.integer(0.49*ntr)
rF = randomForest(Class ~ . ,data =credit_card.train,ntree = 39,
                  samplesize = samp,maxnodes=44)
rF_pred = predict(rF,credit_card.test)
credit_card.testc$Pred = rF_pred

confusionMatrix(credit_card.testc$Pred,credit_card.testc$Class)

roc.curve(credit_card.testc$Class,credit_card.testc$Pred,plotit = TRUE,
          col="darkred",main = "Кривая ROC для алгоритма Random Forest",
          col.main="black")

