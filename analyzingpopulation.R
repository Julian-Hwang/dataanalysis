#1. 데이터수집
population=read.csv('C:/Users/g_julian5383/Downloads/서울특별시 서대문구_주민등록 인구수_20210831.csv')
population

#2. 탐색
library("dplyr")
str(population)
table(is.na(population))
View(population)

#3. 전처리및 가공
population$전체인구<-round(population$남+population$여)

clean_population=population[,c(1,5,6,7,8,9)]
clean_population
str(clean_population)
View(clean_population)

clean_population$남.19세비율.<-round(clean_population$남.19세이상./clean_population$전체인구*1,digit=2)
clean_population$여.19세비율.<-round(clean_population$여.19세이상./clean_population$전체인구*1,digit=2)
clean_population$남.65세비율.<-round(clean_population$남.65세이상./clean_population$전체인구*1,digit=2)
clean_population$여.65세비율.<-round(clean_population$여.65세이상./clean_population$전체인구*1,digit=2)

#4. 분석
summary(clean_population)

#5. 예측모델
m_1=lm(남.19세비율.~전체인구,data=clean_population)
m_1
coef(m_1)
deviance(m_1)

m_2=lm(여.19세비율.~전체인구,data=clean_population)
m_2
coef(m_2)
deviance(m_2)

m_3=lm(남.65세비율.~전체인구,data=clean_population)
m_3
coef(m_3)
deviance(m_3)

m_4=lm(여.65세비율.~전체인구,data=clean_population)
m_4
coef(m_4)
deviance(m_4)