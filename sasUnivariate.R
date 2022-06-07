install.packages("moments")
install.packages("nortest")
install.packages("BSDA")

eunjin <- function(x){
	print(summary(x))
	cat("적률\n")
	cat("N : " , length(x), '\n')
	weight <- rnorm(n=length(x), mean=0, sd=1)
	cat("가중합: ", sum(x = x, w = weight), '\n')
	cat("평균 : " , mean(x), '\n')
	cat("관측치합:", sum(x), '\n')
	cat("표준편차 : " , sd(x), '\n')
	cat("분산 : " , sd(x)^2, '\n')

	library(moments)
	cat("왜도 :", skewness(x), "\n")
	cat("첨도 :", kurtosis(x), "\n")
	

	ss<-0
	for (i in 1:length(x)){
		ss <- ss+ (x[i]^2)
	}
	cat("제곱합", ss, "\n")
  #수정제곱합
	css<-sum((x-mean(x))^2)
	cat("수정제곱합 : ", css, '\n')
	cat("변동 계수 : ", sd(x)/mean(x), '\n')
	cat("평균의 표준 오차 : ", sd(x)/sqrt(length(x)), '\n')
	
	cat("\n기본 통계 측도\n")
	cat("평균 : " , mean(x), '\n')
	cat("중위수: ", median(x), '\n')
	
	y <- table(x)
	cat("최빈값: ", names(y)[which(y==max(y))], '\n')
	
	cat("\n 변이측도 \n")
	cat("표준편차 : " , sd(x), '\n')
	cat("분산 : " , sd(x)^2, '\n')
	cat("범위: ",max(x) - min(x) ,'\n')
	cat('사분위수 범위', IQR(x) , '\n\n')
	
	cat('위치모수 검정: Mu0=0 \n')
	
	library(nortest)
	library(BSDA)
	
	
	cat("스튜던트 t 검정\n")
	print(t.test(x,mu=0))
	cat("부호 \n")
	print(SIGN.test(x, md=median(x)))
	cat("부호 순위", '\n')
	print(wilcox.test(x, mu=0))
	
	
	
	cat("\n분위 수(정의5)\n")
	tmp <- c(0.99, 0.95, 0.90, 0.75, 0.5, 0.25, 0.1, 0.05, 0.01, 0)
	for (i in 1:length(tmp))
	  cat(tmp[i]*100,'% |', quantile(x, tmp[i]), '\n')
	
	cat("극 관측값 \n")
	x1<-sort(x)
	cat("최소 | 위치\n")
	for( i in 1:5) {
	  cat(x1[i], '|', which(x == x1[i]), '\n')
	}
	
	cat("최대 | 위치")
	for ( i in (length(x)-4) : length(x)) {
	  cat(x[i], '|', which(x == x1[i]), '\n')
	}
	
	par(mfrow = c(1, 2))
	boxplot(x, main = "Box Plot")

	cat("stem-leaf plot : \  ")
	stem(x)
	
	qqnorm(x, main = '정규 분위수')
	qqline(x,col="red",lwd=2)

}



x <- iris$Petal.Length
eunjin(x)


