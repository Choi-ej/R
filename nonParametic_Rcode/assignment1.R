# Paired two sample t-test
# Define the before and after data
before <- c(68, 70, 73, 77, 80, 82, 83, 90)
after <- c(71, 69, 75, 81, 88, 76, 73, 79)

# Differences
diff <- before-after

# Since the two data sets were observed from the same group, there is no need for a test of equal variances
# Do the paired t-test
 t.test(before, after, paired = TRUE, alternative = "greater")


# 
# define the before and after data 
a<- c(78,62,72,58,64,63,71,70,74)
b<-c(75,79,64,83,65,69,77,71,69,82,70,75,72,78,68,71) 

# Apply the F-test to check the variances of the two samples are equal or not
var.test(a, b)
# p value is greater than 0.05, Do not reject H0. We can assume that two sample variances are same.

# Do the two sample t-test under same variance.
t.test(a, b, paired=F, alternative="two.sided", var.equal=T)

# R defaults to a 95% confidence level (alpha = .05). Specifying a custom value in conf.level will set a new confidence level in the t-test.


rn <- c()
for ( i in 2:9) {
	rn <- c (rn, paste( as.character(i), " times table"))
}

rn

cn<-c()
for(i in 1:9){
	cn<-c(cn, i)
}
cn

gugu<-matrix(nrow = 8, ncol = 9)
gugu


colnames(gugu) = cn
gugu

rownames(gugu) = rn
gugu


for (i in 1:9){
	for (j in 2:9) {
		gugu[i,j] <- i*j
	}
}

gugu