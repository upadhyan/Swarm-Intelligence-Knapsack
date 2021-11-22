ACO_analysis <- function(){
  library(onewaytests)
  library(car)
  ##IMPORT AND CONFIGURE DATA

  GA_1 <- read.csv("./a_baseline_output/baseline_25966.csv", header = TRUE)
  blocking_1 <- as.data.frame(matrix(1, nrow=nrow(GA_1), ncol=1))
  colnames(blocking_1) <- "block"
  GA_1 <- cbind(GA_1, blocking_1)
  
  GA_2 <- read.csv("./a_baseline_output/baseline_43824.csv", header = TRUE)
  blocking_2 <- as.data.frame(matrix(2, nrow=nrow(GA_2), ncol=1))
  colnames(blocking_2) <- "block"
  GA_2 <- cbind(GA_2, blocking_2)
  
  GA_3 <- read.csv("./a_baseline_output/baseline_46822.csv", header = TRUE)
  blocking_3 <- as.data.frame(matrix(3, nrow=nrow(GA_3), ncol=1))
  colnames(blocking_3) <- "block"
  GA_3 <- cbind(GA_3, blocking_3)
  
  GA_4 <- read.csv("./a_baseline_output/baseline_8742.csv", header = TRUE)
  blocking_4 <- as.data.frame(matrix(4, nrow=nrow(GA_4), ncol=1))
  colnames(blocking_4) <- "block"
  GA_4 <- cbind(GA_4, blocking_4)
  
  GA_5 <- read.csv("./a_baseline_output/baseline_22872.csv", header = TRUE)
  blocking_5 <- as.data.frame(matrix(5, nrow=nrow(GA_5), ncol=1))
  colnames(blocking_5) <- "block"
  GA_5 <- cbind(GA_5, blocking_5)
  
  GA_6 <- read.csv("./a_baseline_output/baseline_12137.csv", header = TRUE)
  blocking_6 <- as.data.frame(matrix(6, nrow=nrow(GA_6), ncol=1))
  colnames(blocking_6) <- "block"
  GA_6 <- cbind(GA_6, blocking_6)
  
  GA_7 <- read.csv("./a_baseline_output/baseline_33644.csv", header = TRUE)
  blocking_7 <- as.data.frame(matrix(1, nrow=nrow(GA_7), ncol=1))
  colnames(blocking_7) <- "block"
  GA_7 <- cbind(GA_7, blocking_7)
  
  all_GA <- rbind(GA_1, GA_2, GA_3, GA_4, GA_5, GA_6, GA_7)

  
  ACO_1 <- read.csv("./knapsack_output/AOC_30369_25966.csv", header = FALSE)
  Ablock_1 <- as.data.frame(matrix(1, nrow=nrow(ACO_1), ncol=1))
  ACO_1 <- cbind(ACO_1, Ablock_1)
  
  ACO_2 <- read.csv("./knapsack_output/AOC_25070_43824.csv", header = FALSE)
  Ablock_2 <- as.data.frame(matrix(2, nrow=nrow(ACO_2), ncol=1))
  ACO_2 <- cbind(ACO_2, Ablock_2)
  
  ACO_3 <- read.csv("./knapsack_output/AOC_48518_46822.csv", header = FALSE)
  Ablock_3 <- as.data.frame(matrix(3, nrow=nrow(ACO_3), ncol=1))
  ACO_3 <- cbind(ACO_3, Ablock_3)
  
  ACO_4 <- read.csv("./knapsack_output/AOC_7719_8742.csv", header = FALSE)
  Ablock_4 <- as.data.frame(matrix(4, nrow=nrow(ACO_4), ncol=1))
  ACO_4 <- cbind(ACO_4, Ablock_4)
  
  ACO_5 <- read.csv("./knapsack_output/AOC_46329_22872.csv", header = FALSE)
  Ablock_5 <- as.data.frame(matrix(5, nrow=nrow(ACO_5), ncol=1))
  ACO_5 <- cbind(ACO_5, Ablock_5)
  
  ACO_6 <- read.csv("./knapsack_output/AOC_19591_12137.csv", header = FALSE)
  Ablock_6 <- as.data.frame(matrix(6, nrow=nrow(ACO_6), ncol=1))
  ACO_6 <- cbind(ACO_6, Ablock_6)
  
  ACO_7 <- read.csv("./knapsack_output/AOC_8510_33644.csv", header = FALSE)
  Ablock_7 <- as.data.frame(matrix(7, nrow=nrow(ACO_7), ncol=1))
  ACO_7 <- cbind(ACO_7, Ablock_7)
  
  all_ACO <- rbind(ACO_1, ACO_2, ACO_3, ACO_4, ACO_5, ACO_6, ACO_7)

  
  ##NORMALITY TEST
  print(shapiro.test(ACO_1[,2]))
  print(shapiro.test(ACO_2[,2]))
  print(shapiro.test(ACO_3[,2]))
  print(shapiro.test(ACO_4[,2]))
  print(shapiro.test(ACO_5[,2]))
  print(shapiro.test(ACO_6[,2]))
  print(shapiro.test(ACO_7[,2]))
  
  ##MEDIAN ANALYSIS
  
  ##boxplots
  par(mfrow=c(1,2))
  boxplot(optimal_difference~block, data = all_GA, 
          main="Optimal Difference Distribution - GA", 
          ylab="Optimal Difference", 
          xlab="Data Set")
  boxplot(all_ACO[,2]~all_ACO[,5], data = all_ACO, 
          main="Optimal Difference Distribution - ACO", 
          ylab="Optimal Difference", 
          xlab="Data Set")
  
  
  
  print(wilcox.test(GA_1$optimal_difference, ACO_1[,2], paired = TRUE, conf.int = TRUE, conf.level = 0.99))
  print(wilcox.test(GA_2$optimal_difference, ACO_2[,2], paired = TRUE, conf.int = TRUE, conf.level = 0.99))
  print(wilcox.test(GA_3$optimal_difference, ACO_3[,2], paired = TRUE, conf.int = TRUE, conf.level = 0.99))
  print(wilcox.test(GA_4$optimal_difference, ACO_4[,2], paired = TRUE, conf.int = TRUE, conf.level = 0.99))
  print(wilcox.test(GA_5$optimal_difference, ACO_5[,2], paired = TRUE, conf.int = TRUE, conf.level = 0.99))
  print(wilcox.test(GA_6$optimal_difference, ACO_6[,2], paired = TRUE, conf.int = TRUE, conf.level = 0.99))
  print(wilcox.test(GA_7$optimal_difference, ACO_7[,2], paired = TRUE, conf.int = TRUE, conf.level = 0.99))
  
  par(mfrow=c(1,2))
  hist(all_GA$optimal_difference, breaks = seq(from=0.95, to=1, by=0.005), 
       main = "Distribution of Optimal Difference - GA",
       xlab = "Optimal Difference")
  hist(all_ACO[,2], breaks = seq(from=0.68, to=1, by=0.02), 
       main = "Distribution of Optimal Difference - ACO",
       xlab = "Optimal Difference")
  
  
  ##VARIANCE ANALYSIS
  
  #data prep
  
  TestType<-data.frame(rep(c("1", "2"), each = 210))
  colnames(TestType) <- "type"
  
  GAod <-all_GA$optimal_difference
  GAod <- data.frame(GAod)
  colnames(GAod) <- "GA"
  
  ACOod <- all_ACO[,2]
  ACOod <- data.frame(ACOod)
  colnames(ACOod) <- "ACO"
  
  df1 <- cbind(GAod, ACOod)
  
  ALLod <- data.frame(GA = c(df1[,"GA"], df1[,"ACO"]))
  
  BFdf <- cbind(ALLod, TestType)
  
  

  #Levene's test
  print(leveneTest(BFdf[,1]~BFdf[,2], data = BFdf))
 
}
  