
local({
  .Table <- data.frame(Probability=dbinom(0:10, size=10, prob=0.5))
  rownames(.Table) <- 0:10 
  print(.Table)
})
local({
  .Table <- data.frame(Probability=dbinom(0:10, size=10, prob=0.2))
  rownames(.Table) <- 0:10 
  print(.Table)
})
BinomialSamples <- as.data.frame(matrix(rbinom(1*5, size=10, prob=0.5), ncol=5))
rownames(BinomialSamples) <- "sample"
colnames(BinomialSamples) <- paste("obs", 1:5, sep="")
#seleccionar BinomialSamples para ver el resultado
BinomialSamples <- within(BinomialSamples, {
  mean <- rowMeans(BinomialSamples[,1:5])
  sum <- rowSums(BinomialSamples[,1:5])
  sd <- apply(BinomialSamples[,1:5], 1, sd)
})
BinomialSamples <- as.data.frame(matrix(rbinom(1*100, size=10, prob=0.5), ncol=100))
rownames(BinomialSamples) <- "sample"
colnames(BinomialSamples) <- paste("obs", 1:100, sep="")
BinomialSamples <- within(BinomialSamples, {
  mean <- rowMeans(BinomialSamples[,1:100])
  sum <- rowSums(BinomialSamples[,1:100])
  sd <- apply(BinomialSamples[,1:100], 1, sd)
})
pbinom(c(2,5,9), size=10, prob=0.2, lower.tail=TRUE)
pbinom(c(2,3,4,5,9), size=10, prob=0.2, lower.tail=TRUE)
qbinom(c(0.2,0.5,0.6,0.75,0.8), size=10, prob=0.2, lower.tail=TRUE)
local({
  .x <- 0:7
  plotDistr(.x, dbinom(.x, size=10, prob=0.2), xlab="Number of Successes", ylab="Probability Mass",
   main="Binomial Distribution:  Binomial trials=10, Probability of success=0.2", discrete=TRUE)
})
local({
  .Table <- data.frame(Probability=dpois(0:10, lambda=3))
  rownames(.Table) <- 0:10 
  print(.Table)
})
sum(dpois(0:10, lambda=3))

