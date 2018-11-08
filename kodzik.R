install.packages("genalg")
library(genalg)

fitness <- function(chr){
  correctCounter <- 0
  
  for(row in 1:nrow(testMatrix)){
    neg1 <- FALSE
    neg2 <- FALSE
    neg3 <- FALSE
    
    if(testMatrix[row, 1] < 0){
      id1 <- abs(testMatrix[row, 1])
      nr1 <- chr[id1]
      neg1 <- TRUE
    }
    else {
      nr1 <- chr[testMatrix[row, 1]]
    }
    
    if(testMatrix[row, 2] < 0){
      id2 <- abs(testMatrix[row, 2])
      nr2 <- chr[id2]
      neg2 <- TRUE
    }
    else {
      nr2 <- chr[testMatrix[row, 2]]
    }
    
    if(testMatrix[row, 3] < 0){
      id3 <- abs(testMatrix[row, 3])
      nr3 <- chr[id3]
      neg3 <- TRUE
    }
    else {
      nr3 <- chr[testMatrix[row, 3]]
    }
    
    if (neg1 == TRUE)
      nr1 = !nr1
    
    if (neg2 == TRUE)
      nr2 = !nr2
    
    if (neg3 == TRUE)
      nr3 = !nr3
    
    if ((nr1 | nr2 | nr3) == TRUE)
      correctCounter = correctCounter + 1
  }
  return ((-1) * correctCounter)
}

testMatrix <- read.table("test2.csv", header = FALSE)
matrix <- rbga.bin(size = 4, popSize = 200, iters = 100, mutationChance = 0.05, elitism = T, evalFunc = fitness)

time1 <- system.time(rbga.bin(size = 4, popSize = 200, iters = 100, mutationChance = 0.05, elitism = T, evalFunc = fitness))

testMatrix <- read.table("test20-91.csv", header = FALSE)
time2 <- system.time(rbga.bin(size = 20, popSize = 200, iters = 100, mutationChance = 0.05, elitism = T, evalFunc = fitness))

testMatrix <- read.table("test50-218.csv", header = FALSE)
time3 <- system.time(rbga.bin(size = 50, popSize = 200, iters = 100, mutationChance = 0.05, elitism = T, evalFunc = fitness))

testMatrix <- read.table("test75-325.csv", header = FALSE)
time4 <- system.time(rbga.bin(size = 75, popSize = 200, iters = 100, mutationChance = 0.05, elitism = T, evalFunc = fitness))

testMatrix <- read.table("test100-430.csv", header = FALSE)
time5 <- system.time(rbga.bin(size = 100, popSize = 200, iters = 100, mutationChance = 0.05, elitism = T, evalFunc = fitness))


plotData <- data.frame(clauses = c(7, 91, 218, 325), times = c(time1[3], time2[3], time3[3], time4[3]))
plot(plotData$clauses, plotData$times, type="l", lwd=3, xlab="Ilosc klauzul", ylab="Potrzebny czas", col="blue")


testMatrix <- read.table("test20-20.csv", header = FALSE)
timeCla1 <- system.time(rbga.bin(size = 20, popSize = 200, iters = 100, mutationChance = 0.05, elitism = T, evalFunc = fitness))

testMatrix <- read.table("test20-40.csv", header = FALSE)
timeCla2 <- system.time(rbga.bin(size = 20, popSize = 200, iters = 100, mutationChance = 0.05, elitism = T, evalFunc = fitness))

testMatrix <- read.table("test20-80.csv", header = FALSE)
timeCla3 <- system.time(rbga.bin(size = 20, popSize = 200, iters = 100, mutationChance = 0.05, elitism = T, evalFunc = fitness))

testMatrix <- read.table("test20-91.csv", header = FALSE)
timeCla4 <- system.time(rbga.bin(size = 20, popSize = 200, iters = 100, mutationChance = 0.05, elitism = T, evalFunc = fitness))

plotDataCla <- data.frame(clauses = c(20, 40, 80, 91), times = c(timeCla1[3], timeCla2[3], timeCla3[3], timeCla4[3]))
plot(plotDataCla$clauses, plotData$times, type="l", lwd=3, xlab="Ilosc klauzul", ylab="Potrzebny czas", col="blue")