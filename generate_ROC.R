generate_ROC <-  function(scores, real, target){
  # Generar curva
  real = c(2, 2, 1, 2, 2, 2, 2, 1,
          2, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 1)
  scores = c(0.9, 0.8, 0.7, 0.6, 0.55, 0.54, 0.53, 0.52, 0.5, 0.5,
          0.5, 0.5, 0.38, 0.37, 0.36, 0.35, 0.34, 0.33, 0.30, 0.1)
  target = 2
  # real <- lsorted
  # R es (x,y)
  x <- c()
  y <- c() 
  real <- sort(real,decreasing = T)
  FP <- 0
  TP <- 0
  #f(i) son los scores
  fprev <- -9999999
  i <- 1
  while(i <=  abs(real)){
    if(real != fprev){
      x <- c(x,(FP / N))
      y <- c(y,(TP / P))
      #push(FP/N, TP/P) onto R
      fprev <- scores(i)
    }
    if(real[i] > 0){
      TP <- TP + 1
    }else{
      FP <- FP +1 
    }
  }
  x <- c(x,(FP / N))
  y <- c(y,(TP / P))
  
  plot(x = x,
       y = y,
       xlim=c(0, 1),
       ylim=c(0, 1),
       type = "l",
       main = "Curva ROC",
       xlab = "Falso Positivo",
       ylab = "Verdadero Positivo" )
  
}