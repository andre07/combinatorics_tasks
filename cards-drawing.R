# n - liczba wszystkich kart
# k - liczba kart losowanych jednorazowo
# Wynikiem funkcji jest liczba losowań, którą należy wykonać w celu wylosowania
# każdej karty przynajmniej jeden raz.

losuj_karty <- function(n, k, B = 1000) {
  
  ile_kart <- function(N, K) {
    x <- sample.int(N, K, replace = F)
    while (length(table(x)) < N) x <- c(x, sample.int(N, K, replace = F))
    list(wektor = x, ile = length(x) / K)
  }
  
  RNGkind("Mersenne-Twister")
  RNGkind()
  
  ile <- rep(NA, B)
  
  for (b in 1:B) {
    ile[b] <- ile_kart(n, k)$ile
  }
  
  hist(ile, main = paste0("Liczba potrzebnych losowań", "\n(n=", n, ", k = ", k, ")"), xlab = "liczba losowań")
  abline(v = mean(ile), col = "red")
  abline(v = quantile(ile, c(.9, .95, .99)), col = "grey", lty = 2)
  text(mean(ile), 1, "mean", cex = .9, col = "red", pos = 3)
  text(quantile(ile, c(.9)), 1, "CIL 90%", cex = .8, col = "red", srt = 90, pos = 4)
  text(quantile(ile, c(.95)), 1, "CIL 95%", cex = .8, col = "red", srt = 90, pos = 4)
  text(quantile(ile, c(.99)), 1, "CIL 99%", cex = .8, col = "red", srt = 90, pos = 4)
  
  parametry <- as.data.frame(matrix(NA, 7, 2))
  colnames(parametry) <- c(" ", "l.los")
  parametry[,1] <- c("min", "mean", "ConfIntLev 90%", "ConfIntLev 95%", "ConfIntLev 97%", "ConfIntLev 99%", "max")
  parametry[,2] <- c(ceiling(min(ile)), ceiling(mean(ile)), ceiling(quantile(ile, .9)), ceiling(quantile(ile, .95)),
                     ceiling(quantile(ile, .97)),ceiling(quantile(ile, .99)), ceiling(max(ile)))
  list(par = parametry)
  
}

losuj_karty(20, 8)