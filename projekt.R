#"D:\R-4.1.3\bin\R.exe" CMD BATCH --vanilla "--args dane.csv" projekt.R

#args = commandArgs(trailingOnly=TRUE)
#if (length(args)==0) {
#  stop("Nie podano argumentow.")
#}
#dataRaw <- read.csv2(file=args[1], header=TRUE)
setwd("D:/Studia/IV Semestr/R/Projekt")
dir.create("output")
dir.create("tmp")

library(Hmisc)
library(dplyr)
library(stats)
library(purrr)

dataRaw <- read.csv2("dane.csv")
cn <- names(dataRaw)
groupsNames <- c()

for (i in 1:length(dataRaw)){
  if(cn[i] == "grupa" | cn[i] == "group"){
    grID <- i
    for(j in 1:nrow(dataRaw)){
      if(!(dataRaw[j, i] %in% groupsNames)){
        groupsNames <- append(groupsNames, c(dataRaw[j, i]))
      }
    }
    break
  }
}

#usuniecie brakow i wypisanie wartosci odstajacych

dataModified <- dataRaw

sink("output/zmiany.txt")
for(i in 1:length(dataRaw)){
  if(any(is.na(dataRaw[, i]))){
    cat("W kolumnie ", i, "wystepuja wartosci puste w wierszach ")
    cat(which(is.na(dataRaw[, i])))
    cat(".\nUzupelniono srednia wynoszaca ", mean(dataRaw[, i], na.rm = TRUE), "\n\n")
  } #else {
    #cat("W kolumnie ", i, "nie wystepuja zadne wartosci puste \n")
  #}
  dataModified[, i] <- impute(dataRaw[, i], mean)
}


for(i in 1:length(dataModified)){
  if(is.numeric(dataModified[, i])){
    cat("\nWartosci odstajace w kolumnie ", cn[i], "\n")
    cat(boxplot.stats(dataModified[, i])$out)
    if(!length(boxplot.stats(dataModified[, i])$out)){
      cat("Brak wartosci odstajaych\n")
    }
  }
}

sink()

pdf("output/Wartosci odstajace.pdf")
for(i in 1:length(dataModified)){
  if(is.numeric(dataModified[, i])){
    boxplot(dataModified[, i], main = c("wartosci odstajace w ", cn[i]))
  }
}
dev.off()

#charakterystyka danych
sink("output/Opis danych.txt")
ch <- data.frame(row.names = c("rodzaj", "typ", "maksymalna wartosc", "minimalna wartosc"))
for(i in 1:length(dataModified)){
  ch[1, i] = class(dataModified[1, i])
  if(is.numeric(dataModified[1, i])){
    ch[2, i] = "Dane ilosciowe"
    ch[3, i] = max(dataModified[, i])
    ch[4, i] = min(dataModified[, i])
  } else {
    ch[2, i] = "Dane jakosciowe"
    ch[3, i] = "Nie dotyczy"
    ch[4, i] = "Nie dotyczy"
  }
}
colnames(ch) = cn
print(ch)
sink()

#podsumowanie danych
sink("output/Podsumowanie.txt")
cat("Podsumowanie statystyk dla poszczegolnych grup bez podzialu na plec\n")

tmp <- data.frame("grupa" = dataModified[, grID])

for(i in 1:length(dataModified)){
  if(!is.numeric(dataModified[, i])){
    next
  }
  tmp[2] <- dataModified[, i]
  cat("\n", cn[i], "\n")
  
  summary <- group_by(tmp, tmp[1]) %>%
    summarize_if(is.numeric, list(minimun = min,
                                  maksimum = max,
                                  mediana = median,
                                  srednia = mean,
                                  odchylenie_standardowe = sd,
                                  IQR = IQR))
  
  #write.csv2(summary, file="tmp_csv.csv")
  #summary_csv <- read.csv2("tmp_csv.csv")
  #print(summary_csv)
  print(as.data.frame(summary))
  cat("\n")
}
rm(tmp)

cat("\n\nPodsumowanie statystyk dla poszczegolnych grup z podzialem na plec\n")

genderID <- which(cn == "plec" | cn == "gender" | cn == "sex")

#summary <- group_by(dataModified, dataModified[, grID], dataModified[, genderID], .add = TRUE) %>%

tmp <- data.frame("grupa" = dataModified[, grID], "plec" = dataModified[, genderID])
for(i in 1:length(dataModified)){
  if(!is.numeric(dataModified[, i])){
    next
  }
  tmp[3] <- dataModified[, i]
  cat("\n", cn[i], "\n")
  
  summary <- group_by(tmp, tmp[1], tmp[2], .add = TRUE) %>%
    summarize_if(is.numeric, list(minimun = min,
                                  maksimum = max,
                                  mediana = median,
                                  srednia = mean,
                                  odchylenie_standardowe = sd,
                                  IQR = IQR))
    
    #write.csv2(summary, file="tmp_csv.csv")
    #summary_csv <- read.csv2("tmp_csv.csv")
    #print(summary_csv)
  print(as.data.frame(summary))
  cat("\n")
}
sink()