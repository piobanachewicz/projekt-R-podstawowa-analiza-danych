#"D:\R-4.1.3\bin\R.exe" CMD BATCH --vanilla "--args dane.csv" projekt.R
setwd("D:/Studia/IV Semestr/R/Projekt")
dataRaw <- read.csv2("dane.csv")

#args = commandArgs(trailingOnly=TRUE)
#if (length(args)==0) {
#  stop("Nie podano argumentow.")
#}
#dataRaw <- read.csv2(file=args[1], header=TRUE)

library(Hmisc)
library(dplyr)
library(stats)
library(purrr)
library(ggpubr)
library(car)
library(dunn.test)
library(FSA)
library(RColorBrewer)

dir.create("output")
dir.create("output/wykresy")

cn <- names(dataRaw)
groupsNames <- c()

for (i in 1:length(dataRaw)){
  if(cn[i] == "grupa" | cn[i] == "group" | cn[i] == "groups" | cn[i] == "grupy"){
    grID <- i
    break
  }
}

groupsNames <- unique(dataRaw[, grID])

#usuniecie brakow

dataModified <- dataRaw

sink("Zmiany.txt")
for(i in 1:length(dataRaw)){
  if(any(is.na(dataRaw[, i]))){
    cat("W danych", cn[i], "wystepuja wartosci puste w wierszach ")
    cat(which(is.na(dataRaw[, i])))
    cat(". Uzupelniono srednia wynoszaca ", mean(dataRaw[, i], na.rm = TRUE), "\n")
    dataModified[, i] <- as.numeric(impute(dataRaw[, i], mean))
  } else {
    cat("W danych", cn[i], "nie wystepuja zadne wartosci puste \n")
  }
}
sink()

#charakterystyka danych
sink("Opis danych.txt")
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

#wartosci odstajace
sink("output/Wartosci odstajce.txt")
for(i in 1:length(dataModified)){
  if(is.numeric(dataModified[, i])){
    cat("Wartosci odstajace w kolumnie ", cn[i], "\n")
    cat(boxplot.stats(dataModified[, i])$out)
    if(!length(boxplot.stats(dataModified[, i])$out)){
      cat("Brak wartosci odstajaych\n")
    }
    cat("\n\n")
  }
}
sink()

pdf("output/wykresy/Wartosci odstajace.pdf")
  for(i in 1:length(dataModified)){
  if(is.numeric(dataModified[, i])){
    boxplot(dataModified[, i], 
            main = c("wartosci odstajace w ", cn[i]),
            ylab = cn[i],
            col = "lightgreen",
            medcol= "darkgreen",
            outcol = "darkgreen",
            outpch = 18)
  }
}
dev.off()


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
                                  SD = sd,
                                  IQR = IQR,
                                  wariancja = var
                                  ))
  
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
                                  SD = sd,
                                  IQR = IQR,
                                  wariancja = var
                                  ))
    
    #write.csv2(summary, file="tmp_csv.csv")
    #summary_csv <- read.csv2("tmp_csv.csv")
    #print(summary_csv)
  print(as.data.frame(summary))
  cat("\n")
}
sink()


#ocena zgodnosci danych z rozkdlaem normalnym
rozklad <- data.frame()
rozklad_iloczyn <- c()
sink("output/ocena zgodnosci z rozkladem normalnym.txt")
for(i in 1:length(dataModified)){
  if(!is.numeric(dataModified[, i])){
    rozklad[1:length(groupsNames), i] <- NA
    next
  }
  tmp <- data.frame("grupa" = dataModified[, grID], "testowana" = dataModified[, i])
  
  shapiro <- group_by(tmp, grupa) %>%
    summarise(
      statistic = shapiro.test(testowana)$statistic,
      p.value = shapiro.test(testowana)$p.value,
    )
  
  cat("\n", cn[i], "\n")
  print(as.data.frame(shapiro))
  cat("\n")
  
  for(j in 1:length(shapiro$p.value)){
    if(shapiro$p.value[j] > 0.05){
      cat("Dla grupy", groupsNames[j], "rozkład danych nie różni się od rozładu normalnego. P-value =", shapiro$p.value[j],"\n")
      rozklad[j, i] <- 1
    } else {
      cat("Dla grupy", groupsNames[j], "rozkład danych różni się znacząco od rozładu normalneg. P-value =", shapiro$p.value[j],"\n")
      rozklad[j, i] <- 0
    }
    
    if(any(is.na(rozklad[, i]))){
      next
    }
    if(sum(rozklad[, i]) == nrow(rozklad)){
      rozklad_iloczyn[i] <- 1
    } else {
      rozklad_iloczyn[i] <- 0
    }
  }
}
#cat("\n\nWykresy gestosci zosatna zapisne w folderze Wykresy gestosci.")
sink()
rm(tmp)


#wykresy gestosci
dir.create("output/wykresy/Wykresy gestosci")
#pdf(file = "output/Wykresy gestosci/test.pdf")

for(i in 1:length(dataModified)){
  #path <- paste("output/Wykresy gestosci/", cn[i], ".jpg", sep ="")
  
  if(is.numeric(dataModified[, i])){
    dens <- ggdensity(dataModified,
              x=cn[i],
              color=cn[grID],
              fill=cn[grID],
              palette = "ucscgb",
              linetype = "blank",
              ylab = "gestość"
    )
    dens
    path <- paste("output/wykresy/Wykresy gestosci/", cn[i], ".jpg", sep ="")
    ggexport(dens, filename = path)
  }
}
#dev.off()

rm(path)

#Ocena homogenicznosci wariancji
homogenicznosc <- c()
sink("output/ocena homogenicznosci wariancji.txt")
for(i in 1:length(dataModified)){
  if(!is.numeric(dataModified[, i])){
    homogenicznosc[i] <- NA
    next
  }  
  
  tmp <- data.frame("grupa" = dataModified[, grID], "testowana" = dataModified[, i])
  #print(leveneTest(testowana ~ grupa, data=tmp))
    if(leveneTest(testowana ~ grupa, data=tmp)$"Pr(>F)"[1] > 0.05){
      cat("Dla danej", cn[i], "możemy zalożyć homogeniczność danych (p-value = ", leveneTest(testowana ~ grupa, data=tmp)$"Pr(>F)"[1] ,").\n")
      homogenicznosc[i] <- 1
    } else {
      cat("Dla danej", cn[i], "nie możemy zalożyć homogeniczności danych (p-value = ", leveneTest(testowana ~ grupa, data=tmp)$"Pr(>F)"[1] ,").\n")
      homogenicznosc[i] <- 0
    }
}
sink()
rm(tmp)

#testy statystyczne
dir.create("output/wykresy/Wykresy zaleznosci")
sink("output/porownanie grup.txt")
for(i in 1:length(rozklad_iloczyn)){
  if(i == grID){
    next
  }
  tmp <- data.frame("grupa" = dataModified[, grID], "testowana" = dataModified[, i])
  
  if(is.na(rozklad_iloczyn[i]) | is.na(homogenicznosc[i])){
    
    tmp_p <- chisq.test(tmp$testowana, tmp$grupa)$p.value
    
    path <- paste("output/wykresy/Wykresy zaleznosci/", cn[i], ".jpg", sep ="")
    jpeg(file = path)
    
    barplot(table(tmp$testowana, tmp$grupa),
            ylim = c(0,30),
            beside = TRUE,
            col = terrain.colors(length(unique(tmp$testowana))),
            xlab = cn[grID],
            ylab = "wartos",
            legend = unique(tmp$testowana)
    )
    dev.off()
    
    if(tmp_p < 0.05){
      cat("Dla danej", cn[i], "występują różnice pomiędzy grupami. P-value =", tmp_p,"\n")
    } else {
      cat("Dla danej", cn[i], "nie występują różnice pomiędzy grupami. P-value =", tmp_p,"\n")
    }
    #cat("\nWykres zapisano do pliku", path)
    
    rm(path)
    next
  }
  
  if(length(groupsNames) > 2){
        
      if(rozklad_iloczyn[i] == 1 & homogenicznosc[i] == 1){
        #test ANOVA (post hoc Tukeya)
        tmp_p <- summary(aov(testowana ~ grupa, data = tmp))[[1]][["Pr(>F)"]][[1]]
        if(tmp_p < 0.05){
          cat("Dla danej", cn[i], "występują różnice pomiędzy grupami. P-value =", tmp_p,"\nTest Tukeya:\n\n")
          print(TukeyHSD(aov(testowana ~ grupa, data = tmp)))
          cat("\n")
        } else {
          cat("Dla danej", cn[i], "nie występują różnice pomiędzy grupami. P-value =", tmp_p,"\n")
        }
        
      } else {
        #test Kruskala-Wallisa (post hoc Dunna)
        tmp_p <- kruskal.test(testowana ~ grupa, data = tmp)$p.value
                    
        if(tmp_p < 0.05){
          cat("Dla danej", cn[i], "występują różnice pomiędzy grupami. P-value =", tmp_p,"\nTest Dunna:\n\n")
          print(dunnTest(tmp$testowana, tmp$grupa))
          cat("\n")
        } else {
          cat("Dla danej", cn[i], "nie występują różnice pomiędzy grupami. P-value =", tmp_p,"\n")
        }
      }
  } else {
      if(rozklad[j,i] == 0){
        #test Wilcoxona (Manna-Whitneya)
        tmp_p <- wilcox.test(testowana ~ grupa, data = tmp)$p.value
        if(tmp_p < 0.05){
          cat("Dla danej", cn[i], "występują różnice pomiędzy grupami. P-value =", tmp_p,"\n")
        } else {
          cat("Dla danej", cn[i], "nie występują różnice pomiędzy grupami. P-value =", tmp_p,"\n")
        }
        
      } else if(rozklad[j,i] == 1){
        if(homogenicznosc[i] == 0){
          #test Welcha
          tmp_p <- t.test(testowana ~ grupa, data = tmp, var.equal = FALSE)
          if(tmp_p < 0.05){
            cat("Dla danej", cn[i], "występują różnice pomiędzy grupami. P-value =", tmp_p,"\n")
          } else {
            cat("Dla danej", cn[i], "nie występują różnice pomiędzy grupami. P-value =", tmp_p,"\n")
          }
          
      } else if(homogenicznosc[i] == 1){
         #test t-Studenta
        tmp_p <- t.test(testowana ~ grupa, data = tmp, var.equal = TRUE)
        if(tmp_p < 0.05){
          cat("Dla danej", cn[i], "występują różnice pomiędzy grupami. P-value =", tmp_p,"\n")
        } else {
          cat("Dla danej", cn[i], "nie występują różnice pomiędzy grupami. P-value =", tmp_p,"\n")
        }
      }
    }
  }
  rm(tmp)
  rm(tmp_p)
}
sink()

#Korelacje
okresl_korelacje <- function(cor){
  if(cor > 1 && cor < -1){
    paste("nie można określić korelacji.")
  } else if(-1 < cor && cor < -0.7){ 
    paste("korelacja jest bardzo silna ujemna.")
  } else if(-0.7 < cor && cor < -0.5){ 
    paste("korelacja jest silna ujemna.")
  } else if(-0.5 < cor && cor < -0.3){ 
    paste("korelacja jest ujemna o średnim natężeniu.")
  } else if(-0.3 < cor && cor < -0.2){ 
    paste("korelacja jest słaba ujemna.")
  } else if(-0.2 < cor && cor < 0.2){ 
    paste("brak korelacji")
  } else if(0.2 < cor && cor < 0.3){ 
    paste("korelacja jest słaba dodatnia.")
  } else if(0.3 < cor && cor < 0.5){ 
    paste("korelacja jest dodatnia o średnim natężeniu.")
  } else if(0.5 < cor && cor < 0.7){ 
    paste("korelacja jest silna dodatnia.")
  } else if(0.7 < cor && cor < 1){ 
    paste("korelacja jest bardzo silna dodatnia.")
  }
}

sink("output/korelacje.txt")
for(i in 1:length(groupsNames)){
  cat("_________Korelacje w grupie", groupsNames[i],"_________\n")
  
  kor_table <- data.frame((matrix(ncol = length(cn), nrow = length(cn))), row.names = cn)
  colnames(kor_table) <- cn
  kor_table[grID,grID] <- "#"
  
  dir.create(paste("output/wykresy/korelacje_",groupsNames[i], sep = ""))
  tested <- dataModified %>% filter(dataModified[, grID] == groupsNames[i])
  #pdf(file = paste("output/korelacja_",groupsNames[i],".pdf", sep = ""))
  for(j in 1:length(dataModified)){
    if (j == grID){
      next
    }
    cat("\n")
    for(k in 1:length(dataModified)){
      if (k == grID | j == k | !any(is.numeric(dataModified[, j])) | !any(is.numeric(dataModified[, k]))){
        kor_table[j,k] <- "#"
        kor_table[k,j] <- "#"
        next
      }
      
      kor <- cor.test(tested[, j], tested[, k], method="pearson")
      
      if(kor$p.value < 0.05){
        cat("Dla danych",cn[j], "i", cn[k], okresl_korelacje(kor$estimate),"\n")
        
        if(kor$estimate < -0.2){
          kor_table[j,k] <- "-"
        } else if(kor$estimate > 0.2){
          kor_table[j,k] <- "+"
        } else {
          kor_table[j,k] <- "0"
        }
        
        kor_plot <- ggscatter(tested,
                              x=cn[j], 
                              y=cn[k],
                              add="reg.line",
                              conf.int=TRUE,
                              cor.coef=TRUE,
                              cor.method="pearson",
                              color="darkgreen",
                              fill="lightgreen",
        )
        ggexport(kor_plot, filename = paste("output/wykresy/korelacje_",groupsNames[i],"/korelacja_",cn[j],"_",cn[k],".jpg", sep = ""))
        #dev.off()
        #cat("Wykresy zostały narysowane w output/wykresy/korelacje_",groupsNames[i],"/korelacja_",cn[j],"_",cn[k],".jpg\n\n", sep = "")
      } else {
        cat("Dla danych",cn[j], "i", cn[k], "p-value =", kor$p.value, "co wskazuje na brak korelacji miedzy danymi (p-value < 0.05).\n")
        kor_table[j,k] <- "0"
      }
    }
    write.csv2(kor_table, file = (paste("output/wykresy/korelacje_", groupsNames[i],"/korelacje_", groupsNames[i],".csv", sep = "")))
  }
  cat("\n")
}
sink()

sink("output/korelacje_podsumowanie.txt")
for(i in 1:length(groupsNames)){
  cat("Korelacje w gupie",groupsNames[i],":\n")
  print(read.csv2(paste("output/wykresy/korelacje_", groupsNames[i],"/korelacje_", groupsNames[i],".csv", sep = "")))
  cat("\n")
}
sink()