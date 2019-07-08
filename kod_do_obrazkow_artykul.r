

library(plotly)





dane = read.csv("typologia_2017-01-7_UTF.csv", sep = "\t", row.names = 1, encoding = "UTF-8")

colnames(dane) = gsub("\\.", "+", colnames(dane))
colnames(dane) = gsub("^X", "", colnames(dane))





custom_colors = rgb(
rbind(
    c(250,0,232),
    c(115,192,0),
    c(255,136,5),
    c(99,26,255),
    c(223,137,255),
    c(168,0,45),
    c(0,196,255),
    c(211,179,176),
    c(82,99,255),
    c(204,0,129),
    c(76,70,60),
    c(0,189,148),
    c(255,211,112),
    c(255,85,132)
), 
alpha = 155,
names = c("szkice", "fakt", "urzęd", "konwers", "lit", "media", "nd", "nklas", "inf-por", "publKs", "publDz", "publPer", "publReg", "qmow"),
#names = sort(unique(gsub("_.*", "", rownames(dane)))),
maxColorValue = 255
)

#text.types = c("Esej", "Fakt", "Ius", "Konwers", "Lit", "Mmedia", "ND", "Nklas", "Inf-por", "PubKs", "PublDz", "PublPer", "PublReg", "Sejm")

#text.types = sort(unique(gsub("_.*", "", rownames(dane))))

text.types = c("szkice", "fakt", "urzęd", "konwers", "lit", "media", "nd", "nklas", "inf-por", "publKs", "publDz", "publPer", "publReg", "qmow")




# następujące 3 zmienne mają takie same wartości:
# pos_cialo_obce_luzne pos_cialo_obce_nominalne pos_numcol

# usuwanie ich
dane = dane[, apply(dane,2,sd) != 0]


# losowe przemieszanie próbek, żeby na wykresie nie było warstw:
set.seed(1)
dane = dane[sample(1:length(dane[,1])),]


wynik = prcomp(scale(dane))






# najsilniejsze predyktory na PC1
# sort(wynik$rotation[,1])[1:5]
# sort(wynik$rotation[,1], decreasing=T)[1:5]

predyktory1 = colnames(dane) %in% names(c(sort(wynik$rotation[,1])[1:5], sort(wynik$rotation[,1], decreasing=T)[1:5], sort(wynik$rotation[,2])[1:2], sort(wynik$rotation[,2], decreasing=T)[1:2]))

optymalna_separacja = dane[, predyktory1]

# dodanie marginesu
zakres.obrazka = c(min(wynik$rotation[,1]) + 0.2 * min(wynik$rotation[,1]), max(wynik$rotation[,1]) + 0.2 * max(wynik$rotation[,1]))

# get the variation explained by the PCs:
  expl.var = round(((wynik$sdev^2)/sum(wynik$sdev^2)*100),1)
  PC1_lab = paste("PC1 (",expl.var[1],"%)", sep="")
  PC2_lab = paste("PC2 (",expl.var[2],"%)", sep="")







#### Wersje czarno-białe


# rys. 2
#
#
png("Rys__2.png", width = 9, height = 7, units = "in", res = 300)
plot(wynik$x[,2] ~ wynik$x[,1], type = "n", xlab = PC1_lab, ylab = PC2_lab, main = "Analiza głównych czynników", xlim = c(-12,20))
points(wynik$x[,1], wynik$x[,2], pch = as.numeric(factor(gsub("_.*","",rownames(dane)))), col = grey(as.numeric(factor(gsub("_.*","",rownames(dane))))/14) )
legend("topright", legend = names(custom_colors), text.col = grey(as.numeric(factor(text.types))/14),  pch = as.numeric(factor(text.types)), col = grey(as.numeric(factor(text.types))/14), bty = "n")
dev.off()

# rys. 3
#
#
png("Rys__3.png", width = 9, height = 7, units = "in", res = 300)
custom_colors1 = as.character(factor(c("darkgrey","black"))[as.numeric(predyktory1)+1])
plot(wynik$rotation[,2] ~ wynik$rotation[,1], type = "n", xlim = zakres.obrazka, xlab = PC1_lab, ylab = PC2_lab, main = "Analiza głównych czynników (ładunki)")
segments(0,0,wynik$rotation[,1], wynik$rotation[,2], col = "lightblue")
text(wynik$rotation[,1], wynik$rotation[,2], labels = colnames(dane), col = custom_colors1)
dev.off()





#### Wersje kolorowe na GitHub




# rys. 2
#
#
png("Rys_2.png", width = 9, height = 7, units = "in", res = 150)
plot(wynik$x[,2] ~ wynik$x[,1], type = "n", xlab = PC1_lab, ylab = PC2_lab, main = "Analiza głównych czynników", xlim = c(-12,20))
points(wynik$x[,1], wynik$x[,2], col = custom_colors[gsub("_.*","",rownames(dane))] , pch = 20 )
legend("topright", legend = names(custom_colors), text.col = custom_colors,  pch = 20, col = custom_colors, bty = "n")
dev.off()

# rys. 3
#
#
png("Rys_3.png", width = 9, height = 7, units = "in", res = 150)
custom_colors1 = as.character(factor(c("red","blue"))[as.numeric(predyktory1)+1])
plot(wynik$rotation[,2] ~ wynik$rotation[,1], type = "n", xlim = zakres.obrazka, xlab = PC1_lab, ylab = PC2_lab, main = "Analiza głównych czynników (ładunki)")
segments(0,0,wynik$rotation[,1], wynik$rotation[,2], col = "lightblue")
text(wynik$rotation[,1], wynik$rotation[,2], labels = colnames(dane), col = custom_colors1)
dev.off()











# rys. 2
#
#

# plot interactively
p <- plot_ly(x = wynik$x[,1], y = wynik$x[,2], type = "scatter", mode = "markers", color = gsub("_.*","",rownames(dane)), colors = custom_colors, text = gsub("_.*","",rownames(dane)))
p


# rys. 3
#
#

# plot interactively
p <- plot_ly(x = wynik$rotation[,1], y = wynik$rotation[,2], text = colnames(dane)) %>%
#add_segments(x = 0, y = 0, xend = wynik$rotation[,1], yend = wynik$rotation[,2], mode = "lines", colors=c("red"))

add_segments(x = 0, y = 0, xend = wynik$rotation[,1], yend = wynik$rotation[,2], mode = "lines",     linetypes = list(
      color = 'rgb(17, 157, 255)', opacity = 0.2)
) %>%
add_text(wynik$rotation[,1], wynik$rotation[,2], text = colnames(dane), textposition = "top right", showlegend = F)
p




  add_trace(
    x = x,
    y = y,
    marker = list(
      color = 'rgb(17, 157, 255)',
      size = 20,
      line = list(
        color = 'rgb(231, 99, 250)',
        width = 2
      )























# wywalenie najmocniejszych predyktorów:

dane = dane[,!predyktory1]

wynik = prcomp(scale(dane))

# get the variation explained by the PCs:
  expl.var = round(((wynik$sdev^2)/sum(wynik$sdev^2)*100),1)
  PC1_lab = paste("PC1 (",expl.var[1],"%)", sep="")
  PC2_lab = paste("PC2 (",expl.var[2],"%)", sep="")

# rys 4
png("PCA_bez_15predyktorow.png", width=9, height=7, units="in", res=300)
plot(wynik$x[,2] ~ wynik$x[,1], type = "n", xlab = PC1_lab, ylab = PC2_lab, main = "Analiza głównych czynników", xlim = c(-6,14))
points(wynik$x[,1], wynik$x[,2], col=custom_colors[gsub("_.*","",rownames(dane))] , pch = 20 )
legend("topright", legend = names(custom_colors), text.col = custom_colors,  pch = 20, col = custom_colors, bty = "n")
dev.off()


# plot interavtively
p <- plot_ly(x = wynik$x[,1], y = wynik$x[,2], color = gsub("_.*","",rownames(dane)), colors = custom_colors, text = gsub("_.*","",rownames(dane)))
p






predyktory2 = names(sort(wynik$rotation[,1], decreasing=T)[1:5])
predyktory3 = names(sort(wynik$rotation[,2])[1:5])

dane = dane[, !names(dane) %in% c(predyktory2, predyktory3)]

wynik = prcomp(scale(dane))

# get the variation explained by the PCs:
  expl.var = round(((wynik$sdev^2)/sum(wynik$sdev^2)*100),1)
  PC1_lab = paste("PC1 (",expl.var[1],"%)", sep="")
  PC2_lab = paste("PC2 (",expl.var[2],"%)", sep="")

# rys 5
png("PCA_bez_10_dalszych_predyktorow.png", width=9, height=7, units="in", res=300)
plot(wynik$x[,2] ~ wynik$x[,1], type = "n", xlab = PC1_lab, ylab = PC2_lab, main = "Analiza głównych czynników", xlim = c(-7,10))
points(wynik$x[,1], wynik$x[,2], col=custom_colors[gsub("_.*","",rownames(dane))] , pch = 20 )
legend("topright", legend = names(custom_colors), text.col = custom_colors,  pch = 20, col = custom_colors, bty = "n")
dev.off()


# plot interavtively
p <- plot_ly(x = wynik$x[,1], y = wynik$x[,2], color = gsub("_.*","",rownames(dane)), colors = custom_colors, text = gsub("_.*","",rownames(dane)))
p






optymalna_separacja_2 = dane[, names(dane) %in% c(predyktory2, predyktory3)]
optymalna_separacja = cbind(optymalna_separacja, optymalna_separacja_2)

wynik = prcomp(scale(optymalna_separacja))

# get the variation explained by the PCs:
  expl.var = round(((wynik$sdev^2)/sum(wynik$sdev^2)*100),1)
  PC1_lab = paste("PC1 (",expl.var[1],"%)", sep="")
  PC2_lab = paste("PC2 (",expl.var[2],"%)", sep="")

# rys 4
plot(wynik$x[,2] ~ wynik$x[,1], type = "n", xlab = PC1_lab, ylab = PC2_lab, main = "Analiza głównych czynników")
points(wynik$x[,1], wynik$x[,2], col=custom_colors[gsub("_.*","",rownames(dane))] , pch = 20 )






# stary poczciwy biplot:
# biplot(prcomp(scale(dane)))



