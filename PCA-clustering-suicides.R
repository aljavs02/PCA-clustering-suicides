library(tidyverse)
library(cluster)
library(ggrepel)
library(GGally)       
library(fpc)      
library(corrplot)
library(DataExplorer) 
library(plotly)       
library(FactoMineR)   
library(factoextra)
library(psych)
library(countrycode)
library(eurostat)
library(maps)
library(sf)
library(scales)
library(countrycode)


#Wczytanie zbioru i pozbycie się przejściowych kolumn służących do wyliczenia wskaźnika samobójstw 

suic <- read.csv2("./data/dane.csv")
suic <- suic[,-c(2,3,4)]
colnames(suic)[1] = "country"
colnames(suic)[7] = "general_diseases"


### Podstawowe zależności między danymi

# Obliczenie korelacji między zmiennymi za pomocą funkcji "cor" oraz wizualizacja korelacji za pomocą funkcji "corrplot".
cor(suic[,3:8])
corrplot(cor(suic[, 3:8]), order = "hclust", tl.cex = 0.7)


### Analiza PCA (Principal Component Analysis)

# Wykonanie testu Bartletta, który służy do sprawdzenia hipotezy o tym, że macierz korelacji jest macierzą jednostkową (czyli zmienna niezależna nie jest skorelowana z żadną inną zmienną). 
cortest.bartlett(cor(suic[, 3:8]), n = nrow(suic)) 

# WYkonanie testu KMO (Kaiser-Meyer-Olkin), który służy do oceny przydatności analizy PCA dla danego zbioru danych.
KMO(suic[,3:8]) 

# Wykonanie PCA za pomocą funkcji "principal", gdzie "nfactors" określa liczbę składowych do utworzenia, a "rotate" określa sposób rotacji składowych. Wyniki PCA są zapisane w zmiennej "pr1".
pr1 <- principal(suic[,3:8], nfactors = 6, rotate = "verimax") 

# Wyświetlenie ładunków (loadings), które określają, jak mocno każda zmienna wpływa na daną składową oraz wartości własnych, które określają, jak wiele wariancji jest wyjaśniane przez każdą składową.
pr1$loadings 

# Wyświetlenie wykresu osypiska, który przedstawia wartości własne i pozwala określić, ile składowych należy wybrać (według kryterium Keisera, wybiera się te, które mają wartości własne większe niż 1).
plot(pr1$values, type="b") 
abline(h=1, col="red") 
plot(cumsum(pr1$values)/6, type="b") 


#Po ponownym przeprowadzeniu redukcji wymiarów można wyodrębnić następujące składowe: 
  
#PC1 - silniejszy wpływ czynników związanych z kondycją społeczną danego kraju
  #silna dodatnia korelacja ze wskaźnikiem samobójstw (0,8), Stopniodniami ogrzewania (0,71) oraz wskaźnikiem chorobliwości (0,59) - zmienne te mają największy wpływ na wyjaśnienie wariancji w PC1.

#PC2 - przeważający wpływ czynników ekonomicznych 
  #silna dodatnia korelacja ze wskaźnikiem bezrobocia (0,62), wskaźnikiem rozwodów (0,63), wskaźnikiem deprywacji materialnej (0,38), wskaźnikiem wykonywanych nadgodzin (0,36) - zmienne te mają największy wpływ na wyjaśnienie wariancji w PC2


# Wykonanie PCA dla dwóch składowych i zapisanie wyników w zmiennej "pr2".
pr2 <- principal(suic[,3:8], nfactors=2, rotate="verimax") 

# Wyświetlenie ładunków składowych oraz wykresu biplot, który przedstawia zależności między zmiennymi oraz ich korelacje z dwoma pierwszymi składowymi.
pr2$loadings
biplot(pr2)

suic.pca <- pr2$scores %>% 
  as.data.frame() %>% 
  mutate(country = suic$country)

options(ggrepel.max.overlaps = Inf)

suic.pca %>% 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_text_repel(aes(label = country), size = 2.5) + 
  geom_point(shape = 21, fill = "#6D6BBD", size = 3) + theme_bw() + xlab("Kondycja społeczna kraju") + ylab("Czynniki ekonomiczne") 

# Regresja dla wymiarów pca 

df <-  as.data.frame(pr2$scores)
df$y = suic$suicide_rate

df

model1 <- lm(y ~ RC1, df)
summary(model1)

model2 <- lm(y ~ ., df)
summary(model2)

anova(model1, model2)
### Grupowanie

## Grupowanie hierarchiczne

# Przeprowadzenie standaryzacji zmiennych 

suic.scaled <- scale(suic[, 3:8]) %>% as.data.frame()
rownames(suic.scaled) <- suic$country


# sprawdzenie rezultatu
summary(suic.scaled) 
pairs(suic.scaled)


# Stworzenie macierzy dystansu potrzebnej w celu przedstawienia odległości między obserwacjami w zbiorze danych.
ds <- dist(suic.scaled, method = "euclidean")
ds

# Gupowanie metodą warda ma na celu stworzenie hierarchii klastrów, które są jak najbardziej podobne do siebie wewnątrz, ale jak najbardziej różne między sobą na zewnątrz.
hc1s <- hclust(ds, method = "ward.D2")

## Dendogram

# Wyswietlenie dendrogramu - plot na obiekcie hclust
plot(hc1s)
fviz_dend(hc1s, k = 4, rect = TRUE, color_labels_by_k = TRUE, cex = 0.5, palette = c("#FFEC8B","#87CEFF","#CD5555","#7CCD7C"))

# Wybór liczby grup
cutree(hc1s, k = 2) 
cutree(hc1s, k = 3)
cutree(hc1s, k = 4)
cutree(hc1s, k = 5) 
cutree(hc1s, k = 4) %>% table()

suic$cluster.w <- cutree(hc1s, k = 4) %>% as.factor()

#Na podstawie powyższego dendogramu, można dojść do wniosku, że optymalną liczbą grup będzie 4, ponieważ odległość pomiędzy klastrami 4 i 5 jest największa. Również posługując się funkcję "cutree" dochodzimy do podobnego wniosku. 

## Grupowanie k-średnich

# Stworzenie obiektu klasy k-means

set.seed(10)
km1s <- kmeans(suic.scaled, centers = 4, nstart = 10)

km1s$cluster
km1s$withinss
km1s$tot.withinss

# Stworzenie wykresu osypiska WSS
x <- rep(0, 10) 
# petla:
for(i in 1:10)
  x[i] <- kmeans(suic.scaled, centers = i, nstart = 10)$tot.withinss

# WYkres:
plot(x, type = "b")

# Kryterium ch (Calinskiego-Harabasza) 
kms.ch <- kmeansruns(suic.scaled, criterion = "ch", runs = 10)
plot(kms.ch$crit, type = "b")

# wniosek: 2 grupy

# Kryterium asw (Average Silhouette)
kms.asw <- kmeansruns(scale(suic.scaled), criterion = "asw", runs = 10)
plot(kms.asw$crit, type = "b")

# wniosek: 2 grupy


# Grupowanie według wszystkich kryteriów
fviz_nbclust(suic.scaled, clara, method = "wss") # 4 (ciężko określić)
fviz_nbclust(suic.scaled, clara, method = "silhouette") # 4
fviz_nbclust(suic.scaled, clara, method = "gap_stat") # 1 

# clusterboot

# hclust
clusterboot(suic.scaled, B = 500,
            clustermethod = hclustCBI, method = "ward.D2", k = 4)

# kmeans
clusterboot(suic.scaled, B = 500,
            clustermethod = kmeansCBI, krange = 4)

# clara
clusterboot(suic.scaled, B = 500,
            clustermethod = claraCBI, k = 4)

## Zestawienie wyników grupowań

# Analiza wyników grupowania hierarchicznego:
suic %>% 
  pivot_longer(2:8) %>%
  group_by(cluster.w, name) %>% 
  summarise(mean.v = mean(value)) %>% 
  pivot_wider(names_from = name, values_from = mean.v)

plot_boxplot(suic[, 2:9], by = "cluster.w", ggtheme = theme_bw(), geom_boxplot_args = list("fill" = "#87CEFF"))

# Analiza wyników grupowania metodą k-średnich:
suic$cluster.km <- km1s$cluster %>% as.factor()

suic %>% 
  pivot_longer(2:8) %>%
  group_by(cluster.km, name) %>% 
  summarise(mean.v = mean(value)) %>% 
  pivot_wider(names_from = name, values_from = mean.v)

suic$cluster.km <- as.factor(suic$cluster.km)


plot_boxplot(suic[, c(2:10)], by = "cluster.km", ggtheme = theme_bw(), geom_boxplot_args = list("fill" = "#7CCD7C"))


# Wizualizacja wynikow grupowania za pomoca PCA:

# grupowanie hierarchiczne:

sui1 <- suic %>% 
  select(-country,-cluster.km)
rownames(sui1) <- suic$country
pc1s <- PCA(sui1, quali.sup = 8, graph = FALSE)
fviz_pca_biplot(pc1s, habillage = 8, repel = TRUE, labelsize = 3)

# grupowanie k-means:

sui2 <- suic %>% 
  select(-country,-cluster.w)
rownames(sui2) <- suic$country
pc2s <- PCA(sui2, quali.sup = 8, graph = FALSE)
fviz_pca_biplot(pc2s, habillage = 8, repel = TRUE, labelsize = 3)

# dodatkowe wizualizacje dla grupowania k-średnich:

set.seed(10)
seta <- kmeans(suic.scaled, centers = 4, nstart = 10)

fviz_cluster(seta, data = suic.scaled, repel = TRUE, labelsize = 8)

fviz_cluster(seta, data = suic.scaled, repel = TRUE, labelsize = 8, 
             show.clust.cent = FALSE, ggtheme = theme_bw(), 
             ellipse.type = "norm", ellipse.level = 0.85)


# DODATKOWO wyniki grupowania hierarchicznego postanowiliśmy zwizualizować za pomocą naniesienia kolorystyki odpoiwednich grup na wykres mapy Europy. 

# Stworzenie tabeli  podstawowymi informacjami charakteryzującymi poszczególne państwa Europy. 
SHP <- get_eurostat_geospatial(resolution = 10, 
                               nuts_level = 0, 
                               year = 2021) 

# Usunięcie wierszy z informacjiami o krajach niezawartych w ogólnej analizie. 
SHP <- SHP[!(SHP$id %in% c("RS", "LI", "UK", "AL", "ME", "MK")),] 

# Przekształcenie oryginalnych państw krajów na ich angielskie odpowiedniki. 
SHP$NAME_LATN <- as.factor(SHP$NAME_LATN)
SHP$country<- countryname(SHP$NAME_LATN)
SHP[2, 13]<- "Belgium"
SHP[11,13]<- "Switzerland"
SHP[19, 13]<- "Greece"

# Sortowanie danych zgodne z porządkiem alfabetycznym.
SHP <- SHP[order(SHP$country), ]

# Wygenerowanie kolumny z angielskimi nazwami krajów w sui1.
sui1$country <- rownames(sui1)

# Przyporządkowanie grup do krajów w zbiorze SHP oraz zmiana nazwy kolumny na bardziej oczywistą. 
sui1$cluster.w <- as.factor(sui1$cluster.w)
SHP <- cbind(SHP, sui1$cluster.w)
colnames(SHP)[13] <- "group"

# Wizualizacja wyników 
SHP %>% ggplot(aes(fill = group)) + geom_sf() + scale_x_continuous(limits = c(-22, 35)) +
  scale_y_continuous(limits = c(35, 68)) + theme_bw() + 
  scale_fill_manual(values=c("#7CCD7C","#CD5555","#87CEFF","#FFEC8B")) 

# Na wykresie znajdują się tylko państwa uwzględnione w głównej analizie, stąd brak terytoium Wielkiej Brytanii, Rosji, Bośni i Hercegowniy, Czarnogóry, Albanii, Serbii. 


