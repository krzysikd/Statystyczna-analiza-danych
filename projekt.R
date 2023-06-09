#Daniel Krzysik
#zadanie projektowe

#Analiza samobojstw na przestrzeni lat 1985-2016

#potrzebne pakiety
install.packages("tidyverse") #pakiet do selekcji danych %>%
library(tidyverse)
install.packages("plotly") #pakiet wykresow
library(plotly)
install.packages("ggplot2") #pakiet wykresow
library(ggplot2)
install.packages("gganimate") #pakiet animacji
library(gganimate)
install.packages("countrycode") #pakiet krajow
library(countrycode)
install.packages("highcharter") #wykresy highcarter
library(highcharter)
install.packages("htmlwidgets") #do zapisu wykresu typu highcarter
library(htmlwidgets)
install.packages("shiny") #do ui,server
library(shiny)
install.packages("packHV") #do wykresow typu hist_boxplot
library(packHV)
install.packages("scales") # do wykresow typu ggplot
library(scales) 
install.packages("gridExtra") #laczenie wykresow
library(gridExtra)
install.packages("moments") #statystyka opisowa
library(moments)
library(broom)

#Wprowadzenie 
#Wi�kszo�� danych wykorzystanych w tej analizie pochodzi ze �wiatowej Organizacji Zdrowia.
#�mier� w wyniku samob�jstwa zdarza si� w ka�dym kraju. Jednak kraje o wysokim wska�niku 
#samob�jstw znacznie si� r�ni�. W tym projekcie najpierw przeanalizowany zostanie wska�nik samob�jstw
#w skali globalnej w latach 1985-2016. Celem projektu jest znalezienie i wyja�nienie zale�no�ci mi�dzy
#wska�nikiem samob�jstw a p�ci�, grup� wiekow�/pokoleniem lub PKB w poszczeg�lnych krajach.

getwd() # zwraca aktualny katalog roboczy
# zmieniamy katalog roboczy na ten w ktorym znajduja sie pliki z danymi
# setwd("sciezka do katalogu roboczego")
# np. setwd("C:/Users/Dom_PC/Documents/R/Lab9")
#katalogi oddzielamy "/"
setwd("E:/STUDIA/SEMESTR IV/Statystyczna analiza danych/Projekt")
getwd()

#wczytujemy plik
(dane <- read.csv("master.csv", sep = ","))
#usuwamy kolumne country-year
(dane <- dane[,-8])
#usuwamy kolumne HDI z powodu braku danych
(dane <- dane[,-8])

#nadajemy nowe nazwy kolumna
(
  names(dane) <-
    c(
      "Kraj",
      "Rok",
      "Plec",
      "Wiek",
      "LiczbaSamobojstw",
      "Populacja",
      "SamobojstwaNa100k",
      "PKBnaROK",
      "PKBnaMieszkanca",
      "Pokolenie"
    )
)
dane

#dzieki pakietowi countrycode przyporzadkowywuje kazdemu kraju odpowiedni kontynent
(
  dane$Kontynent <- countrycode(
    sourcevar = dane$Kraj,
    origin = "country.name",
    destination = "continent"
  )
)

#kraje Ameryki Poludniowej
(
  AmerykaPoludniowa <-
    c(
      'Argentina',
      'Brazil',
      'Chile',
      'Colombia',
      'Ecuador',
      'Guyana',
      'Paraguay',
      'Suriname',
      'Uruguay'
    )
)

#Segregacja krajow Ameryki na Ameryke Poludniowa i Ameryke Polnocna
#South America przypisujemy kazdemu krajowi ktory znalazl sie w "AmerykaPoludniowa"
(dane$Kontynent[dane$Kraj %in% AmerykaPoludniowa] <-
    'South America')
#North America przypisujemy reszte krajow
(dane$Kontynent[dane$Kontynent == 'Americas'] <- 'North America')

unique(dane[, 11]) #sprawdzamy czy prawidlowo dodalismy kontynenty

dane[6007,]

#--------------------------------------------------------------#

(grupowanieKrajow <- count(dane,Kraj))

(sortowanieKrajow <- arrange(grupowanieKrajow, n))

#pomini�cie pierwszych 11 kraj�w ze zbyt ma�� liczb� danych (mniej ni� 100 obserwacji)
(dane <- dane %>%
  filter(!(Kraj %in% head(sortowanieKrajow$Kraj, 11)))) 

#--------------------------------------------------------------#

#skew
skew(dane$SamobojstwaNa100k)

#srednia arytmetyczna
mean(dane$SamobojstwaNa100k)

#minimum z proby
min(dane$SamobojstwaNa100k)

#maksimum z proby
max(dane$SamobojstwaNa100k)

#przedzial zmiennosci proby (min, max)
range(dane$SamobojstwaNa100k)

#miary rozproszenia
#rozstep max - min
max(dane$SamobojstwaNa100k) - min(dane$SamobojstwaNa100k)

#wariancja
var(dane$SamobojstwaNa100k)

#moment centralny rzedu 2
moment(dane$SamobojstwaNa100k, order = 2, central = TRUE)

#odchylenie standardowe (pierwiastek z wariancji probkowej)
sd(dane$SamobojstwaNa100k)

#dlugosc
length(dane$SamobojstwaNa100k)

#mediana probkowa
median(dane$SamobojstwaNa100k)

#Kurtoza
kurtosis(dane$SamobojstwaNa100k)

#kwantyle probkowe
quantile(dane$SamobojstwaNa100k, seq(0,1, by = .1))

quantile(dane$SamobojstwaNa100k, probs = c(0.25, 0.5, 0.75, 0.90))

#rozst�p mi�dzykwartylny (r�nica mi�dzy czwarym a drugim kwartylem)
quantile(dane$SamobojstwaNa100k)[4] - quantile(dane$SamobojstwaNa100k)[2]

#odchylenie przeci�tne od mediany
mad(dane$SamobojstwaNa100k)

#rozstep cwiartkowy
IQR(dane$SamobojstwaNa100k)

#blad standardowy
sd(dane$SamobojstwaNa100k)/sqrt(length(dane$SamobojstwaNa100k))

#wspolczynnik zmiennosci
sd(dane$SamobojstwaNa100k)/mean(dane$SamobojstwaNa100k)

#liczba samob�jstw jest silnie skorelowana z liczb� ludno�ci
cor(dane$LiczbaSamobojstw, dane$Populacja)

#u�ycie wsp�czynnika samob�jstw mo�e by� lepsze
cor(dane$SamobojstwaNa100k, dane$Populacja) # brak korelacji

#korelacja miedzy liczba ludnosci a liczba samobojstw
(
df <- dane %>%
  select(Populacja , LiczbaSamobojstw) %>%
  cor()
)

#podsumowanie
summary(dane)

#--------------------------------------------------------------#



#--------------------------------------------------------------#

#animacja liczby samobojstw wzgledem populacji w kazdym kontynencie, zmieniajac rok
(
  g <- ggplot(dane, aes(LiczbaSamobojstw, Populacja, colour = Kraj)) +
    geom_point(alpha = 0.7, show.legend = FALSE) +
    scale_size(range = c(2, 12)) +
    scale_x_log10() +
    facet_wrap(~ Kontynent) +
    # pakiet gganimate pozwala na zmiane wykresu co rok
    labs(title = 'Rok: {frame_time}', x = 'LiczbaSamobojstw', y = 'Populacja') +
    transition_time(Rok) +
    ease_aes('linear')
)


#zapisanie animacji do folderu
anim_save(filename = "LSamobojstw.gif", animation = g)


(srednia <- (sum(as.numeric(dane$LiczbaSamobojstw)) / sum(as.numeric(dane$Populacja))) * 100000)

#-------
(globalna_analiza <- dane %>%
  group_by(Rok) %>%
  summarize(populacja = sum(Populacja), 
            samobojstwa = sum(LiczbaSamobojstw), 
            samobojstwa100k = (samobojstwa / populacja) * 100000)
)
  
  ggplot(globalna_analiza, aes(x = Rok, y = samobojstwa100k)) + 
  geom_line(col = "deepskyblue3", size = 1) + 
  geom_point(col = "deepskyblue3", size = 2) + 
  geom_hline(yintercept = srednia, linetype = 2, color = "grey35", size = 1) +
  labs(title = "Samobojstwa na swiecie (na 100tys.)",
       subtitle = "Tendencja w czasie, 1985 - 2016.",
       x = "Rok", 
       y = "Samobojstwa na 100tys.") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2)) + 
  scale_y_continuous(breaks = seq(10, 20))

#-------

#Wspolczynnik liczby samobojstw w poszczegolnych latach wzgledem plci
(
  wykres_plci <- dane %>%
    select(Rok, Plec, LiczbaSamobojstw, Populacja) %>%
    group_by(Rok, Plec) %>%
    summarise(Lsam = round((
      sum(LiczbaSamobojstw) / sum(Populacja)
    ) * 100000, 2))
)


#tworzenie wykresu
(
  hc2 <- highchart() %>%
    hc_add_series(wykres_plci, hcaes(
      x = Rok, y = Lsam, group = Plec
    ), type = "scatter") %>% #wyswietlamy dane
    hc_tooltip(
      headerFormat = "",
      pointFormat = paste("Samobojstw: <b>{point.y}</b> <br> Rok: <b>{point.x}</b>")
    ) %>% #css
    hc_title(
      text = "<i>Samobojstwa przez plec</i>",
      style = list(color = "white", useHTML = TRUE)
    ) %>%  #tytul
    hc_subtitle(text = "1985-2015") %>% #podtytul
    hc_yAxis(title = list(text = "Samobojstwa na 100k ludzi"), #opis oxi y
             plotLines = list(list(
               color = "black", width = 1, dashStyle = "Dash", 
               value = mean(srednia),
               label = list(text = "Srednia = 13.27", 
               style = list(color = "black", fontSize = 11))))
             ) %>%
    hc_xAxis(title = list(text = "Rok"))
)

#1. Wska�nik samob�jstw w�r�d m�czyzn jest ~3,5x wy�szy
#2. Zar�wno wska�nik samob�jstw m�czyzn, jak i kobiet osi�gn�� najwy�szy poziom w 1995 roku i od tego czasu spada
#3. Stosunek ten, wynosz�cy 3,5 : 1 (m�czy�ni : kobiety), utrzymuje si� na wzgl�dnie sta�ym poziomie od po�owy lat 90.
#4. Jednak w latach 80. stosunek ten wynosi� zaledwie 2,7 : 1 (m�czyzna : kobieta)

#zapisanie do pliku
saveWidget(hc2, "hc2.html")

#Wybieramy kraj Polska, grupujemy po wieku i plci oraz sumujemy liczbe samobojstw
(
    wykres_wiek <- dane %>%
    select(Rok, Wiek, LiczbaSamobojstw, Populacja) %>%
    group_by(Rok, Wiek) %>%
    summarise(Lsam = round((sum(LiczbaSamobojstw) / sum(Populacja)) * 100000, 2))
)

#wyswietlamy wykresem
(
  highchart() %>% 
  hc_add_series(wykres_wiek, hcaes(x = Rok, y = Lsam, group = Wiek), type = "line") %>%
  hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", 
             pointFormat = paste("Rok: <b>{point.x}</b> <br>","Wiek: <b>{point.Wiek}</b><br>",
                                 "Samobojstwa: <b>{point.y}</b>")) %>%
  hc_title(text = "Samobojstwa na �wiecie wed�ug wieku") %>% 
  hc_subtitle(text = "1985-2015") %>%
  hc_xAxis(title = list(text = "Rok")) %>%
  hc_yAxis(title = list(text = "Samob�jstwa na 100 tys. mieszka�c�w"))
)

#1. W skali globalnej prawdopodobie�stwo pope�nienia samob�jstwa wzrasta wraz z wiekiem
#2. Od 1995 roku wska�nik samob�jstw w�r�d os�b w wieku >= 15 lat maleje liniowo.
#3. Wska�nik samob�jstw w kategorii "5-14" pozostaje mniej wi�cej statyczny i niewielki (< 1 na 100 tys. rocznie).

saveWidget(hc3, "hc3.html") #zapisujemy plik

#Liczba samobojstw w stosunku do pokolenia

#wybieramy dane grupujac po wieku oraz zsumowana liczbe samobojstw wyswietlamy rosnaco
(g4a <- dane %>%
    group_by(Wiek) %>%
    summarise(LS = sum(LiczbaSamobojstw)) %>%
    arrange(LS))
factor(dane$Wiek)
(
  hc4a <- highchart() %>%
    hc_add_series(g4a, hcaes(x = Wiek, y = LS), type = "pie") %>%
    hc_tooltip(
      headerFormat = "",
      pointFormat = paste("Samobojstw: <b>{point.y}</b><br> Wiek: <b>{point.Wiek}</b>")
    ) %>% #css
    hc_title(text = "Liczba Samobojstw w poszczegolnym wieku") %>% #tytul wykresu
    hc_yAxis(title = list(text = "Liczba samobojstw")) %>%  #opis osi y
    hc_xAxis(title = list(text = "Pokolenia"))
)

#ze wzgl�du na nak�adanie si� r�nych kategorii wiekowych, pr�ba interpretacji trendu wska�nik�w samob�jstw pokoleniowych w czasie stwarza problemy.

saveWidget(hc4a, "hc4a.html") #zapisujemy plik

#Ile osob na 100k popelnia samobojstwo w poszczegolnym kontynencie

#grupujemy po kontynencie oraz sortujemy rosnaco wyliczona ilosc osob popelniajacych samobojstwo na 100k
(g5 <- dane %>%
    group_by(Kontynent) %>%
    summarise(LS = round((sum(LiczbaSamobojstw) / sum(Populacja)) * 100000, 2)) %>%
    arrange(LS))

(g5$Kontynent <- factor(g5$Kontynent, ordered = T, levels = g5$Kontynent)) #sortowaie

#wykres
(
  Kontynent_plot <- ggplot(g5, aes(x = Kontynent, y = LS, fill = Kontynent)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Samobojstwa na �wiecie (na 100 tys.), wedlug kontynentow",
       x = "Kontynent", 
       y = "Samobojstwa na 100k", 
       fill = "Kontynent") +
  theme(legend.position = "none", title = element_text(size = 10)) + 
  scale_y_continuous(breaks = seq(0, 20, 1), minor_breaks = F)
)

(g5a <- dane %>%
  group_by(Rok, Kontynent) %>%
  summarise(LS = round((sum(LiczbaSamobojstw) / sum(Populacja)) * 100000, 2))
)

(g5a$Kontynent <- factor(g5a$Kontynent, ordered = T, levels = g5$Kontynent)) #sortowaie

(Kontynent_czas_plot <- ggplot(g5a, aes(x = Rok, y = LS, col = factor(Kontynent))) + 
  facet_grid(Kontynent ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Tendencje w czasie na Kontynentach", 
       x = "Rok", 
       y = "Samob�jstwa na 100 tys.", 
       color = "Kontynent") + 
  theme(legend.position = "none", title = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F))
  grid.arrange(Kontynent_plot, Kontynent_czas_plot, ncol = 2)

#1. Wska�nik europejski jest og�lnie najwy�szy, ale od 1995 r. stale spada o ~40%.
#2. Wska�nik europejski za rok 2015 jest podobny do wska�nika dla Azji i Oceanii.
#3. Linia trendu dla Afryki jest spowodowana nisk� jako�ci� danych - tylko 3 kraje dostarczy�y dane.


#Wykres samobojstw w roznych krajach przez dana plec
(
  g7 <- dane %>%
    group_by(Kraj, Kontynent) %>%
    summarise(LS = round((sum(LiczbaSamobojstw) / sum(Populacja)) * 100000, 2)) %>%
    arrange(desc(LS))
)

(srednia <- (sum(as.numeric(dane$LiczbaSamobojstw)) / sum(as.numeric(dane$Populacja))) * 100000)

(g7$Kraj <- factor(g7$Kraj, ordered = T, levels = rev(g7$Kraj)))

(
  ggplot(g7, aes(x = Kraj, y = LS, fill = Kontynent)) + 
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = srednia, linetype = 2, color = "grey35", size = 1) +
  labs(title = "Samob�jstwa na �wiecie na 100 tys. mieszka�c�w, wed�ug kraj�w",
       x = "Kraj", 
       y = "Samobojstwa na 100k", 
       fill = "Kontynent") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 45, 2)) + 
  theme(legend.position = "bottom")
)

#1. Na Litwie wska�nik ten jest zdecydowanie najwy�szy: > 41 samob�jstw na 100 tys. mieszka�c�w
#2. Znaczna nadreprezentacja kraj�w europejskich o wysokich wska�nikach, niewiele o niskich.


#wykres samobojst 100k osob przez wybor kraju

shinyApp(ui <- fluidPage(
  titlePanel("Samobojstwa 100k osob w danym kraju"),
  
  sidebarLayout(sidebarPanel(
    selectInput(
      "Kraj",
      "Wybierz kraj:",
      choices = unique(dane$Kraj),
      selected = "Poland"
    )
  ),
  mainPanel(plotOutput("Wykres")))
),

server <- function(input, output) {
  output$Wykres <- renderPlot({
    ggplotly(
      dane %>%
        filter(Kraj == input$Kraj) %>%
        ggplot(aes(
          x = Rok, y = LiczbaSamobojstw , colour = Pokolenie
        ))
      + geom_point()
      + labs(title = "Liczba Samobojstw",
             x = "",
             y = "")
      + facet_grid(~ Plec)
    )
  })
})

#--------------------------------------------------------------#
#Czy wraz z bogaceniem si� kraju spada liczba samob�jstw?
#To zale�y od kraju - w prawie ka�dym kraju istnieje wysoka korelacja mi�dzy Rok, a PKBnaMieszkanca tzn. w miar� up�ywu czasu PKB ro�nie liniowo.

(KrajRokPKB <- dane %>%
    group_by(Kraj, Rok) %>%
    summarize(PKBnaMieszkanca = mean(PKBnaMieszkanca)))

(KrajRokPKBKorelacja <- KrajRokPKB %>%
    ungroup() %>%
    group_by(Kraj) %>%
    summarize(RokPKBKorelacja = cor(Rok, PKBnaMieszkanca)))

(mean(as.numeric(KrajRokPKBKorelacja$RokPKBKorelacja)))

#Obliczy�em korelacje Pearsona mi�dzy "rokiem" a "PKBnaMieszkanca" w ka�dym kraju, a nast�pnie podsumowa�em wyniki:
#�rednia korelacja wynios�a 0.8926335, co oznacza, �e s� one wysoce dodatnio skorelowane.
# W wi�kszo�ci kraj�w wraz ze wzrostem PKB wzrasta r�wnie� liczba samob�jstw. Jednak zdazaja sie wyjatki.

#--------------------------------------------------------------#
#Czy kraje bogatsze maj� wy�szy wska�nik samob�jstw?
#Zamiast przygladac sie trendom w poszczegolnych krajach, biore kazdy kraj i obliczam
#jego sredni PKB (na mieszkanca) we wszystkich latach, dla ktorych dostepne sa dane.
#nastepnie mierze, jak to sie ma do wspolczynnika samobojstw w danych kraju we wszystkich latach

#wynikiem koncowym jest jeden punkt danych dla kazdego kraju, ktory ma dac ogolne pojecie
#o zamoznosci kraju i jego wspolczynniku samobojstw

(KrajSredniePKB <- dane %>%
  group_by(Kraj, Kontynent) %>%
  summarize(LS = (sum(as.numeric(LiczbaSamobojstw)) / sum(as.numeric(Populacja))) * 100000, 
            PKBnaMieszkanca = mean(PKBnaMieszkanca)))

(ggplot(KrajSredniePKB, aes(x = PKBnaMieszkanca, y = LS, col = Kontynent)) + 
    geom_point() + 
    scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000)) + 
    labs(title = "Korelacja mi�dzy PKB (na mieszkanca) a liczb� samob�jstw na 100k", 
         subtitle = "Wykres zawiera wszystkie kraje",
         x = "PKB (na mieszkanca)", 
         y = "Sambojstwa na 100k", 
         col = "Kontynent") )

model1 <- lm(LS ~ PKBnaMieszkanca, data = KrajSredniePKB)

summary(model1)

(
  pkb_cooks <- model1 %>%
  augment() %>%
  arrange(desc(.cooksd)) %>%
  filter(.cooksd < 4/nrow(.)) %>% 
  inner_join(KrajSredniePKB, by = c("LS", "PKBnaMieszkanca")) %>%
  select(Kraj, Kontynent, PKBnaMieszkanca, LS)
)

model2 <- lm(LS ~ PKBnaMieszkanca, data = pkb_cooks)

summary(model2)

(ggplot(pkb_cooks, aes(x = PKBnaMieszkanca, y = LS, col = Kontynent)) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(group = 1)) + 
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000)) + 
  labs(title = "Korelacja mi�dzy PKB (na mieszkanca) a liczb� samob�jstw na 100k",
       subtitle = "Wykres z usunieciem 6 krajow",
       x = "PKB (na mieszkanca)", 
       y = "Sambojstwa na 100k", 
       col = "Kontynent") )

#Istnieje s�aba, ale istotna dodatnia zale�no�� liniowa - bogatsze kraje wi��� si� z wy�szymi 
#wska�nikami samob�jstw, ale jest to zale�no�� s�aba, co wida� na poni�szym wykresie.


#--------------------------------------------------------------#
#Por�wnanie Wielkiej Brytanii, Irlandii, Ameryki, Francji, Danii i Polski

(zestawDanych <- dane %>%
  filter(Kraj %in% c("United Kingdom", "Ireland","United States", "France", "Denmark", "Poland")))
  
(g10 <- zestawDanych %>%
  group_by(Kraj, Rok) %>%
  summarize(LS = (sum(as.numeric(LiczbaSamobojstw)) / sum(as.numeric(Populacja))) * 100000))

(ggplot(g10, aes(x = Rok, y = LS, col = Kraj)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(se = F, span = 0.2) + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F) + 
  labs(title = "UK, Irlandia, USA, Francja, Dania i Polska", 
       subtitle = "Samob�jstwa na 100 tys. ludno�ci, 1985 - 2015", 
       x = "Rok", 
       y = "Samobojstwa na 100k", 
       col = "Kraj"))

#1. Wska�nik samob�jstw w Wielkiej Brytanii jest niezmiennie najni�szy od 1990 roku, a od oko�o 1995 roku utrzymuje si� na do�� statycznym poziomie.
#2. Francja mia�a najwy�szy wska�nik, ale obecnie jest on mniej wi�cej r�wny ameryka�skiemu i polskiemu
#3. Stany Zjednoczone wykazuj� najbardziej niepokoj�c� tendencj� - od 2000 roku wska�nik wzr�s� liniowo o ~1/3.

(zestawDanych %>%
  group_by(Kraj, Plec, Rok) %>%
  summarize(LS = (sum(as.numeric(LiczbaSamobojstw)) / sum(as.numeric(Populacja))) * 100000) %>%
  ggplot(aes(x = Rok, y = LS, col = Kraj)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(se = F, span = 0.2) + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F) + 
  facet_wrap(~ Plec, scales = "free_y", nrow = 2) +
  labs(title = "UK, Irlandia, USA, Francja, Dania i Polska", 
       subtitle = "Samob�jstwa na 100 tys. ludno�ci, 1985 - 2015", 
       x = "Rok", 
       y = "Samobojstwa na 100k", 
       col = "Kraj"))

#1. Odmienne linie trendu dla m�czyzn i kobiet w Irlandii oraz Polsce - w 1990 roku wzrasta wska�nik dla m�czyzn, ale nie mo�na tego samego zaobserwowa� dla kobiet
#2. W przypadku m�czyzn i kobiet we Francji odnotowano spadek wska�nika do poziomu zbli�onego do ameryka�skiego.



#--------------------------------------------------------------#

(
  samobojstwa_plec <- dane %>% 
  group_by(Plec,Rok) %>% 
  summarize(LS = (sum(as.numeric(LiczbaSamobojstw)) / sum(as.numeric(Populacja))) * 100000)
)

(
  samobojstwa_kobiety <- samobojstwa_plec %>% 
  filter(Plec=='female')
)

(
  samobojstwa_mezczyzni <- samobojstwa_plec %>% 
  filter(Plec=='male')
)

(test <- t.test(x = samobojstwa_mezczyzni$LS, y = samobojstwa_kobiety$LS))

var.test(LS ~ Plec, data = samobojstwa_plec)

#--------------------------------------------------------------#

(
  KrajWiek <- dane %>% 
    group_by(Rok,Kraj,Wiek) %>% 
    summarize(LS = (sum(as.numeric(LiczbaSamobojstw)) / sum(as.numeric(Populacja))) * 100000)
)

(modelWiek <- lm(LS~Wiek,data = KrajWiek))

summary(modelWiek)

#--------------------------------------------------------------#

(
  df1 <- dane %>%
  group_by(Plec, LiczbaSamobojstw)%>%
  count()
)

(
  ggplot(dane, aes(x= Wiek ,y=SamobojstwaNa100k)) +
  geom_boxplot(color="red" , outlier.color="black")
)


#dystrybuanta empiryczna
dane %>%
  ggplot(aes(x = SamobojstwaNa100k)) +
  stat_ecdf()


#gestosc
dane %>%
  ggplot(aes(SamobojstwaNa100k)) +
  geom_density(kernel = "gaussian")



#-------------------------------------------------------------#

(ggplot(dane, aes(x=SamobojstwaNa100k))+
  geom_density(bins=30, colour="red", fill="blue")+
  labs(title = 'SamobojstwaNa100k',
     subtitle= 'Wska�nik danych dotycz�cych samob�jstw' ,  x= 'SamobojstwaNa100k' ,  y= 'Gestosc'))

#--------------------------------------------------------------#

(
  suicides_sex <- dane %>% 
  group_by(Rok, Plec)
)

with(suicides_sex, shapiro.test(dane[Plec == 'female']))
with(suicides_sex, shapiro.test(dane[Plec == 'male']))


#--------------------------------------------------------------#


(
  KrajWiek <- dane %>% 
    group_by(Rok,Kraj,Wiek) %>% 
    summarize(LS = (sum(as.numeric(LiczbaSamobojstw)) / sum(as.numeric(Populacja))) * 100000)
)

#--------------------------------------------------------------#

(
  plecPKB <- dane %>% 
  group_by(Kraj, Plec) %>% 
  summarise(CLS = mean(sum(as.numeric(LiczbaSamobojstw))),
                   PKB = mean(as.numeric(PKBnaMieszkanca)))
)

(kobietyPKB <- plecPKB[which(plecPKB$Plec == 'female'),])

(
  kobietyPKBlm = lm(CLS ~ PKB, data = kobietyPKB)
)

summary(kobietyPKBlm)



(mezczyzniPKB <- plecPKB[which(plecPKB$Plec == 'male'),])

(
  mezczyzniPKBlm = lm(CLS ~ PKB, data = mezczyzniPKB)
)

summary(mezczyzniPKBlm)


#Og�lnie rzecz bior�c, wska�niki samob�jstw s� znacznie wy�sze w�r�d m�czyzn. Wska�nik samob�jstw 
#m�czyzn jest 3-4 razy wy�szy ni� wska�nik samob�jstw kobiet. Zjawisko to wyst�puje we wszystkich 
#90 krajach. Wska�nik samob�jstw zar�wno w�r�d m�czyzn, jak i kobiet by� najwy�szy w 1995 roku i 
#od tego czasu spada. W roku 2015 wska�niki samob�jstw spad�y mniej wi�cej do tego samego poziomu, 
#co w latach 1988-1991. W skali globalnej wska�niki samob�jstw rosn� wraz z wiekiem, przy czym grupa
#wiekowa 5-14 lat (najm�odsza grupa wiekowa w danych) ma najni�sze wska�niki samob�jstw, a grupa 
#wiekowa 75+ - najwy�sze. 

#Podczas wykonywania wizualizacji m�j horyzont my�lowy poszerzy� si�. Czuj� si� pewniej w obs�udze R. 
#My�l� jednak, �e gdyby by�o wi�cej czasu, m�g�bym bardziej dog��bnie zbada� dane. Ponadto zestaw danych
#m�g�by zawiera� wi�cej zmiennych, takich jak rasa, religia, metoda samob�jstwa,(straty ekonomiczne 
#zwi�zane ze wzrostem obci��enia prac�), cz�sto�� wyst�powania my�li samob�jczych i inne.
