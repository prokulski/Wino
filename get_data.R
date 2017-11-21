library(rvest)
library(tidyverse)
library(lubridate)

# funkcja pobiera liste win z kolejnych stron serwisu Winezja.pl
# w odpowiedzi dostajemy wektor z wzglednymi URLami do stron produktow
getIndex <- function(n_pages)
{
   urle_all <- vector()

   for(strona in 1:n_pages) {
      # progress bar :)
      cat(paste0("\rstrona = ", strona, " / ", n_pages))


      page_url <- sprintf("https://winezja.pl/wina?str=strona-%d-rozmiar-30-sortowanie-nazwa-kierunek-rosnaco", strona)

      # index
      page <- read_html(page_url)

      # urle do strn z winami
      urle <- page %>%
         html_node("div.wineList") %>%
         html_node("table") %>%
         html_nodes("td") %>%
         html_nodes("div.wineDesc") %>%
         html_node("div.wineName") %>%
         html_node("h2") %>%
         html_node("a") %>%
         html_attr("href")

      urle_all <- c(urle_all, urle)

      # poczekaj chwile, zeby nie draznic serwera
      Sys.sleep(sample(seq(0.1, 1, 0.1), 1))
   }

   return(unique(urle_all))
}


# funkcja pobiera ze strony pojedynczego produktu (wina) wszystkie interesujace nas informacje
# zwraca data.frame (jeden rzad) z danymi
getWine <- function(wine_url) {

   # strona z winem
   page <- read_html(paste0("https://winezja.pl/", wine_url))

   # tabela z cechami
   # cechy
   label_names <- page %>%
      html_node("div.detailsTab") %>%
      html_nodes("div") %>%
      html_node("span.productLabel") %>%
      html_text() %>% trimws() %>%
      gsub(":", "", ., fixed = TRUE)

   # wartości
   wino <- page %>%
      html_node("div.detailsTab") %>%
      html_nodes("div") %>%
      html_node("span.productDescription") %>%
      html_text() %>%
      trimws() %>%
      gsub("\t", "", ., fixed = TRUE) %>%
      gsub("\n", "", ., fixed = TRUE) %>%
      gsub("\r", "", ., fixed = TRUE) %>%
      gsub("[ ]+", " ", .) %>%
      gsub(" ,", ",", ., fixed = TRUE) %>%
      set_names(label_names) %>%
      as.list()

   # ranking expertów
   experts <- page %>%
      html_node("div.rating") %>%
      html_node("div.expertRating") %>%
      html_nodes("span") %>%
      .[c(2,4,5)] %>%
      html_text()

   # oceny
   oceny <- page %>%
      html_node("div.indicators-wrap") %>%
      html_node("div.indicators") %>%
      html_node("div") %>%
      html_children() %>%
      html_attr("class") %>%
      gsub("is", "", .) %>%
      as.numeric()

   # cena
   cena <- page %>%
      html_node("span.priceActive") %>%
      html_text() %>%
      gsub("[\n\r ]+", "", .) %>%
      gsub("zł", "", .) %>%
      gsub(",", ".", .) %>%
      as.numeric()

   wine_list <- data_frame(nazwa = ifelse(is.null(wino[["Nazwa produktu"]]), NA, wino[["Nazwa produktu"]]),
                           kolor = ifelse(is.null(wino[["Kolor wina"]]), NA, wino[["Kolor wina"]]),
                           smak = ifelse(is.null(wino[["Smak"]]), NA, wino[["Smak"]]),
                           rocznik = ifelse(is.null(wino[["Rocznik"]]), NA, wino[["Rocznik"]]) %>% as.numeric(),
                           producent = ifelse(is.null(wino[["Producent"]]), NA, wino[["Producent"]]),
                           kraj = ifelse(is.null(wino[["Kraj"]]), NA, wino[["Kraj"]]),
                           region = ifelse(is.null(wino[["Region"]]), NA, wino[["Region"]]),
                           apelacja = ifelse(is.null(wino[["Apelacja"]]), NA, wino[["Apelacja"]]),
                           kategoria = ifelse(is.null(wino[["Kategoria"]]), NA, wino[["Kategoria"]]),
                           klasyfikacja = ifelse(is.null(wino[["Klasyfikacja"]]), NA, wino[["Klasyfikacja"]]),
                           szczep = ifelse(is.null(wino[["Szczep"]]), NA, wino[["Szczep"]]),
                           beczka = ifelse(is.null(wino[["Beczka"]]), NA, wino[["Beczka"]]),
                           zamkniecie = ifelse(is.null(wino[["Zamknięcie"]]), NA, wino[["Zamknięcie"]]),
                           wiek = ifelse(is.null(wino[["Wiek"]]), NA, wino[["Wiek"]]),
                           temp_podawania = ifelse(is.null(wino[["Temp. podawania"]]), NA, wino[["Temp. podawania"]]),
                           pojemnosc = ifelse(is.null(wino[["Pojemność"]]), NA, wino[["Pojemność"]]) %>% gsub("l.", "", ., fixed = TRUE) %>% gsub(",", ".", .) %>% as.numeric(),
                           kod_produktu = ifelse(is.null(wino[["Kod produktu"]]), NA, wino[["Kod produktu"]]),
                           dostepnosc = ifelse(is.null(wino[["Dostępność"]]), NA, wino[["Dostępność"]]),
                           dostpene_od = ifelse(is.null(wino[["Dostępne od"]]), NA, wino[["Dostępne od"]]) %>% ymd(),
                           popularnosc = ifelse(is.null(wino[["Popularność"]]), NA, wino[["Popularność"]]) %>% gsub("[^[:digit:].]", "", .),
                           expert_rating = as.numeric(experts[1]),
                           expert_rating_przedzial = experts[2],
                           expert_rating_ocena = experts[3],
                           cena = cena,
                           ocena_smak = oceny[1],
                           ocena_aromat = oceny[2],
                           ocena_gladkosc = oceny[3],
                           ocena_budowa = oceny[4])

   return(wine_list)
}


# pobierz liste win
l <- getIndex(n_pages = 27)

# na serwisie w liste win wpada kilka butelek whisky - pewnie jakis blad
# usuwamy je
l <- l[!grepl("whisky", l)]

# zapisujemy dane lokalnie na pozniej
saveRDS(l, file = "lista_stron_win.RDS")


# dla kazdej strony pobierz info o winie
lista_win <- data_frame()

for(w in 1:length(l)) {
   # progress bar :)
   cat(paste0("\rw = ", w, " / ", length(l)))

   # pobierz info i dodaj do pelnej listy
   lista_win <- bind_rows(lista_win, getWine(l[w]))

   # poczekaj chwile, zeby nie draznic serwera
   Sys.sleep(sample(seq(0.1, 1, 0.1), 1))
}


# zapisujemy dane lokalnie na pozniej
saveRDS(lista_win, file = "lista_win.RDS")


