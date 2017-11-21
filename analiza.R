library(tidyverse)
library(forcats) # zbędne dla tidyverse >= 1.2.1
library(stringr) # zbędne dla tidyverse >= 1.2.1


lista_win <- readRDS("lista_win.RDS")

# trochę poprawek
lista_win$popularnosc <- as.numeric(lista_win$popularnosc)
lista_win$expert_rating_ocena <- factor(lista_win$expert_rating_ocena, levels = c("wina poprawne", "wina dobre", "wina bardzo dobre", "wina wyjątkowe", "idealne"))
lista_win$smak <- factor(lista_win$smak, levels = c("Półsłodkie", "Słodkie", "Półwytrawne", "Wytrawne"))
lista_win$kolor <- factor(lista_win$kolor, levels = c("Białe", "Czerwone", "Różowe", "Inne"))

# jaki kraj?
lista_win %>%
   count(kraj) %>%
   arrange(n) %>%
   mutate(kraj = factor(kraj, levels = kraj)) %>%
   ggplot() +
   geom_col(aes(kraj, n)) +
   coord_flip()

# jaki smak?
lista_win %>%
   count(smak) %>%
   filter(!is.na(smak)) %>%
   arrange(n) %>%
   mutate(smak = factor(smak, levels = smak)) %>%
   ggplot() +
   geom_col(aes(smak, n)) +
   coord_flip()

# jaki kolor?
lista_win %>%
   count(kolor) %>%
   filter(!is.na(kolor)) %>%
   arrange(n) %>%
   mutate(kolor = factor(kolor, levels = kolor)) %>%
   ggplot() +
   geom_col(aes(kolor, n)) +
   coord_flip()

# kolor vs smak
lista_win %>%
   filter(!is.na(smak)) %>%
   count(kolor, smak) %>%
   ggplot() +
   geom_tile(aes(kolor, smak, fill = n), color = "gray80") +
   scale_fill_distiller(palette = "YlOrBr")

# smak vs kraj
lista_win %>%
   filter(!is.na(smak)) %>%
   count(kraj, smak) %>%
   ggplot() +
   geom_tile(aes(smak, kraj, fill = n), color = "gray80") +
   scale_fill_distiller(palette = "YlOrBr")

# region vs smak dla poszczególnych krajów
lista_win %>%
   by(.$kraj, function(x) {
      x %>%
         filter(!is.na(region)) %>%
         count(region, smak) %>%
         ungroup() %>%
         group_by(region) %>%
         mutate(p = sum(n)) %>%
         ungroup() %>%
         arrange(p) %>%
         mutate(region = factor(region, levels = unique(region))) %>%
         mutate(smak = factor(smak, levels = c("Słodkie", "Półsłodkie", "Półwytrawne", "Wytrawne"))) %>%
         ggplot() +
         geom_col(aes(region, n, fill = smak)) +
         coord_flip() +
         labs(title = unique(x$kraj)) +
         scale_fill_manual(values = c("Półsłodkie" = "#fdae61", "Słodkie" = "#d7191c",
                                      "Półwytrawne" = "#a6d96a", "Wytrawne" = "#1a9641"))
   })


# ceny wg krajów
lista_win %>%
   group_by(kraj) %>%
   mutate(m_cena = median(cena, na.rm = TRUE)) %>%
   ungroup() %>%
   arrange(m_cena) %>%
   mutate(kraj = factor(kraj, levels = unique(kraj))) %>%
   ggplot() +
   geom_boxplot(aes(kraj, cena)) +
   coord_flip()

# ceny wg smaku
lista_win %>%
   filter(!is.na(smak)) %>%
   group_by(smak) %>%
   mutate(m_cena = median(cena, na.rm = TRUE)) %>%
   ungroup() %>%
   arrange(m_cena) %>%
   mutate(smak = factor(smak, levels = unique(smak))) %>%
   ggplot() +
   geom_boxplot(aes(smak, cena)) +
   coord_flip()


# czy ocena ekspertów ma sens (tj, czy przedziały są poprawie podzielone)
lista_win %>%
   filter(!is.na(expert_rating_ocena)) %>%
   ggplot() +
   geom_boxplot(aes(expert_rating_ocena, expert_rating))

lista_win %>%
   filter(!is.na(expert_rating), !is.na(smak)) %>%
   ggplot() +
   geom_smooth(aes(expert_rating, cena, color = smak), show.legend = FALSE) +
   facet_wrap(~smak, ncol = 2)

# rating ekspertów wg smaku
lista_win %>% filter(!is.na(smak)) %>% ggplot() + geom_boxplot(aes(smak, expert_rating)) + coord_flip()

# popularność wg smaku
lista_win %>% filter(!is.na(smak)) %>% mutate(popularnosc = as.numeric(popularnosc)) %>% ggplot() + geom_boxplot(aes(smak, popularnosc)) + coord_flip() + scale_y_log10()

# oceny ekspertów wg kraju
lista_win %>%
   filter(!is.na(smak), !is.na(expert_rating)) %>%
   group_by(kraj, smak) %>%
   summarise(n = mean(expert_rating, na.rm = TRUE)) %>%
   ungroup() %>%
   group_by(smak) %>%
   mutate(max_n = max(n)) %>%
   ungroup() %>%
   ggplot() +
   geom_col(aes(kraj, n, fill = n == max_n)) +
   coord_flip() +
   facet_wrap(~smak, scales = "free")

lista_win %>%
   filter(!is.na(smak), !is.na(expert_rating)) %>%
   group_by(kraj, smak) %>%
   summarise(n = mean(expert_rating, na.rm = TRUE)) %>%
   ungroup() %>%
   group_by(smak) %>%
   top_n(3, n) %>%
   ungroup() %>%
   arrange(smak, desc(n)) %>%
   select(smak, kraj, n)


# ta Słowacja wygląda interesująco
lista_win %>% filter(kraj == "Słowacja", smak == "Półwytrawne") %>% filter(expert_rating == max(expert_rating, na.rm = TRUE)) %>% select(nazwa, kolor, rocznik, producent, cena, expert_rating)


# oceny cząstkowe w zależności od koloru i smaku
lista_win %>%
   select(c(2, 3, 25:28)) %>%
   filter(!is.na(smak)) %>%
   gather(key, val, -kolor, -smak) %>%
   group_by(kolor, smak, key) %>%
   summarise(n = mean(val)) %>%
   mutate(key = gsub("ocena_", "", key)) %>%
   ggplot() +
   geom_point(aes(n, key)) +
   facet_grid(kolor~smak)


# najlepsze w danej kategorii oceny
lista_win %>%
   select(c(2, 3, 25:28)) %>%
   filter(!is.na(smak)) %>%
   gather(key, val, -kolor, -smak) %>%
   group_by(kolor, smak, key) %>%
   summarise(n = mean(val)) %>%
   mutate(key = gsub("ocena_", "", key)) %>%
   group_by(key) %>%
   filter(n == max(n)) %>%
   ungroup() %>%
   select(key, kolor, smak, n)


# oceny cząstkowe w zależności od kraju i smaku
lista_win %>%
   select(c(3, 6, 25:28)) %>%
   filter(!is.na(smak)) %>%
   gather(key, val, -smak, -kraj) %>%
   group_by(smak, kraj, key) %>%
   summarise(n = mean(val)) %>%
   ungroup() %>%
   mutate(key = gsub("ocena_", "", key)) %>%
   ggplot() +
   geom_tile(aes(smak, kraj, fill = n), color = "gray80") +
   scale_fill_distiller(palette = "YlOrBr") +
   facet_wrap(~key)


# najkepsze kraje wg smaku i składowej oceny
lista_win %>%
   select(c(3, 6, 25:28)) %>%
   filter(!is.na(smak)) %>%
   gather(key, val, -smak, -kraj) %>%
   group_by(smak, kraj, key) %>%
   summarise(n = mean(val)) %>%
   ungroup() %>%
   mutate(key = gsub("ocena_", "", key)) %>%
   group_by(smak, key) %>%
   top_n(1, n) %>%
   ggplot() +
   geom_jitter(aes(key, smak, color = kraj, label = kraj),
               width = 0.1, height = 0.1,
               size = 5)


# w jakiej temperaturze podawać wina?
lista_win %>%
   filter(!is.na(smak)) %>%
   count(smak, kolor, temp_podawania) %>%
   ungroup() %>%
   group_by(smak, kolor) %>%
   arrange(desc(n)) %>%
   mutate(rown = row_number()) %>%
   top_n(-1, rown) %>%
   ungroup() %>%
   select(kolor, smak, temp_podawania) %>%
   spread(kolor, temp_podawania, fill = " ")


# od kiedy wina są dostępne w sklepie
lista_win %>%
   filter(!is.na(dostpene_od)) %>%
   ggplot() +
   geom_density(aes(dostpene_od), fill = "lightgreen")


# czy wina długo dostępne są bardziej popularne?
lista_win %>%
   mutate(popularnosc = as.numeric(popularnosc)) %>%
   filter(!is.na(dostpene_od), !is.na(popularnosc)) %>%
   ggplot() +
   geom_smooth(aes(dostpene_od, popularnosc))


# popularność vs cena
lista_win %>%
   mutate(popularnosc = as.numeric(popularnosc)) %>%
   ggplot() +
   geom_smooth(aes(cena, popularnosc)) +
   scale_x_log10()

lista_win %>%
   filter(cena <= 200) %>%
   mutate(popularnosc = as.numeric(popularnosc)) %>%
   mutate(cena = cut(cena, breaks = seq(0, 200, 10))) %>%
   mutate(cena = fct_rev(cena)) %>%
   ggplot() +
   geom_col(aes(cena, popularnosc)) +
   coord_flip()

# w jakim kraju jaki szczep?
lista_win %>%
   select(kraj, szczep) %>%
   separate(szczep, paste0("Szczep", 1:8), sep = ",") %>%
   gather(dummy, Szczep, -kraj) %>%
   filter(!is.na(Szczep)) %>%
   select(-dummy) %>%
   rowwise() %>%
   mutate(Szczep = str_trim(str_replace(Szczep,"\\(.*\\)", ""))) %>%
   ungroup() %>%
   count(kraj, Szczep, sort = T) %>%
   top_n(3, n) %>%
   ungroup() %>%
   ggplot() +
   geom_col(aes(Szczep, n)) +
   coord_flip() +
   facet_wrap(~kraj, scales="free", ncol = 3)


# średnia ocena szczepów
lista_win %>%
   select(expert_rating, szczep) %>%
   separate(szczep, paste0("Szczep", 1:8), sep = ",") %>%
   gather(dummy, Szczep, -expert_rating) %>%
   filter(!is.na(Szczep)) %>%
   select(-dummy) %>%
   rowwise() %>%
   mutate(Szczep = str_trim(str_replace(Szczep,"\\(.*\\)", ""))) %>%
   ungroup() %>%
   group_by(Szczep) %>%
   summarise(ocena = mean(expert_rating, na.rm = T)) %>%
   ungroup() %>%
   arrange(ocena) %>%
   top_n(10, ocena) %>%
   mutate(Szczep = factor(Szczep, level=Szczep)) %>%
   ggplot() +
   geom_col(aes(Szczep, ocena))

# średnia cena szczepów
lista_win %>%
   filter(!is.na(cena), !is.na(szczep)) %>%
   select(cena, szczep) %>%
   separate(szczep, paste0("Szczep", 1:8), sep = ",") %>%
   gather(dummy, Szczep, -cena) %>%
   filter(!is.na(Szczep)) %>%
   select(-dummy) %>%
   rowwise() %>%
   mutate(Szczep = str_trim(str_replace(Szczep,"\\(.*\\)", ""))) %>%
   ungroup() %>%
   group_by(Szczep) %>%
   summarise(m_cena = mean(cena)) %>%
   ungroup() %>%
   top_n(25, m_cena) %>%
   arrange(m_cena) %>%
   mutate(Szczep = factor(Szczep, levels = Szczep)) %>%
   ggplot() +
   geom_col(aes(Szczep, m_cena)) +
   coord_flip()

# grafy
library(igraph)
graf_df <- lista_win %>%
   select(kraj, szczep) %>%
   separate(szczep, paste0("Szczep", 1:8), sep = ",") %>%
   gather(dummy, Szczep, -kraj) %>%
   filter(!is.na(Szczep)) %>%
   select(-dummy) %>%
   rowwise() %>%
   mutate(Szczep = str_trim(str_replace(Szczep,"\\(.*\\)", ""))) %>%
   ungroup() %>%
   count(kraj, Szczep) %>%
   ungroup() %>%
   filter(n >= quantile(n, 0.75))

graf <- graph_from_data_frame(graf_df)
E(graf)$width <- graf_df$n

com <- cluster_walktrap(graf, weights = E(graf)$width)
V(graf)$color <- com$membership+1

graf_lay <- layout_with_fr(graf, weights = E(graf)$width)

plot(graf,
     vertex.label = V(graf)$name,
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     vertex.label.dist = 0.2,
     vertex.size = 3,
     edge.arrow.size = 1,
     edge.arrow.width = NA,
     edge.width = 8*E(graf)$width/max(E(graf)$width),
     edge.curved = TRUE,
     layout = graf_lay,
     mark.groups = com)
