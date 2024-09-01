library(tidyverse)
library(robonomistClient)

d <-
  data("StatFin/ntp/statfin_ntp_pxt_132g.px", tidy_time = TRUE) |>
  datalock(name = "riistoaste_ntp") |>
  filter(
    Taloustoimi %in% c(
      "B1GPH Bruttoarvonlisäys perushintaan",
      "P51CK Kiinteän pääoman kuluminen, menona",
      "D11K Palkat ja palkkiot, menona",
      "D12K Työnantajan sosiaaliturvamaksut, menona",
      "B13N Toimintaylijäämä / sekatulo, netto"
    ),
    Toimiala %in% c(
      "Yhteensä"
    ),
    Tiedot %in% c(
      "Kausitasoitettu ja työpäiväkorjattu sarja käypiin hintoihin, miljoonaa euroa"
    )
  ) |>
  select(Taloustoimi, time, value) |>
  pivot_wider(names_from = "Taloustoimi") |>
  mutate(
    nettoarvonlisäys = `B1GPH Bruttoarvonlisäys perushintaan` - `P51CK Kiinteän pääoman kuluminen, menona`,
    palkansaajakorvaukset = `D11K Palkat ja palkkiot, menona` + `D12K Työnantajan sosiaaliturvamaksut, menona`,
    nettotoimintaylijäämä = `B13N Toimintaylijäämä / sekatulo, netto`,
    palkansaajat = palkansaajakorvaukset / (palkansaajakorvaukset + nettotoimintaylijäämä),
    pääoma = nettotoimintaylijäämä / (palkansaajakorvaukset + nettotoimintaylijäämä)
  ) |>
  select(time, palkansaajat, pääoma) |>
  pivot_longer(c(palkansaajat, pääoma))

d |>
  ggplot(aes(time, 100 * value, fill = name)) +
  geom_area()

tail(d)

d_hist <-
  data("StatFin_Passiivi/ntp/statfinpas_ntp_pxt_906_199000.px") |>
  datalock(name = "riistoaste_ntp_hist") |>
  mutate(time = yq(paste(Vuosi, str_sub(Neljännes, 1, 1)))) |>
  filter(
    Hinta %in% c(
      "Käypiin hintoihin"
    ),
    Sarjatyyppi %in% c(
      "Kausitasoitettu ja työpäiväkorjattu"
    ),
    Taloustoimi %in% c(
      "B1GPH/A_P Arvonlisäys yhteensä",
      "K1K Kiinteän pääoman kuluminen",
      "D11 Palkat ja palkkiot",
      "D12 Työnantajan sosiaaliturvamaksut",
      "B13NT Toimintaylijäämä ja sekatulo, netto"
    )
  ) |>
  select(Taloustoimi, time, value) |>
  pivot_wider(names_from = "Taloustoimi") |>
  mutate(
    palkansaajakorvaukset = `D11 Palkat ja palkkiot` + `D12 Työnantajan sosiaaliturvamaksut`,
    nettotoimintaylijäämä = `B13NT Toimintaylijäämä ja sekatulo, netto`,
    Palkansaajat = palkansaajakorvaukset / (palkansaajakorvaukset + nettotoimintaylijäämä),
    Pääoma = nettotoimintaylijäämä / (palkansaajakorvaukset + nettotoimintaylijäämä)
  ) |>
  select(time, Palkansaajat, Pääoma) |>
  pivot_longer(c(Palkansaajat, Pääoma))

d_hist |>
  ggplot(aes(time, 100 * value, fill = name)) +
  geom_area()

full_join(d, d_hist, by = c("name", "time")) |>
  pivot_longer(c(value.x, value.y), names_to = "source") |>
  ggplot(aes(time, value, color = name, linetype = source)) +
  geom_line()


dg <-
  full_join(d, d_hist, by = c("name", "time")) |>
  mutate(
    value = 100 * coalesce(value.x, value.y),
    name = str_to_title(name) |> fct_rev()
  ) |>
  select(time, name, value) |>
  summarize(value = mean(value), .by = c("time", "name"))

dg |>
  ggplot(aes(time, value, fill = name)) +
  geom_area(alpha = 0.6) +
  scale_x_date(
    breaks = seq(as.Date("1970-01-01"), by = "5 years", length.out = 14),
    minor_breaks = "years",
    date_labels = "%Y",
    expand = expansion(c(0, 0.05))
  ) +
  geom_line(data = filter(dg, name == "Palkansaajat")) +
  scale_y_continuous(expand = expansion(), breaks = seq(0, 100, by = 20)) +
  labs(
    title = "Tulonjako työn ja pääoman välillä, 1975–2024",
    subtitle = "%, osuus nettoarvonlisäyksestä",
    caption = "Lähde: Tilastokeskus.",
    x = NULL, y = NULL, fill = NULL
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("riistoaste.png", width = 7, height = 4)

###

d_nettokansantulo_uusi <-
  data("StatFin/ntp/statfin_ntp_pxt_132h.px", tidy_time = TRUE) |>
  datalock(name = "kansantulo_ntp") |>
  filter(
    Taloustoimi == "B5N Nettokansantulo",
    Tiedot == "Trendisarja, viitevuosi 2015, miljoonaa euroa"
  ) |>
  select(time, value) |>
  left_join(
    data("StatFin/ntp/statfin_ntp_pxt_11tj.px§Keski§Yht§Alku", tidy_time = TRUE) |>
      select(time, value),
    by = "time"
  ) |>
  transmute(
    time,
    value = value.x / value.y
  )

d_nettokansantulo_hist <-
  data("StatFin_Passiivi/ntp/statfinpas_ntp_pxt_906_199000.px") |>
  datalock(name = "kansantulo_ntp_hist") |>
  mutate(time = yq(paste(Vuosi, str_sub(Neljännes, 1, 1)))) |>
  filter(
    Sarjatyyppi == "Kausitasoitettu ja työpäiväkorjattu",
    case_match(
      Taloustoimi,
      "B5NT Nettokansantulo" ~ Hinta == "Viitevuoden 2000 hintoihin",
      "EP Kokonaisväkiluku (100 henkeä)" ~ TRUE,
      .default = FALSE
    )
    ## Taloustoimi == c("EP Kokonaisväkiluku (100 henkeä)", "B5NT Nettokansantulo")
  ) |>
  drop_na() |>
  select(Taloustoimi, time, value) |>
  pivot_wider(names_from = "Taloustoimi") |>
  mutate(
    value = `B5NT Nettokansantulo` / `EP Kokonaisväkiluku (100 henkeä)`
  ) |>
  select(time, value)

d_nettokansantulo <-
  full_join(d_nettokansantulo_uusi, d_nettokansantulo_hist, by = "time") |>
  mutate(
    value.y = value.y / mean(value.y[year(time) == 1990]),
    value.x = value.x / mean(value.x[year(time) == 1990]),
    value = map2_dbl(value.x, value.y, ~ mean(c(.x, .y), na.rm = TRUE))
  ) |>
  select(time, value)


d_nettokansantulo |>
  mutate(value = 100 * value / mean(value[year(time) == 1975])) |>
  ggplot(aes(time, value)) +
  geom_line() +
  scale_x_date(
    breaks = seq(as.Date("1970-01-01"), by = "5 years", length.out = 14),
    minor_breaks = "years",
    date_labels = "%Y",
    expand = expansion(c(0, 0.05))
  ) +
  scale_y_continuous(expand = expansion()) +
  labs(
    title = "Nettokansantulo asukasta kohden, 1975–2024",
    subtitle = "Indeksi, 1975 = 100",
    caption = "Lähde: Tilastokeskus.",
    x = NULL, y = NULL, fill = NULL
  ) +
  theme_bw()

d_finale <-
  d_nettokansantulo |>
  mutate(value = 100 * value / mean(value[year(time) == 1975])) |>
  right_join(dg, by = "time") |>
  mutate(value = value.x * value.y / 100) |>
  arrange(time)

d_finale |>
  ggplot(aes(time, value)) +
  geom_area(aes(fill = name), alpha = 0.6) +
  geom_line(
    aes(color = name),
    data = summarise(d_finale, name = "Yhteensä", value = sum(value), .by = time)
  ) +
  scale_x_date(
    breaks = seq(as.Date("1970-01-01"), by = "5 years", length.out = 14),
    minor_breaks = "years",
    date_labels = "%Y",
    expand = expansion(c(0, 0.05))
  ) +
  scale_y_continuous(expand = expansion()) +
  scale_color_manual(values = c("black")) +
  labs(
    title = "Nettokansantulo asukasta kohden, 1975–2024",
    subtitle = "Indeksi, 1975 = 100",
    caption = "Lähde: Tilastokeskus.",
    x = NULL, y = NULL, fill = NULL, color = NULL
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("riistoaste2.png", width = 7, height = 4)
