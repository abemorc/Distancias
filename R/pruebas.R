rmarkdown::render(here("Rmd", "Reporte.Rmd"), output_file = here("Reportes", "Reporte.html"))

kbl(dfdatos1Est, digits = 3, booktabs = T)

here()

kbl(head(iris), digits = 3, booktabs = T)

dt <- head(iris)
kbl(dt, booktabs = T) %>%
  kable_styling(latex_options = "striped")
kbl(dt, caption = "Demo table", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"))


correlaciones %>%
  kbl(caption = "CORRELACIONES", digits = 3, align = "c") %>%
  kable_classic_2(full_width = F) %>% 
  footnote(general = "Calculado por el método de Spearman")
