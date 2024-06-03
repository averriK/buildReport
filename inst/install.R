file.remove("NAMESPACE")
# usethis::use_author(
#   given="Alejandro",
#   family = "Verri Kozlowski",
#   role = c("aut", "cre"),
#   email= "averri@fi.uba.ar",
#   comment = c(ORCID = "0000-0002-8535-1170")
# )
# usethis::use_gpl_license(version = 3, include_future = TRUE)
devtools::document()
devtools::check()
remove.packages("buildGPT")
devtools::install()
