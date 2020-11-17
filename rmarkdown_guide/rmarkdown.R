#install.packages('rmarkdown')

#install.packages('tinytex')

tinytex::install_tinytex()



rmarkdown::render('rmd_study1day.Rmd','pdf_document')
