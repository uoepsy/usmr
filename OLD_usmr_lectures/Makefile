HTML_FILES=lecture_*.html

all : $(HTML_FILES)

clean : rm - $(HTML_FILES)

%.html : %.Rmd
	Rscript -e 'rmarkdown::render("$<")'

