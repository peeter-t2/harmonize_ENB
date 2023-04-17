# Harmonize Estonian National Bibliography (ENB)

## Contents

The package offers 4 commands that harmonize the National Bibliography
- marc2tidy() - This function converts a long tsv in marc format into a tidy wide table with extracted information.
- harmonize_publishers() - This function harmonizes publishers in ENB.
- harmonize_places() - This function harmonizes places of publication in ENB.
- harmonize_genres() - This function harmonizes genres in ENB.

---

## Starting up

1) First, install the required package

```
remotes::install_github("peeter-t2/harmonize_ENB")
```

2) Activate the package that was installed, use 
```
library(ENBtools)
```

3) Download the latest ENB file from the server and convert it to tidy format: 1) [Estonian books](https://data.digar.ee/erb/ERB_eestikeelne_raamat.zip), 2) [Books in other languages](https://data.digar.ee/erb/ERB_muukeelne_raamat.zip). First the marc21xml needs to be converted to a tsv file. This can be done via marctools and through converting it first to .mrc file and then to .tsv.

```
# Instructions to convert from marc21xml to tsv.

# Written 20.07.2018 - when downloading updated version of ENB- they have e.g. updated a few titles from V to W -> see example https://erb.nlib.ee/?marc=15213250, in old version had just 245 as Viletsusest viletsusse, now 245 is with W and old title is moved to 246

# 1) download the data dump file (it is in marc21.xml)
# 2) use marcedit 7 (6 can work too), run MARC tools, and convert from xml to mrc

# To run MarcEdit on linux, run 
# mono MarcEdit.exe in the folder

# While converting xml to mrc, use utf-8 encoding just in case.

# 3) can also convert mrc to json, but not necessary,

# then marcedit tools will offer
# Openrefine Data Transfer Tool

# give the mrc as source file
# and tsv as save file (dropdown menu below)
# use export to openrefine and run,
# this should give a wellformed tsv file from xml stored marc file.
# In older versions with no dropdown menu, openrefine export will detect .tsv from the filename and transfer there.

# each operation will take some a few minutes, but it will show progress while doing it....
# MarcEdit 6 had some problems in crashing when converting to tsv i think, but MarcEdit 7 does this quite fine.
# The resulting tsv will have one too many tabs in the first row, which can simply be edited out in notepad++


```

3) marc2tidy() to convert the long tsv into a tidy wide table.

```

file3 <- fread(here("data/raw/ENB_data/ENB_eestikeelne_raamat.tsv"),select=1:4,sep="\t")
works1 <- marc2tidy(file3)
fwrite(works1,here("data/processed/ENB_works_est.tsv"),sep="\t")
works1[,set:="eesti"]

file3 <- fread(here("data/raw/ENB_data/ENB_muukeelne_raamat.tsv"),select=1:4,sep="\t")
works2 <- marc2tidy(file3)
fwrite(works2,here("data/processed/ENB_works_other.tsv"),sep="\t")
works2[,set:="muukeelne"]
works <- rbind(works1,works2,fill=T)

fwrite(works,"ENB_works_both.tsv",sep="\t")

```


4) Harmonize places, genres, publishers

```

works <- fread("ENB_works_both.tsv",sep="\t")
#### harmonize publishers
works <- harmonize_publishers(works) #keeps the original in kirjastus_orig, also creates variable for first year publisher was present
#### harmonize publisher locations
works <- harmonize_places(works)

####
genres <- works[,.(genre=unlist(str_split(genres,"\\$a"))),by=.(RRid,aeg,koht,kirjastus,autor_id,comptitle,meta_eks2,genres,set)][genres!=""&genre!=""&!is.na(genre)|genres==""|is.na(genres)][,genre:=trimws(str_replace_all(genre,"\\.",""))]
genres <- harmonize_genres(genres,"ENG")
```

5) Use the data for further analysis.

There are other useful ways to preprocess the ENB data for analysis. An example is given in the code in [tidy_ENB](https://github.com/peeter-t2/tidy_ENB) github repository. See the file build_ENB_works.Rmd.

There are processed files available for the books and people associated with the ENB, with also simple geotags based on the placenames added.
