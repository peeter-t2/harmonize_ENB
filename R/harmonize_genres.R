
#' Harmonize genres (ENB)
#'
#' This function harmonizes genres in ENB
#' @param genres the data.table format genre_by_row table
#' @keywords genres
#' @export
#' @examples
#' harmonize_genres()
#' see https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/ on installation

harmonize_genres <- function(genres,lang="ENG"){
#check for common topics with the help of genrelist
#genres[str_detect(genre,"jutluse"),unique(genre)]
  genres <- data.table(genres)
  
  if(lang=="EST"){
    #genres <- data.table(genres)
    genres[str_detect(genre,"kõned"),relig:="kõned"]
    genres[str_detect(genre,"ettekanded"),relig:="kõned"]
    
    genres[str_detect(genre,"kirjad"),relig:="kirjad"]
    genres[str_detect(genre,"autograaf"),relig:="kirjad"]
    genres[str_detect(genre,"päevik"),relig:="kirjad"]     
      
    genres[str_detect(genre,"vaimulik"),relig:="relig"]
    genres[str_detect(genre,"hardus"),relig:="relig"]
    genres[str_detect(genre,"katekism"),relig:="relig"]
    genres[str_detect(genre,"palve"),relig:="relig"]
    genres[str_detect(genre,"evange"),relig:="relig"]
    genres[str_detect(genre,"jutluse"),relig:="relig"]
    
    genres[str_detect(genre,"laul"),laul:="laul"]
    
    genres[str_detect(genre,"jutustus"),iluk:="iluk"]
    genres[str_detect(genre,"romaan"),iluk:="iluk"]
    genres[str_detect(genre,"^ilukirj"),iluk:="iluk"] #include vaimulik? not now
    genres[str_detect(genre,"novell"),iluk:="iluk"]
    genres[str_detect(genre,"jutud"),iluk:="iluk"]
    genres[str_detect(genre,"antoloogiad"),iluk:="iluk"]
    
    genres[str_detect(genre,"kroonikad"),kalend="kalend"]
    genres[str_detect(genre,"kalend"),kalend:="kalend"]
    
    genres[str_detect(genre,"album"),album:="album"]
    genres[str_detect(genre,"juubeliväljaand"),album:="album"]
    
    
    genres[str_detect(genre,"kooli"),haridus:="haridus"]
    genres[str_detect(genre,"bukoolika"),haridus:=NA]
    genres[str_detect(genre,"aab"),haridus:="haridus"]
    genres[str_detect(genre,"õppe"),haridus:="haridus"]
    genres[str_detect(genre,"õpik"),haridus:="haridus"]
    genres[str_detect(genre,"töövih"),haridus:="haridus"]
    genres[str_detect(genre,"õppekav"),haridus:="haridus"]
    genres[str_detect(genre,"ülesanded"),haridus:="haridus"]
    genres[str_detect(genre,"lugemikud"),haridus:="haridus"]
    genres[str_detect(genre,"loeng"),haridus:="haridus"]
    genres[str_detect(genre,"kontrolltööd"),haridus:="haridus"]
    
    
    
    
    genres[str_detect(genre,"käsiraa"),juhend:="juhend"]
    genres[str_detect(genre,"juhen"),juhend:="juhend"]
    genres[str_detect(genre,"nõuand"),juhend:="juhend"]
    
    genres[str_detect(genre,"kataloog"),reklaam:="reklaam"]
    genres[str_detect(genre,"infotrükised"),reklaam:="reklaam"]
    genres[str_detect(genre,"kavad"),reklaam:="reklaam"]
    genres[str_detect(genre,"reklaamtrükised"),reklaam:="reklaam"]    
    genres[str_detect(genre,"muuseumijuhid"),reklaam:="reklaam"]    
    genres[str_detect(genre,"sõiduplaanid"),reklaam:="reklaam"]    
    
    
    
    
    genres[str_detect(genre,"luule"),luule:="luule"]
    genres[str_detect(genre,"bukoolika"),luule:="luule"]
    
    genres[str_detect(genre,"kodukor"),bürokr:="bürokr"]
    genres[str_detect(genre,"põhikir"),bürokr:="bürokr"]
    genres[str_detect(genre,"seltsid$"),bürokr:="bürokr"]
    genres[str_detect(genre,"eeskirjad"),bürokr:="bürokr"]
    genres[str_detect(genre,"üleskutsed"),bürokr:="bürokr"]
    genres[str_detect(genre,"seadus"),bürokr:="bürokr"]
    genres[str_detect(genre,"õigus"),bürokr:="bürokr"]
    genres[str_detect(genre,"aruanded"),bürokr:="bürokr"]
    genres[str_detect(genre,"stenogrammid"),bürokr:="bürokr"]
    genres[str_detect(genre,"otsused"),bürokr:="bürokr"]
    genres[str_detect(genre,"määrus"),bürokr:="bürokr"]
    #genres[str_detect(genre,"poliit"),unique(genre)]
    
    
    genres[str_detect(genre,"mäng"),mäng:="mäng"]
    genres[str_detect(genre,"tants"),mäng:="mäng"]
    
    genres[str_detect(genre,"laste"),lastenoorte:="lastenoorte"]
    genres[str_detect(genre,"pildi"),lastenoorte:="lastenoorte"]
    genres[str_detect(genre,"värvi"),lastenoorte:="lastenoorte"]
    genres[str_detect(genre,"noorsoo"),lastenoorte:="lastenoorte"]
    
    genres[str_detect(genre,"näide"),näidend:="näidend"]
    
    genres[str_detect(genre,"biogra"),biogr:="biogr"]
    genres[str_detect(genre,"mälestused"),biogr:="biogr"]   
    
    genres[str_detect(genre,"reisikirj"),reis:="reis"]
    genres[str_detect(genre,"reisi"),reis:="reis"]
    
    #genres[str_detect(genre,"loogia"),unique(genre)]
    genres[str_detect(genre,"dissertats"),relig:="teadus"]
    genres[str_detect(genre,"disputats"),relig:="teadus"]
    genres[str_detect(genre,"autorefer"),relig:="teadus"]
    genres[str_detect(genre,"teadus"),teadus:="teadus"]
    genres[str_detect(genre,"labori"),teadus:="teadus"]
    genres[str_detect(genre,"statistilised"),teadus:="teadus"]
    genres[str_detect(genre,"tees"),teadus:="teadus"]
    genres[str_detect(genre,"kogumikud"),teadus:="teadus"]
    genres[str_detect(genre,"separaa"),teadus:="teadus"]
    
    genres[str_detect(genre,"teatm"),teatm:="teadus"]
    genres[str_detect(genre,"aimekirj"),teatm:="teadus"]
    genres[str_detect(genre,"nimest"),teatm:="teadus"]
    genres[str_detect(genre,"biblio"),teatm:="teadus"]
    genres[str_detect(genre,"magistritöö"),teatm:="teadus"]
    
    
    genres[str_detect(genre,"sõnaraamat"),dict:="sõnastik"]
    genres[str_detect(genre,"sõnastik"),dict:="sõnastik"]
    genres[str_detect(genre,"vestmik"),dict:="sõnastik"]
    
    genres[str_detect(genre,"kokaraamatud"),toit:="toit"]
    genres[str_detect(genre,"toid"),toit:="toit"]
    
    
    genres[str_detect(genre,"e-raamatud"),online:="võrgus"]
    genres[str_detect(genre,"võrguväljaand"),online:="võrgus"]
    
  }
  
  if(lang=="ENG")
  {
    #genres <- data.table(genres)
    
    
    genres[str_detect(genre,"kõned"),relig:="speeches"]
    genres[str_detect(genre,"ettekanded"),relig:="speeches"]
    
    genres[str_detect(genre,"kirjad"),relig:="letters"]
    genres[str_detect(genre,"autograaf"),relig:="letters"]
    genres[str_detect(genre,"päevik"),relig:="letters"]
    
    
    genres[str_detect(genre,"vaimulik"),relig:="relig"]
    genres[str_detect(genre,"hardus"),relig:="relig"]
    genres[str_detect(genre,"katekism"),relig:="relig"]
    genres[str_detect(genre,"palve"),relig:="relig"]
    genres[str_detect(genre,"evange"),relig:="relig"]
    genres[str_detect(genre,"jutluse"),relig:="relig"]
    
    
    genres[str_detect(genre,"laul"),laul:="song"]
    
    genres[str_detect(genre,"jutustus"),iluk:="fiction"]
    genres[str_detect(genre,"romaan"),iluk:="fiction"]
    genres[str_detect(genre,"^ilukirj"),iluk:="fiction"] #include vaimulik? not now
    genres[str_detect(genre,"novell"),iluk:="fiction"]
    genres[str_detect(genre,"jutud"),iluk:="fiction"]
    genres[str_detect(genre,"antoloogiad"),iluk:="fiction"]
    
    genres[str_detect(genre,"kroonikad"),kalend:="calendar"]
    genres[str_detect(genre,"kalend"),kalend:="calendar"]
    
    genres[str_detect(genre,"album"),album:="album"]
    genres[str_detect(genre,"juubeliväljaand"),album:="album"]
    
    
    
    genres[str_detect(genre,"kooli"),haridus:="education"]
    genres[str_detect(genre,"bukoolika"),haridus:=NA]
    genres[str_detect(genre,"aab"),haridus:="education"]
    genres[str_detect(genre,"õppe"),haridus:="education"]
    genres[str_detect(genre,"õpik"),haridus:="education"]
    genres[str_detect(genre,"töövih"),haridus:="education"]
    genres[str_detect(genre,"õppekav"),haridus:="education"]
    genres[str_detect(genre,"ülesanded"),haridus:="education"]
    genres[str_detect(genre,"lugemikud"),haridus:="education"]
    genres[str_detect(genre,"loeng"),haridus:="education"]
    genres[str_detect(genre,"kontrolltööd"),haridus:="education"]
    
    
    
    genres[str_detect(genre,"käsiraa"),juhend:="manual"]
    genres[str_detect(genre,"juhen"),juhend:="manual"]
    genres[str_detect(genre,"nõuand"),juhend:="manual"]
    
    genres[str_detect(genre,"kataloog"),reklaam:="advertisement"]
    genres[str_detect(genre,"infotrükised"),reklaam:="advertisement"]
    genres[str_detect(genre,"kavad"),reklaam:="advertisement"]
    genres[str_detect(genre,"reklaamtrükised"),reklaam:="advertisement"]    
    genres[str_detect(genre,"muuseumijuhid"),reklaam:="advertisement"]    
    genres[str_detect(genre,"sõiduplaanid"),reklaam:="advertisement"]    
    
    
    genres[str_detect(genre,"luule"),luule:="poetry"]
    genres[str_detect(genre,"bukoolika"),luule:="poetry"]
    
    genres[str_detect(genre,"kodukor"),bürokr:="bureaucratic"]
    genres[str_detect(genre,"põhikir"),bürokr:="bureaucratic"]
    genres[str_detect(genre,"seltsid$"),bürokr:="bureaucratic"]
    genres[str_detect(genre,"eeskirjad"),bürokr:="bureaucratic"]
    genres[str_detect(genre,"üleskutsed"),bürokr:="bureaucratic"]
    genres[str_detect(genre,"seadus"),bürokr:="bureaucratic"]
    genres[str_detect(genre,"õigus"),bürokr:="bureaucratic"]
    genres[str_detect(genre,"aruanded"),bürokr:="bureaucratic"]
    genres[str_detect(genre,"stenogrammid"),bürokr:="bureaucratic"]
    genres[str_detect(genre,"otsused"),bürokr:="bureaucratic"]
    genres[str_detect(genre,"määrus"),bürokr:="bureaucratic"]
    #genres[str_detect(genre,"poliit"),unique(genre)]
    
    
    genres[str_detect(genre,"mäng"),mäng:="games"]
    genres[str_detect(genre,"tants"),mäng:="games"]
    
    genres[str_detect(genre,"laste"),lastenoorte:="youth"]
    genres[str_detect(genre,"pildi"),lastenoorte:="youth"]
    genres[str_detect(genre,"värvi"),lastenoorte:="youth"]
    genres[str_detect(genre,"noorsoo"),lastenoorte:="youth"]
    
    genres[str_detect(genre,"näide"),näidend:="drama"]
    
    genres[str_detect(genre,"biogra"),biogr:="biography"]
    genres[str_detect(genre,"mälestused"),biogr:="biography"]
    
    genres[str_detect(genre,"reisikirj"),reis:="travel"]
    genres[str_detect(genre,"reisi"),reis:="travel"]
    
    
    #genres[str_detect(genre,"loogia"),unique(genre)]
    genres[str_detect(genre,"dissertats"),relig:="scholarly"]
    genres[str_detect(genre,"disputats"),relig:="scholarly"]
    genres[str_detect(genre,"autorefer"),relig:="scholarly"]
    genres[str_detect(genre,"teadus"),teadus:="scholarly"]
    genres[str_detect(genre,"labori"),teadus:="scholarly"]
    genres[str_detect(genre,"statistilised"),teadus:="scholarly"]
    genres[str_detect(genre,"tees"),teadus:="scholarly"]
    genres[str_detect(genre,"kogumikud"),teadus:="scholarly"]
    genres[str_detect(genre,"separaa"),teadus:="scholarly"]
    
    genres[str_detect(genre,"teatm"),teatm:="scholarly"]
    genres[str_detect(genre,"aimekirj"),teatm:="scholarly"]
    genres[str_detect(genre,"nimest"),teatm:="scholarly"]
    genres[str_detect(genre,"biblio"),teatm:="scholarly"]
    genres[str_detect(genre,"magistritöö"),teatm:="scholarly"]
    
    
    genres[str_detect(genre,"sõnaraamat"),dict:="dictionary"]
    genres[str_detect(genre,"sõnastik"),dict:="dictionary"]
    genres[str_detect(genre,"vestmik"),dict:="dictionary"]
    
    
    genres[str_detect(genre,"kokaraamatud"),toit:="food"]
    genres[str_detect(genre,"toid"),toit:="food"]
    
    genres[str_detect(genre,"e-raamatud"),online:="online"]
    genres[str_detect(genre,"võrguväljaand"),online:="online"]
    
  
  
  }
  
  
  genres[,genre_standardized:=trimws(str_replace_all(paste(relig,laul,iluk,kalend,haridus,juhend,luule,bürokr,mäng,lastenoorte,näidend,biogr,reis,teatm,teadus,album,reklaam,toit,dict,online,sep=" "),"NA",""))][,genre_standardized:=str_replace_all(genre_standardized,"( )+"," ")]
}
