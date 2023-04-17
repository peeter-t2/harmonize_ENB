
#' marc 2 tidy function (ENB)
#'
#' This function converts a long tsv in marc format into a tidy wide table with extracted information.
#' @param genres the data.table format work_by_row table
#' @keywords publishers
#' @export
#' @examples
#' harmonize_publishers()
#' see https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/ on installation
#' 
#' 

marc2tidy <- function(file3){
### r convert long to wide format



#100, 700 autor
#245 artikli pealkiri
# 260 $c  - ilmumisaasta
# 260 $a $b = <dc:publisher> = Väljaande kirjastus ja ilumiskoht 
#MARC21 540 = <dc:rights> = Juurdepääsu piirangu tähtaeg
#MARC21 542 = <dc:rights> = Autoriõiguse märkus ja litsents 
#773 $n = <dc:type> = Ajalehe kategooria Nt. päevaleht, maakonnaleht jn
#artiklite pealkirjad ajalehtedes samuti - perioodika
#773 = <dc:type> = Artikli ilmumise andmed 
#773 $3 = <dc:type> = Väljaande laad: ajaleht, ajakiri, jätkväljaan
#998 = <dc:identifier> = Viide täistekstile 
#999 = <dc:type> = artikli tüüp 


#less <- file3[Tags%in%c(542)]

less <- file3[Tags%in%c(100,245,246,260,700,100,500,542,650,655,752,856,"001",546,945)]

#use 001 as main id for merging...
#artiklite andmebaas siia kõrvale ka
#773, siia juurde



#less2 <- file3[Tag%in%c(998)]

#c all, mis seal on...


#str_extract("$aCon$tent","\\$aCon")
#str_extract("$aCon$tent","\\$a[^\\$]+")
str_replace("ConteĆµnt","Ćµ", "õ")
#gsub("Ćµ", "õ", "Ćµontent")
#gsub("ab","e","abc")
less[,Content:=str_replace_all(Content, "Ćµ", "õ")]
less[,Content:=str_replace_all(Content, "Ć¤", "ä")]
less[,Content:=str_replace_all(Content, "Ć¼", "ü")]
less[,Content:=str_replace_all(Content, "Ć¶", "ö")]
less[,Content:=str_replace_all(Content, "Ć–", "Õ")]
less[,Content:=str_replace_all(Content, "Ć", "Ü")]


#Ć, Ü

less[Tags==260]
less[Tags==260,a:=str_extract(Content,"\\$a[^\\$]+")]
less[Tags==260,b:=str_extract(Content,"\\$b[^\\$]+")]
less[Tags==260,c:=str_extract(Content,"\\$c[^\\$]+")]
less[Tags==260,e:=str_extract(Content,"\\$e[^\\$]+")]
less[Tags==260,f:=str_extract(Content,"\\$f[^\\$]+")]

less[Tags==260,koht:=str_replace(a,"\\$a","")]
less[Tags==260&str_detect(koht,"Eesti")&!is.na(e),koht:=e]
less[Tags==260,koht:=str_replace(koht,"\\[","")]
less[Tags==260,koht:=str_replace(koht,"\\]","")]
less[Tags==260,koht:=str_replace(koht," :","")]
less[Tags==260,koht:=str_replace(koht,"\\$e","")]
less[Tags==260,koht:=str_replace(koht,"\\(","")]

less[Tags==260,kirjastus:=str_replace(b,"\\$b","")]
less[Tags==260,aeg:=str_replace(c,"\\$cc","")]
less[Tags==260,aeg:=str_replace(aeg,"\\$c","")]
less[Tags==260,aeg:=str_replace(aeg,"\\.","")]
less[Tags==260,aeg:=str_replace(aeg,"\\[","")]
less[Tags==260,aeg:=str_replace(aeg,"\\]","")]
less[Tags==260&str_detect(aeg,"\\-"),aeg:=str_extract(aeg,"\\-[0-9]+")]
less[Tags==260,aeg:=str_replace(aeg,"\\-","NA")]
less[Tags==260,aeg:=str_extract(aeg,"[0-9]+")]


less[Tags==245,title:=str_extract(Content,"\\$a[^\\$]+")]
less[Tags==245,title:=str_replace(title,"\\$a","")]
less[Tags==245,subtitle:=str_extract(Content,"\\$b[^\\$]+")]
less[Tags==245,subtitle:=str_replace(subtitle,"\\$b","")]
less[Tags==245,autor:=str_extract(Content,"\\$c[^\\$]+")]
less[Tags==245,autor:=str_replace(title,"\\$c","")]

less[Tags==542,litsents:=str_extract(Content,"\\$l[^\\$]+")]
less[Tags==542,litsents:=str_replace(litsents,"\\$l","")]

less[Tags==100,autor_id:=Content]
less[Tags==100,autor_name:=str_extract(Content,"\\$a[^\\$]+")]
less[Tags==100,autor_dates:=str_extract(Content,"\\$d[^\\$]+")]

less[Tags==700,teised_autorid:=Content]
#less[Tags==700,autor_dates:=str_extract(Content,"\\$d[^\\$]+")]

check <- less[Tags==500]

less[Tags=="001",RRid:=Content]
less[Tags==500,meta:=Content]

less[Tags==500,meta_eks:=str_extract(meta,"[0-9]+ eks")]
less[Tags==500,meta_kop:=str_extract(meta,"[0-9]+ kop")]


less[Tags==500,fraktuur:=str_extract(meta,"[Ff]raktuur")]
less[Tags==500,antiikva:=str_extract(meta,"[Aa]ntiik")]

less[Tags==650,keel:=Content]
less[Tags==655,genres:=paste0(Content)]
less[Tags==752,print:=Content]
less[Tags==856,links:=paste0(Content)]

less[Tags==546,keeled2:=paste0(Content)]

less[Tags==945,keeled3:=paste0(Content)]

#use 001 as tag
#file3[Tags=="001"]#[.N]
#file3[Tags=="072"]#[.N]
#file3[Tags=="080"]#[.N]
#file3[Tags=="998"]#[.N]
#file3[Tags=="856"]

#file3[RecordNumber==102852]



#save <- file3[RecordNumber==102845]
#save <- file3[RecordNumber==110338]



#
#less[!is.na(links)]

works <- dcast(melt(less[,.(RecordNumber,c,aeg,koht,kirjastus,title,subtitle,autor,autor_id,autor_name,autor_dates,teised_autorid,RRid,meta,meta_eks,meta_kop,fraktuur,antiikva,print,keel,genres,links,litsents,keeled2,keeled3)],id.vars=c("RecordNumber"))[!is.na(value)],RecordNumber~variable,fun.aggregate= function(x) paste(x, collapse=""), value.var = "value")
#works has 193k works


works[,aeg:=str_replace(aeg,"\\?","")]


#check sensibility
#works[aeg2<1500]
#works[aeg2>2020]

#some dates got mistakes with the conversion
works[RRid=="b4572345x",aeg:=2016]
works[RRid=="b12252864",aeg:=1921]
works[RRid=="b12263850",aeg:=1921]
works[RRid=="b21548250",aeg:=1895] #adding middle value to 189-something


works[,aeg2:=as.numeric(aeg)]
works[,comptitle:=paste0(title,subtitle,collapse=" "),by=RecordNumber]
works[,meta_eks2:=as.numeric(str_extract(meta_eks,"[0-9]+"))]
works[,decade:=floor(aeg2/10)*10]

works[,aeg:=as.numeric(aeg)]
#fix a few processing errors manually
#works[!is.na(aeg)&aeg<1500]
#works[!is.na(aeg)&aeg>2020]

return(works)

}

