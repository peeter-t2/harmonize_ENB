
#' Harmonize places (ENB)
#'
#' This function harmonizes places in ENB
#' @param genres the data.table format work_by_row table
#' @keywords places
#' @export
#' @examples
#' harmonize_places()
#' see https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/ on installation
#' 
#' 

harmonize_places <- function(works,fuzzy=T){

  
  
  works <- data.table(works)
    
    
  
  works[,koht_orig:=koht]
  works[,koht:=str_replace(koht,";","")]
  works[,koht:=str_replace(koht,",","")]
#  works[,koht:=str_replace(koht,"s$","")]
 # works[,koht:=str_replace(koht,"l$","")]
  
  
  #works <- works#[!is.na(koht),.N,by=koht]
 # works <- works[,koht:=str_replace_all(koht,"s$","")]
 # works <- works[,koht:=str_replace_all(koht,"l$","")]
  #works <- works[!duplicated(works)]
  #places[places!=""&places!="S.l."&places!="S.l.,"&places!="S. l."&places!="S. l.,"]
  works <- works[koht!=""&koht!="S.l."&koht!="S.l.,"&koht!="S. l."&koht!="S. l.,"]
  works[str_detect(koht,"Paide"),koht:="Paide"]
  works[str_detect(koht,"Weissenstein"),koht:="Paide"]
  works[str_detect(koht,"Вейсенштейн"),koht:="Paide"]
  works[str_detect(koht,"Вейссенштейн"),koht:="Paide"]
  works[str_detect(koht,"Haapsalu"),koht:="Haapsalu"]
  works[str_detect(koht,"Hapsa"),koht:="Haapsalu"]
  works[str_detect(koht,"Гапсаль"),koht:="Haapsalu"]
  
  works[str_detect(koht,"Keila"),koht:="Keila"]
  works[str_detect(koht,"Rakvere"),koht:="Rakvere"]
  works[str_detect(koht,"Wesenberg"),koht:="Rakvere"]
  works[str_detect(koht,"Везенберг"),koht:="Rakvere"]
  
  works[,koht:=str_replace_all(koht,"w","v")]
  works[,koht:=str_replace_all(koht,"W","V")]
  works[,koht:=str_replace_all(koht,"\\?","")]
  works[str_detect(koht,"Tall"),koht:="Tallinn"]
  works[str_detect(koht,"Talinn"),koht:="Tallinn"]
  works[str_detect(koht,"Reva"),koht:="Tallinn"]
  works[str_detect(koht,"Revel"),koht:="Tallinn"]
  works[str_detect(koht,"Ревель"),koht:="Tallinn"]
  works[str_detect(koht,"Reve"),koht:="Tallinn"]
  works[str_detect(koht,"Таллинн"),koht:="Tallinn"]
  works[str_detect(koht,"Таллин|Талiн|Taллин"),koht:="Tallinn"]
  works[str_detect(koht,"Kures"),koht:="Kuressaare"]
  works[str_detect(koht,"Kurres"),koht:="Kuressaare"]
  works[str_detect(koht,"Arensburg"),koht:="Kuressaare"]
  works[str_detect(koht,"Аренсбург"),koht:="Kuressaare"]
  
  
  works[str_detect(koht,"Rakwere"),koht:="Rakvere"]
  works[str_detect(koht,"Rakvere"),koht:="Rakvere"]
  works[str_detect(koht,"Tartu"),koht:="Tartu"]
  works[str_detect(koht,"Taaralinn"),koht:="Tartu"]
  works[str_detect(koht,"Jurjev"),koht:="Tartu"]
  works[str_detect(koht,"Jürjev"),koht:="Tartu"]
  works[str_detect(koht,"Юрьев"),koht:="Tartu"]
  #works[str_detect(koht,"Jurjew"),koht:="Tartu"]
  works[str_detect(koht,"Dorpat"),koht:="Tartu"]
  works[str_detect(koht,"Derpt"),koht:="Tartu"]
  works[str_detect(koht,"Дерпт"),koht:="Tartu"]
  works[str_detect(koht,"Тарту"),koht:="Tartu"]
  
  works[str_detect(koht,"Tarto"),koht:="Tartu"]
  works[str_detect(koht,"Leningrad"),koht:="Saint Petersburg"]
  works[str_detect(koht,"Leningraad"),koht:="Saint Petersburg"]
  works[str_detect(koht,"Petersburg"),koht:="Saint Petersburg"]
  works[str_detect(koht,"Peterburg"),koht:="Saint Petersburg"]
  works[str_detect(koht,"Peterburi"),koht:="Saint Petersburg"]
  works[str_detect(koht,"Petrograd"),koht:="Saint Petersburg"]
  works[str_detect(koht,"Петроград"),koht:="Saint Petersburg"]
  works[str_detect(koht,"Петрогад"),koht:="Saint Petersburg"]
  works[str_detect(koht,"Петербург"),koht:="Saint Petersburg"]
  works[str_detect(koht,"Leeningrad"),koht:="Saint Petersburg"]
  works[str_detect(koht,"Leeningraad"),koht:="Saint Petersburg"]
  works[str_detect(koht,"Peeterburg"),koht:="Saint Petersburg"]
  works[str_detect(koht,"Peeterburi|Санктпетербург"),koht:="Saint Petersburg"]
  works[str_detect(koht,"Peter[s]?b[o]?u[r]?g"),koht:="Saint Petersburg"]
  works[str_detect(koht,"Peterpurg|St\\.-Pétersbourg|St\\. Pétersbourg"),koht:="Saint Petersburg"]
  works[str_detect(koht,"СПБ"),koht:="Saint Petersburg"]
  works[str_detect(koht,"Ленинград"),koht:="Saint Petersburg"]

  works[str_detect(koht,"Кохтла-Ярве"),koht:="Kohtla-Järve"]
  works[str_detect(koht,"Wittenberg"),koht:="Wittenberg"]
  works[str_detect(koht,"Vittenberg"),koht:="Wittenberg"]
  works[str_detect(koht,"Киев"),koht:="Kiev"]
  works[str_detect(koht,"Немме"),koht:="Nõmme"] #can be from other Tallinn's too
  works[str_detect(koht,"Gutmannsbach"),koht:="Häädemeeste"]
  
  works[str_detect(koht,"Viljandi"),koht:="Viljandi"]
  works[str_detect(koht,"Fellin"),koht:="Viljandi"]
  works[str_detect(koht,"Феллин"),koht:="Viljandi"]
  works[str_detect(koht,"Pärnu"),koht:="Pärnu"]
  works[str_detect(koht,"Perno"),koht:="Pärnu"]
  works[str_detect(koht,"Pernu"),koht:="Pärnu"]
  works[str_detect(koht,"Pernov"),koht:="Pärnu"]
  works[str_detect(koht,"Pernau"),koht:="Pärnu"]
  works[str_detect(koht,"Пернов"),koht:="Pärnu"]
  works[str_detect(koht,"Põltsamaa"),koht:="Põltsamaa"]
  works[str_detect(koht,"Oberpahlen"),koht:="Põltsamaa"]

  works[str_detect(koht,"Petser|Петсери|Печеры"),koht:="Petseri"]  
  works[str_detect(koht,"Narva"),koht:="Narva"]
  works[str_detect(koht,"Нарва"),koht:="Narva"]
  works[str_detect(koht,"Riga"),koht:="Riga"]
  works[str_detect(koht,"Riia"),koht:="Riga"]
  works[str_detect(koht,"Рига"),koht:="Riga"]
  works[str_detect(koht,"Rija"),koht:="Riga"]
  works[str_detect(koht,"Rīga|Rīgā"),koht:="Riga"]
  works[str_detect(koht,"Ria"),koht:="Riga"]
  works[str_detect(koht,"Valga"),koht:="Valga"]
  works[str_detect(koht,"Valk"),koht:="Valga"]
  works[str_detect(koht,"Валк"),koht:="Valga"]
  works[str_detect(koht,"Vilniu"),koht:="Vilnius"]
  works[str_detect(koht,"Mitau"),koht:="Jelgava"]
  works[str_detect(koht,"Pari"),koht:="Paris"]
  
  works[str_detect(koht,"Vändra"),koht:="Vändra"]
  works[str_detect(koht,"Keila"),koht:="Keila"]
  works[str_detect(koht,"Võru"),koht:="Võru"]
  works[str_detect(koht,"Võro"),koht:="Võru"]
  works[str_detect(koht,"Verro"),koht:="Võru"]
  works[str_detect(koht,"Верро"),koht:="Võru"]
  works[str_detect(koht,"Helsingi"),koht:="Helsinki"]
  works[str_detect(koht,"Helsinki"),koht:="Helsinki"]
  works[str_detect(koht,"Nev-York"),koht:="New York City"]
  works[str_detect(koht,"Nev York"),koht:="New York City"]
  works[str_detect(koht,"New York"),koht:="New York City"]
  works[str_detect(koht,"Москва|Mосква|Moskova|Moskva"),koht:="Moscow"]
  works[str_detect(koht,"Моskova|Moscou|Moscov"),koht:="Moscow"]
  works[str_detect(koht,"Vaivara"),koht:="Vaivara"]
  works[str_detect(koht,"Vaivara"),koht:="Vaivara"]
#  works[str_detect(koht,"Eesti"),koht:="Eesti"]
  works[str_detect(koht,"Berliin|Берлин"),koht:="Berlin"]
  
  works[str_detect(koht,"Frankfurt Oder"),koht:="Frankfurt (Oder)"]
  works[str_detect(koht,"Frankfurt "),koht:="Frankfurt am Main"]
  works[str_detect(koht,"Frankfurt"),koht:="Frankfurt am Main"]
  works[str_detect(koht,"Vittenberg"),koht:="Wittenberg"]
  works[str_detect(koht,"München"),koht:="Munich"]
  works[str_detect(koht,"Praha"),koht:="Prague"]
  works[str_detect(koht,"Dantzig"),koht:="Gdansk"]
 
  works[str_detect(koht,"Vashington"),koht:="Washington"]
  works[str_detect(koht,"Venemaa"),koht:="Russia"]
  works[str_detect(koht,"Stochkholm|Stokholm"),koht:="Stockholm"]
  works[str_detect(koht,"Stuttgard"),koht:="Stuttgart"]
  works[str_detect(koht,"Varszava|Варшава|Varsav"),koht:="Warsaw"]
  works[str_detect(koht,"Jyväskyla"),koht:="Jyväskylä"]
  works[str_detect(koht,"Казан"),koht:="Kazan"]
  works[str_detect(koht,"Vien"),koht:="Vienna"]
  works[str_detect(koht,"Киiв|Киïв"),koht:="Kiev"]
  works[str_detect(koht,"Helsingfor"),koht:="Helsingfors"]
  works[str_detect(koht,"Los Angele"),koht:="Los Angeles"]
  works[str_detect(koht,"Brüsse|Bruxelle"),koht:="Bruxelles"]
  works[str_detect(koht,"Свердловск"),koht:="Sverdlovskiy"]
  works[str_detect(koht,"Hannover-Döhren"),koht:="Hannover"]
  works[str_detect(koht,"Leipzig"),koht:="Leipzig"]
  works[str_detect(koht,"Пярну"),koht:="Pärnu"]
  works[str_detect(koht,"Viesbaden"),koht:="Wiesbaden"]
  works[str_detect(koht,"Montrea"),koht:="Montreal"]
  works[str_detect(koht,"Greifsvald"),koht:="Greifswald"]
  works[str_detect(koht,"Minneapoli"),koht:="Minneapolis"]  
  works[str_detect(koht,"Niagaara Falls Kanada"),koht:="Niagara Falls"]
  works[str_detect(koht,"Lakevood"),koht:="Lakewood"]
  works[str_detect(koht,"Одесса"),koht:="Odessa"]
  works[str_detect(koht,"Мiнск"),koht:="Minsk"]
  works[str_detect(koht,"Breslau"),koht:="Wroclaw"]
  works[str_detect(koht,"Augsburg-Hochfeld"),koht:="Augsburg"]
  works[str_detect(koht,"Braunschveig"),koht:="Braunschweig"]
  
  
  
  
  # 
  # 11	Habaja	194
  # 12	Tüki	191
  # 18	Harkujärve	108
  # 19	Metsakasti	108
  # 26	Saarde	94
  # 27	Jädivere	92
  # 29	Muraste	84
  # 31	Saaremaa	78
  # 33	USA	77
  # 35	Imavere	68
  # 36	Jäneda	66
  # 38	Lelle	58
  # 43	Alliku	47
  # 50	Rootsi	42
  # 54	Hannover-Döhren	39
  # 56	Tõravere	37
  # 58	Koidu Harjumaa	36
  # 59	Veimar	34
  # 62	Свердловск	33
  # 63	Aruvalla	31
  # 64	Tammneeme	31
  # 65	Силламяэ	31
  # 66	Viesbaden	30
  # 68	Montrea	29
  # 69	Laitse	28
  # 70	Kingissepa	28
  # 71	Greifsvald	28
  # 72	Halliste	27
  # 73	Saksamaa	27
  # 75	Rooma	27
  # 76	Niagaara Falls Kanada	27
  # 77	Moskau	27
  # 78	Елгава	27
  # 79	Lakevood	26
  # 80	Zuric	26
  # 81	Тбилиси	26
  # 82	St.-Pétersbourg	25
  # 83	Псков	25
  # 84	Йыхви	25
  # 85	Ереван	25
  # 86	Екатеринбург	25
  # 87	Митава	24
  # 88	Urvaste	23
  # 89	Borå	23
  # 90	Mосква	23
  # 91	Раквере	23
  # 92	Brusse	22
  # 93	Obinitsa	22
  # 94	Madise	22
  # 95	Новосибирск	22
  # 96	Pärnamäe	21
  # 97	Печеры	21
  # 98	Õisu	20
  # 99	Kullamaa	20
  # 100	Tõrvandi	20
  # 101	United Kingdom	20
  # 102	Одесса	20
  # 103	Käsmu	19
  # 104	Minneapoli	19
  # 105	Tokio	19
  # 106	Bruxelle	19
  # 107	Мiнск	19
  # 108	Саку	19
  # 109	Soome	18
  # 110	Йошкар-Ола	18
  # 111	Jälgimäe	18
  # 112	Breslau	18
  # 113	Ташкент	18
  # 114	Taллин	18
  # 115	Mõdriku	17
  # 116	Augsburg-Hochfeld	17
  # 117	Kambja	17
  # 118	Kolga	16
  # 119	Pürksi	16
  # 120	Braunschveig	16
  # 121	Вильянди	16
  # 122	Dordecht Netherlands	16
  # 123	Санктпетербург	15
  # 124	Helme	15
  # 125	Painküla	15
  # 126	Ижкар	15
  # 127	Kündja	15
  # 128	Берлин	15
  # 129	Талiн	15
  # 130	Русе	15
  # 131	Dantzig	14
  # 132	Abja	14
  # 133	Hilana	14
  # 134	St. Pétersbourg	14
  # 135	Воронеж	14
  # 136	Баку	14
  # 137	Köln etc.	14
  # 138	Nõmme	13
  # 139	Lüganuse	13
  # 140	Haanja	13
  # 141	Lohusalu	13
  # 142	Kihnu	13
  # 143	Хаапсалу	13
  # 144	Душанбе	13
  # 145	Muhu	12
  # 146	Vääna-Jõesuu	12
  # 147	Koguva	12
  # 148	Turu	12
  # 149	Laulasmaa	12
  # 150	Saue Harjumaa	12
  # 151	Melliste	12
  # 152	Varbuse	12
  # 153	Kloogaranna	12
  # 154	Base	12
  # 155	Rigā	12
  # 156	Фрунзе	12
  # 157	Tartto	12
  # 158	Voodsville	12
  # 159	Koigi	11
  # 160	Genf	11
  # 161	Mooste	11
  # 162	Amherst Mass.	11
  # 163	Savariae	11
  # 164	Jõelähtme	11
  # 165	Pikavere	11
  # 166	Alu Raplamaa	11
  # 167	Leppneeme	11
  # 168	Estland	11
  # 169	Ярославль	11
  # 170	Vürzburg	11
  # 171	Petroskoi	11
  # 172	Алматы	11
  # 173	Tudulinna	10
  # 174	Vigala	10
  # 175	Tarvastu	10
  # 176	Pilistvere	10
  # 177	Geislingen/St.	10
  # 178	Vormsi	10
  # 179	Võduvere	10
  # 180	Varsav	10
  # 181	Oomiste	10
  # 182	In Heilbronn	10
  # 183	Arle	10
  # 184	Куремяэ	10
  # 185	Miitavi	9
  # 186	Libau	9
  # 187	Karilatsi	9
  # 188	Porkuni	9
  # 189	Schvarzenbeck	9
  # 190	Austraalia	9
  # 191	Padise	9
  # 192	Luksemburg	9
  # 193	Punsa	9
  # 194	Kanavere	9
  # 195	Abo	9
  # 196	Västerå	9
  # 197	Petropoli	9
  # 198	Darpata	9
  # 199	Прага	9
  # 200	Либава	9
  # 201	Kasse	9
  # 202	Выру	9
  # 203	Улаан-баатар	9
  # 204	Canada	9
  # 205	Tаллинн	9
  # 206	Virumaa	8
  # 207	Pajusi	8
  # 208	Velise	8
  # 209	Reola	8
  # 210	Kiltsi	8
  # 211	Tahkuranna	8
  # 212	Kuusiku	8
  # 213	s. l.	8
  # 214	Seedrioru	8
  # 215	Viitka	8
  # 216	Torino	8
  # 217	Suurupi	8
  # 218	Darmstadt-Eberstadt	8
  # 219	Paduvere	8
  # 220	Vaela	8
  # 221	Ruhingu	8
  # 222	Königstein im Taunu	8
  # 223	Tilga	8
  # 224	S. l.	8
  # 225	Jurjeff	8
  # 226	Кегель	8
  # 227	Париж	8
  # 228	Marburg/Lahn	8
  # 229	Скопje	8
  # 230	Hamburg:	7
  # 231	Ambla	7
  # 232	Polli	7
  # 233	Hageri	7
  # 234	Ridala	7
  # 235	Taani	7
  # 236	Vaivara	7
  # 237	Lehtse	7
  # 238	Viru-Nigula	7
  # 239	North Adelaide South Australia	7
  # 240	S.l.:	7
  # 241	S.	7
  # 242	Leisi	7
  # 243	Illuka	7
  # 244	Albu	7
  # 245	Ida-Virumaa	7
  # 246	Pulli	7
  # 247	Lümanda	7
  # 248	Mikitamäe	7
  # 249	Tabivere	7
  # 250	Kose Harjumaa	7
  # 251	Mähkli	7
  # 252	Ижевск	7
  # 253	Львiв	7
  # 254	Luhametsa	7
  # 255	Stettin	7
  # 256	Кронштадт	7
  # 257	Нью-Йорк	7
  # 258	Taрту	7
  # 259	Ашгабат	7
  # 260	Варна	7
  # 261	Kiidjärve	6
  # 262	Karuse	6
  # 263	Valgjärve	6
  # 264	Alexandria Virginia	6
  # 265	Kurgja	6
  # 266	Noarootsi	6
  # 267	Eschenburg	6
  # 268	Kobenhavn	6
  # 269	Põlula	6
  # 270	Palade	6
  # 271	Viidumäe	6
  # 272	Roosna-Alliku	6
  # 273	Kalana	6
  # 274	Loo Harjumaa	6
  # 275	Jüri Harjumaa	6
  # 276	Kääpa	6
  # 277	Сыктывкар	6
  # 278	Kelvingi	6
  # 279	Võsupere	6
  # 280	Venevere	6
  # 
  # 
  
  
  works[str_detect(koht,"Nev"),koht:=str_replace(koht,"Nev","New")]
  works[str_detect(koht,"  "),koht:=str_replace(koht," +"," ")]
  works[str_detect(koht,"[()]"),koht:=str_replace(koht,"[()]","")]
  
  works[,koht:=trimws(koht)]
  
  works[str_detect(koht,"Aarhu"),koht:="Aarhus"]
  
  if (fuzzy==T){
    #make this into a function
    # standardize places that have only one character or less difference in their first word until space
    plotdata_kohad <- works[!is.na(koht)&koht!=""][,.N,by=koht][order(koht)]#[N>100]
    plotdata_kohad[,prevkoht:=lag(koht)]
    #used to have first 8 characters comparsion
    #plotdata_kohad[,subkoht:=substr(koht,1,8)]
    #plotdata_kohad[,subnext:=substr(nextkoht,1,8)]
    #plotdata_kohad[distthem<=1]
    #plotdata_kohad[subkoht==subnext]
    
    plotdata_kohad[,subkoht:=str_extract(koht,"[^ ]+")]
    plotdata_kohad[str_detect(koht," "),subkoht:=ifelse(nchar(subkoht)>4,yes=subkoht,no=str_extract(koht,"[^ ]+ [^ $]+"))]
    plotdata_kohad[,subprev:=str_extract(prevkoht,"[^ ]+")]
    plotdata_kohad[str_detect(prevkoht," "),subprev:=ifelse(nchar(subprev)>4,yes=subprev,no=str_extract(prevkoht,"[^ ]+ [^ $]+"))]
    library(stringdist)

    plotdata_kohad[,distthem:=stringdist(subkoht,subprev)]

    to <- plotdata_kohad[distthem<=1,prevkoht]
    from <- plotdata_kohad[distthem<=1,koht]
    map = setNames(to, from)
    plotdata_kohad[,koht2:=map[koht]]
    
    for (i in 1:8){
      plotdata_kohad[is.na(koht2),koht2:=koht]
      plotdata_kohad[,prevkoht:=lag(koht2)]
      plotdata_kohad[,subkoht:=str_extract(koht2,"[^ ]+")]
      plotdata_kohad[str_detect(koht2," "),subkoht:=ifelse(nchar(subkoht)>4,yes=subkoht,no=str_extract(koht2,"[^ ]+ [^ $]+"))]
      plotdata_kohad[,subprev:=str_extract(prevkoht,"[^ ]+")]
      plotdata_kohad[str_detect(prevkoht," "),subprev:=ifelse(nchar(subprev)>4,yes=subprev,no=str_extract(prevkoht,"[^ ]+ [^ $]+"))]
      plotdata_kohad[,distthem:=stringdist(subkoht,subprev)]
      to <- plotdata_kohad[distthem<=1,prevkoht]
      from <- plotdata_kohad[distthem<=1,koht2]
      map = setNames(to, from)
      plotdata_kohad[,koht2:=map[koht2]]
    }
    
    plotdata_kohad[is.na(koht2),koht2:=koht]
    to <- plotdata_kohad[,koht2]
    from <- plotdata_kohad[,koht]
    map = setNames(to, from)
    
    #overview
    plotdata_kohad2 <- plotdata_kohad[,.(N=sum(N)),by=koht2]
    
    works[,koht:=map[koht]]
    
    #check
    #works[,.N,by=koht]
  }
}


#notes:
# old2 <- with_finfo
# old2[,koht_orig:=koht]
# old2[,koht:=str_replace(koht,";","")]
# old2[,koht:=str_replace(koht,",","")]
# old2[,koht:=str_replace(koht,"s$","")]
# old2[,koht:=str_replace(koht,"l$","")]
# 
# 
# old2 <- old2#[!is.na(koht),.N,by=koht]
# old2 <- old2[,koht:=str_replace_all(koht,"s$","")]
# old2 <- old2[,koht:=str_replace_all(koht,"l$","")]
# #old2 <- old2[!duplicated(old2)]
# #places[places!=""&places!="S.l."&places!="S.l.,"&places!="S. l."&places!="S. l.,"]
# old2 <- old2[koht!=""&koht!="S.l."&koht!="S.l.,"&koht!="S. l."&koht!="S. l.,"]
# old2[str_detect(koht,"Paide"),koht:="Paide%20linn"]
# old2[str_detect(koht,"Weissenstein"),koht:="Paide%20linn"]
# old2[str_detect(koht,"Вейсенштейн"),koht:="Paide%20linn"]
# old2[str_detect(koht,"Вейссенштейн"),koht:="Paide%20linn"]
# old2[str_detect(koht,"Haapsalu"),koht:="Haapsalu%20linn"]
# old2[str_detect(koht,"Hapsa"),koht:="Haapsalu%20linn"]
# old2[str_detect(koht,"Гапсаль"),koht:="Haapsalu%20linn"]
# 
# old2[str_detect(koht,"Keila"),koht:="Keila%20linn"]
# old2[str_detect(koht,"Rakvere"),koht:="Rakvere%20linn"]
# old2[str_detect(koht,"Wesenberg"),koht:="Rakvere%20linn"]
# old2[str_detect(koht,"Везенберг"),koht:="Rakvere%20linn"]
# 
# old2[,koht:=str_replace_all(koht,"w","v")]
# old2[,koht:=str_replace_all(koht,"W","V")]
# old2[,koht:=str_replace_all(koht,"\\?","")]
# old2[str_detect(koht,"Tall"),koht:="hotell Tallinn"]
# old2[str_detect(koht,"Talinn"),koht:="hotell Tallinn"]
# old2[str_detect(koht,"Reva"),koht:="hotell Tallinn"]
# old2[str_detect(koht,"Revel"),koht:="hotell Tallinn"]
# old2[str_detect(koht,"Ревель"),koht:="hotell Tallinn"]
# old2[str_detect(koht,"Reve"),koht:="hotell Tallinn"]
# old2[str_detect(koht,"hotell Tallinn"),koht:="Tallinn"]
# old2[str_detect(koht,"Kures"),koht:="Kuressaare%20linn"]
# old2[str_detect(koht,"Kurres"),koht:="Kuressaare%20linn"]
# old2[str_detect(koht,"Arensburg"),koht:="Kuressaare%20linn"]
# old2[str_detect(koht,"Аренсбург"),koht:="Kuressaare%20linn"]
# 
# 
# old2[str_detect(koht,"Rakwere"),koht:="Rakvere%20linn"]
# old2[str_detect(koht,"Rakvere"),koht:="Rakvere%20linn"]
# old2[str_detect(koht,"Tartu"),koht:="Tartu%20linn"]
# old2[str_detect(koht,"Taaralinn"),koht:="Tartu%20linn"]
# old2[str_detect(koht,"Jurjev"),koht:="Tartu%20linn"]
# old2[str_detect(koht,"Jürjev"),koht:="Tartu%20linn"]
# old2[str_detect(koht,"Юрьев"),koht:="Tartu%20linn"]
# #old2[str_detect(koht,"Jurjew"),koht:="Tartu%20linn"]
# old2[str_detect(koht,"Dorpat"),koht:="Tartu%20linn"]
# old2[str_detect(koht,"Derpt"),koht:="Tartu%20linn"]
# old2[str_detect(koht,"Дерпт"),koht:="Tartu%20linn"]
# 
# old2[str_detect(koht,"Tarto"),koht:="Tartu%20linn"]
# old2[str_detect(koht,"Leningrad"),koht:="Peterburi"]
# old2[str_detect(koht,"Leningraad"),koht:="Peterburi"]
# old2[str_detect(koht,"Peterburg"),koht:="Peterburi"]
# old2[str_detect(koht,"Peterburi"),koht:="Peterburi"]
# old2[str_detect(koht,"Petrograd"),koht:="Peterburi"]
# old2[str_detect(koht,"Петроград"),koht:="Peterburi"]
# old2[str_detect(koht,"Петрогад"),koht:="Peterburi"]
# old2[str_detect(koht,"Петербург"),koht:="Peterburi"]
# old2[str_detect(koht,"Leeningrad"),koht:="Peterburi"]
# old2[str_detect(koht,"Leeningraad"),koht:="Peterburi"]
# old2[str_detect(koht,"Peeterburg"),koht:="Peterburi"]
# old2[str_detect(koht,"Peeterburi"),koht:="Peterburi"]
# old2[str_detect(koht,"СПБ"),koht:="Peterburi"]
# old2[str_detect(koht,"Ленинград"),koht:="Peterburi"]
# 
# old2[str_detect(koht,"Немме"),koht:="Nõmme"] #can be from other Tallinn's too
# old2[str_detect(koht,"Gutmannsbach"),koht:="Häädemeeste"]
# 
# old2[str_detect(koht,"Viljandi"),koht:="Viljandi%20linn"]
# old2[str_detect(koht,"Fellin"),koht:="Viljandi%20linn"]
# old2[str_detect(koht,"Феллин"),koht:="Viljandi%20linn"]
# old2[str_detect(koht,"Pärnu"),koht:="Pärnu%20linn"]
# old2[str_detect(koht,"Perno"),koht:="Pärnu%20linn"]
# old2[str_detect(koht,"Pernu"),koht:="Pärnu%20linn"]
# old2[str_detect(koht,"Pernov"),koht:="Pärnu%20linn"]
# old2[str_detect(koht,"Pernau"),koht:="Pärnu%20linn"]
# old2[str_detect(koht,"Пернов"),koht:="Pärnu%20linn"]
# old2[str_detect(koht,"Põltsamaa"),koht:="Põltsamaa%20linn"]
# old2[str_detect(koht,"Oberpahlen"),koht:="Põltsamaa%20linn"]
# 
# old2[str_detect(koht,"Narva"),koht:="Narva%20linn"]
# old2[str_detect(koht,"Нарва"),koht:="Narva%20linn"]
# old2[str_detect(koht,"Riga"),koht:="Riia%20linn"]
# old2[str_detect(koht,"Riia"),koht:="Riia%20linn"]
# old2[str_detect(koht,"Рига"),koht:="Riia%20linn"]
# old2[str_detect(koht,"Rija"),koht:="Riia%20linn"]
# old2[str_detect(koht,"Ria"),koht:="Riia%20linn"]
# old2[str_detect(koht,"Valga"),koht:="Valga%20linn"]
# old2[str_detect(koht,"Valk"),koht:="Valga%20linn"]
# old2[str_detect(koht,"Валк"),koht:="Valga%20linn"]
# 
# old2[str_detect(koht,"Vändra"),koht:="Vändra%20alev"]
# old2[str_detect(koht,"Keila"),koht:="Keila%20linn"]
# old2[str_detect(koht,"Võru"),koht:="Võru%20linn"]
# old2[str_detect(koht,"Verro"),koht:="Võru%20linn"]
# old2[str_detect(koht,"Верро"),koht:="Võru%20linn"]
# old2[str_detect(koht,"Helsingi"),koht:="Helsinki"]
# old2[str_detect(koht,"Helsinki"),koht:="Helsinki"]
# old2[str_detect(koht,"Nev-York"),koht:="New-York"]
# old2[str_detect(koht,"Nev York"),koht:="New York"]
# old2[str_detect(koht,"Москва"),koht:="Moskva"]
# old2[str_detect(koht,"Vaivara"),koht:="Vaivara%20vald"]
# old2[str_detect(koht,"Vaivara"),koht:="Vaivara%20vald"]
# old2[str_detect(koht,"Eesti"),koht:="Eesti"]
# old2[,koht:=trimws(koht)]
# places2 <- old2[!is.na(koht),.N,by=koht]
# places2 <- places2[order(-N)]
# 
# #for geoplacement, exclude major foreign cities, but generally would be fine
# places2 <-places2[!koht%in%c("Omski","Venemaa","Peterburi","Moskva","Tomsk","Permi","Peterhof","Омск","Novo-Nikolajevsk","Oudova","Vladivostok","Novonikolajevsk","Novosibirsk","Tsarskoje-Selo","Stockholm","London","Budapest","Norrköping","Arvika","Kopenhaagen","Londoni","Köningsberg","Katrineholm","Amsterdam","Gatschina","Danzig","Genf","Varszava","Brno","Nürnberg","Göteborg","Berliini","Praha","Södertälje","Brooklyn","New-York","San","Francisco","Chicago","Cochrane","(Vis.)","Vashington","D.C.","Philadelphia","Portland","Miitavi","Marienburg","Liibavi","Libau","Helsinki","Helsingi","Porvoo","Lahti","Kuopio","Tampereella","Viipuri","Berlin","Herrnhut","Hamburi","Leipzig","Berlin-Stieglitz","Magdeburg","Meissen","Dresden","Taani","Eesti","Saksamaa","USA","Germany")]
# 
# 
# #then could separate-tag, sources from outside estonia or nearby..
# #venemaal: Omski Venemaa Peterburi Moskva Tomsk Permi Peterhof Омск Novo-Nikolajevsk Oudova  Vladivostok Novonikolajevsk Novosibirsk Tsarskoje-Selo
# 
# #euroopas:  Stockholm London Budapest Norrköping Kopenhaagen Londoni Köningsberg Katrineholm Amsterdam Gatschina Danzig Genf Varszava Brno Nürnberg Göteborg Berliini Praha Södertälje 
# 
# #ameerikas: Brooklyn New-York San Francisco Chicago Cochrane (Vis.) Vashington D.C.  Philadelphia Portland
# 
# #lätis: Miitavi Marienburg Liibavi Libau
# #soomes: Porvoo Lahti Kuopio Tampereella Viipuri
# #saksamaal: Berlin Herrnhut Hamburi Leipzig Berlin-Stieglitz Magdeburg Meissen Dresden
# #riigid: Taani Eesti Saksamaa USA Germany 
# #unknown:  