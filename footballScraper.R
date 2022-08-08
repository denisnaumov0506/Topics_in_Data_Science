install.packages('jsonlite')
install.packages('rvest')
install.packages('tidyverse')
install.packages('lubridate')
install.packages('httr')

library(jsonlite)
library(httr)
library(rvest)
library(tidyverse)
library(lubridate)

readr::read_csv()

# Marktwerte scrapen

url = "https://www.transfermarkt.de/bundesliga/marktwertaenderungen/wettbewerb/L1/plus//galerie/0?verein_id=&land_id=0&pos=&detailpos="

url %>%
  read_html() %>%
  html_elements('.responsive-table') %>%
  html_elements('.inline-table') %>%
  html_elements('.hauptlink') %>%
  html_elements('a') %>%
  html_attr('title') -> player_name

player_name[1]

url %>%
  read_html() %>%
  html_elements('.responsive-table') %>%
  html_elements('.zentriert') %>%
  html_elements('a') %>%
  html_attr('title') -> player_verein

player_verein[2]

url %>%
  read_html() %>%
  html_elements('.responsive-table') %>%
  html_elements('.rechts.hauptlink') %>%
  html_text() %>%
  str_extract("[0-9]*") -> marktwert

length(marktwert)
marktwert[1] = 0.5
marktwert

length(player_name)
length(player_verein)
length(marktwert)

# marktwerte über die Zeit von 2010 bis heute
# ?stichtag=2010-11-01
url_new = "https://www.transfermarkt.de/bundesliga/marktwerteverein/wettbewerb/L1/plus/"

url_new %>%
  read_html() %>%
  html_elements('.inline-select') %>%
  html_elements('option') %>%
  html_attr('value') -> dates

dates
list_marktwert = c()
list_verein = c()
list_date = c()

for (date in dates) {
  print(date)
  
  url_new_new = paste0(url_new, '?stichtag=', date)
  
  url_new_new %>%
    rvest::read_html() %>%
    html_elements('div') %>%
    html_elements('.responsive-table .items tbody [class=rechts] a') -> element
  
  element %>%
    html_text() %>%
    str_extract("[0-9]*\\,[0-9]*") -> items_marktwert
  
  element %>%
    html_attr('title') -> verein
  
  
  # change ',' to '.'
  for (i in 1:length(items_marktwert)) {
    items_marktwert[i] = gsub(',', '.', items_marktwert[i])
  }
  
  list_marktwert = append(list_marktwert, items_marktwert)
  list_verein = append(list_verein, verein)
  list_date = append(list_date, rep(c(date), each=18))

}

list_marktwert
list_verein
list_date

df = data.frame(verein=list_verein, marktwert=list_marktwert, date=list_date)
df

readr::write_csv(df, file = 'bundesliga_marktwerte_2010_2022.csv')

# Let us now scrape all ids of all countries (total: 252)

get_all_country_ids = function(max_) {
  base_url = "https://www.transfermarkt.de/wettbewerbe/national/wettbewerbe/"
  list_countries = list()
  list_slugs = c()
  list_names = c()
  list_ids = c()
  i = 1
  id = 1
  
  while (i < max_) {
    
    new_url = paste0(base_url, id)
    
    res = httr::GET(new_url)
    status = httr::status_code(res)
    
    if (status == 200) {
      i = i + 1
      new_url %>%
        read_html() %>%
        html_elements('.box .clearer.relevante-wettbewerbe-auflistung') %>%
        html_element('a') -> element_country_NEW
      
      element_country_NEW %>%
        html_attr('title') -> title
      
      element_country_NEW %>%
        html_attr('href') %>%
        strsplit('/') -> slug
      
      print(length(slug))
      
      if (length(slug) != 0) {
        slug = slug[[1]][2]
      } else {
        slug = NaN
      }
      
      print(length(title))
      
      if (length(title) == 0) {
        title = NaN
      }
      
      
      print(title)
      print(id)
      list_slugs = append(list_slugs, slug)
      list_names = append(list_names, title)
      list_ids = append(list_ids, id)
      id = id + 1
    } else {
      print(sprintf('For id number %d no country could be found!!!', id))
      id = id + 1
    }

  }
  list_countries[[1]] = list_ids
  list_countries[[2]] = list_names
  list_countries[[3]] = list_slugs
  list_countries
}


country_id = get_all_country_ids(252)
print(country_id)

df_countries = data.frame(id=country_id[[1]], names=country_id[[2]], slug=country_id[[3]])
df_countries

readr::write_csv(df_countries, file = 'country_codes.csv')

# Let us now scrape all ids of all european countries (total: ?)

get_all_european_country_ids = function() {
  base_url = "https://www.transfermarkt.de/wettbewerbe/europa"
  list_countries = list()
  list_names = c()
  list_ids = c()
  
  base_url %>%
    read_html() %>%
    html_elements('div .content.text-rechts #europa_Map area') -> elements
  
  elements %>%
    html_attr('href') %>%
    str_extract('[0-9]+') -> id
  
  elements %>%
    html_attr('title') -> title
    
    
  print(title)
  print(id)
  list_names = append(list_names, title)
  list_ids = append(list_ids, id)
  
  list_countries[[1]] = list_ids
  list_countries[[2]] = list_names
  list_countries
}

test = get_all_european_country_ids()
print(test)

df_euro = data.frame(id_code=test[[1]], country=test[[2]])
df_euro

write_csv(df_euro, file='euro_country_codes.csv')

# Let us now scrape all ids of bundesliga

get_all_club_ids_bundesliga = function() {
  base_url = 'https://www.transfermarkt.de/bundesliga/startseite/wettbewerb/L1'
  
  list_bundesliga = list()
  list_names = c()
  list_ids = c()
  
  base_url %>%
    rvest::read_html() %>%
    html_elements('.responsive-table .grid-view .items tbody [class="zentriert no-border-rechts"] a') -> elements
  
  elements %>%
    html_attr('title') -> titles
  
  elements %>%
    html_attr('href') %>%
    str_extract("[/][0-9]*[/]") %>%
    str_extract("[0-9]+") -> id
  
  print(titles)
  print(id)
  
  list_bundesliga[[1]] = id
  list_bundesliga[[2]] = titles
  
  list_bundesliga
}

club_id_bundesliga = get_all_club_ids_bundesliga()
print(club_id_bundesliga)

df_bundesliga = data.frame(club_id=club_id_bundesliga[[1]], club_name=club_id_bundesliga[[2]])
df_bundesliga

# scrape all ids of FC Bayern München

get_all_players_of_club = function(club_id, season) {
  base_url = paste0("https://www.transfermarkt.de/fc-bayern-munchen/startseite/verein/", club_id, "/saison_id/", season)
  
  list_player = list()
  
  base_url %>%
    read_html() -> html_
  
  html_ %>%
    html_elements('.items tbody .rn_nummer') %>%
    html_text() -> rueckennummer
  
  html_ %>%
    html_elements('.items tbody .posrela .hauptlink .hide-for-small a') %>%
    html_text() -> name
  
  html_ %>%
    html_elements('.items tbody .posrela tr:last-child td') %>%
    html_text() -> position
  
  html_ %>%
    html_elements('.items tbody [class="zentriert"]') -> birth_html
  
  birth = c()
  
  for (item in birth_html) {
    if (length(html_children(item)) == 0) {
      birth = append(birth, html_text(item))
    }
  }
  
  print(birth)
  
  birth %>%
    str_extract('[0-9]+[.][0-9]+[.][0-9]+') -> birth_date
  
  birth %>%
    str_extract('[(][0-9]+[)]') %>%
    str_extract('[0-9]+') -> age
  
  html_ %>%
    html_elements(' .items tbody [class="zentriert "]') -> nationality_html
  
  nationality = c()
  
  for (i in 1:length(nationality_html)) {
    n = html_elements(nationality_html[[i]], 'img') %>% html_attr('title')
    if(length(n) == 1) {
      nationality = append(nationality, n)
    } else {
      nationality = append(nationality, paste0(n[1], '/', n[2]))
    }
  }
  
  html_ %>%
    html_elements('.items tbody [class="rechts hauptlink"]') -> marktwerte_html
  
  print(html_elements(marktwerte_html[[11]], 'a') %>% html_text())
  marktwerte = c()
  
  for (i in 1:length(marktwerte_html)) {
    n = html_elements(marktwerte_html[[i]], 'a') %>% html_text()
    if(length(n) == 0) {
      marktwerte = append(marktwerte, NaN)
    } else {
      marktwerte = append(marktwerte, n)
    }
  }
  
  print(length(rueckennummer))
  print(length(name))
  print(length(position))
  print(length(birth))
  print(length(birth_date))
  print(length(age))
  print(length(nationality))
  print(marktwerte)
  
  
  list_player[[1]] = rueckennummer
  list_player[[2]] = name
  list_player[[3]] = position
  list_player[[4]] = birth_date
  list_player[[5]] = age
  list_player[[7]] = nationality
  list_player[[8]] = marktwerte
  list_player
}

fc_bayern_munchen = get_all_players_of_club(15, 2021)
df_fc_bayern_player = data.frame(
  rueckennummer=fc_bayern_munchen[[1]],
  name=fc_bayern_munchen[[2]],
  pos=fc_bayern_munchen[[3]],
  birth_date=fc_bayern_munchen[[4]],
  age=fc_bayern_munchen[[5]],
  nationalities=fc_bayern_munchen[[7]],
  marktwerte=fc_bayern_munchen[[8]]
  )
df_fc_bayern_player

write_csv(df_fc_bayern_player, file='fc_bayern_munchen_players.csv')

eintracht_frankfurt = get_all_players_of_club(15, 2021)
df_eintracht_frankfurt_player = data.frame(
  rueckennummer=eintracht_frankfurt[[1]],
  name=eintracht_frankfurt[[2]],
  pos=eintracht_frankfurt[[3]],
  birth_date=eintracht_frankfurt[[4]],
  age=eintracht_frankfurt[[5]],
  nationalities=eintracht_frankfurt[[7]],
  marktwerte=eintracht_frankfurt[[8]]
)
df_eintracht_frankfurt_player

# test/test



          