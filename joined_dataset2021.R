if (exists("joined_dataset") == FALSE) {
  
  #Varningar vid inläsning, tycker inte om text i kollonerna
  tibble(dir="Indata2022/Sjöar/Vattenkemi/") %>%
    mutate(files = map(dir, ~list.files(here(.x)))) %>% 
    unnest(files) %>%
    transmute(path = paste0(dir, files)) %>% 
    mutate(data = map(path, ~import_slu_mvm_data_excel(here(.x), numeric_var = 31, sheet = 6, bad_quality_na =FALSE))) %>% #bytte here(.x) till .x
    mutate(path = str_extract(basename(path), "^[^_]+(?=_)"),
           Regionala = if_else(path %in% c("RMÖ","SRK"), 1, 0),
           data = map2(data, Regionala, ~.x %>% mutate(Regionala = .y,
                                                       #`Vattenzon(P,L,SP)`=`Vattenzon(P,L,SP)`%>%as.character()
           ))) %>%
    pull(data) %>%
    reduce(full_join) %>%
    dplyr::select(1:30, Regionala, everything()) %>%
    filter(!(`EU id` =="SE665234-135000" |  `EU id` ==  "SE665197-135082")) %>%   #ta bort stationer från övre Fryken
    mutate(`EU id` = case_when(`EU id` == "SE667022-134595" ~ "SE665218-134998",
                               TRUE ~ `EU id`),
           Övervakningsstation=case_when(is.na(Övervakningsstation)~Övervakningsstation,
                                         TRUE~Övervakningsstation),
           Övervakningsstation = case_when(Övervakningsstation == "Övre Fryken, 50 m söder om fiskodling" ~ "Övre Fryken",
                                           Övervakningsstation =="Övre Fryken, Torsby" ~"Övre Fryken",
                                           TRUE ~ Övervakningsstation),
           `Stationskoordinat N/X`= case_when (`Stationskoordinat N/X`== 6666576 ~ 6648600,
                                               TRUE ~ `Stationskoordinat N/X`),
           `Stationskoordinat E/Y` =case_when (`Stationskoordinat E/Y`== 390739 ~394983,
                                               TRUE ~ `Stationskoordinat E/Y`))  ->
    indexberakningar
  
  
  #Indexberakningar_data <- import_slu_mvm_data_excel(here("Indata2019/Sjöar/Växtplankton/Trend_stora_vplankton.xlsx"), numeric_var = 31, sheet = 3)
  #IKEU_indexberakningar <- import_slu_mvm_data_excel(here("Indata2019/Sjöar/Växtplankton/IKEU_vplankton.xlsx"), numeric_var = 31, sheet = 3)
  
  #indexberakningar <- full_join(Indexberakningar_data, IKEU_indexberakningar)
  var_for_sum<-c("Xanthophyceae (mm3/l)", "Chrysophyceae (mm3/l)", "Synurophyceae (mm3/l)",
                 "Raphidophyceae (mm3/l)", "Choanoflagellidea (mm3/l)", "Dinophyceae (mm3/l)",
                 "Euglenophyceae (mm3/l)", "Chlorophyta (mm3/l)", "Cyanobacteria (mm3/l)",
                 "Haptophyta (mm3/l)", "Bacillariophyta (mm3/l)", "Cryptophyta (mm3/l)", 
                 "Charophyta (mm3/l)", "Övriga Växtplankton (mm3/l)")
  
  
  #Hittar inte Stationsnamn
  tibble(dir="Indata2022/Sjöar/Vattenkemi/") %>%
    mutate(files = map(dir, ~list.files(here(.x)))) %>%
    unnest(files) %>%
    transmute(path = paste0(dir, files)) %>%
    mutate(data = map(path, ~import_slu_mvm_data_excel(here(.x), numeric_var = 32, sheet = 4))) %>%
    mutate(path = str_extract(basename(path), "^[^_]+(?=_)"),
           Regionala = if_else(path %in% c("RMÖ","SRK"), 1, 0),
           data = map2(data, Regionala, ~.x %>% mutate(Regionala = .y))) %>%
    pull(data) %>%
    reduce(full_join)%>%
    dplyr::select(1:28, Regionala, everything()) %>%
    mutate_at(30:42, Vectorize(function(x) {
      if (is.na(x)) {
        0
      } else {
        x
      }
    })) %>%
    rowwise %>%
    mutate(Biovolym2 = sum(c_across(`Xanthophyceae (mm3/l)`:`Övriga Växtplankton (mm3/l)`), na.rm=TRUE)%>% round(3),
           Övervakningsstation=case_when(is.na(Övervakningsstation)~Övervakningsstation,
                                         TRUE~Övervakningsstation)) ->
    vaxtplankton
  
  #Incombatible types vid "reduce(full_join)"
  tibble(dir="Indata2022/Sjöar/Vattenkemi/") %>%
    mutate(files = map(dir, ~list.files(here(.x)))) %>%
    unnest(files) %>%
    transmute(path = paste0(dir, files)) %>%
    mutate(data = map(path, ~import_slu_mvm_data_excel(here(.x), numeric_var = 29, sheet = 2))) %>%
    mutate(path = str_extract(basename(path), "^[^_]+(?=_)"),
           Regionala = if_else(path %in% c("RMÖ","SRK"), 1, 0),
           data = map2(data, Regionala, ~.x %>% mutate(Regionala = .y,
                                                       Provkommentar = Provkommentar %>% as.character(),
                                                       Undersökningstyp=Undersökningstyp %>% as.character()))) %>%
    pull(data) %>%
    reduce(full_join) %>% 
    dplyr::select(1:25, Regionala, everything()) %>%
    filter(!(`EU id` =="SE665234-135000" |  `EU id` ==  "SE665197-135082"))%>%   #ta bort stationer från övre Fryken
    mutate(`EU id` = case_when(`EU id` == "SE667022-134595" ~ "SE665218-134998", 
                               TRUE ~ `EU id`))%>%
    mutate(Övervakningsstation=case_when(is.na(Övervakningsstation)~Övervakningsstation,
                                         TRUE~Övervakningsstation),
           Övervakningsstation = case_when(Övervakningsstation == "Övre Fryken, 50 m söder om fiskodling" ~ "Övre Fryken",
                                           Övervakningsstation =="Övre Fryken, Torsby" ~"Övre Fryken", 
                                           TRUE ~ Övervakningsstation))->
    vattenkemi
  
  
  #Vattenkemi_data <- import_slu_mvm_data_excel(here("Indata2019/Sjöar/Vattenkemi/trend_stora_kemi.xlsx"), 26)
  #IKEU_Vattenkemi <- import_slu_mvm_data_excel(here("Indata2019/Sjöar/Vattenkemi/IKEU_kemi.xlsx"), 26)
  #rename(`Kfyll (µg/l)` = `Kfyll (µg/l)...54`) %>%
  # mutate(`Kfyll (µg/l)` = coalesce(`Kfyll (µg/l)`, `Kfyll (µg/l)...55`) %>% coalesce(`Kfyll (mg/m3)`)) %>%
  #select(-`Kfyll (µg/l)...55`, -`Kfyll (mg/m3)`)
  
  #vattenkemi <- full_join(Vattenkemi_data, IKEU_Vattenkemi)
  
  indexberakningar %>% # join the files
    full_join(vattenkemi) %>%
    full_join(vaxtplankton) %>%
    mutate( # `Kfyll (µg/l)` = coalesce(`Kfyll (mg/m3)`, `Kfyll (µg/l)`), # same unit, different names
      `Min provdjup (m)` = coalesce(`Min provdjup (m)`, `Min Provdjup (m)`), # merge duplicate column
      `Max provdjup (m)` = coalesce(`Max provdjup (m)`, `Max Provdjup (m)`),
      `Biovolym (mm3/l)` = coalesce(`Biovolym (mm3/l)`, Biovolym2)
    ) %>%
    dplyr::select( #-`Kfyll (mg/m3)`,
      -`Min Provdjup (m)`, -`Max Provdjup (m)`, -Biovolym2
    ) %>%
    mutate(
      `EU id` = case_when(
        `EU id` == "SE655587-158869" ~ "SE655605-158820", # Stora Envättern   dessa tre från fisk eftersom de finns med i VISS
        `EU id` == "SE656419-164404" ~ "SE656515-164330", # Stensjön
        `EU id` == "SE656590-164240" ~ "SE656640-164224", # Längsjön"SE655275-153234"~ "NW655349-153274",  #Älgsjön
        `EU id` == "SE656612-164132" ~ "SE656574-164098", # Årsjön
        `EU id` == "SE627443-149526" ~ "NW627437-149509", # Brunnsjön
        `EU id` == "SE653737-125017" ~ "NW653647-125036", # Ejgdesjön
        `EU id` == "SE645289-128665" ~ "NW645343-128665", # Fräcksjön
        `EU id` == "SE649314-149514" ~ "NW649253-149503", # Geten
        `EU id` == "SE633209-141991" ~ "NW633246-141963", # Gyslättasjön
        `EU id` == "SE643914-127698" ~ "NW643960-127717", # Härsvatten
        `EU id` == "SE683582-154935" ~ "NW683582-154935", # Källsjön
        `EU id` == "SE656640-164224" ~ "SE656590-164240", # Längsjön
        `EU id` == "SE662682-132860" ~ "SE656590-164240", # Örvattnet
        `EU id` == "SE674570-141911" ~ "NW674570-141911", # Rådsjön
        `EU id` == "SE652902-125783" ~ "NW652888-125811", # Rotehogstjärnen
        `EU id` == "SE666268-142230" ~ "NW666191-142252", # Skifsen
        `EU id` == "SE656515-164330" ~ "SE656419-164404", # Stensjön
        `EU id` == "SE664620-148590" ~ "NW664611-148550", # Västa Skälsjön
        `EU id` == "SE619510-135900" ~ "SE619626-135565", # Östra Ringsjön
        `EU id` == "SE619750-135450" ~ "SE620062-135224", # Västra Ringsjön
        TRUE ~ `EU id`
      ),
      `MS_CD C3` = case_when(
        `EU id` == "SE656612-164132" ~ "WA17355956", # Årsjön
        `EU id` == "SE649314-149514" ~ "WA74940653", # Geten
        `EU id` == "SE683582-154935" ~ "WA29887255", # Källsjön
        `EU id` == "SE674570-141911" ~ "WA24280365", # Rådsjön
        `EU id` == "SE666268-142230" ~ "WA70203019", # Skifsen
        TRUE ~ `MS_CD C3`
      ),
      Övervakningsstation = case_when(
        Övervakningsstation == "V. Skälsjön" ~ "Västra Skälsjön",
        Övervakningsstation == "Stora Skärsjön" ~ "St Skärsjön",
        Övervakningsstation == "Värmullen, norra viken" ~ "Värmullen",
        TRUE ~ Övervakningsstation
      )
    )%>%
    filter(`EU id`!="SE624180-141251")-> joined_dataset
  
}


#if(exists("Indexberakningar_data")==T){rm(Indexberakningar_data)}
#if(exists("IKEU_indexberakningar")==T){rm(IKEU_indexberakningar)}
if(exists("indexberakningar")==T){rm(indexberakningar)}

#if(exists("Vaxtplankton_data")==T){rm(Vaxtplankton_data)}
#if(exists("IKEU_Vaxtplankton")==T){rm(IKEU_Vaxtplankton)}
if(exists("vaxtplankton")==T){rm(vaxtplankton)}
#if(exists("Vattenkemi_data")==T){rm(Vattenkemi_data)}
#if(exists("IKEU_Vattenkemi")==T){rm(IKEU_Vattenkemi)}
if(exists("vattenkemi")==T){rm(vattenkemi)}


#setwd('C:/R/Sveriges-Vattenmiljo2023-main')


#write.table(joined_dataset, "Tabeller/joined_dataset.txt",
#            sep="\t",
#            row.names=FALSE,
#            fileEncoding = "utf-8")
