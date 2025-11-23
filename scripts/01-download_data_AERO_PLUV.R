# ALL DATA CONVERTED FROM UTC to Sao Paulo GMT.

##########################
## DADOS AEROPORTOS MESONET

# Dados em UTC

meteo_mesonet <- read_csv("./data/data_input/aeroportos_mesonet2.csv")
meteo_mesonet <- read_csv("./data/data_aeroportos/asos.csv")

meteo_mesonet <- meteo_mesonet %>%
  mutate(site = case_when(station == "SBBI" ~ "Aeroporto Bacacheri Histórico",
                          station == "SBCT" ~ "Aeroporto Curitiba Histórico",
                          TRUE ~ "station"),
         date = with_tz(valid, "America/Sao_Paulo"),
         temp = (tmpf-32)*5/9,
         umid = relh,
         wd = drct,
         ws = sknt * 0.51444,
         prec = NA) %>%
  select(date, site, temp, umid, wd, ws) %>%
  unique()

meteo_mesonet$ws[meteo_mesonet$ws > 499] <- NA
tz(meteo_mesonet$date)


# gerar arquivo planilha unificada
write.csv(meteo_mesonet,
          "./data/data_input/meteo_mesonet.csv",
          row.names = FALSE)




###########################################

########### PLUVIOMETROS - CEMADEN ########


#http://www2.cemaden.gov.br/mapainterativo/#
library(tidyverse)


# Dados horários em UTC

# puxar banco de dados meteo da pasta
?list.files
temp <- list.files(path = "./data/dados_CEMADEN",
                   pattern = "*.csv") # listar arquivos .csv do diretório
head(temp)




# aplicar leitura das planilhas contidas na listagem temp
dir <- "./data/dados_CEMADEN"
temp.qualified <- paste(dir, temp, sep = "/")
myfiles <- lapply(temp.qualified,
                  read.delim,
                  #skip = 8, # must save all excel files in CSV UTF-8 format and it works.
                  sep = ";",
                  dec = ",",
                  header = T,
                  check.names = T)
class(myfiles)
summary(myfiles)
View(myfiles)


# checking if all data frames have the same column names
my_func <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      print('Warning: Names are not the same')
      break
    }
    else if(i==tail(names(y),n=1)) {
      print('Names are identical')
    }
  }
}

my_func(myfiles[[1]], myfiles[[20]])

# unificar planilhas de dados
pluviometros <- do.call("rbind", myfiles)

pluviometros_coord <- pluviometros %>%
  filter(municipio == "ALMIRANTE TAMANDARÉ" | municipio == "ITAPERUÇU" |
           municipio == "CAMPO LARGO" | municipio == "RIO BRANCO DO SUL" |
           municipio == "COLOMBO") %>%
  select(municipio, nomeEstacao, latitude, longitude) %>% unique()

pluviometros <- pluviometros %>%
  filter(municipio == "ALMIRANTE TAMANDARÉ" | municipio == "ITAPERUÇU" |
           municipio == "CAMPO LARGO" | municipio == "RIO BRANCO DO SUL" |
           municipio == "COLOMBO") %>%
  mutate(site = paste0(municipio, "_", nomeEstacao),
         date = ymd_hms(datahora),
         prec = valorMedida,
         day = as.Date(date),
         hour = hour(date)) %>%
  group_by(site, day, hour) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  mutate(hour = paste(hour, ":00:00", sep =""),
         date = ymd_hms(paste(day, hour))) %>%
  ungroup() %>%
  select(site, date, prec)

pluviometros$date <- with_tz(pluviometros$date, tz = "America/Sao_Paulo")

tz(pluviometros$date)
# gerar arquivo planilha unificada
write.csv(pluviometros,
          "./data/data_input/pluviometros.csv",
          row.names = FALSE)



# gerar arquivo planilha unificada
write.csv(pluviometros_coord,
          "./data/data_input/pluviometros_coord.csv",
          row.names = FALSE)





# Download aeroports data

# https://mesonet.agron.iastate.edu/request/download.phtml?network=BR__ASOS
# OTHER REF: https://docs.ropensci.org/riem/articles/forecast.html

# OU download using R package "riem"

# R Package ----

library("devtools")
install_github("ropenscilabs/riem")
library(riem)

### Example:




# Curitiba - Aeroporto Bacacheri [SBCT]
aero_curitiba <- riem_measures(
  station = "SBCT",
  date_start = "2023-06-01",
  date_end = "2025-11-30"
)


# São José dos Pinhais [SBBI]

aero_saojose <- riem_measures(
  station = "SBBI",
  date_start = "2023-06-01",
  date_end = "2025-11-30"
)

tz(aero_curitiba$valid)




##########################
## DADOS AEROPORTOS MESONET

# Dados em UTC
meteo_mesonet <- bind_rows(aero_curitiba, aero_saojose)



meteo_mesonet <- meteo_mesonet %>%
  mutate(site = case_when(station == "SBBI" ~ "Aeroporto Bacacheri Histórico",
                          station == "SBCT" ~ "Aeroporto Curitiba Histórico",
                          TRUE ~ "station"),
         date = with_tz(valid, "America/Sao_Paulo"),
         temp = (tmpf-32)*5/9,
         umid = relh,
         wd = drct,
         ws = sknt * 0.51444,
         prec = NA) %>%
  select(date, site, temp, umid, wd, ws) %>%
  unique()

meteo_mesonet$ws[meteo_mesonet$ws > 499] <- NA
tz(meteo_mesonet$date)

meteo_mesonet$date <- with_tz(meteo_mesonet$date, tz = "America/Sao_Paulo")


# gerar arquivo planilha unificada
#write.csv(meteo_mesonet,
 #         "./data/meteo_mesonet.csv",
  #        row.names = FALSE)
save(meteo_mesonet,
     file =  "./data/meteo_mesonet.Rda")



###########################################

########### PLUVIOMETROS - CEMADEN ########


#http://www2.cemaden.gov.br/mapainterativo/# > "Todos os municípios"
library(tidyverse)


# Dados horários em UTC

library(data.table)

files <- list.files(path = "./data/data_input/CEMADEN/ALL",
                    pattern = "\\.csv$",
                    recursive = TRUE,
                    full.names = TRUE)

DT <- rbindlist(lapply(files, function(f) {
  d <- fread(f)
  d[, source := f]   # add file path as a column
  d
}), use.names = TRUE, fill = TRUE)


pluviometros_coord <- DT %>%
  filter(municipio == "ALMIRANTE TAMANDARÉ" | municipio == "ITAPERUÇU" |
           municipio == "CAMPO LARGO" | municipio == "RIO BRANCO DO SUL" |
           municipio == "COLOMBO") %>%
  select(municipio, nomeEstacao, latitude, longitude) %>% unique()

pluviometros <- DT %>%
  filter(municipio == "ALMIRANTE TAMANDARÉ" | municipio == "ITAPERUÇU" |
           municipio == "CAMPO LARGO" | municipio == "RIO BRANCO DO SUL" |
           municipio == "COLOMBO") %>%
  mutate(site = paste0(municipio, "_", nomeEstacao),
         date = ymd_hms(datahora),
         prec = valorMedida,
         day = as.Date(date),
         hour = hour(date)) %>%
  group_by(site, day, hour) %>%
  summarise(prec = sum(prec, na.rm = TRUE)) %>%
  mutate(hour = paste(hour, ":00:00", sep =""),
         date = ymd_hms(paste(day, hour))) %>%
  ungroup() %>%
  select(site, date, prec) %>%
  arrange(date)

pluviometros$date <- with_tz(pluviometros$date, tz = "America/Sao_Paulo")


checagem <- pluviometros %>%
  mutate(day = day(date),
         month = month(date),
         year = year(date)) %>%
  select(day, month, year) %>%
  unique() %>%
  group_by(year, month) %>%
  summarize(n = n())

tz(pluviometros$date)

# gerar arquivo planilha unificada
#write.csv(pluviometros,
        #  "./data/pluviometros.csv",
         # row.names = FALSE)

save(pluviometros,
     file =  "./data/pluviometros.Rda")


# gerar arquivo planilha unificada
#write.csv(pluviometros_coord,
        #  "./data/pluviometros_coord.csv",
        #  row.names = FALSE)

save(pluviometros_coord,
     file =  "./data/pluviometros_coord.Rda")
