
# Habe die tor scrapping Funktion (fast) direkt von Alex uebernommen
# konvertiere hier die Daten so, dass sie zur Profilwahl App
# passen. Mache auch minimale Checks. Diese koennen noch
# ausgeweitet werden.
tor.to.pw = function(tor = app$tor, app=getApp()) {
  restore.point("tor2pw")
  stud = tor$stud
  pw = empty.pw()
  pw$studname = stud$name
  pw$matnr = stud$matrikelnummer

  degree = stud$degree
  if (startsWith(degree,"Bachelor")) {
    pw$bama = "ba"
  } else if (startsWith(degree,"Master")) {
    pw$bama = "ma"
  } else {
    return(list(ok = FALSE, msg="Konnte Abschluss (Bachelor oder Master) nicht korrekt einlesen."))
  }

  tor.mods = tor$module %>%
    rename(code = modul_code)


  all.mods = app$glob$all.mods %>%
    filter(bama == pw$bama)

  pmods = semi_join(tor.mods, all.mods, by="code")
  pw$mods = all.mods %>% semi_join(pmods, by="code")

  pw$mopr = semi_join(app$glob$all.mopr, pw$mods, by=c("code","bama"))

  pw$other.mods = anti_join(tor.mods, all.mods, by="code") %>%
    rename(titel = modul_name)

  list(ok=TRUE,pw=pw)
}


import.tor = function(pdf.file) {
  restore.point("import.tor")
  txt = pdf_text(pdf.file)
  stud = extract_tor_stud(txt)
  module = extract_tor_module(txt)
  aah = extract_tor_aah(txt)
  list(stud=stud, module=module, aah=aah)
}

# Code zum groessten Teil von Alex uebernommen
extract_tor_stud <- function(tor){
  restore.point("extract_tor_stud")
  x <- enframe(tor) %>%
    separate_rows(value, sep="\n") %>%
    separate(value, into= c("name", "type", "type2", "date", "grade", "state", "lp", "notes", "try"), sep="\\s{2,}")

  x = as.matrix(x)
  name <- x[4,1]

  row = which(startsWith(x[,3], "Matrikelnummer:"))
  matrikelnummer = x[row,4]

  row = which(startsWith(x[,1], "Abschluss:"))
  degree = x[row,2]

  studentinfo <- tibble(name, matrikelnummer,degree)

  return(studentinfo)


  # Code unten funktioniert leider nicht auf Linux
  anrede <- x[3,1] %>% pull()
  name <- x[4,1] %>% pull()
  street <- x[5,1] %>% pull()
  place <- x[7,1] %>% pull()
  matrikelnummer <- as.numeric(x[17,4] %>% pull())
  fspo <- x[18,4] %>% pull()
  degree <- x[19,2] %>% pull()
  semester <- x[20,2] %>% pull()
  born <- dmy(x[19,4] %>% pull())

  studentinfo <- tibble(anrede, name, street, place, matrikelnummer, fspo, degree, semester, born)

  return(studentinfo)
}

extract_tor_module <- function(tor){
  restore.point("extract_tor_module")
  df = tor_transcript_df(tor)
  module <- df %>%
    filter( type == "MO") %>%
    rename(
      modul_code = code,
      modul_name = name
    )
  return(module)
}

# Annerkannte Leistungen
extract_tor_aah <- function(tor, add.A.to.code=TRUE){
  restore.point("extract_tor_aah")
  df = tor_transcript_df(tor) %>%
    filter(has.substr(notes,"AAH"))  %>%
    mutate(code )
  return(df)
}

tor_transcript_df = function(tor) {
  restore.point("tor_transcript_df")

  tor = sep.lines(tor)


  # pre-select rows starting with a number
  # so fixing two-lines module names does
  # work also at the end of table
  first4 = substring(trimws(tor), 1,4)
  entry.row = suppressWarnings(which(!is.na(as.integer(first4))))
  rows = sort(unique(c(entry.row, entry.row+1)))

  suppressWarnings(
    df <- enframe(tor[rows]) %>%
    #separate_rows(value, sep="\n") %>%
    separate(value, into= c("name", "type", "type2", "date", "grade", "state", "lp", "notes", "try"), sep="\\s{2,}") %>%
    mutate( date = dmy(date),
            try = as.numeric(ifelse( notes %in% c("AAH", "AAF"), try, notes)),
            notes = ifelse(notes == "AAH" | notes == "AAF", notes, NA),
            lp = as.numeric(lp),
            grade = as.numeric(str_replace(grade, ",", ".")),
            zusatz = ifelse(name == "4000", 1, NA)) %>% # Seminare, ESP, ASQ, Bachelorarbeit und Zusatzfächer
    fill(zusatz, .direction = "down") %>%
    filter( is.na(zusatz) ) %>%
    select(-zusatz) %>%
    # deal with names written on two lines
    mutate(
      keep = !is.na(date),
      name = ifelse(keep & !lead(keep) & !is.na(lead(type)) &!is.na(lead(type2)), paste0(name, " ", trimws(lead(type))), name)
    ) %>%
    filter(keep) %>%
    filter(type2 != "Geburtsdatum:") %>%
    mutate(
      code = str_extract(name, "[[:digit:]]+"),
      #modul_name = str_extract(name, "[[:alpha:]].*")
      name = str.right.of(trimws(name), " ") %>% trimws()
    ) %>%
    select(code=code, name=name, everything()) %>%
    select(-keep)
  )
  df
}


old.extract_tor_module <- function(tor){
  restore.point("extract_tor_module")
  module <- enframe(tor) %>%
    separate_rows(value, sep="\n") %>%
    separate(value, into= c("name", "type", "type2", "date", "grade", "state", "lp", "notes", "try"), sep="\\s{2,}") %>%
    mutate( date = dmy(date),
            try = as.numeric(ifelse( notes %in% c("AAH", "AAF"), try, notes)),
            notes = ifelse(notes == "AAH" | notes == "AAF", notes, NA),
            lp = as.numeric(lp),
            grade = as.numeric(str_replace(grade, ",", ".")),
            zusatz = ifelse(name == "4000", 1, NA)) %>% # Seminare, ESP, ASQ, Bachelorarbeit und Zusatzfächer
    fill(zusatz, .direction = "down") %>%
    filter( is.na(zusatz) ) %>%
    select(-zusatz) %>%
    filter( !is.na(date)) %>%
    filter( type2 != "Geburtsdatum:") %>%
    # choose module only
    filter( type == "MO") %>%
    mutate(
      modul_code = str_extract(name, "[[:digit:]]+"),
      #modul_name = str_extract(name, "[[:alpha:]].*")
      modul_name = str.right.of(trimws(name), " ") %>% trimws()
    )
  return(module)
}

