
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
  pw$geburtstag = stud$geburtstag

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

  # Filtere nur bestandene Module
  tor.mods = tor.mods %>% filter(state=="BE")


  all.mods = app$glob$all.mods %>%
    filter(bama == pw$bama)

  pmods = semi_join(tor.mods, all.mods, by="code")
  pw$mods = all.mods %>% semi_join(pmods, by="code")

  pw$mopr = semi_join(app$glob$all.mopr, pw$mods, by=c("code","bama"))

  pw$other.mods = anti_join(tor.mods, all.mods, by="code") %>%
    rename(titel = modul_name)

  pw = add.aah.modules.to.pw(tor, pw)

  list(ok=TRUE,pw=pw)
}

# Written on 2023-01-17
# Add AAH modules mapped by module name
add.aah.modules.to.pw = function(tor, pw, aah.df = getApp()$glob$aah.df) {
  restore.point("add.aah.modules.to.pw")

  # No aah modules defined
  if (NROW(aah.df)==0) return(pw)

  # We match aah modules by name, not by code!
  tor.aah = tor$aah %>% filter(state=="BE")

  # Student has no aah modules
  if (NROW(tor.aah)==0) return(pw)


  # Match aah modules by name (not code) in lowercase letters
  tor.aah$lname = tolower(tor.aah$name)
  aah.df$lname = tolower(aah.df$name)
  paah = left_join(tor.aah, aah.df, by="lname", suffix=c("",".y")) %>%
    filter(!is.na(profil))



  # No matched aah module found
  if (NROW(paah)==0) return(pw)

  aah.mopr = paah %>%
    transmute(modulid = paste0("AAH_", code), bama=bama, profil=profil,last.sem.modul=NA, code=code,  titel = name, ects=lp)

  aah.mods = aah.mopr %>%
    group_by(modulid, code, last.sem.modul, titel, ects, bama) %>%
    summarize(
      profile = paste0(profil,collapse=", ")
    ) %>%
    ungroup()


  pw$mods = bind_rows(pw$mods, aah.mods)
  pw$mopr = bind_rows(pw$mopr, aah.mopr)

  no.paah = anti_join(tor.aah, aah.df, by="lname")
  other.aah = no.paah %>%
    select(code, titel = name, type, type2, date, grade, state, lp, notes)
  pw$other.mods = bind_rows(pw$other.mods, other.aah)
  pw

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
  name <- x[3,1]

  row = which(startsWith(x[,3], "Matrikelnummer:"))
  matrikelnummer = parse_number(x[row,3])

  row = which(startsWith(x[,1], "Abschluss:"))
  degree = x[row,2]
  #geburtstag <- as_character(dmy(x[19,4]))
  geburtstag <- dmy(x[19,4])
  street <- x[4,1]
  place <- x[6,1]
  fspo <- x[18,3] %>% parse_number()
  semester <- substr(x[20,2], start = 1, stop = 2) %>% parse_number()

  studentinfo <- tibble(name, matrikelnummer, degree, geburtstag, street, place, fspo, semester)

  #studentinfo <- tibble(name, matrikelnummer,degree, geburtstag)

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
extract_tor_aah <- function(tor,...){
  restore.point("extract_tor_aah")
  df = tor_transcript_df(tor) %>%
    filter(has.substr(notes,"AAH"))
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

