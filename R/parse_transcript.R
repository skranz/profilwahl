# Code zum groessten Teil von Alex uebernommen

import.tor = function(pdf.file) {
  restore.point("import.tor")
  txt = pdf_text(pdf.file)
  stud = extract_tor_stud(txt)
  module = extract_tor_module(txt)
  list(stud=stud, module=module)
}


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


