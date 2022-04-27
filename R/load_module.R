load.all.module = function(from.sem=NA, to.sem=NA, db = get.stukodb(), glob=getApp()$glob, all.prof = glob$all.prof) {
  restore.point("load.all.module")
  mods = dbGet(db,"modul")
  if (!is.na(from.sem))
    mods = mods[mods$semester >= from.sem,]
  if (!is.na(to.sem))
    mods = mods[mods$semester <= to.sem,]

  mopr = dbGet(db, "modulschwerpunkt")
  mopr$bama = tolower(substring(mopr$schwerpunkt,1,2))
  mopr$profil = tolower(substring(mopr$schwerpunkt,4))

  # Benutze nur Profile die in all.prof angegeben sind
  mopr = semi_join(mopr, all.prof, by = c("profil","bama"))
  #mopr = left_join(mopr, select(all.prof,profil, bama, profil_label), by =c("profil","bama"))

  mopr = semi_join(mopr, mods, by=c("semester","modulid"))
  mods = semi_join(mods, mopr, by=c("semester","modulid"))

  mopr = mopr %>%
    group_by(modulid, bama, profil) %>%
    summarize(
      last.sem.profil = max(semester)
    ) %>%
    ungroup()

  mods = mods %>%
    group_by(modulid, code) %>%
    summarize(
      last.sem.modul = max(semester),
      titel = last(titel),
      ects = median(ects)
    ) %>%
    ungroup()

  mopr = left_join(mopr, mods, by="modulid")

  pr.str = mopr %>%
    group_by(code, bama) %>%
    summarize(
      profile=paste0(unique(sort(profil)), collapse=", ")
    )

  mods = inner_join(mods, pr.str, by="code")

  glob$all.mods = mods
  glob$all.mopr = mopr
  invisible(list(mods=mods, mopr=mopr))
}

add.aah.to.modules = function(aah.df = glob$aah.df, glob = getApp()$glob) {
  restore.point("add.aah.to.modules")

  aah.df = aah.df %>%
    rename(titel = name) %>%
    mutate(code = paste0("A-",code), modulid=code)

  aah.mod = aah.df %>%
    group_by(code, bama, titel) %>%
    summarize(profile = paste0(profil, collapse=", "))

  all.mods = bind_rows(aah.mod, glob$all.mods)

  all.mopr = bind_rows(aah.df, glob$all.mopr)
}
