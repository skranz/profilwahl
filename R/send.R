sendbtn.click = function(pw=app$pw,..., app=getApp(), db=get.pwdb()) {
  restore.point("sendbtn.click")
  check = check.pw.for.step3()
  if (!check$ok) {
    timedMessage("msg3UI", check$msg)
    return()
  }

  pw$pwid = random.string()
  pw$email = app$email
  pw$create_time = Sys.time()
  pw$status = "o"
  pw$num_profile = length(pw$profile)
  #pw$geburtstag = ""

  pr.mod.to.promod = function(pr.mod, profil_num) {
    pr.mod %>% transmute(
      pwid = pw$pwid, profil = pw$profile[profil_num],profil_num = profil_num, code=code, modulname=titel, ects=ects
    )
  }

  # Speichere in Datenbank
  dbWithTransaction(db,{
    dbInsert(db, "pw",pw, mode="insert")
    promod = pr.mod.to.promod(pw$pr1.mods,1)
    dbInsert(db,"promod", promod)
    if (pw$num_profile >= 2) {
      promod = pr.mod.to.promod(pw$pr2.mods,2)
      dbInsert(db,"promod", promod)
    }
  })


  ui = tagList(
    h4("Profilauswahl wurde versandt"),
    p("Vielen Dank, Ihre Profilauswahl wurde versandt und wird bearbeitet. Sie sollten in den nächsten Stunden eine Email an ", app$email, " mit einer Bestätigung erhalten.")
  )
  setUI("mainUI",ui)
}
