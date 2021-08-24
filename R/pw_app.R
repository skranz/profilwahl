example = function() {
  restore.point.options(display.restore.point = TRUE)
  setwd("c:/libraries/stuko")
  app = pwApp()
  viewApp(app, launch.browser = TRUE)
}

pwApp = function(stuko.db.dir=getwd(), prof.csv = "profile.csv", antrag.tpl.file = "profilwahl_antrag_tpl.docx") {
  restore.point("pwApp")
  app = eventsApp()

  app$glob$antrag.tpl.file = antrag.tpl.file
  app$glob$stuko.db = get.stukodb(stuko.db.dir)
  app$glob$all.prof = read.csv(prof.csv,encoding = "UTF-8", strip.white = TRUE)


  load.all.module(glob = app$glob)
  app$pw = empty.pw()
  app$ui = pw.ui()
  set.all.ui()
  appInitHandler(function(..., app) {
    pw.init.handlers()
  })
  app
}

empty.pw = function() {
  list(bama = NA, studname=NA, matnr=NA, profil=NULL)
}

pw.ui = function() {
  ui = fluidPage(
    shiny::includeCSS(system.file("www/profilwahl.css",package="profilwahl")),
    title="Profile festlegen (WiWi Uni Ulm)",
    navlistPanel(id="pwPanel",
    tabPanel("Schritt 1: Transkript einlesen",value="step1", step1_ui()),
    tabPanel("Schritt 2: Profile festlegen",value="step2",  uiOutput("step2UI")),
    tabPanel("Schritt 3: Antrag herunterladen",value="step3", uiOutput("step3UI")),
    widths = c(2,10)
  ))
  ui
}

pw.init.handlers = function() {
  buttonHandler("nextbtn1", function(...) show.step(2))
  buttonHandler("nextbtn2", function(...) show.step(3))

  buttonHandler("prevbtn2", function(...) show.step(1))
  buttonHandler("prevbtn3", function(...) show.step(2))

  setFileInputHandler("tor_file", load.tor.handler)
  selectChangeHandler("profil", profil.select.handler)

  classEventHandler("btnpr1",event = "click", function(id,...) {
    move_modul(from=1, id)
  })
  classEventHandler("btnpr2",event = "click", function(id,...) {
    move_modul(from=2, id)
  })

  setDownloadHandler("downloadBtn",filename="Profilantrag.docx", content = function(file) {
    make.antrag(file)
  })
  downloadHandler()
}

# Bewege ein Modul von Profil 1 zu Profil 2 oder anders herum.
move_modul = function(from, id, app=getApp()) {
  pw = app$pw
  restore.point("move_modul")
  ind = as.integer(str.right.of(id,"_"))
  if (from == 1) {
    pw$pr2.mods = bind_rows(pw$pr2.mods, pw$pr1.mods[ind,]) %>%
      arrange(titel)
    pw$pr1.mods = pw$pr1.mods[-ind,]
  } else {
    pw$pr1.mods = bind_rows(pw$pr1.mods, pw$pr2.mods[ind,]) %>%
      arrange(titel)
    pw$pr2.mods = pw$pr2.mods[-ind,]
  }
  app$pw = pw
  show.pair.profiles()
  setUI("step3UI", step3_ui())
}

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

# Lade ein Transcript of Records (tor)
load.tor.handler = function(file=NULL, ..., app=getApp()) {
  restore.point("load.transcript.handler")
  if (is.null(file)) return()
  cat("\nA transcript is loaded: ")
  fpath = file$datapath
  if (is.null(fpath) | !file.exists(fpath)) return()
  app$tor = import.tor(fpath)
  restore.point("load.transcript.handler2")
  res = tor.to.pw(app$tor)
  if (!res$ok) {
    html = c("<h3>Fehler beim importieren der Kurse</h3>", res$msg)
    setUI("torUI", HTML(html))
    return()
  }
  pw = app$pw = res$pw


  mods.df = pw$mods %>%
    select(Code=code, LP=ects, Name=titel, Profile=profile)

  mods.html = simple_html_table("mods_tab", mods.df, extra = " class = 'modultable'")

  other.mods.df =  pw$other.mods %>%
    select(Code=code, Name=titel)

  other.mods.html = simple_html_table("other_mods_tab", other.mods.df, extra = " class = 'modultable'")

  ui = tagList(
    h3("Eingelesene Daten"),
    p("Name: ", pw$studname),
    p("Matrikelnummer:", pw$matnr),
    p("Studiengang:", ifelse(pw$bama=="ba","Bachelor WiWi", "Master WiWi")),
    h4("Module die mind. einem Profil zugeordnet sind: "),
    HTML(mods.html),
    h4("Andere Module (kein Profil):"),
    other.mods.html,
    br()
  )
  setUI("torUI", ui)

  app$pw$feas = find.feasible.profiles()
  setUI("step3UI", step3_ui())
  setUI("step2UI", step2_ui())
}

# Nutzer hat ein Profil oder ein Profilpaar ausgewaehlt.
profil.select.handler = function(value, ..., app=getApp)  {
  restore.point("profil.select.handler")
  ind = as.integer(value)
  # Kein Profil ausgewählt
  if (ind == 0) {
    app$pw$profil = NULL
    setUI("profilUI",HTML(""))
    setUI("step3UI", step3_ui())
    return()
  }

  pw = app$pw
  count.single = NROW(pw$feas$single)

  # Ein einziges Profil
  if (ind <= count.single) {
    pw$profil = pw$pr1 = pw$feas$single$profil[ind]
    pw$pr1.mods = filter(pw$mopr, profil == pw$pr1) %>% arrange(titel)

    prof.mods.df = pw$pr1.mods %>%
      select(Code=code, LP=ects, Name=titel)
    tab.html = simple_html_table("prof_mods_table", prof.mods.df, extra = " class = 'modultable table-striped table-sm'")

    prof = filter(app$glob$all.prof, bama==pw$bama, profil==pw$pr1)

    sum_ects = sum(pw$pr1.mods$ects)
    ui = tagList(
      h4(paste0("Profil (", pw$profil, "): ",prof$profil_label, " (", sum_ects, " von min. ", prof$min_ects," LP)")),
      HTML(tab.html)
    )
    app$pw = pw
    setUI("profilUI",ui)
    setUI("step3UI", step3_ui())
    return()
  }

  # Zwei Profile
  ind = ind-count.single
  pw$pr1 = pw$feas$pair$pr1[ind]
  pw$pr2 = pw$feas$pair$pr2[ind]

  pw$profil = c(pw$pr1, pw$pr2)


  prof.mopr = filter(pw$mopr, profil == pw$pr1 | profil == pw$pr2)

  prof.mods = prof.mopr %>%
    group_by(code, ects, titel) %>%
    summarize(
      can1 = any(profil == pw$pr1),
      can2 = any(profil == pw$pr2),
      fixed = (can1+can2 == 1)
    ) %>%
    arrange(titel) %>%
    ungroup()

  pw$pr1.mods = filter(prof.mods, can1==TRUE)
  pw$pr2.mods = filter(prof.mods, can1==FALSE)
  pw$all.fixed = all(prof.mods$fixed)

  app$pw = pw

  setUI("step3UI", step3_ui())
  show.pair.profiles()

}

# Zeige Modultabellen bei der Auswahl von 2 Profilen
show.pair.profiles = function(pw=app$pw, app=getApp()) {
  restore.point("show.pair.profiles")

  btns1 = simpleButtonVector(id = paste0("btnpr1_", seq_len(NROW(pw$pr1.mods))),"Zu Profil 2", class="btnpr1")
  pr1.df = pw$pr1.mods %>%
      mutate(btn = ifelse(!fixed, btns1,"")) %>%
      select(btn, Code=code, LP=ects, Name=titel)


  tab1.html = simple_html_table("pr1_
                                mod_table", pr1.df, extra = " class = 'modultable table-striped table-sm'",header = c("","Code","LP","Name"))

  btns2 = simpleButtonVector(id = paste0("btnpr2_", seq_len(NROW(pw$pr2.mods))),"Zu Profil 1", class="btnpr2")
  pr2.df = pw$pr2.mods %>%
      mutate(btn = ifelse(fixed, "", btns2)) %>%
      select(btn, Code=code, LP=ects, Name=titel)
  tab2.html = simple_html_table("pr2_mod_table", pr2.df, extra = " class = 'modultable table-striped table-sm'",header = c("","Code","LP","Name"))



  ects1 = sum(pw$pr1.mods$ects)
  ects2 = sum(pw$pr2.mods$ects)

  prof1 = filter(app$glob$all.prof, bama==pw$bama, profil==pw$pr1)
  prof2 = filter(app$glob$all.prof, bama==pw$bama, profil==pw$pr2)

  ui = tagList(
    if (!pw$all.fixed) {
      helpText("Module mit einem Knopf 'Zu Profil 1' oder 'Zu Profil 2' können in das jeweilige andere Profil geschoben werden. Sie muessen am Ende eine Aufteilung haben, welche die minimale Anzahl an Leistungspunkten in beiden Profilen erfüllt. Dies ist nicht immer möglich.")
    },
    h4(paste0("Profil 1 (", pw$pr1, "): ",prof1$profil_label, " (", ects1, " von min. ", prof1$min_ects," LP)")),
    HTML(tab1.html),
    h4(paste0("Profil 2 (", pw$pr2, "): ",prof2$profil_label, " (", ects2, " von min. ", prof2$min_ects," LP)")),
    HTML(tab2.html),
    br()
  )
  setUI("profilUI", ui)

}

setFileInputHandler = function(id,fun, app=getApp()) {
  restore.point("setFileInputHandler")
  observe(fun(app$input[[id]]))
}

show.step = function(step = 1, app=getApp()) {
  panel = paste0("step", step)
  updateNavlistPanel(app$session, "pwPanel", panel)
}

set.all.ui = function(pw = app$pw, app = getApp()) {
  setUI("step2UI",step2_ui(pw))
  setUI("step3UI",step3_ui(pw))
}

step1_ui = function() {
  tagList(
    helpText("Laden Sie bitte Ihr Kurstranskript als PDF Datei hier hoch"),
    fileInput("tor_file", "Kurstransskript (PDF)",
                multiple = FALSE,
                accept = c(".pdf")),
    uiOutput("torUI"),
    simpleButton("nextbtn1","Weiter")
  )
}

step2_ui = function(pw = app$pw, app=getApp()) {
  has.feas = NROW(pw$feas$single) > 0


  if (!has.feas) {
    main = tagList(
      h3("Profile festlegen:"),
      p("Leider haben Sie noch nicht genügend passende Leistungspunkte für ein Profil.")
    )
  } else {
    lab = c("Keine Auswahl", pw$feas$single$profil, pw$feas$pair$label)
    choices = 0:length(lab)
    names(choices) = lab

    main = tagList(
      h3("Profil Auswahl"),
      selectInput("profil", label="Wählen Sie ein Profil (oder ein Paar) aus:",choices = choices),
      uiOutput("profilUI")
    )
  }
  tagList(
    main,
    simpleButton("prevbtn2","Zurück"),
    simpleButton("nextbtn2","Weiter")
  )
}


step3_ui = function(pw = app$pw, app=getApp()) {
  res = check.pw.for.step3(pw)
  if (!res$ok) {
    return(tagList(
      h4("Profilwahl kann noch nicht abgeschlossen werden"),
      p(res$msg),
      br(),
      simpleButton("prevbtn3","Zurück")
    ))
  }
  tagList(
    h3("Antrag erstellen"),
    uiOutput("msg3UI"),
    downloadButton("downloadBtn","Antrag erstellen und herunterladen"),
    br(),br(),
    simpleButton("prevbtn3","Zurück"),
  )
}

check.pw.for.step3 = function(pw = app$pw, app=getApp()) {
  restore.point("check.pw.for.step3")
  profil = pw[["profil"]]
  if (length(profil)<1) {
    return(list(ok=FALSE, msg="Wählen Sie bitte zunächst ein oder zwei Profile"))
  }
  if (length(profil)>=2) {
    # Check Kreditpunkte
    enough1 = has.enough.ects(pw$pr1,bama = pw$bama,pr.mods = pw$pr1.mods)
    if (!enough1) {
      return(list(ok=FALSE, msg = "Ihre Module in Profil 1 haben nicht genügend LP."))

    }
    enough2 = has.enough.ects(pw$pr2,bama = pw$bama,pr.mods = pw$pr2.mods)
    if (!enough2) {
      return(list(ok=FALSE, msg = "Ihre Module in Profil 2 haben nicht genügend LP."))
    }
  }
  return(list(ok=TRUE))

}
