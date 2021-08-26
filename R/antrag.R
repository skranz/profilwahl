make.antrag = function(file, pw=app$pw, app=getApp()) {
  restore.point("make.antrag")

  #file = "test.docx"
  tpl_file = app$glob$antrag.tpl.file
  pw$profile

  doc = read_docx(tpl_file)

  date_label = format(Sys.time(),"%d.%m.%Y")
  # Change bookmarks
  doc = doc %>%
    body_replace_text_at_bkm("date_label", date_label) %>%
    body_replace_text_at_bkm("studname",pw$studname) %>%
    body_replace_text_at_bkm("matnr",as.character(pw$matnr))

  profil = pw[["profil"]]
  if (length(profil)>=1) {
    prof = filter(app$glob$all.prof, bama==pw$bama, profil==pw$pr1)
    doc = add_antrag_profil(doc, prof, pw$pr1.mods,1)
  }
  if (length(profil)>=2) {
    prof = filter(app$glob$all.prof, bama==pw$bama, profil==pw$pr2)
    doc = add_antrag_profil(doc, prof, pw$pr2.mods,2)
  }


  print(doc, target = file)

  file
}

add_antrag_profil = function(doc, prof, mods, num=1) {
  restore.point("add_antrag_profil")
  tab = mods %>%
    select(Modulcode=code, LP=ects, Modulname=titel)

  doc = doc %>%
    body_add_par(paste0("Profil ",num,": ",prof$profil_label), style = "heading 1") %>%
    body_add_par(" ") %>%
    body_add_table(tab, style="Plain Table 1")
  doc
}
