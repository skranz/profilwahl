# Find possible profiles

find.feasible.profiles = function(pw = app$pw, app=getApp()) {
  all.prof = filter(app$glob$all.prof, bama == pw$bama)
  #all.prof$min_ects = 7

  single.pr = find.single.profiles(pw$mopr, all.prof)

  pairs = find.profile.pairs(pw$mopr, all.prof,single.pr = single.pr)

  list(single = single.pr, pairs = pairs)
}

find.single.profiles = function(mopr, all.prof) {
  restore.point("find.single.profiles")
  pr = mopr %>%
    group_by(profil, bama) %>%
    summarize(sum_ects = sum(ects)) %>%
    left_join(all.prof, by=c("profil","bama")) %>%
    filter(sum_ects >= min_ects)
  pr
}

find.profile.pairs = function(mopr, all.prof, single.pr = find.single.profiles(mopr, all.prof)) {
  restore.point("find.profile.pairs")

  min.ects = all.prof$min_ects
  names(min.ects) = all.prof$profil


  n.pr = NROW(single.pr)
  res.li = vector("list", n.pr * (n.pr-1) / 2)
  if (n.pr <= 1) return(NULL)
  pr.inds = 1:n.pr
  ind1 = 1; ind2 = 2
  res.ind = 0

  for (ind1 in 1:(n.pr-1)) {
    pr1 = single.pr$profil[ind1]
    min1 = min.ects[pr1]
    rows1 = which(mopr$profil == pr1)
    for (ind2 in (ind1+1):n.pr) {
      pr2 = single.pr$profil[ind2]
      min2 = min.ects[pr2]
      rows2 = which(mopr$profil == pr2)

      rows = union(rows1, rows2)
      sum_ects = sum(mopr$ects[rows])

      # notwendige bedingung um beide profile
      # zu wählen. Evtl. nicht hinreichend, da
      # Kurse ggf. nicht ausreichend schön
      # gesplitted werden können.
      # Wir checken hinreichende Bedingung
      # aber nicht hier.
      if (sum_ects >= min1+min2) {
        res.ind = res.ind + 1
        res.li[[res.ind]] = as_tibble(list(pr1 = pr1, pr2 = pr2, sum_ects=sum_ects, min_ects = min1+min2))
      }
    }
  }
  if (res.ind == 0) return(NULL)
  res = do.call(rbind, res.li[1:res.ind]) %>% as.data.frame()
  res2 = res
  res2$pr1 = res$pr2; res2$pr2 = res$pr1
  res = bind_rows(res, res2) #%>% arrange(pr1, pr2)
  res$label = paste0(res$pr1, " - ", res$pr2)
  res
}


has.enough.ects = function(profil, bama, pr.mods, all.prof = getApp()$glob$all.prof) {
  min_ects = all.prof$min_ects[bama == all.prof$bama & profil == all.prof$profil]

  sum(pr.mods$ects) >= min_ects

}

