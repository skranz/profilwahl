
pw.login.module = function(..., app=getApp()) {
  loginModule(container.id = "mainUI", ...,smtp=app$glob$mail.config, login.fun = pw.login.fun)
}

pw.login.fun = function(userid,..., app=getApp()) {
  restore.point("pw.login.fun")
  glob = app$glob

  app$email = userid
  app$pw$email = userid

  setUI("mainUI", pw.ui())
  set.all.ui()
}
