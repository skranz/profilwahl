
timedMessage = function (id, msg = "", html = msg, ui = HTML(html), millis = 3000,empty.msg = "", empty.ui = HTML(empty.msg), app = getApp()) {
    restore.point("timedMessage")
    try({
        setUI(id, ui)
        dsetUI(id, ui)
    })
    obs.id = paste0("..timedMessage..", id)
    flag.id = paste0("flag", obs.id)
    app[[flag.id]] = FALSE
    if (!is.null(app[[obs.id]]))
        try(app[[obs.id]]$destroy())
    if (!is.finite(millis))
        return()
    app[[obs.id]] = observe({
        if (!isTRUE(app[[flag.id]])) {
            app[[flag.id]] = TRUE
            invalidateLater(millis)
            return()
        }
        try(app[[obs.id]]$destroy())
        try({
            setUI(id, empty.ui)
            dsetUI(id, empty.ui)
        })
    })
}

simpleButtonVector = function(id, label="",icon=NULL, size=c("default","sm","xs")[1], class=paste0("btn btn-default action-button", if (size != "default") paste0(" btn-",size)), extra.class = "", extra.head="") {
  if (is.null(icon)) {
    icon=""
  }
  paste0('<button id="',id,'" type="button" class="',class, ' ',extra.class,'" ',extra.head,'>',icon,label,'</button>')
}


paste.df.cols = function (mat, cols = 1:NCOL(mat),sep="", empty.sep=FALSE, ...) {
  restore.point("paste.df.cols")
  if (NROW(cols) == 2) {
    if (empty.sep) {
      sep1 = ifelse(!empty.sep | nchar(mat[[cols[1]]])>0 | nchar(mat[[cols[2]]])>0, sep,"")
      return(paste0(mat[[cols[1]]],sep1, mat[[cols[2]]], ...))
    } else {
      return(paste(mat[[cols[1]]],mat[[cols[2]]],sep=sep, ...))
    }
  } else if (NROW(cols) == 3) {
    if (empty.sep) {
      sep1 = ifelse(!empty.sep | nchar(mat[[cols[1]]])>0 | nchar(mat[[cols[2]]])>0, sep,"")
      sep2 = ifelse(!empty.sep | nchar(mat[[cols[2]]])>0 | nchar(mat[[cols[3]]])>0, sep,"")
      return(paste0(mat[[cols[1]]],sep1, mat[[cols[2]]],sep2, mat[[cols[3]]],
          ...))
    } else {
      return(paste(mat[[cols[1]]],mat[[cols[2]]],mat[[cols[3]]],sep=sep, ...))
    }
  } else {
      if (is.character(cols))
        cols = match(cols, colnames(mat))
      code = paste("mat[[", cols, "]]", collapse = ",sep,")
      code = paste("paste0(", code, ",...)", sep = "")
      return(eval(parse(text = code)))
  }
}
