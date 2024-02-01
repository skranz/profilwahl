get.pwdb = function(db.dir=getwd(), db.name="pwdb.sqlite", app=getApp()) {
  db = app$glob$pwdb

  if (is.null(db)) db = getOption("pwdb.connection")

  if (!is.null(db)) {
    if (!dbIsValid(db)) db = NULL
  }

  if (is.null(db)) {
    db = dbConnect(RSQLite::SQLite(),file.path(db.dir, db.name))
    schema.file = system.file("schema/pwdb.yaml",package = "profilwahl")
    schemas = dbmisc::load.and.init.schemas(schema.file)
    db = set.db.schemas(db, schemas)
    options(pwdb.connection = db)
  }
  db
}


# Get stukodb without schemas
get.stukodb = function(db.dir=getwd(), db.name="stukodb.sqlite", app=getApp()) {
  restore.point("get.stukodb")
  db = app$glob$stukodb

  if (is.null(db)) db = getOption("stuko.db.connection")

  if (!is.null(db)) {
    if (!dbIsValid(db)) db = NULL
  }

  if (is.null(db)) {
    db = dbConnect(RSQLite::SQLite(),file.path(db.dir, db.name))
    #db = set.db.schemas(db, schemas)
    options(stukodb.connection = db)
  }
  db
}
