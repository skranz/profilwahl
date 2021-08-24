
# Get stukodb without schemas
get.stukodb = function(db.dir=getwd(), db.name="stukodb.sqlite", app=getApp()) {
  restore.point("get.stukodb")
  db = app$glob$db

  if (is.null(db)) db = getOption("stuko.db.connection")

  if (!is.null(db)) {
    if (!dbIsValid(db)) db = NULL
  }

  if (is.null(db)) {
    db = dbConnect(RSQLite::SQLite(),file.path(db.dir, db.name))
    #db = set.db.schemas(db, schemas)
    options(stuko.db.connection = db)
  }
  db
}
