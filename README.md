# profilwahl

Eine Shiny-App zur Wahl von Profilen fuer BA/MA Wiwi Uni ulm

- Fuer ein Beispiel lasse den Code in `example` in `pw_app.R` laufen und passe die Pfade ggf. an.
  - Beispiele der Dateien `profile.csv` and `profilwahl_antrag_tpl.docs` koennen hier gefunden werden `inst/example_material`


### TO DO:

- Im Moment werden alle Module aus der Stuko Datenbank erlaubt, die in irgendeineinem Semester (auch zukuenftigen) Teil des Profils sind. Wir sollten eine sinnvolle Restriktion ueberlegen und implementieren.

- Der Antrag sieht noch nicht schön aus und kann sicherlich verbessert werden. Der relevant Code ist in `antrag.R`.

- Wenn man eine Email senden möchte, bräuchte man ein Login-System. Bei freier Eingabe einer Emailadresse ohne Login, könnte die App missbraucht werden um Spam zu versenden.

- Die App wird keine 3 Profile handhaben koennen. Fuer diesen Fall sollten Studis ein Profil manuell anlegen. Die Tabelle mit Modulen und Profilen ist aber ggf. hilfreich.
