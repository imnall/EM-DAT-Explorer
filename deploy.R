# Charger la librairie
library(rsconnect)

# Réessayer de configurer le compte
rsconnect::setAccountInfo(
  name='####',    # à completer
  token='####',   # à completer
  secret='####'  # à completer
)

rsconnect::deployApp(
  appFiles = c(
    'global.R',
    'app.R',
    'server.R',
    'ui.R',
    'data/data_cleaned_final.rds',
    'www/style.css'
  )
)
