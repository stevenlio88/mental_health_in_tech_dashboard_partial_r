# Mental Health In Tech Dashboard R (Partial)

This repo contains the necessary files for deploying a R version of the Mental Health In Tech Dashboard (Partial) app to Heroku.
The deployed app is hosted [here](https://dsci532-testdashboard-r.herokuapp.com/).

Steps to reproduce:

1. `git clone ...`
2. `cd mental_health_in_tech_dashboard_partial_r`
3. `heroku create --stack container your-heroku-site`
4. `git push heroku main`
5. Wait ~15 min for the build to finish.
6. `heroku ps:scale web=1`
7. Navigate to `https://you-heroku-site.herokuapp.com` in your browser
