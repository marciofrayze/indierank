# Configuration file used when you deploy on Heroku.
# This was created based on the informations provided at: https://devcenter.heroku.com/articles/deploying-rails-applications-with-the-puma-web-server

workers Integer(ENV['WEB_CONCURRENCY'] || 2)
threads_count = 7
threads threads_count, threads_count

preload_app!

rackup      DefaultRackup
port        ENV['PORT']     || 3000
environment ENV['RACK_ENV'] || 'development'

