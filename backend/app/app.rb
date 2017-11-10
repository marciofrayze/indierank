require 'rackstep'

require 'data_mapper'
require 'dm-sqlite-adapter'

DataMapper::Logger.new($stdout, :debug)
DataMapper.setup(:default, ENV['DATABASE_URL'] || 'sqlite::memory:')

require_relative 'models/ranking'

DataMapper.finalize
DataMapper.auto_upgrade!

require_relative 'controllers/services'

# App routes
class App < RackStep::App
  add_route('GET', '/search', SearchService)

  add_route('POST', '/add', AddService) 

  add_route('GET', '/all', AllService)
end
