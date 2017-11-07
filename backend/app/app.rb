require 'rackstep'
require_relative 'controllers/fake_service'

# App routes
class App < RackStep::App
  add_route('GET', '/', FakeSearchService)

  add_route('POST', '/add', FakeAddService) 
end
