require 'json'

# Fake service for tests
class FakeService < RackStep::Controller
  def process_request
    # Creating a Hash with some info that we will return to the rating as JSON
    # (simulating a service).
    ranking = {}
    ranking['plate'] = 'ABC-1234'
    ranking['ratings'] = []

    rating = {}

    rating['comment'] = 'really bad driver'
    rating['score'] = 1

    ranking['ratings'] << rating

    response.header['Access-Control-Allow-Origin'] = '*'
    response.header['Access-Control-Allow-Headers'] = 'origin, content-type, accept, authorization, bearer'
    response.header['Access-Control-Allow-Methods'] = 'GET, OPTIONS, HEAD, POST, PUT'
    response.header['Access-Control-Max-Age'] = '1209600'

    response.body = ranking.to_json
  end
end
