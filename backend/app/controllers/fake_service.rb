require 'json'

# Fake service for tests
class FakeService < RackStep::Controller
  def process_request
    # Creating a Hash with some info that we will return to the rating as JSON
    # (simulating a service).
    ranking = {}
    ranking['plate'] = 'ABC-1234'
    ranking['ratings'] = []

    rating1 = {}

    rating1['comment'] = 'really bad driver'
    rating1['score'] = 1

    rating2 = {}    
    rating2['comment'] = 'Fine driver. No problems.'
    rating2['score'] = 5

    ranking['ratings'] << rating1
    ranking['ratings'] << rating2

    response.header['Access-Control-Allow-Credentials'] = 'true'
    response.header['Access-Control-Allow-Origin'] = '*'
    response.header['Access-Control-Allow-Headers'] = 'origin, content-type, accept, authorization, bearer'
    response.header['Access-Control-Allow-Methods'] = 'GET, PUT, DELETE, HEAD, OPTIONS'

    #response.header['Access-Control-Max-Age'] = '1209600'

    response.body = ranking.to_json
  end
end
