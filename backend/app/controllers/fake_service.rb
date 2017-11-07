require 'json'

# Fake service for tests
class FakeSearchService < RackStep::Controller
  def process_request
    # Creating a Hash with some info that we will return to the rating as JSON
    # (simulating a service).

    plate = request.params['plate']

    ranking = {}

    print plate

    ranking['plate'] = plate
    ranking['ratings'] = []

    if plate.eql? 'AAA-1111'
      rating1 = {}

      rating1['comment'] = 'really bad driver'
      rating1['score'] = 1

      rating2 = {}
      rating2['comment'] = 'Fine driver. No problems.'
      rating2['score'] = 5

      ranking['ratings'] << rating1
      ranking['ratings'] << rating2
    end

    response.header['Access-Control-Allow-Credentials'] = 'true'
    response.header['Access-Control-Allow-Origin'] = '*'
    response.header['Access-Control-Allow-Headers'] = 'origin, content-type, accept, authorization, bearer'
    response.header['Access-Control-Allow-Methods'] = 'GET, PUT, DELETE, HEAD, OPTIONS'

    #response.header['Access-Control-Max-Age'] = '1209600'

    response.body = ranking.to_json
  end
end



class FakeAddService < RackStep::Controller
  def process_request

    plate = request.params['plate']
    score = request.params['score']
    description = request.params['comment']

    review = {}

    print plate
    print score
    print description

    review['plate'] = plate
    review['score'] = Integer(score)
    review['comment'] = description

    response.header['Access-Control-Allow-Credentials'] = 'true'
    response.header['Access-Control-Allow-Origin'] = '*'
    response.header['Access-Control-Allow-Headers'] = 'origin, content-type, accept, authorization, bearer'
    response.header['Access-Control-Allow-Methods'] = 'GET, PUT, DELETE, HEAD, OPTIONS'

    #response.header['Access-Control-Max-Age'] = '1209600'

    response.body = review.to_json
  end
end
