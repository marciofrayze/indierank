require 'json'

class SearchService < RackStep::Controller
  def process_request

    plate = request.params['plate']

    print plate

    ratings = Ranking.all(plate: plate)

    ratings = [] if ratings == nil

    ranking = {}

    ranking['plate'] = plate   
    ranking['ratings'] = ratings

    response.header['Access-Control-Allow-Credentials'] = 'true'
    response.header['Access-Control-Allow-Origin'] = '*'
    response.header['Access-Control-Allow-Headers'] = 'origin, content-type, accept, authorization, bearer'
    response.header['Access-Control-Allow-Methods'] = 'GET, PUT, DELETE, HEAD, OPTIONS'

    response.body = ranking.to_json
  end
end



class AddService < RackStep::Controller
  def process_request

    # Input parameters
    plate = request.params['plate']
    score = request.params['score']
    comment = request.params['comment']

    # Just for log
    print plate
    print score
    print comment

    # Creating model and saving data
    ranking = Ranking.new(plate: plate, score: score, comment: comment)
    ranking.save!

    # Return
    response_body = {}

    response_body['plate'] = plate
    response_body['score'] = Integer(score)
    response_body['comment'] = comment

    response.header['Access-Control-Allow-Credentials'] = 'true'
    response.header['Access-Control-Allow-Origin'] = '*'
    response.header['Access-Control-Allow-Headers'] = 'origin, content-type, accept, authorization, bearer'
    response.header['Access-Control-Allow-Methods'] = 'GET, PUT, DELETE, HEAD, OPTIONS'

    response.body = response_body.to_json
  end
end

class AllService < RackStep::Controller
  def process_request

    response_body = Ranking.all

    response.header['Access-Control-Allow-Credentials'] = 'true'
    response.header['Access-Control-Allow-Origin'] = '*'
    response.header['Access-Control-Allow-Headers'] = 'origin, content-type, accept, authorization, bearer'
    response.header['Access-Control-Allow-Methods'] = 'GET, PUT, DELETE, HEAD, OPTIONS'

    response.body = response_body.to_json
  end
end
