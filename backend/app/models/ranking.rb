class Ranking
  include DataMapper::Resource

  property :id,         Serial
  property :plate,      String,     required: true,     length: 8
  property :score,      Integer,    required: true
  property :comment,    String,     required: true,     length: 255

end