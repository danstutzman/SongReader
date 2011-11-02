require 'rubygems'
require 'rest-client'

#HOST='http://www.songreader.net'
HOST='http://0.0.0.0:3000'
RestClient.post("#{HOST}/photos",
  { 'photo[uploaded_picture]'.intern =>
    File.new("/Users/dstutzman/Desktop/Paula.jpeg", 'rb'),
    'photo[uploaded_mp3]'.intern =>
    File.new('output/mp3/photo3a.mp3', 'rb')
  }
)
