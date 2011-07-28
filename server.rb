require 'rubygems'
require 'bundler'
Bundler.setup

require 'sinatra'

get '/' do
  send_file 'index.html'
end
get '/photo.jpeg' do
  send_file 'photo.jpeg'
end
get '/boxes.json' do
  content_type :json
  send_file 'boxes.json'
end
get '/json2.js' do
  send_file 'json2.js'
end
