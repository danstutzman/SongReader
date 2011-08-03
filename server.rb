require 'rubygems'
require 'bundler'
Bundler.setup

require 'sinatra'

get '/' do
  send_file 'index.html'
end
get '/photo1.jpeg' do
  send_file 'photo1.jpeg'
end
get '/photo2.jpeg' do
  send_file 'photo2.jpeg'
end
get '/boxes.json' do
  content_type :json
  send_file 'boxes.json'
end
get '/json2.js' do
  send_file 'json2.js'
end
get '/annotation.js' do
  send_file 'annotation.js'
end
