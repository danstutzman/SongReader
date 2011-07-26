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
