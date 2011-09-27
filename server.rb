require 'rubygems'
require 'bundler'
Bundler.setup

require 'sinatra'

get '/' do
  send_file 'index.html'
end
get '/slider.html' do
  send_file 'slider.html'
end

get '/boxes1.json' do
  content_type :json
  send_file 'boxes1.json'
end
get '/boxes2.json' do
  content_type :json
  send_file 'boxes2.json'
end
get '/boxes3.json' do
  content_type :json
  send_file 'boxes3.json'
end

get '/json2.js' do
  send_file 'json2.js'
end
get '/annotation.js' do
  send_file 'annotation.js'
end

get '/input/:case_name.jpeg' do
  send_file "input/#{params['case_name']}.jpeg"
end
get '/input/:case_name.json' do
  content_type :json
  send_file "input/#{params['case_name']}.json"
end
