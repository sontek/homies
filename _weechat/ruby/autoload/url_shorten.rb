# Copyright (c) 2008, Daniel Bretoi <daniel@bretoi.com>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY Daniel Bretoi ''AS IS'' AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL <copyright holder> BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# * Simpler, more robust version of tinyurl (doesn't rely on html output)
# * Echo's urls from all channels that are over the plugin's maxlen value
#     in a shortened format.
# * allows for manual shortening of urls
# * set config variable 'shortener' to one of:
#
#	Value		Service used
#	-----		------------
#	qurl	http://qurl.com/
#	tinyurl	http://tinyurl.com/
#	isgd	http://is.gd/
#	trim	http://tr.im/
#	bitly	http://bit.ly/
#
#   Note: attempting to use the bitly shortener without setting the
#	  bitly_login and bitly_key config variables will fail.
#
#   Note: 'trim' and 'bitly' shorteners require the 'rubygems' and
#	  'json' ruby modules.
#
# 2009-12-12, FlashCode <flashcode@flashtux.org>
#     version 1.5: fix wrong license in register()
# 2009-11-29, penryu <penryu@gmail.com>
#     version 1.4: fixed URI encoding bug, added shorteners
#		   changed default shortener, as qurl is _slow_
# 2009-11-29, penryu <penryu@gmail.com>
#     version 1.3: add bit.ly shortener routine
#		   add ability to choose shortener from config
# 2009-05-02, FlashCode <flashcode@flashtux.org>:
#     version 1.2: sync with last API changes
# 2008-11-11, FlashCode <flashcode@flashtux.org>:
#     version 1.1: conversion to WeeChat 0.3.0+

require 'net/http'
require 'uri'

SCRIPT_NAME = 'url_shorten'
SCRIPT_AUTHOR = 'Daniel Bretoi <daniel@bretoi.com>'
SCRIPT_DESC = 'Shorten url'
SCRIPT_VERSION = '1.5'
SCRIPT_LICENSE = 'BSD'

DEFAULTS = {
  'maxlen'      => '50',
  'color'       => 'red',
  'shortener'   => 'tinyurl',
  'bitly_login' => '',
  'bitly_key'   => '',
}


def weechat_init
  Weechat.register SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE, SCRIPT_DESC, "", ""
  Weechat.hook_command SCRIPT_NAME, SCRIPT_DESC, "url", "url: url to shorten", "", SCRIPT_NAME, ""
  #Weechat.hook_signal "*,irc_in2_privmsg", "msg_shorten", ""
  Weechat.hook_print "", "", "://", 1, "msg_shorten", ""
  DEFAULTS.each_pair { |option, def_value|
    cur_value = Weechat.config_get_plugin(option)
    if cur_value.nil? || cur_value.empty?
      Weechat.config_set_plugin(option, def_value)
    end
  }

  if Weechat.config_get_plugin("shortener").eql?('bitly')
    cfg_bitly_login = Weechat.config_get_plugin("bitly_login")
    cfg_bitly_key   = Weechat.config_get_plugin("bitly_key")
    if cfg_bitly_login.empty? || cfg_bitly_key.empty?
      yellow = Weechat.color("yellow")
      Weechat.print("", "#{yellow}WARNING: The bit.ly shortener requires a valid API login and key.")
      Weechat.print("", "#{yellow}WARNING: Please configure the `bitly_login' and `bitly_key' options before using this script.")
    end
  end

  return Weechat::WEECHAT_RC_OK
end

def fetch(uri_str, limit = 10)
  raise ArgumentError, 'HTTP redirect too deep' if limit == 0
  
  response = Net::HTTP.get_response(URI.parse(uri_str))
  case response
  when Net::HTTPSuccess     then response.body
  when Net::HTTPRedirection then fetch(response['location'], limit - 1)
  else
    response.error!
  end
end

def qurl_shorten(url)
  fetch('http://www.qurl.com/automate.php?url=' + URI.encode(url)).gsub('www.','')
end

def tinyurl_shorten(url)
  fetch('http://tinyurl.com/api-create.php?url=' + URI.encode(url))
end

def isgd_shorten(url)
  fetch('http://is.gd/api.php?longurl=' + URI.encode(url))
end

def trim_shorten(url)
  require 'rubygems'
  require 'json'

  params = ['url=' + URI.encode(url)]
  params << 'newtrim=1'
  #params << 'sandbox=1'  # comment out for release
  json = fetch('http://api.tr.im/v1/trim_url.json?' + params.join('&'))
  data = JSON.parse(json)

  if data['status']['result'].eql?('OK')
    begin
      data['url']
    rescue NoMethodError => ex
      "Failure parsing tr.im result: #{ex}"
    end
  else
    data['status']['message']
  end
end

def bitly_shorten(url)
  require 'rubygems'
  require 'json'

  params = ['longUrl=' + URI.encode(url)]
  params << 'login=' + Weechat.config_get_plugin('bitly_login')
  params << 'apiKey=' + Weechat.config_get_plugin('bitly_key')
  api_url = 'http://api.bit.ly/shorten?version=2.0.1&' + params.join('&')

  begin
    url_data = JSON.parse(fetch(api_url))
  rescue Exception => ex
    return "Failure shortening url: " + ex.to_s
  end

  if url_data['statusCode'].eql?('OK')
    begin
      res = url_data['results']
      res[res.keys[0]]['shortUrl']
    rescue NoMethodError => ex
      "Failure parsing bitly result: #{ex}"
    end
  else
    url_data['errorMessage']
  end
end

def shortener(url)
  service = Weechat.config_get_plugin('shortener').tr('.','')
  begin
    send("#{service}_shorten", url)
  rescue NoMethodError => e
    "Shortening service #{service} not supported."
  end
end

def regexp_url
  @regexp_url ||= Regexp.new('https?://[^\s,>]*')
  @regexp_url
end

def url_shorten(data,buffer,msg)
  if (msg.empty?)
    return Weechat::WEECHAT_RC_OK
  end
  url = (msg.scan regexp_url).to_s
  short = shortener(url)
  color = Weechat.color(Weechat.config_get_plugin("color"))
  Weechat.print(Weechat.current_buffer, "[url]\t#{color}#{short}");
  return Weechat::WEECHAT_RC_OK
end

def msg_shorten(data,buffer,time,tags,displayed,highlight,prefix,message)
  if (message.empty?)
    return Weechat::WEECHAT_RC_OK
  end

  matchdata = message.match(regexp_url)
  return Weechat::WEECHAT_RC_OK unless matchdata

  url = matchdata[0].to_s
  maxlen = Weechat.config_get_plugin("maxlen").to_i
  return Weechat::WEECHAT_RC_OK if url.length < maxlen

  short = shortener(url)
  color = Weechat.color(Weechat.config_get_plugin("color"))
  Weechat.print(buffer, "[url]\t%s%s" % [color, short])
  return Weechat::WEECHAT_RC_OK
end
