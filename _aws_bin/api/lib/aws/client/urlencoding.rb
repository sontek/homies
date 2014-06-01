require 'cgi'

module AWS
  module Client

    #
    # Performs AWS's preferred method of URLEncoding.
    #
    # Copyright:: Copyright (c) 2008 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class UrlEncoding
      # Convert a string into URL encoded form.
      def UrlEncoding.encode(plaintext)
        CGI.escape(plaintext.to_s).gsub("+", "%20").gsub("%7E", "~")
      end
    end

  end
end

