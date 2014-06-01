require 'uri'
require 'aws/client/identityhandler'
require 'aws/client/httpdestinationhandler'
require 'aws/client/awsqueryhandler'
require 'aws/client/v0signaturehandler'
require 'aws/client/v1signaturehandler'
require 'aws/client/v2signaturehandler'
require 'aws/client/awsqueryurihandler'
require 'aws/client/httphandler'

module AWS
  module Client

    #
    # Constructs a handler chain appropriate for issuing requests using the AWS/QUERY protocol.
    # This class is deprecated in favor of the use of the AwsQuery class.  Clients generated with
    # up-to-date versions of the generator will no longer depend on this class.
    #
    # Copyright:: Copyright (c) 2008 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class AwsQueryChainHelper < Array

      @@identity_arg_keys = [:aws_access_key, :aws_secret_key, :http_authorization, :http_client_x509_cert, :http_client_x509_key]


      #
      # Creates a chain.  Possible arguments are:
      # [:endpoint]
      #   The HTTP URL at which the service is located.
      # [:signature_algorithm]
      #   The AWS signature version to be used to sign outgoing requests.  Current choices are:
      #     :V0 :V1 :V2
      # [:aws_access_key]
      #   An AWS access key to associate with every outgoing request.
      #   This parameter is optional and may be specified on a per-request basis as well.
      # [:aws_secret_key]
      #   An AWS secret key to associate with every outgoing request.
      #   This parameter is optional and may be specified on a per-request basis as well.
      # [:ca_file]
      #   A Certificate Authority file to pass to the HttpHandler.
      # [:verbose]
      #   A verbosity flag to pass to the HttpHandler.
      #
      def initialize(args)
        # remap args, add defaults where necessary:

        # support deprecated :uri mechanism of specifying service endpoint
        args[:endpoint] = args[:uri] if args[:endpoint].nil?

        # default to V2 if no algorithm is specified
        args[:signature_algorithm] = :V2 if args[:signature_algorithm].nil?

        # support the deprecated mechanism for specifying AWS account information
        args[:aws_access_key] = args[:access_key] if !args.has_key?(:aws_access_key) && args.has_key?(:access_key)
        args[:aws_secret_key] = args[:secret_key] if !args.has_key?(:aws_secret_key) && args.has_key?(:secret_key)

        # build up the chain:

        # allow user to preload identity attributes to be used on all requests
        identity_args = {}
        
        @@identity_arg_keys.each {|k|
          identity_args[k] = args[k] if args.has_key?(k)
        }
        
        self << IdentityHandler.new(identity_args) unless identity_args.empty?

        # set the remote endpoint
        self << HttpDestinationHandler.new(args[:endpoint])

        # use the AwsQuery protocol
        self << AwsQueryHandler.new

        # select a signing algorithm
        case args[:signature_algorithm].to_sym
        when :V0
          self << V0SignatureHandler.new
        when :V1
          self << V1SignatureHandler.new
        when :V2
          self << V2SignatureHandler.new
        end

        # collect the query string and update the destination URL
        self << AwsQueryUriHandler.new

        # make connection over HTTP
        self << HttpHandler.new( {:ca_file => args[:ca_file], :verbose => args[:verbose]} )
      end

    end

  end
end
