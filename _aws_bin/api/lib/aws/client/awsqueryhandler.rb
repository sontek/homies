require 'rubygems'
require 'json'
require 'aws/client/handler'
require 'aws/client/querystringmap'

module AWS
  module Client

    #
    # Processes requests using the AWS/QUERY protocol.
    #
    # Copyright:: Copyright (c) 2008 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class AwsQueryHandler < Handler

      def before(job)
        query_string_map = QueryStringMap.new(job.request[:value])
        query_string_map['Action'] = job.request[:operation_name].to_s
        query_string_map['ContentType'] = 'JSON'

        job.request[:query_string_map] = query_string_map
        job.request[:http_verb] = 'POST'
      end

      def after(job)
        operation_name = job.request[:operation_name]

        reply = job.reply

        json_result = nil
        begin
          puts reply[:value] if job.show_json
          json_result = JSON.parse(reply[:value])
        rescue
          code = reply[:http_status_code]
          message = reply[:http_status_message]

          raise "#{code} : #{message}" unless code.to_i == 200
          raise "Failed parsing response: #{$!}\n"
        end

        reply[:value] = get_value(operation_name, json_result)
      end

     private
      def get_value(operation_name, json_result)
        # If there was an error, unwrap it and return
        return {"Error" => json_result["Error"]} if json_result["Error"]

        # Otherwise unwrap the valid response
        json_result = json_result["#{operation_name}Response"]
        json_result = json_result["#{operation_name}Result"]
        return json_result

      end
    end

  end
end
