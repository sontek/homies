require 'optparse'
require 'json'

module AWS
  module Client
    class CommandLine
      attr_accessor :default_filter, :empty_response, :error_handler, :result_key, :service_call
      
      def initialize
        # Filters map
        @output_filters = {}
        
        # Additional command line options
        @options = {}
 
        # Display Exclusions
	@display_exclusions = []
        
        # Service call parameters
        @params = {}
        
        # Initialize the options parser
        init_parser

        # Yield this instance to a block
        yield self if block_given?
      end
      
      # Assign a block to run for a particular output value 
      def filter(key, &block)
        @output_filters[key] = block if block_given?
      end

      def exclude(key)
        @display_exclusions << key
      end
      
      # Add custom options to the parser
      def option(*opts, &block)
        @parser.on(*opts, &block)
      end
      
      # Store params for the service call
      def service_param(key, value)
        @params[key] = value
      end
      
      # Perform the service call
      def run(args=nil)
        begin
          raise "ERROR - No service call configured" if @service_call.nil?
          
          parse_params(args)

          response = @service_call.call(@params)
          
          return if @options[:show_json]

          if response.nil? || (response.respond_to?(:empty?) && response.empty?)
            if (@empty_response.nil? || @empty_response.empty?)
              puts "Empty response from service."
            else
              puts @empty_response
            end
          elsif !response['Error'].nil?
            print_error(response['Error'])
          else
            results = response[@result_key] || response
            print_results(results)
          end
        rescue
          puts $!
        end
      end

      def is_numeric?(s)                                                                                                                                                                        
        !!Float(s) rescue false                                                                                                                                                                 
      end                                                                                                                                                                                       
                                                                                                                                                                                                
      # Convert epoch time to a format the service requires                                                                                                                                     
      def convert_time(timevalue)
        if (is_numeric?(timevalue))
          Time.at(timevalue.to_i).utc.strftime('%Y-%m-%dT%H:%M:%SZ')
        else
          begin
            Time.parse(timevalue).utc.strftime('%Y-%m-%dT%H:%M:%SZ')
          rescue
            raise "Invalid time value: '#{timevalue}'. Must be a timestamp (seconds since epoch) or a valid time string (e.g. YYYY-MM-DDTHH:MM:SSZ)"
          end
        end
      end

    private

      # Parse parameters from any CLI args, can be overridden
      def parse_params(args)
        @parser.parse(args) unless args.nil?
        @params[:show_json] = @options[:show_json]
      end
      
      # Configures the default option parser with options available to all
      # CLIs
      def init_parser
        @parser = OptionParser.new do |opts|
          opts.on('-j', '--show-json', 'Show the raw JSON response') do |json|
            @options[:show_json] = json
          end
          opts.summary_width = 50
        end
      end

      # Print an error message
      def print_error(error={})
        if !@error_handler.nil?
          @error_handler.call(error)
        else
          puts "Service returned an error."
          puts "Type: #{error['Type']}" if error['Type']
          puts "Code: #{error['Code']}" if error['Code']
          puts "Details: #{error['Details']}" if error['Details']
          puts "Message: #{error['Message']}" if error['Message']
        end
      end
      
      # Print the results
      def print_results(results)
        if results.nil? || (results.respond_to?(:empty?) && results.empty?)
          puts @empty_response
        elsif results.kind_of? Array
          print_headers(results.first)
          
          results.each do |result|
            print_result(result)
          end
        else
          print_headers(results)
          print_result(results)
        end
      end

      # Prints the keys from the result objects
      def print_headers(results)
        return if results.nil?

        if results.respond_to?(:to_hash)       
          headers = results.to_hash.keys.sort.reject{|e| @display_exclusions.include?(e)}.join(@output_delimiter || ' | ')
          puts headers
          headers.length.times do print '-' end
          print "\n"
        end
      end
      
      # Print the result, if object can be converted to a Hash, print the
      # values with a delimiter, otherwise just print the result
      def print_result(result)
        if !@default_filter.nil?
          puts @default_filter.call(result)
        elsif result.respond_to?(:to_hash)
          result_output = []

          result.to_hash.keys.sort.reject{|e| @display_exclusions.include?(e)}.each do |key|
            val = result[key]
            
            output = val.nil? ? 
                       'N/A' : 
                       @output_filters.key?(key) ? @output_filters[key].call(val) : val
                       
            result_output << output
          end

          puts result_output.join(@output_delimiter || ' | ')
        elsif result.respond_to?(:to_a)
          puts result.to_a.join(', ')
        else
          puts result
        end
      end        
      
    end
  end
end
