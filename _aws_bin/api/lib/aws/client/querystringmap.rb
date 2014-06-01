require 'aws/client/urlencoding'

module AWS
  module Client

    #
    # A hash containing query string parameters that produces a query string via to_s.
    # Also consumes hashes representing hierarchies of data to encode as query parameters.
    #
    # Copyright:: Copyright (c) 2008 Amazon.com, Inc. or its affiliates.  All Rights Reserved.
    #
    class QueryStringMap < Hash
      # Instantiate a QueryStringMap with the contents of the specified hash.  If no hash is provided,
      # an empty map is created.
      def initialize(hash = {})
        add_flattened(hash)
      end

      # Returns the query string representation of this map by collapsing its key-value pairs into URL parameters.
      def to_s
        qstr = ''
        isFirst = true
        each_pair { |k,v|
          if isFirst then
            isFirst = false
          else
            qstr << '&'
          end
          qstr << UrlEncoding.encode(k.to_s)
          unless(v == "" or v.nil?) then
            qstr << '='
            qstr << UrlEncoding.encode(v.to_s)
          end
        }
        return qstr
      end

      private
     
      def add_flattened(hash)
        stack = []
        add_flattened_helper(stack, hash)
      end

      def add_flattened_helper(stack, obj)
        return if obj.nil?
    
        case obj
          when Hash then

            non_nil_member = false

            obj.each_pair { |k,v|
              stack.push(k)
              add_flattened_helper(stack, v)
              stack.pop
              non_nil_member = true if not v.nil?
            }

            # empty hash, hash with only nil members
            unless(non_nil_member) then
              self[get_key(stack)] = "" unless stack.empty?
            end


          when Array then

            # Do artificial list member wrapping (Client requires this level of indirection, but doesn't validate the member name)
            stack.push("member")

            non_nil_member = false

            obj.each_index { |i|
              v = obj[i]
              stack.push(i + 1) # query string arrays are 1-based
              add_flattened_helper(stack, v)
              stack.pop
              non_nil_member = true if not v.nil?
            }

            stack.pop

            # empty list, or list with only nil members
            self[get_key(stack)] = "" unless (non_nil_member)
          else
            # this works for symbols also, because sym.id2name == sym.to_s
            self[get_key(stack)] = obj.to_s
        end
      end

      def get_key(stack)
        key = ''
        stack.each_index { |i|
          key << '.' if(i > 0)
          key << stack[i].to_s
        }
        return key
      end

    end

  end
end
