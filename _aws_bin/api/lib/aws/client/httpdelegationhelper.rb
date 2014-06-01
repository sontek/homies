module AWS
  module Client

    class HttpDelegationHelper
      def self.add_delegation_token(delegate_identity, request_identity)
        token = ""
        first = true

        delegate_identity.each do |k,v|
          if(first)
            first = false
          else
            token << ';'
          end
      
          token << "#{k}=#{v}"
        end

        request_identity[:http_delegation] = token if (token.length > 0)
      end
    end

  end
end
