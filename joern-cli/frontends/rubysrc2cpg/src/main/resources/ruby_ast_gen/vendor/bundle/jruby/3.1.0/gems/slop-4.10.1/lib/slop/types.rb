module Slop
  # Cast the option argument to a String.
  class StringOption < Option
    def call(value)
      value.to_s
    end
  end

  # Cast the option argument to a symbol.
  class SymbolOption < Option
    def call(value)
      value.to_sym
    end
  end

  # Cast the option argument to true or false.
  # Override default_value to default to false instead of nil.
  # This option type does not expect an argument. However, the API
  # supports value being passed. This is to ensure it can capture
  # an explicit false value
  class BoolOption < Option
    attr_accessor :explicit_value

    FALSE_VALUES = [false, 'false', 'no', 'off', '0'].freeze
    TRUE_VALUES = [true, 'true', 'yes', 'on', '1'].freeze
    VALID_VALUES = (FALSE_VALUES + TRUE_VALUES).freeze

    def valid?(value)
      # If we don't want to validate the type, then we don't care if the value
      # is valid or not. Otherwise we would prevent boolean flags followed by
      # arguments from being parsed correctly.
      return true unless config[:validate_type]

      return true if value.is_a?(String) && value.start_with?("--")
      value.nil? || VALID_VALUES.include?(value)
    end

    def call(value)
      self.explicit_value = value
      !force_false?
    end

    def value
      if force_false?
        false
      else
        super
      end
    end

    def force_false?
      FALSE_VALUES.include?(explicit_value)
    end

    def default_value
      config[:default] || false
    end

    def expects_argument?
      false
    end
  end
  BooleanOption = BoolOption

  # Cast the option argument to an Integer.
  class IntegerOption < Option
    INT_STRING_REGEXP = /\A[+-]?\d+\z/.freeze

    def valid?(value)
      value =~ INT_STRING_REGEXP
    end

    def call(value)
      value.to_i
    end
  end
  IntOption = IntegerOption

  # Cast the option argument to a Float.
  class FloatOption < Option
    FLOAT_STRING_REGEXP = /\A[+-]?(?:0|[1-9]\d*)(?:\.\d*)?(?:[eE][+-]?\d+)?\z/.freeze

    def valid?(value)
      value =~ FLOAT_STRING_REGEXP
    end

    def call(value)
      value.to_f
    end
  end

  # Collect multiple items into a single Array. Support
  # arguments separated by commas or multiple occurences.
  class ArrayOption < Option
    def call(value)
      @value ||= []
      if delimiter
        @value.concat value.split(delimiter, limit)
      else
        @value << value
      end
    end

    def default_value
      config[:default] || []
    end

    def delimiter
      config.fetch(:delimiter, ",")
    end

    def limit
      config[:limit] || 0
    end
  end

  # Cast the option argument to a Regexp.
  class RegexpOption < Option
    def call(value)
      Regexp.new(value)
    end
  end

  # An option that discards the return value, inherits from Bool
  # since it does not expect an argument.
  class NullOption < BoolOption
    def null?
      true
    end
  end
end
