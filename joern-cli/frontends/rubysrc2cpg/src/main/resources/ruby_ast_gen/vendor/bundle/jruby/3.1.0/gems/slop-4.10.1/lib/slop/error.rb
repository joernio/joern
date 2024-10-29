module Slop
  # Base error class.
  class Error < StandardError
  end

  # Raised when calling `call` on Slop::Option (this
  # method must be overriden in subclasses)
  class NotImplementedError < Error
  end

  # Raised when an option that expects an argument is
  # executed without one. Suppress with the `suppress_errors`
  # config option.
  class MissingArgument < Error
    attr_reader :flags

    # Get all the flags that matches
    # the option with the missing argument
    def initialize(msg, flags)
      super(msg)
      @flags = flags
    end
  end

  # Raised when an unknown option is parsed or when trying to fetch an
  # unexisting option via `Slop::Result#fetch`.
  # Suppress with the `suppress_errors` config option.
  class UnknownOption < Error
    attr_reader :flag

    def initialize(msg, flag)
      super(msg)
      @flag = flag
    end
  end

  # Raised when a required option is *not* parsed.
  # Suppress with the `suppress_errors` config option.
  class MissingRequiredOption < Error
  end

  # Raised when a given option is provided by the user and does not
  # match the expected format for that type. This is only raised if
  # validate_types is set to true.
  class InvalidOptionValue < Error
    attr_reader :flag

    def initialize(msg, flag)
      super(msg)
      @flag = flag
    end
  end
end
