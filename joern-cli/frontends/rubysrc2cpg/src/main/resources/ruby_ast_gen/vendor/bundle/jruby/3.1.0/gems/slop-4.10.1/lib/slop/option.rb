module Slop
  class Option
    DEFAULT_CONFIG = {
      help: true,
      tail: false,
      underscore_flags: true,
      required: false,
    }

    # An Array of flags this option matches.
    attr_reader :flags

    # A custom description used for the help text.
    attr_reader :desc

    # A Hash of configuration options.
    attr_reader :config

    # An Integer count for the total times this option
    # has been executed.
    attr_reader :count

    # A custom proc that yields the option value when
    # it's executed.
    attr_reader :block

    # The end value for this option.
    attr_writer :value

    def initialize(flags, desc, **config, &block)
      @flags  = flags
      @desc   = desc
      @config = DEFAULT_CONFIG.merge(config)
      @block  = block
      reset
    end

    # Reset the option count and value. Used when calling .reset
    # on the Parser.
    def reset
      @value = nil
      @count = 0
    end

    # Since `call()` can be used/overriden in subclasses, this
    # method is used to do general tasks like increment count. This
    # ensures you don't *have* to call `super` when overriding `call()`.
    # It's used in the Parser.
    def ensure_call(value)
      @count += 1

      if value.nil? && expects_argument?
        if default_value
          @value = default_value
        elsif !suppress_errors?
          raise Slop::MissingArgument.new("missing argument for #{flag}", flags)
        end
      else
        if validate_type? && !valid?(value) && !suppress_errors?
          raise Slop::InvalidOptionValue.new("invalid value for #{flag}", flags)
        end

        @value = valid?(value) && call(value)
      end

      block.call(@value) if block.respond_to?(:call)
    end

    # This method is called immediately when an option is found.
    # Override it in sub-classes.
    def call(_value)
      raise NotImplementedError,
        "you must override the `call' method for option #{self.class}"
    end

    # By default this method does nothing. It's called when all options
    # have been parsed and allows you to mutate the `@value` attribute
    # according to other options.
    def finish(_result)
    end

    # Override this if this option type does not expect an argument
    # (i.e a boolean option type).
    def expects_argument?
      true
    end

    # Override this if you want to ignore the return value for an option
    # (i.e so Result#to_hash does not include it).
    def null?
      false
    end

    # Returns the value for this option. Falls back to the default (or nil).
    def value
      @value || default_value
    end

    # Returns the default value for this option (default is nil).
    def default_value
      config[:default]
    end

    # Returns true if we should ignore errors that cause exceptions to be raised.
    def suppress_errors?
      config[:suppress_errors]
    end

    # Returns true if an exception should be raised when this option isn't supplied.
    def required?
      config[:required]
    end

    # Returns true if an exception should be raised when this option value can't
    # be parsed into the desired type or does not conform to the expected type's
    # format
    def validate_type?
      config[:validate_type] || config[:validate_types]
    end

    # Returns all flags joined by a comma. Used by the help string.
    def flag
      flags.join(", ")
    end

    # Returns the last key as a symbol. Used in Options.to_hash.
    def key
      key = config[:key] || flags.last.sub(/\A--?/, '')
      key = key.tr '-', '_' if underscore_flags?
      key.to_sym
    end

    # Override this if you want to provide a custom validator for a type. This
    # method must return whether the provided value is valid for the current
    # argument's type
    def valid?(value)
      true
    end

    # Returns true if this option should be displayed with dashes transformed into underscores.
    def underscore_flags?
      config[:underscore_flags]
    end

    # Returns true if this option should be displayed in help text.
    def help?
      config[:help]
    end

    # Returns true if this option should be added to the tail of the help text.
    def tail?
      config[:tail]
    end

    # Returns 1 if this option should be added to the tail of the help text.
    # Used for sorting.
    def tail
      tail? ? 1 : -1
    end

    # Returns the help text for this option (flags and description).
    def to_s(offset: 0)
      "%-#{offset}s  %s" % [flag, desc]
    end
  end
end
