require 'parser/current'
require 'fileutils'
require 'json'
require 'logger'

require_relative 'ruby_ast_gen/version'
require_relative 'ruby_ast_gen/node_handling'

module RubyAstGen

  @logger = Logger.new(STDOUT)

  def self.logger
    @logger
  end

  # Main method to parse the input and generate the AST output
  def self.parse(opts)
    input_path = opts[:input]
    output_dir = opts[:output]
    exclude_regex = Regexp.new(opts[:exclude])
    self.setup_logger(opts[:log])

    FileUtils.mkdir_p(output_dir)

    if File.file?(input_path)
      process_file(input_path, output_dir, exclude_regex, input_path)
    elsif File.directory?(input_path)
      process_directory(input_path, output_dir, exclude_regex)
    else
      puts "Error: #{input_path} is neither a file nor a directory."
      exit 1
    end
  end

  private

  # Process a single file and generate its AST
  def self.process_file(file_path, output_dir, exclude_regex, base_dir)
    # Get the relative path of the file to apply exclusion rules
    relative_path = file_path.sub(%r{^.*\/}, '')
    relative_input_path = file_path.sub("#{base_dir}/", '')
    # Skip if the file matches the exclusion regex
    if exclude_regex && exclude_regex.match?(relative_input_path)
      @logger.info "Excluding: #{relative_input_path}"
      return
    end
    
    return unless ruby_file?(file_path) # Skip if it's not a Ruby-related file

    begin
      ast = parse_file(file_path, relative_input_path)
      return unless ast

      output_path = File.join(output_dir, "#{relative_path}.json")

      # Write the AST as JSON to the output file
      File.write(output_path, JSON.pretty_generate(ast))
      @logger.info "Processed: #{relative_input_path} -> #{output_path}"
    rescue StandardError => e
      @logger.error "'#{relative_input_path}' - #{e.message}"
    end
  end

  def self.process_directory(dir_path, output_dir, exclude_regex)
    Dir.glob("#{dir_path}/**/*").each do |path|
      next unless File.file?(path) && ruby_file?(path)

      relative_dir = path.sub("#{dir_path}/", '')
      if exclude_regex.match?(relative_dir)
        @logger.info "Excluding: #{relative_dir}"
        next
      end
      # Create mirrored directory structure in output
      relative_path = path.sub(dir_path, '')
      output_subdir = File.join(output_dir, File.dirname(relative_path))
      FileUtils.mkdir_p(output_subdir)

      process_file(path, output_subdir, exclude_regex, dir_path)
    end
  end

  def self.parse_file(file_path, relative_input_path)
    code = File.read(file_path)
    buffer = Parser::Source::Buffer.new(file_path)
    buffer.source = code
    parser = Parser::CurrentRuby.new
    ast = parser.parse(buffer)
    return unless ast
    json_ast = NodeHandling::ast_to_json(ast, code, file_path: relative_input_path)
    json_ast[:file_path] = file_path
    json_ast[:rel_file_path] = relative_input_path
    json_ast
  rescue Parser::SyntaxError => e
    @logger.error "Failed to parse #{file_path}: #{e.message}"
    nil
  end

  def self.ruby_file?(file_path)
    ext = File.extname(file_path)
    ['.rb', '.gemspec', 'Rakefile'].include?(ext) || file_path.end_with?('.rb')
  end

  def self.setup_logger(level)
    case level.downcase
    when 'debug'
      @logger.level = Logger::DEBUG
    when 'info'
      @logger.level = Logger::INFO
    when 'warn'
      @logger.level = Logger::WARN
    when 'error'
      @logger.level = Logger::ERROR
    when 'fatal'
      @logger.level = Logger::FATAL
    else
      @logger.level = Logger::WARN
    end
  end
end
