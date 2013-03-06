#
# format.rb: Lisp module.
#
# Copyright (C) 2003 Nikolai Weibull <source@pcppopper.org>.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
# Author: Nikolai Weibull <source@pcppopper.org>
#
# Latest Revision: 2003-11-16
#

# This module contains various Lisp facilities that are of general usefulness.
# Lisp's rich string formatting language is implemented in the Format module.
# More modules may be added later.
#
# TODO: the floating point functions are rather horendous
module Lisp
  # This module implements the Common Lisp (format) function and it's language.
  module Format
    # An error, positioned somewhere in the input format.  This is used to be
    # able to show the user exactly where something failed in the input format
    # string.
    module Positioned
      # Create at a given position.
      def initialize(pos = -1)
	@pos = pos
      end

      # The position at which the error occured.
      attr :pos, true
    end

    # An argument was of the wrong type, or there aren't enough arguments. 
    class ArgumentError < ArgumentError; include Positioned; end

    # An index was not valid.  Used for argument jumping errors.
    class IndexError < IndexError; include Positioned; end

    # A parameter was wrong in some sense.
    class ParameterError < ArgumentError; include Positioned; end

    # The format string contained a syntax error.
    class SyntaxError < SyntaxError; include Positioned; end

    # The format string was ended before a parameter could be read.
    class MissingParameterError < SyntaxError; end

    # The format string was ended before a parameter could be completed.
    class IncompleteParameterError < SyntaxError; end

    # The format string was malformed somehow.
    class MalformedError < SyntaxError; end

    # A modifier was wrongly given, or there were too many of the same.
    class ModifierError < SyntaxError; end

    # Represents output for a State object.
    class Output
      # Create a new Output object.
      def initialize(str = '', col = 0)
	@output, @col = str, col
      end

      # Add +str+ to the output.
      def <<(str)
	if str.is_a? Fixnum
	  @output << str.chr
	  # How should tabs be handled here?  They should perhaps be
	  # interpreted as eight (8) characters wide?  This of course depends
	  # on where @col is already at (8 - @col % 8).
	  @col = str == ?\n ? 0 : @col + 1
	else
	  @output << str
	  @col += str.length - (str.rindex(?\n) or 0)
	end
      end

      # Convert this object to its string representation, which is the output
      # gathered so far.
      # TODO: this is rather silly
      def to_s
	@output
      end

      # The current output column.
      attr :col, true
    end

    # This class represents the state of a given Formatter.  It keeps track of
    # output gathered and arguments left to be processed.
    class State
      # Create a state from arguments and a destination output
      def initialize(args, output)
	@args = args
	@arg_pos = 0
	@outputs = [output]
	@case_conv = nil
      end

      # Push a new Output buffer to collect output in.
      def push_output
	@outputs << Output.new('', @outputs.last.col)
      end

      # Pop and return the latest output buffer.
      def pop_output
	@outputs.pop
      end

      # Delegates output to the top-most output buffer.
      def output(str)
	@outputs.last << str
      end

      # Retrieve the current output column.
      def col
	@outputs.last.col
      end

      # Retrieve the latest output buffer.
      def latest_output
	@outputs.last
      end

      # Move +n+ steps forward or backward depending on sign amongst the
      # arguments.  Movement is relative or absolute depending on the boolean
      # value of +relative+.
      def args_move(n = 1, relative = true)
	@arg_pos = relative ? @arg_pos + n : n
	raise IndexError.new,
	  'too few arguments' if @arg_pos > @args.size
	raise IndexError.new,
	  'cannot move past first argument' if @arg_pos < 0
      end
      
      # Get the current argument and move forward one argument.
      def next_arg
	args_move(1, true)
	@args[@arg_pos - 1]
      end

      # Get the previously returned argument, without moving.
      def previous_arg
	args_move(-1, relative)
	next_arg
      end

      # Push back the previously returned argument.
      def push_back_arg
	args_move(-1, true)
      end

      # Get the number of arguments left to process
      def args_left
	@args.size - @arg_pos
      end

      attr :case_conv, true
    end

    # Given a format string, this class lexes it and returns directives it
    # finds.  This includes gathering parameters and modifiers for the coming
    # directives.  The necessary information to create a directive is gathered
    # and then passed to Directives::Factory#build.  Error checking
    # is performed, so that incomplete parameters are caught and malformed
    # modifiers are supressed.
    class Lexer
      @@errorstates = {
	:START => [MissingParameterError,
	  'format string ended before parameter was found'],
	:CHAR => [IncompleteParameterError,
	  'format string ended before character parameter could be read'],
	:INTEGER => [IncompleteParameterError,
	  'format string ended before integer parameter could be read'],
      }

      # Create a new Lexer using the given format string +format+.
      def initialize(format)
	@format = format
	@pos = 0
      end

      # Read the next token, given the previous one in +previous+, and return
      # it.  This may either be a 'real' directive created by
      # Directives::Factory#build, or a Literal created in-line.  If no more
      # tokens remain, +nil+ is returned.
      def next_token(previous)
	if have_more_input
	  ch = next_char
	  if ch == ?~
	    return Directives::Factory.build(params, modifiers, directive,
					     previous, @pos)
	  else
	    literal = Directives::Literal.new(ch.chr)
	    while have_more_input and (ch = peek_char) != ?~
	      literal << next_char.chr
	    end
	    literal << ch.chr if not have_more_input and ch == ?~
	    return [literal, 0]
	  end
	else
	  return nil
	end
      end

    private

      # Returns +true+ if more input exists.
      def have_more_input
	@pos < @format.length
      end

      # Reads next character in input.
      def next_char
	@pos += 1
	@format[@pos - 1]
      end

      # View the next character in input.
      def peek_char
	@format[@pos]
      end

      # Unread previously read character in input.
      def unget_char
	@pos -= 1
      end

      # Read parameters to the coming directive.  Raises various errors
      # sub-classed from PositionedError when given incomplete
      # parameters, e.g. when it runs out of input.
      def params
	params = []
	sign = 1
	value = 0
	state = :START
	while have_more_input
	  ch = next_char
	  case state
	  when :START
	    case ch
	    when ?'
	      state = :CHAR
	    when ?V, ?v
	      params << Parameters::Argument.new(@pos)
	      state = :DONE
	    when ?#
	      params << Parameters::ArgumentCount.new(@pos)
	      state = :DONE
	    when ?,
	      params << Parameters::Default.new(@pos)
	      state = :START
	    when ?+, ?-, ?0, ?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9
	      sign = (ch == ?-) ? -1 : 1
	      value = (ch == ?+ or ch == ?-) ? 0 : (ch - ?0)
	      state = :INTEGER
	    else
		unget_char
		state = :DONE
	    end
	  when :INTEGER
	    if (?0..?9).to_a.include? ch
	      value = value * 10 + (ch - ?0)
	    else
	      unget_char
	      value *= sign
	      params << Parameters::Integer.new(@pos, value)
	      state = :DONE
	    end
	  when :CHAR
	    params << Parameters::Character.new(@pos, ch)
	    state = :DONE
	  when :DONE
	    if ch == ?,
	      state = :START
	    else
	      unget_char
	      break
	    end
	  end
	end
	unless state == :DONE
	  raise @@errorstates[state][0].new(@pos), @@errorstates[state][1]
	end
	return params
      end

      # Read modifiers to the coming directive.  Raises ModifierError if
      # a specific modifier is given more than once.
      def modifiers
	modifiers = []
	while have_more_input
	  ch = next_char
	  if ch == ?: or ch == ?@
	    unless modifiers.include? ch
	      modifiers << ch
	    else
	      raise ModifierError.new(@pos), "duplicate #{ch.chr} modifier"
	    end
	  else
	    unget_char
	    break
	  end
	end
	return modifiers
      end

      # Reads the directive.  Raises MalformedFormatError if no input
      # remains to read the directive from.
      def directive
	unless have_more_input
	  raise MalformedFormatError.new(@pos),
	    'format string ended before directive was found'
	end
	next_char
      end
    end

    # Given a lexer, parses the tokens it constructs.
    # It works, quite simply, by maintaining a stack of nesting levels and
    # directives/tokens are added to the current stack level.  When a directive
    # removes a level of nesting the previous level's last directive gets
    # connected, using Directive#connect.
    class Parser
      # Create a Parser, given a Lexer, +lexer+.
      def initialize(lexer)
	@lexer = lexer
      end

      # Do the actual parsing of the tokens.  Returns a list of the
      # top-level tokens to run.
      def parse
	stacks = [[]]
	level = 0
	until (token = @lexer.next_token(stacks[level>0?level-1:0].last)).nil?
	  directive = token[0]
	  nesting = token[1]
	  ntop = stacks[level].last.nil? ?
	    directive : stacks[level].last.join(directive)
	  stacks[level] << directive
	  if nesting > 0
	    stacks.push []
	    level += 1
	  elsif nesting < 0
	    directives = stacks.pop
	    level -= 1
	    stacks[level].last.connect directives
	  end
	end
	if stacks.size > 1
	  raise SyntaxError.new(stacks[-2].last.pos), 'unterminated directive'
	end
	return stacks[0]
      end
    end

    # Engine for the format Lexer and Parser.  Given an input format string and
    # an initial State, it lets the Parser create the directives and then
    # simply iterates over them, Directive#execute:ing them each.
    class Formatter
      # Create a Formatter from a String +format+ and a State +state+.
      def initialize(format, state)
	@parser, @state = Parser.new(Lexer.new(format)), state
      end

      # Parse and run the result.
      def run
	Format.execute_directives(@state, @parser.parse)
	return @state.latest_output.to_s
      end
    end

    # This module includes format parameter representation classes.  These are
    # passed to directives when they are created.
    module Parameters
      # Base class from which the sub-classes get their behavior.  Every
      # parameter should sub-class from this class.
      class Parameter
	# Create a parameter, found at +pos+ in the input format, and with
	# +value+ as its value - if it has one.
	def initialize(pos, value = nil)
	  @pos, @value = pos, value
	end

	# Retrieve this parameters value in the given State, using +default+ if
	# it is not defined.
	def value(state, default)
	  @value or default
	end

	# Position in the input format that this parameter was found.
	attr :pos
      end

      # Represents the +V+ and +v+ parameters, which retrieve their value from
      # the current argument.
      class Argument < Parameter
	# Retrieve this parameters value by getting the value of the current
	# arguments integral value.
	def value(state, default)
	  begin
	    arg = state.next_arg
	  rescue => e
	    e.pos = @pos if e.respond_to?(:pos) and e.pos == -1
	    raise
	  end
	  if arg.nil?
	    default
	  elsif arg.respond_to? :to_i
	    arg.to_i
	  else
	    raise ArgumentError.new(@pos),
	      'argument not a number or a number string'
	  end
	end
      end

      # Represents the <tt>#</tt> parameter, which retrieves the number of
      # arguments left to process.
      class ArgumentCount < Parameter
	# Retrieve this parameters value by getting the number of arguments
	# left to process.
	def value(state, default)
	  state.args_left
	end
      end

      # Parameter used when the parameter was not specified, so use its default
      # value.
      class Default < Parameter; end

      # Represents integral parameters, specified directly in the input format.
      class Integer < Parameter; end

      # Represents character parameters, specified directly in the input
      # format.
      class Character < Parameter; end
    end

    # This module contains all the formatting directives that are defined in
    # the Lisp formatting embedded language.  The base class for all directives
    # is aptly named Directive, and all directive classes should sub-class from
    # it.
    module Directives
      # An unknown directive was found in the format string.
      class UnknownDirectiveError < SyntaxError; end

      # Represents a string literal in the input format.  This is used for
      # combining running lengths of characters with a single pseudo-directive.
      class Literal < String
	# Outputs the string literal this instance represents to the output
	# stream.
	def execute(state)
	  state.output self
	end

	# If the following directive is also a Literal, its contents is simply
	# added to this ones, and returns self.  Otherwise the other directive
	# is returned.
	def join(other)
	  other.is_a?(Literal) ? self << other : other
	end
      end

      # Base class for formatting directives.  This class makes it easy for
      # sub-classes to access the parameters and modifiers that were given to
      # it when created.
      class Directive
	# Given a set of parameters, modifiers, an owning directive if
	# available, and a position in the output string, create a new
	# Directive.
	def initialize(params, modifiers, top, pos)
	  @params, @modifiers, @pos = params, modifiers, pos
	end

	# This method should be overridden in sub-classes.  It gets called when
	# the directive is to be executed.  It does nothing by default.
	def execute(state); end

	# Join this directive with the following one, given in +other+.  The
	# default is to simply return +other+.  The only known use for this is
	# in SkipWhitespace, where some processing of other is done if it is a
	# Literal.
	def join(other)
	  other
	end

	# The position at which this directive occurs in the input string.
	attr :pos

      protected

	# Read the +param+:th parameter in the Lisp::Format::State +state+,
	# using +default+ as a default value if the parameter was not given.
	def param(param, state, default)
	  param < @params.size ? @params[param].value(state, default) : default
	end

	# Check if the colon (<tt>:</tt>) modifier was given.
	def colon_mod?
	  modifier? ?:
	end

	# Check if the at (<tt>@</tt>) modifier was given.
	def at_mod?
	  modifier? ?@
	end

	# Check if the given +modifier+ was given.  This method should not
	# generally be called, as #colon_mod? and #at_mod? are easier and
	# better to use.
	def modifier?(modifier)
	  @modifiers.include? modifier
	end

	def arg_error(message)
	  raise ArgumentError.new(@pos), message
	end

	def param_error(pnum, message)
	  raise ParameterError.new(@params[pnum].pos), message
	end
      end

      # Super-class for 'printing' directives, namely ~A (Ascii) and ~S
      # (SExpression).  These directives print, in some sense, their argument
      # in a Ruby friendly manner.  This means that their argument is either
      # converted to a string using Object#to_s or Object#inspect.
      class Print < Directive
	# All parameters except +inspect+ are simply passed on to
	# Directive#initialize.  If +inspect+ is true, string arguments are
	# inspected as well as all other objects.
	def initialize(params, modifiers, top, pos, inspect = false)
	  super params, modifiers, top, pos
	  @inspect = inspect
	end

	# Output the given argument as it generally prints in Ruby.  The full
	# form is:
	#   ~mincol,colinc,minpad,padchar:@[AS]
	# with the following interpretations
	# [+mincol+ (0)]
	#   minimum number of columns to output, 
	# [+colinc+ (1)]
	#   number of columns to increase by, until +mincol+ is reached,
	# [+minpad+ (0)]
	#   minimum amount of padding to add (added before +mincol+ is
	#   checked),
	# [+padchar+ (?\s)]
	#   character to pad with,
	# [:]
	#   +nil+ is output as +nil+ (In Lisp, this outputs +nil+ as
	#   <tt>()</tt>, which isn't generally useful in Ruby.  TODO: come up
	#   with better use for this modifier),
	# [@]
	#   padding is done on the right.
	def execute(state)
	  padmethod = at_mod? ? :rjust : :ljust
	  mincol = param(0, state, 0)
	  colinc = param(1, state, 1)
	  minpad = param(2, state, 0)
	  padchar = param(3, state, ?\s).chr
	  arg = state.next_arg
	  # XXX: this needs checking use .to_s here?
	  str = (arg.is_a? String and not @inspect) ? arg.to_s : arg.inspect
	  str = str.send(padmethod, str.length + minpad, padchar)
	  k = ((mincol - str.length) / colinc.to_f).ceil
	  if k > 0
	    str = str.send(padmethod, str.length + colinc * k, padchar)
	  end
	  state.output str
	end
      end

      # Represents the ~A (Ascii) directive.
      class Ascii < Print; end

      # Represents the ~S (S-Expression) directive.
      class SExpression < Print
	# Sets +inspect+ to +true+ in the argument list.
	def initialize(*args)
	  super(*args << true)
	end
      end

      # Super-class for number printing directives, namely ~D (Decimal, ~B
      # (Binary), ~O (Octal), and ~X (Hexadecimal).  The only difference
      # between these directives is the output radix, which defaults to ten
      # (10), which is decimal.
      class Number < Directive
	# Create a Number directive, using the given output radix +radix+.
	def initialize(params, modifiers, top, pos, radix = 10)
	  super params, modifiers, top, pos
	  @radix = radix
	end

	# Output the given argument using its integral representation.  The
	# argument must respond to the :to_int message, or else it isn't
	# considered an integer.  This is the normal way in which Ruby
	# operates.  The full form is 
	#   ~mincol,padchar,commachar,commainterval:@[DBOX]
	# with the following interpretations
	# [+mincol+ (0)]
	#   minimum number of columns to output,
	# [+padchar+ (?\s)]
	#   character to pad with,
	# [+commachar+ (,)]
	#   character to use for number grouping (see : below),
	# [+commainterval+ (3)]
	#   how often to output +commachar+ above,
	# [:]
	#   the number is output, with numbers grouped into +commainterval+
	#   sized groups of numbers, using +commachar+ as separator,
	# [@]
	#   numbers are always output with sign prepended.
	#
	# An ArgumentError is raised if the argument does not respond to the
	# #to_int message.
	def execute(state)
	  mincol = param(0, state, 0)
	  padchar = param(1, state, ?\s).chr
	  commachar = param(2, state, ?,).chr
	  interval = param(3, state, 3)
	  arg = state.next_arg
	  if arg.respond_to? :to_int
	    str = arg.to_int.to_s(@radix)
	    if colon_mod?
	      str.gsub!(/(\d)(?=(\d{#{interval}})+(?!\d))/, "\\1#{commachar}")
	    end
	    str = '+' + str if at_mod? and num >= 0
	    state.output str.rjust(mincol, padchar)
	  else
	    arg_error 'argument is not an integer'
	  end
	end
      end

      # Represents the ~D (Decimal) directive.
      class Decimal < Number; end

      # Represents the ~B (Binary) directive.
      class Binary < Number
	# Set +radix+ to 2.
	def initialize(args)
	  super(*args << 2)
	end
      end

      # Represents the ~O (Octal) directive.
      class Octal < Number
	# Set +radix+ to 8.
	def initialize(args)
	  super(*args << 8)
	end
      end

      # Represents the ~X (Hexadecimal) directive.
      class Hexadecimal < Number
	# Set +radix+ to 16.
	def initialize(args)
	  super(*args << 16)
	end
      end

      # Represents the ~R (Radix) directive.  This outputs numbers in a given
      # radix, or using alternative forms, such as Roman numerals or
      # cardinal/ordinal English numbers.
      class Radix < Directive
	@@names = {
	   1 => 'one',	     2 => 'two',	 3 => 'three',
	   4 => 'four',	     5 => 'five',	 6 => 'six',
	   7 => 'seven',     8 => 'eight',	 9 => 'nine',
	  10 => 'ten',	    11 => 'eleven',	12 => 'twelve',
	  13 => 'thirteen', 14 => 'fourteen',	15 => 'fifteen',
	  16 => 'sixteen',  17 => 'seventeen',	18 => 'eighteen',
	  19 => 'nineteen', 20 => 'twenty',	30 => 'thirty',
	  40 => 'forty',    50 => 'fifty',	60 => 'sixty',
	  70 => 'seventy',  80 => 'eighty',	90 => 'ninety'
	}
	@@illions = %w[ \ 
	  thousand	million		billion		  trillion
	  quadrillion   quintillion	sextillion	  septillion
	  octillion	nonillion	decillion	  undecillion
	  duodecillion  tredecillion	quattuordecillion quindecillion
	  sexdecillion  septendecillion octodecillion     novemdecillion
	  vigintillion
	]
	@@ordinal_ones = %w[
	  \	    first     second      third	      fourth
	  fifth	    sixth     seventh     eight	      ninth
	  tenth	    eleventh  twelfth     thirteenth  fourteenth
	  fifteenth sixteenth seventeenth eighteenth  nineteenth
	]
	@@ordinal_tens = %w[
	  \	    \	      twentieth	  thirtieth fortieth
	  fiftieth  sixtieth  seventieth  eightieth ninetieth
	]
	@@romans = [
	  [1000, 'M'], [900, 'CM'], [500, 'D'], [400, 'CD'], [100, 'C'],
	  [90,  'XC'], [50,   'L'], [40, 'XL'], [10,   'X'], [9,  'IX'],
	  [5,    'V'], [4,   'IV'], [1,   'I']
	]
	@@old_romans = [
	  [1000,  'M'],	[900, 'DCCCC'], [500, 'D'], [400, 'CCCC'],
	  [100,	  'C'],	[90,  'LXXXX'], [50,  'L'], [40,  'XXXX'],
	  [10,	  'X'], [9,   'VIIII'], [5,   'V'], [4,   'IIII'],
	  [1,	  'I']
	]

	# Output the given argument using one of a variety of methods.  Either
	# the argument is output using a specific radix, using cardinal or
	# ordinal English numbers, or Roman numerals.  The full form is
	#   ~radix,mincol,padchar,commachar,commainterval:@R
	# with the same interpretation as for ~D (Decimal / Number), but using
	# the given +radix+ instead.  An ArgumentError is raised if the
	# argument is not an integer, or, in the case of Roman numerals, if the
	# argument is not a Fixnum value.
	#
	# If none of the parameters are given the output instead depends on the
	# combination of modifiers:
	# [<em>no modifiers</em>]
	#   the number is output as a cardinal English number,
	# [:]
	#   the number is output as an ordinal English number,
	# [@]
	#   the number is output as a Roman numeral,
	# [:@]
	#   the number is output as an old Roman numeral.
	#
	# An ArgumentError is raised if the argument is not an integer.
	def execute(state)
	  if @params.size > 0
	    # XXX: may be ugly to modify these
	    n = @params.shift.value
	    Factory.build(@params, @modifiers, nil, @pos, n).execute(state)
	  else
	    arg = state.next_arg
	    if arg.respond_to? :to_int
	      conversion = :cardinal
	      conversion = :ordinal if colon_mod?
	      conversion = :roman if at_mod?
	      conversion = :old_roman if colon_mod? and at_mod?
	      state.output self.send(conversion, arg.to_int)
	    else
	      arg_error 'argument is not an integer'
	    end
	  end
	end

      private

	# Convert the given number to a cardinal English number string.
	def cardinal(n)
	  return 'zero' if n.zero?
	  out = []
	  negative = n < 0
	  n = n.abs
	  power = 0
	  while n > 0
	    before, after = n.divmod(1000)
	    if after > 0
	      block = cardinal_100s(after) + (power.zero? ? '' : ' ')
	      if power < @@illions.size
		block << @@illions[power]
	      else
		block << 'times ten to the ' + (power * 3).to_s + ' power'
	      end
	      out << block
	    end
	    n = before
	    power += 1
	  end
	  negative and out << 'minus'
	  out.reverse.join(', ')
	end

	# Deal with hundreds for cardinals.
	def cardinal_100s(n)
	  out = []
	  q, r = n.divmod(100)
	  out << @@names[q] + ' hundred' if q > 0
	  if r >= 20
	    out << @@names[r / 10 * 10] + (r%10>0 ? '-' + @@names[r % 10] : '')
	  elsif r > 0
	    out << @@names[r]
	  end
	  out.join(' ')
	end

	# Convert the given number to an ordinal English number string.
	def ordinal(n)
	  return 'zeroth' if n.zero?
	  out = []
	  if n < 0
	    out << 'minus'
	    n = n.abs
	  end
	  hundreds, tens_ones = n.divmod(100)
	  if hundreds > 0
	    out << cardinal(hundreds * 100) + (tens_ones.zero? ? 'th' : '')
	  end
	  if tens_ones >= 20
	    tens, ones = tens_ones.divmod(10)
	    if ones.zero?
	      out << @@ordinal_tens[tens]
	    else
	      out << @@names[tens * 10] + '-' + @@ordinal_ones[ones]
	    end
	  elsif tens_ones > 0
	    out << @@ordinal_ones[tens_ones]
	  end
	  out.join(' ')
	end

	# Helper method for roman numerals.
	def roman_helper(n, table)
	  unless n.is_a? Fixnum
	    arg_error 'only Fixnum values can be converted to roman numerals'
	  end
	  out = []
	  table.each do |decimal, roman|
	    q, r = n.divmod(decimal)
	    if q > 0
	      out << roman * q
	      n = r
	    end
	  end
	  out.join('')
	end

	# Convert the given number to a Roman numeral.
	def roman(n)
	  roman_helper(n, @@romans)
	end

	# Convert the given number to an old Roman numeral.
	def old_roman(n)
	  roman_helper(n, @@old_romans)
	end
      end

      # Represents the ~R (Plural) directive.  It outputs English plural
      # suffixes depending on the given argument.
      class Plural < Directive
	# Outputs the given argument as an English plural suffix depending on
	# the arguments value.  If arg is not #eql? to 1, a lowercase +s+ is
	# output, else nothing is output.  The meaning of modifiers are:
	# [:]
	#   backs up one argument before checking,
	# [@]
	#   outputs +y+ or +ies+ instead of +s+ and _nothing_,
	# [:@]
	#   a combination of the two above.
	def execute(state)
	  arg = colon_mod? ? state.previous_arg : state.next_arg
	  if at_mod?
	    state.output((arg.eql? 1) ? 'y' : 'ies')
	  else
	    state.output(?s) unless arg.eql? 1
	  end
	end
      end

      # Represents the ~C (Character) directive.  It outputs a character
      # argument in one of several different representations depending on given
      # modifiers.
      # TODO: UNICODE this shit.
      class Character < Directive
	# Outputs the given argument in one of the ways described in the table
	# below depending on given modifiers:
	# [<tt>no modifiers</tt>]
	#   simply output the character as a normal character,
	# [:]
	#   spells out control-bits on the input character, such as
	#   Control-Meta-X,
	# [@]
	#   outputs the character in such as way that it can be read in by the
	#   Ruby interpreter again as input, using the ?_char_ syntax,
	# [:@]
	#   has the same effect as : only.  The CLTL2 suggests that this
	#   outputs unusual shift keys in a manner to make it easy to locate
	#   them on a keyboard, but since no such standard has arisen, this is
	#   not done.
	def execute(state)
	  arg = state.next_arg
	  arg_error 'argument not a Fixnum' unless arg.is_a? Fixnum
	  ch = arg & 0xff
	  if colon_mod?
	    state.output('Control-') if (ch & 0x7f) < 0x1f
	    state.output('Meta-')    if ch >= 0x7f
	    state.output(ch & 0x7f | 0x60)
	  elsif at_mod?
	    state.output('?' + convert_char(ch))
	  else
	    state.output ch
	  end
	end

      private

	# Convert a character to a escaped character string if possible.
	# XXX: This should perhaps only use \x or \NNN syntax
	def convert_char(ch)
	  if [?\\, ?\n, ?\t, ?\r, ?\f, ?\v, ?\a, ?\e, ?\b, ?\s].include? ch
	    ch.chr.inspect[1..-2]
	  else
	    if (ch & 0x7f) < 0x1f
	      "\\C-" + convert_char(ch | 0x60)
	    elsif ch >= 0x7f
	      "\\M-" + convert_char(ch & 0x7f | 0x60)
	    else
	      ch
	    end
	  end
	end
      end

      # Represents the ~F (Fixed-format floating-point) directive.  This
      # outputs floating point values with various kinds of padding and such.
      class FFFP < Directive
	# Outputs the given argument using a floating-point representation.
	# The full form is
	#   ~w,d,k,overflowchar,padchar@F
	# with the following interpretations
	# [+w+ (+nil+)]
	#   if non-+nil+, the output will be exactly +w+ characters long,
	# [+d+ (+nil+)]
	#   if non-+nil+, this is the number of digits output after the decimal
	#   point (<tt>.</tt>),
	# [+k+ (0)]
	#   scaling factor - the number is first scaled using this value,
	# [+overflowchar+ (+nil+)]
	#   if non-+nil+, this character is used when this directive would
	#   produce output longer than that specified with the +w+ directive,
	# [+padchar+ (?\s)]
	#   character to pad with if +w+ is non-nil and output isn't wide
	#   enough yet,
	# [@]
	#   numbers are always output with sign prepended.
	#
	# An ArgumentError is raised if the argument is not a number or a
	# string that can be converted to a number.
	def execute(state)
	  width = param(0, state, nil)
	  digits = param(1, state, nil)
	  scale = param(2, state, 0)
	  overflowchar = param(3, state, nil)
	  padchar = param(4, state, ?\s)
	  arg = state.next_arg
	  if arg.respond_to? :to_f
	    num = arg.to_f * (10 ** scale)
	    str = (at_mod? and num >= 0) ? '+' : ''
	    str = sign + (digits.nil? ? num.to_s : sprintf("%.#{digits}f",num))
	    str = sign + sprintf("%.#{$1}f", num) if str =~ /e-([0-9]+)$/
	    unless width.nil?
	      if not digits.nil? and width == digits + 1
		str.sub!(/^([+-]?)0\./, '\1.')
	      end
	      str = str.rjust(width, padchar.chr) if str.length < width
	      if str.length > width and digits.nil?
		prec = width - (str.index(/\./) + 1) 
		str = sign + sprintf("%#.#{prec}f", num)
	      end
	      str.sub!(/\.$/, '') if str.length > width and digits.nil?
	      if str.length > width and not overflowchar.nil?
		str = overflowchar.chr * width
	      end
	    end
	    state.output str
	  elsif arg.respond_to? :to_int
	    state.push_back_arg
	    parameters = @params[0].nil? ? [] : [@params[0]]
	    Factory.build(parameters, [], ?D, nil, @pos).execute(state)
	  else
	    arg_error 'argument is not a number or a number string'
	  end
	end
      end

      # Represents the ~E (Exponential floating-point) directive.  This outputs
      # floating point values in what is known as exponential or scientific
      # floating point format, such as 1.0e+3 for 1000.
      class ExpFP < Directive

	# Outputs the argument using exponential floating-point format.  The
	# full form is
	#   ~w,d,e,k,overflowchar,padchar,exponentchar@E
	# with the following interpretations
	# [+w+ (+nil+)]
	#   if non-+nil+, the output will be exactly +w+ characters long,
	# [+d+ (+nil+)]
	#   if non-+nil+, this is the number of digits output after the decimal
	#   point (<tt>.</tt>),
	# [+e+ (+nil+)]
	#   if non-+nil+, the exponent part of the output will be exactly +e+
	#   characters long,
	# [+k+ (1)]
	#   scaling factor - the number is first scaled using this value,
	# [+overflowchar+ (+nil+)]
	#   if non-+nil+, this character is used when this directive would
	#   produce output longer than that specified with the +w+ directive,
	# [+padchar+ (?\s)]
	#   character to pad with if +w+ is non-nil and output isn't wide
	#   enough yet,
	# [+exponentchar+ (?e)]
	#   character to use for exponent divider,
	# [@]
	#   numbers are always output with sign prepended.
	#
	# An ArgumentError is raised if the argument is not a number or a
	# string that can be converted to a number.
	def execute(state)
	  width = param(0, state, nil)
	  digits = param(1, state, nil)
	  edigits = param(2, state, nil)
	  scale = param(3, state, 1)
	  overflowchar = param(4, state, nil)
	  padchar = param(5, state, ?\s).chr
	  exponentchar = param(6, state, ?e).chr
	  arg = state.next_arg
	  if arg.respond_to? :to_f
	    num = arg.to_f
	    sign = (num >= 0 and at_mod?) ? '+' : ''
	    exp = Math.log10(num.abs).floor - (scale - 1)
	    exp_str = exponentchar + (exp >= 0 ? '+' : '-') +
	      (edigits.nil? ? exp.abs.to_s : exp.abs.to_s.rjust(edigits, '0'))
	    if digits.nil? and width.nil? and edigits.nil?
	      str = sign + (num * (10 ** -exp)).to_s + exp_str
	    else
	      if digits.nil?
		prec = width - sign.length -
		  ((num * (10 ** -exp)).to_s.index(/\./) + 1) - exp_str.length
		str = sign + sprintf("%#.#{prec}f", num) + exp_str
	      else
		if scale > 0
		  if scale < digits + 2
		    prec = digits - scale + 1
		  else
		    param_error 3, 'scale must be < digits + 2'
		  end
		else
		  prec = -scale + (digits + scale)
		end
		str = sign + sprintf("%#.#{prec}f", num * (10**-exp)) + exp_str
	      end
	      unless width.nil?
		if scale <= 0 and str.length > width
		  str.sub!(/^([+-]?)0\./, '\1.')
		end
		str = str.rjust(width, padchar) if str.length < width
	      end
	      unless width.nil? and overflowchar.nil?
		if not edigits.nil? and (exp_str.length - 2) > edigits
		  str = overflowchar.chr * width
		end
	      end
	    end
	    state.output str
	  elsif arg.respond_to? :to_int
	    state.push_back_arg
	    parameters = @params[0].nil? ? [] : [@params[0]]
	    Factory.build(parameters, [], ?D, nil, @pos).execute(state)
	  else
	    arg_error 'argument is not a number or a number string'
	  end
	end
      end

      # Represents the ~G (General floating-point) directive.  This outputs its
      # argument using either a format like ~F or ~E depending upon given
      # parameters and the magnitude of the argument.
      class GeneralFP < Directive
	# Outputs the argument using exponential floating-point format.  The
	# algorithm to decide what format to use looks something like
	#   let
	#     10^n-1 <= arg < 10^n, or n = 0 if arg = 0,
	#     ee = e + 2, or ee = 4 if e undefined,
	#     ww = w - ee, or ww = nil if w undefined,
	#     dd = d - n, or dd = max(q, min(n, 7)) where q = number of
	#	characters necessary to print arg without loss of information
	#   in
	#     0 <= dd <= d, and print using
	#	~ww,dd,,overflowchar,padcharF~ee@T
	#     or print using
	#	~w,d,e,k,overflowchar,padchar,exponentcharG
	# Use of the @ modifier in the directives above depends on if it was
	# passed to ~G in the first place.
	#
	# The full form is
	#   ~w,d,e,k,overflowchar,padchar,exponentchar@G
	# with the following interpretations
	# [+w+ (+nil+)]
	#   if non-+nil+, the output will be exactly +w+ characters long,
	# [+d+ (+nil+)]
	#   if non-+nil+, this is the number of digits output after the decimal
	#   point (<tt>.</tt>),
	# [+e+ (+nil+)]
	#   if non-+nil+, the exponent part of the output will be exactly +e+
	#   characters long,
	# [+k+ (1)]
	#   scaling factor - the number is first scaled using this value,
	# [+overflowchar+ (+nil+)]
	#   if non-+nil+, this character is used when this directive would
	#   produce output longer than that specified with the +w+ directive,
	# [+padchar+ (?\s)]
	#   character to pad with if +w+ is non-nil and output isn't wide
	#   enough yet,
	# [+exponentchar+ (?e)]
	#   character to use for exponent divider,
	# [@]
	#   numbers are always output with sign prepended.
	#
	# An ArgumentError is raised if the argument is not a number or a
	# string that can be converted to a number.
	def execute(state)
	  width = param(0, state, nil)
	  digits = param(1, state, nil)
	  edigits = param(2, state, nil)
	  scale = param(3, state, 1)
	  overflowchar = param(4, state, nil)
	  padchar = param(5, state, ?\s)
	  exponentchar = param(6, state, nil)
	  arg = state.next_arg
	  if arg.respond_to? :to_f
	    num = arg.to_f
	    n = num == 0.0 ? 0 : Math.log10(num.abs).floor + 1
	    ee = edigits.nil? ? 4 : edigits + 2
	    ww = w.nil? ? nil : w - ee
	    if d.nil?
	      q = num.to_s.length
	      d = Math.max(q, Math.min(n, 7))
	    end
	    dd = d - n
	    if 0 <= dd and dd <= d
	      state.push_back_arg
	      parameters = [
		Parameters::Integer.new(@pos, ww),
		Parameters::Integer.new(@pos, dd),
		Parameters::Default.new(@pos),
		overflowchar.nil? ?
		  Parameters::Default.new(@pos) : 
		  Parameters::Character.new(@pos, overflowchar),
		padchar.nil? ?
		  Parameters::Default.new(@pos) :
		  Parameters::Character.new(@pos, padchar),
	      ]
	      Factory.build(parameters, @modifiers, ?F, nil,
			    @pos).execute(state)
	      Factory.build([Parameters::Integere.new(@pos, ee)], @modifiers,
			    ?T, nil, @pos).execute(state)
	    else
	      state.push_back_arg
	      Factory.build(@params, @modifiers, ?E, nil, @pos).execute(state)
	    end
	  elsif arg.respond_to? :to_int
	    state.push_back_arg
	    parameters = @params[0].nil? ? [] : [@params[0]]
	    Factory.build(parameters, [], ?D, nil, @pos).execute(state)
	  else
	    arg_error 'argument is not a number or a number string'
	  end
	end
      end

      # Represents the ~$ (Dollars floating-point) directive.  This directive
      # outputs a floating point argument 
      class DollarFP < Directive
	# Outputs the argument using a floating point format that suits dollar
	# values.  The full form is
	#   ~d,n,w,padchar:@$
	# with the following interpretations
	# [+d+ (2)]
	#   number of digits to print after the decimal point (<tt>.</tt>),
	# [+n+ (1)]
	#   number of digits to print before the decimal point (<tt>.</tt>),
	# [+w+ (0)]
	#   minimum width of the field,
	# [+padchar+ (?\s)]
	#   character used to produce right-adjusting padding with,
	# [:]
	#   the sign of the value is output before any padding,
	# [@]
	#   numbers are always output with sign prepended.
	def execute(state)
	  digits = param(0, state, 2)
	  idigits = param(1, state, 1)
	  width = param(2, state, 0)
	  padchar = param(3, state, ?\s)
	  arg = state.next_arg
	  if arg.respond_to :to_int
	    sign = (arg >= 0 ? (at_mod? ? '+' : '') : '-')
	    str = sprintf("%0#{idigits + digits + 1}.#{digits}f", arg.abs)
	    if colon_mod?
	      str = sign + str.rjust(width, padchar.chr)
	    else
	      str = (sign + str).rjust(width, padchar.chr)
	    end
	    state.output str
	  elsif arg.respond_to? :to_i
	    state.push_back_arg
	    parameters = @params[2].nil? ? [] : [@params[2]]
	    Factory.build(?D, parameters, [], nil, @pos).execute(state)
	  else
	    arg_error 'argument is not a number or a number string'
	  end
	end
      end

      # Parent class for character outputting directives, such as ~% (NewLine),
      # ~| (NewPage), and ~~ (Tilde), which output the same character a given
      # number of times.
      class CharacterDirective < Directive
	# Create and set the character to use to +ch+.
	def initialize(params, modifiers, top, pos, ch)
	  super params, modifiers, top, pos
	  @ch = ch
	end

	# Outputs the specific character (depending on the
	# sub-classing directive) a given number of times.  The full form is
	#   ~n[%|~]
	# with the following interpretations
	# [+n+ (1)]
	#   number of times to output the specific character.
	def execute(state)
	  n = param(0, state, 1)
	  state.output(@ch.chr * n)
	end
      end

      # Represents the ~% (Newline) directive.  This outputs a new-line (?\n)
      # character a given number of times.
      class NewLine < CharacterDirective
	# Set the output character to a new-line (?\n).
	def initialize(*args)
	  super(*args << ?\n)
	end
      end

      # Represents the ~% (Freshline) directive.  This outputs a new-line (?\n)
      # character a given number of times, depending on if it is already at the
      # first output column or not.
      class FreshLine < Directive
	# Outputs a new-line character a given number of times depending on if
	# it is already at the first output column or not.  If it is it outputs
	# it the given number minus one (1).  The full form is
	#   ~n&
	# with the following interpretations
	# [+n+ (1)]
	#   number of times to output the specific character (maybe minus one).
	def execute(state)
	  n = param(0, state, 1)
	  return if n.zero?
	  state.output(?\n) if state._col > 0
	  (n - 1).times do state.output(?\n) end
	end
      end

      # Represents the ~| (Newpage) directive.  This outputs a new-page (?\f)
      # character a given number of times.
      class NewPage < CharacterDirective
	# Set the output character to a new-page (?\f).
	def initialize(*args)
	  super(*args << ?\f)
	end
      end

      # Represents the ~~ (Tilde) directive.  This outputs a tilde (~)
      # character a given number of times.
      class Tilde < CharacterDirective
	# Set the output character to a tilde (~).
	def initialize(*args)
	  super(*args << ?~)
	end
      end

      # Represents the ~?\n (Real new-line) directive.  This is used to skip
      # the new-line and any following white-space characters in the input
      # format.  This is useful in long format strings, where the string has to
      # be split into multiple lines without ruining indentation.
      class SkipWhitespace < Directive
	# Join this directive with the following.  This removes any new-line
	# and/or following white-space from the directive that follows, if it
	# is a string Literal.
	def join(other)
	  return other if not other.is_a?(FormatLiteral) or colon_mod?
	  other.sub!(/^[\s\t]+/, '')
	  other[0,0] = '\n' if at_mod?
	  other
	end
      end

      # Represents the ~T (Tabulate) directive.  This tabulates to a given
      # position in the output using white-space.
      class Tabulate < Directive
	# The output is spaced over to a given position, depending on where it
	# already is and parameters given to this directive.
	#   ~colnum,colinc:@T
	# with the following interpretations
	# [+colnum+ (1)]
	#   column to move to,
	# [+colinc+ (1)]
	#   number of columns to space over by if already at or beyond
	#   +colnum+,
	# [@]
	#   performs relative tabulation.  +colnum+ is treated as the column to
	#   begin from (spacing over to it if necessary), and then moves over
	#   to a column that is the smallest multiple of +colinc+.
	#
	# If output is already at or beyond +colnum+, then output is spaced
	# over to column <em>colnum + k * colinc</em>, for the smallest _k_
	# possible.  An example of the @ modifiers effect is for the instance
	# of the ~T directive ~3,8@T, which moves over three columns, and then
	# to the first eight-sized tab-stop.
	def execute(state)
	  colnum = param(0, state, 1)
	  colinc = param(1, state, 1)
	  padchar = ?\s.chr
	  if at_mod?
	    state.output(padchar * colnum)
	    state.output(padchar * (colinc - state.col % colinc))
	  else
	    if state.col < colnum
	      state.output(padchar * (colnum - state.col))
	    elsif colinc > 0
	      k = 1 + (state.col - colnum) / colinc
	      state.output(padchar * ((colnum + k * colinc) - state.col))
	    end
	  end
	end
      end

      # Represents the ~* (Argument jumping) directive.  This moves arbitrarily
      # amongst the arguments passed to the formatting engine.
      class ArgJump < Directive
	# Moves backwards or forwards, relative or absolute, among the
	# formatting arguments.  The full form is
	#   ~n:@*
	# with the following interpretations
	# [+n+ (1)]
	#   the amount of arguments to move or the argument to move to if
	#   moving to an absolute position, indexed from zero (0),
	# [:]
	#   move backwards +n+ arguments
	# [@]
	#   move to the +n+:th argument, using zero-based indexing (absolute).
	def execute(state)
	  n = param(0, state, 1)
	  state.args_move(colon_mod? ? -n : n, !at_mod?)
	end
      end

      # Represents the ~? (Indirection) directive.  This takes the two
      # following arguments and formats them, using the first as the formatting
      # string, and the second as its arguments and inserts it into the output.
      class Indirection < Directive
	# Uses the next argument as a formatting string and then, depending on
	# modifiers specified, uses one of a set of different arguments as
	# input to this formatting.  The full form is
	#   ~@?
	# with the following interpretations
	# [@]
	#   the formatting string is, much like a macro, inserted into the
	#   formatting stream, so to speak, and consumes arguments and so on
	#   from the current state, instead, as per default, consuming a second
	#   argument and reading arguments from it.
	def execute(state)
	  if at_mod?
	    formatter = Formatter.new(state.next_arg, state)
	  else
	    format = state.next_arg
	    state = State.new(state.next_arg, state.latest_output)
	    formatter = Formatter.new(format, state)
	  end
	  formatter.run
	end
      end

      # Represents the ~( (Begin case-conversion) directive.  Everything
      # contained within it and its matching pair ~) (End case-conversion)
      # directive is subject to case conversion, such as upcasing or
      # capitalization.
      class BeginCaseConversion < Directive
	# Outputs the contained output case converted using some method that
	# depends on the combination of modifiers given to this directive.  The
	# full form is
	#   ~:@(
	# with the following interpretations
	# [<em>no modifiers</em>]
	#   output is downcased,
	# [:]
	#   every word is capitalized in the output,
	# [@]
	#   the first word is capitalized in the output,
	# [:@]
	#   output is upcased.
	def execute(state)
	  conv = :DOWN
	  conv = :CAP if colon_mod?
	  conv = :CAP_FIRST if at_mod?
	  conv = :UP if colon_mod? and at_mod?
	  state.case_conv = conv
	  state.push_output
	end
      end

      # Represents the ~) (End case-conversion) directive.  Everything
      # contained within it and its matching pair ~( (Begin case-conversion)
      # directive is subject to case conversion, such as upcasing or
      # capitalization.
      class EndCaseConversion < Directive
	# This does the actual work for the ~( directive, in that it collects
	# all the output between its matching directive (~() and outputs it
	# using one of the conversions set up by the ~( (BeginCaseConversion)
	# directive.
	def execute(state)
	  output = state.pop_output.to_s
	  state.output(
		       case state.case_conv
		       when :DOWN
			 output.downcase
		       when :CAP
			 output.gsub(/\w+/) do |w| w.capitalize end
		       when :CAP_FIRST
			 output.capitalize
		       when :UP
			 output.upcase
		       end
		      )
	end
      end

      # Represents the ~[ (Begin conditional expression) directive.  This is
      # useful to choose among a set of directives depending on arguments and
      # numbers.
      class BeginConditional < Directive
	# Set up some variables and basically pass on to super.
	def initialize(*args)
	  super(*args)
	  if (colon_mod? or at_mod?) and @params.size > 0
	    param_error 0, 'no parameters allowed together with' +
	      ': and/or @ modifiers'
	  end
	  @clauses = []
	  @default = -1
	end

	# Process one of the given clauses, depending on the numeric value of
	# the given argument, or a specified parameter.  The full form is
	#   ~n[clause0~;clause1~:;clause2~]
	# or
	#   ~:[clause0~;clause1~]
	# or
	#   ~@[clause0~]
	# with the following interpretations
	# [+n+ (nil)]
	#   if given, this value will be used to choose a clause instead of
	#   reading an arguments value.  This is only useful if +n+ is in fact
	#   the arguments left parameter (<tt>#</tt>).  An error is raised if
	#   this argument does not #respond_to? :to_int,
	# [:]
	#   instead of choosing a clause by number, the argument is tested as a
	#   boolean value, and if false, then first clause is executed, else
	#   the second one is,
	# [@]
	#   instead of choosing a clause by number, the argument is tested as a
	#   boolean value, and if true, the single clause is executed.
	def execute(state)
	  test = state.next_arg if colon_mod? or at_mod?
	  if colon_mod?
	    c = test ? 1 : 0
	  elsif at_mod?
	    if test
	      state.push_back_arg
	      c = 0
	    end
	  else
	    n = param(0, state, nil)
	    if n.nil?
	      arg = state.next_arg
	      if arg.respond_to? :to_int
		n = arg.to_int
	      else
		arg_error 'argument is not an integral value'
	      end
	    end
	    if n < @clauses.size
	      c = n
	    elsif @default != -1
	      c = @default
	    end
	  end
	  Format.execute_directives(state, @clauses[c]) unless c.nil?
	end

	# Connect a set of directives to this conditional directive.
	# A SyntaxError is raised if multiple clauses have been marked as
	# 'default', or if too many clauses have been specified for a given
	# set of modifiers.
	def connect(directives)
	  @clauses = []
	  clause = []
	  directives.each do |d|
	    if d.is_a? ClauseSeparator
	      @clauses << clause
	      clause = []
	      if d.colon_mod?
		if @default == -1
		  @default = @clauses.size
		else
		  raise SyntaxError.new(d.pos),
		    'default clause has already been set'
		end
	      end
	    elsif d.is_a? EndConditional
	      @clauses << clause
	      break
	    else
	      clause << d
	    end
	  end
	  must = 'two' if colon_mod? and @clauses.size != 2
	  must = 'one' if at_mod? and @clauses.size != 1
	  unless must.nil?
	    raise SyntaxError.new(@pos), "must specify exactly #{must} clauses"
	  end
	end
      end

      # Represents the ~; (Clause separator) directive.  It separates clauses
      # inside conditional and justifying directives (~[...~] and ~<...~>).  It
      # may not appear anywhere else.
      class ClauseSeparator < Directive
	# Check that +top+ is either a BeginConditional or a
	# BeginJustification.  If not, a SyntaxError is raised.
	def initialize(params, modifiers, top, pos)
	  super params, modifiers, top, pos
	  unless top.is_a? BeginConditional
	    raise SyntaxError.new(@pos),
	      '~; directive must be contained within a conditional (~[...~])' +
	      ' or a justification (~<...~>)'
	  end
	end
      end

      # Represents the ~] (End conditional expression) directive.  It ends a
      # conditional expression, and is an error if it appears without a
      # matching opening conditional expression.
      class EndConditional < Directive
	def initialize(params, modifiers, top, pos)
	  super params, modifiers, top, pos
	  unless top.is_a? BeginConditional
	    raise SyntaxError.new(@pos), '~) without matching ~('
	  end
	end
      end

      # Represents the ~{ (Begin iteration) directive.  A given set of
      # directives is used iteratively over a set of arguments (depends on the
      # modifiers specified) a given number of times, or until it runs out of
      # arguments.
      class BeginIteration < Directive
	# Create and set up some private variables
	def initialize(params, modifiers, top, pos)
	  super params, modifiers, top, pos
	  @directives = []
	end

	# Iteratively run the contained directives using sets of arguments
	# depending upon what modifiers where given.  The full form is
	#   ~n:@{...~}
	# with the following interpretations
	# [n]
	#   maximum number of times the iteration should be performed,
	# [<em>no modifiers</em>]
	#   the iteration reads an argument, which must be an Array, and uses
	#   it as the arguments to the contained directives,
	# [:]
	#   the iteration reads an argument, which must be an Array
	#   containing sub-arrays, and uses the sub-arrays as the arguments to
	#   the contained directives, moving to the next one on each iteration,
	# [@]
	#   the iteration uses the rest of the arguments as arguments to the
	#   contained directives,
	# [:@]
	#   the iteration uses the rest of the arguments, which must be Arrays,
	#   using each Array as the set of arguments to the contained
	#   directives.
	def execute(state)
	  n = param(0, state, nil)
	  return if not n.nil? and n == 0
	  if colon_mod? and at_mod?
	    execute_sets(n, state, false)
	  elsif at_mod?
	    execute_args(n, nil, state, false)
	  elsif colon_mod?
	    execute_sets(n, state, true)
	  else
	    execute_args(n, state.next_arg, state, true)
	  end
	end

	# Connect a set of directives to this iteration directive.
	def connect(directives)
	  directives.pop
	  @directives = directives
	end

      private

	# Execute the iteration using sub-lists either from the following
	# argument or from the remaining arguments.
	def execute_sets(n, state, use_next_arg)
	  if use_next_arg
	    sets = state.next_arg
	    arg_error 'argument not an array' unless sets.is_a? Array
	    sets = sets[0...n] unless n.nil?
	  else
	    m = n
	    sets = []
	    while state.args_left > 0 and (m.nil? or m > 0)
	      sets << state.next_arg
	      m -= 1 unless m.nil?
	    end
	  end
	  sets.each do |set|
	    execute_args(nil, set, state, true)
	  end
	end

	# Execute the iteration while there are arguments left and we haven't
	# reached our limit of iterations.
	def execute_args(n, args, state, use_new_state)
	  if use_new_state
	    arg_error 'argument not an array' unless args.is_a? Array
	    state = State.new(args, state.latest_output)
	  end
	  while state.args_left > 0 and (n.nil? or n > 0)
	    Format.execute_directives(state, @directives)
	    n -= 1 unless n.nil?
	  end
	end
      end

      # Represents the ~} (End iteration) directive.  See BeginIteration for an
      # explanation of how these directives work together.
      class EndIteration < Directive
	# Raise a SyntaxError if +top+ is not a BeginIteration.
	def initialize(params, modifiers, top, pos)
	  super params, modifiers, top, pos
	  unless top.is_a? BeginIteration
	    raise SynaxError, '~} without matching ~{'
	  end
	end
      end

      # Factory for directives.  Formatting directives should be created using
      # singleton methods in this class.
      class Factory
	@@directives = {
	  ?A  => [Ascii,		4,  [],		 0],
	  ?S  => [SExpression,		4,  [],		 0],
	  ?D  => [Decimal,		4,  [],		 0],
	  ?B  => [Binary,		4,  [],		 0],
	  ?O  => [Octal,		4,  [],		 0],
	  ?X  => [Hexadecimal,		4,  [],		 0],
	  ?R  => [Radix,		5,  [],		 0],
	  ?P  => [Plural,		0,  [],		 0],
	  ?C  => [Character,		0,  [],		 0],
	  ?F  => [FFFP,			5,  [?:],	 0],
	  ?E  => [ExpFP,		7,  [?:],	 0],
	  ?G  => [GeneralFP,		7,  [?:],	 0],
	  ?$  => [DollarFP,		4,  [],		 0],
	  ?%  => [NewLine,		1,  [?:, ?@],	 0],
	  ?&  => [FreshLine,		1,  [?:, ?@],	 0],
	  ?|  => [NewPage,		1,  [?:, ?@],	 0],
	  ?~  => [Tilde,		0,  [?:, ?@],	 0],
	  ?\n => [SkipWhitespace,	0,  [[?:, ?@]],	 0],
	  ?T  => [Tabulate,		2,  [],		 0],
	  ?*  => [ArgJump,		1,  [[?:, ?@]],	 0],
	  ??  => [Indirection,		0,  [?:],	 0],
	  ?(  => [BeginCaseConversion,	0,  [],		 0],
	  ?)  => [EndCaseConversion,	0,  [?:, ?@],	 0],
	  ?[  => [BeginConditional,	1,  [[?:, ?@]],	 1],
	  ?;  => [ClauseSeparator,	0,  [?@],	 0],
	  ?]  => [EndConditional,	0,  [?@],	-1],
	  ?{  => [BeginIteration,	1,  [],		 1],
	  ?}  => [EndIteration,		0,  [?:, ?@],	-1],
	}

	# Create a directive given a set of paramaters, modifiers, the
	# character representing the directive, possible owning directive, and
	# position in the format string.
	def self.build(params, modifiers, directive, top, pos)
	  @@directives.include? directive or
	    raise UnknownDirectiveError.new(pos), 'unknown format directive'
	  idx = directive.chr.upcase[0]
	  params.size <= @@directives[idx][1] or
	    raise ParameterError.new(pos),
	      'too many parameters given, expected no more than ' +
	      @@directives[idx][1].to_s
	  @@directives[idx][2].each do |illegal|
	    if illegal.is_a? Array and (modifiers == illegal or
	      modifiers.reverse == illegal)
	      raise ModifierError.new(pos),
		'cannot specify both : and @ modifiers'
	    elsif modifiers.include? illegal
	      raise ModifierError.new(pos),
		'cannot specify the ' + mod.chr + 'modifier'
	    end
	  end
	  return [
	    @@directives[idx][0].new(params, modifiers, top, pos),
	    @@directives[idx][3]
	  ]
	end
      end
    end

  private

    # Execute a set of directives in a given state.
    def self.execute_directives(state, directives)
      directives.each do |directive|
	begin
	  directive.execute state
	rescue => e
	  e.pos = d.pos if e.respond_to?(:pos) and e.pos == -1
	  raise
	end
      end
    end
  end

  def Lisp.format(format, *args)
    begin 
      state = Format::State.new(args, Format::Output.new)
      formatter = Format::Formatter.new(format, state)
      formatter.run
    rescue => e
      puts 'Format error: ' + e.message
      puts format
      puts ' ' * (e.pos - 1) + '^' if e.respond_to? :pos
      raise
    end
  end
end

if $0 == __FILE__
  require 'test/unit'
  require 'test/unit/ui/console/testrunner'

  class FormatTest < Test::Unit::TestCase
    def test_decimal
      assert(Lisp.format("~D", 1) == "1")
    end

    def test_decimal_params
      assert(Lisp.format("~5,'-D~6D", 1, 2) == "----1     2")
    end

    def test_decimal_modifiers
      assert(Lisp.format("~5,'-,'.:@D~@D", 1013, 2) == "+1.013+2")
      assert(Lisp.format("~7,,,2:@D~@D", 1013, 2) == " +10,13+2")
    end

    # Tests for ~v, ~V, ~#
  end

  Test::Unit::UI::Console::TestRunner.run(FormatTest)
end

# vim: set sts=2 sw=2 fdn=4 fdm=syntax:
