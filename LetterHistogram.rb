class LetterHistogram

  attr_accessor :text

  def initialize(text = "Hello World")
    @text = text

  end

  def calculateFrequencies

    text.downcase!

    x = Hash.new(0)
     @text.each_char { |y| x[y] = x[y]+1}
     x
  end

  def display
   x = calculateFrequencies

   for abc in ("a".."z")
     puts "#{abc}: #{'*' * x[abc]}"

   end

  end

  private :calculateFrequencies

end