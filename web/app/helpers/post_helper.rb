module PostHelper
  def quote(text)
    text.lines.map { |ln|
      "> #{ln}"
    }.join("\n")
  end
end
